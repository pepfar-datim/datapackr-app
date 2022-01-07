generateComparisonTable<-function(d,d2_session) {

  psnus <-    d$data$analytics$psnu_uid %>% unique() %>% unlist()

  #Break up into 2048 character URLS (approximately)
  n_requests <-
    ceiling(nchar(paste(
      psnus, sep = "", collapse = ";"
    )) / 2048)
  n_groups <- split(psnus, rep_len(1:n_requests, length(psnus)))

  prios <-
    n_groups %>% purrr::map_dfr(function(x)
      getExistingPrioritization(x, d$info$cop_year, d2_session))


  if (d$info$tool  == "OPU Data Pack") {
    d_datapack <- d$datim$OPU } else {
      d_datapack <- d$datim$MER
    }

  #Not sure why the prioriziations are not the same
  d_datapack <- d$data$analytics %>%
    dplyr::select(-prioritization) %>%
    dplyr::left_join(prios,by=c("psnu_uid")) %>%
    dplyr::select(-upload_timestamp)

  diffWithNAs<-function(x,y) {
    ifelse(is.na(x),0,x) - ifelse(is.na(y),0,y)
  }


  d_datim <- datapackr::getCOPDataFromDATIM(country_uids = d$info$country_uids,
                                            streams = c("mer_targets"),
                                            cop_year = d$info$cop_year,
                                            d2_session = d2_session)
  #Deal with codes versus uids
  d_datim$categoryOptionCombo[d_datim$categoryOptionCombo == "default"] <- datapackr::default_catOptCombo()

  if (NROW(d_datim) > 0) {

    d_datim <-  d_datim  %>%
      dplyr::select(dataElement,period,orgUnit,categoryOptionCombo,attributeOptionCombo,value,deleted) %>%
      dplyr::filter(is.na(deleted)) %>%
      dplyr::select(-deleted) %>%
      datapackr::adorn_import_file(.,
                                   cop_year = d$info$cop_year,
                                   d2_session = d2_session) %>%
      dplyr::select(-prioritization) %>%
      dplyr::left_join(prios,by=c("psnu_uid")) %>%
      dplyr::rename("datim_value" = "target_value") %>%
      dplyr::select(-upload_timestamp)

    d$datim$analytics <- d_datim

  } else {
    d_datim <- d_datapack[0,] %>%
      dplyr::rename("datim_value" = "target_value")
  }

  #Save this analytics object for later use in the memo generation
  d$datim$analytics <- d_datim

  d_compare <- dplyr::full_join(d_datapack, d_datim) %>%
    dplyr::mutate(
      change_type = dplyr::case_when(
        target_value == datim_value ~ "No change",
        is.na(target_value) & !is.na(datim_value) ~ "Deletion",
        !is.na(target_value) & is.na(datim_value) ~ "New value",
        target_value != datim_value ~ "Update"
      )
    ) %>%  dplyr::mutate(datim_value = ifelse(is.na(datim_value),0,datim_value),
                         target_value = ifelse(is.na(target_value),0,target_value)) %>%
    dplyr::mutate(identical = target_value == datim_value,
                  "Diff" = target_value - datim_value) %>%
    dplyr::filter(!identical)%>%
    tidyr::pivot_longer(cols=c(datim_value,target_value,Diff),names_to = "value_type") %>%
    dplyr::mutate(value_type = dplyr::recode(value_type, datim_value = "Current",target_value = "Proposed", Diff = "Difference")) %>%
    dplyr::select(
      "OU" = ou,
      "Country" = country_name,
      "SNU1" = snu1,
      "PSNU" = psnu,
      "Prioritization" = prioritization,
      "Mechanism" = mechanism_code,
      "Partner" = partner_desc,
      "Agency" = funding_agency,
      "Data element" = dataelement_name,
      "Disagg" = categoryoptioncombo_name,
      "Indicator" = indicator,
      "Age" = age,
      "Sex" = sex,
      "KeyPop" = key_population,
      "NumDenom" = numerator_denominator,
      "Support Type" = support_type,
      "HTS Modality" = hts_modality,
      "Value type" = value_type,
      "Value" = value
    ) %>%
    dplyr::mutate(`Agency` = case_when(`Mechanism` %in% c("00000","00001") ~ "Dedupe",
                                       TRUE ~ `Agency`),
                  `Partner` = case_when(`Mechanism` %in% c("00000","00001") ~ "Dedupe",
                                        TRUE ~ `Partner`)
    ) %>%
    dplyr::filter(!stringr::str_detect(Indicator,"AGYW_PREV"))

  d$data$compare<-d_compare

  return(d)
}
