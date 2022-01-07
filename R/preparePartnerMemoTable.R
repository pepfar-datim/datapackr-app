preparePartnerMemoTable <- function(d, d2_session) {

  inds <- getMemoIndicators(d, d2_session)

  df <- d  %>%
    purrr::pluck("data") %>%
    purrr::pluck("analytics") %>%
    dplyr::filter(!is.na(target_value)) %>%
    dplyr::select(dataelement_id,
                  categoryoptioncombo_id,
                  mechanism_code,
                  funding_agency,
                  partner_desc,
                  value = target_value) %>%
    dplyr::group_by(dataelement_id, categoryoptioncombo_id, mechanism_code, funding_agency, partner_desc) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::mutate(combi = paste0("#{", dataelement_id, ".", categoryoptioncombo_id, "}")) %>%
    plyr::ddply(., plyr::.(mechanism_code, funding_agency, partner_desc),
                function(x)
                  evaluateIndicators(x$combi, x$value, inds)) %>%
    dplyr::rename("Mechanism" = mechanism_code, "Agency" = funding_agency, "Partner" = partner_desc, Value = value) %>%
    dplyr::select(-id, -numerator, -denominator) %>%
    dplyr::mutate(name =  stringr::str_replace_all(name, "^COP2[01] Targets ", "")) %>%
    dplyr::mutate(name = stringr::str_trim(name)) %>%
    tidyr::separate("name", into = c("Indicator", "N_OR_D", "Age"), sep = " ") %>%
    dplyr::mutate(Indicator = case_when(Indicator == "GEND_GBV" & N_OR_D == "Physical" ~
                                          "GEND_GBV Physical and Emotional Violence",
                                        Indicator == "GEND_GBV" & N_OR_D == "Sexual" ~
                                          "GEND_GBV Sexual Violence",
                                        TRUE ~ Indicator)) %>%
    dplyr::select(-"N_OR_D") %>%
    dplyr::mutate(Age = case_when(Age == "15-" ~ "<15",
                                  Age == "15+" ~ "15+",
                                  Age == "18-" ~"<18",
                                  Age == "18+" ~ "18+",
                                  TRUE ~ "Total")) %>%
    dplyr::mutate(Age = case_when(Indicator %in% c("CXCA_SCRN", "OVC_HIVSTAT", "KP_PREV", "PMTCT_EID",
                                                   "KP_MAT", "VMMC_CIRC", "PrEP_NEW", "PrEP_CURR", "GEND_GBV")  ~ "Total",
                                  TRUE ~ Age)) %>%
    tidyr::complete(., tidyr::nesting(Mechanism, Agency, Partner), Indicator, Age, fill = list(Value = 0)) %>%
    tidyr::drop_na()

  df_rows <- memoStructure(cop_year = d$info$cop_year) %>%
    purrr::pluck("row_order") %>%
    dplyr::select(ind, options)

  d_base <- tidyr::crossing(df_rows, dplyr::distinct(unique(df[, c("Mechanism", "Partner", "Agency")]))) %>%
    dplyr::mutate(Value = 0) %>%
    dplyr::rename("Indicator" = ind,
                  Age = options)

  #Calculate totals

  d_totals <- dplyr::bind_rows(d_base,df) %>%
    dplyr::group_by(`Indicator`, `Age`) %>%
    dplyr::summarise(`Value` = sum(`Value`)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(`Partner` = "Total", `Mechanism` = "Total", Agency = "Total")

  #Remove dedupe
  #TODO: Are we dealing with codes of mechanisms here??
  d_partners <- dplyr::filter(df, !(`Mechanism` %in% c("00001", "00000"))) #nolint

  d_indicators <- memoStructure(d$info$cop_year) %>%
    purrr::pluck("row_order") %>%
    dplyr::filter(in_partner_table) %>%
    dplyr::select(ind, options) %>%
    dplyr::mutate(indicator_name = factor(paste(ind, options)))

  #Put totals at the bottom of the table
  partner_levels <- c(sort(unique(df$Partner)), "Total")
  agency_levels <- c(sort(unique(df$Agency)), "Total")

  #Return the final data frame
  d$data$partners_table <-dplyr::bind_rows(df, d_totals)  %>%
    dplyr::mutate(indicator_name = paste(`Indicator`, `Age`)) %>%
    dplyr::mutate(`Label` = indicator_name) %>%
    dplyr::arrange(`Agency`, `Partner`, `Mechanism`, indicator_name) %>%
    dplyr::select(`Agency`, `Partner`, `Mechanism`, `Label`, `Value`) %>%
    tidyr::pivot_wider(names_from = `Label`, values_from = `Value`, values_fill = 0) %>%
    dplyr::select(`Agency`, `Partner`, `Mechanism`, d_indicators$indicator_name) %>%
    dplyr::mutate(`Partner` = factor(Partner, levels = partner_levels),
                  `Agency` = factor(Agency, levels = agency_levels)) %>%
    dplyr::arrange(`Agency`, `Partner`, `Mechanism`)

  return(d)
}
