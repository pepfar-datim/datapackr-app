prepareSNUSummaryTable <- function(d) {

  can_sum <- d$info$schema %>%
    dplyr::filter(value_type == "integer") %>%
    dplyr::pull(indicator_code) %>%
    unique(.)

  if (d$info$tool == "Data Pack") {
    df <- dplyr::bind_rows(d$data$MER, d$data$SUBNAT_IMPATT)
  } else if (d$info$tool == "OPU Data Pack") {
    df <- d$data$SNUxIM
  }

  if (NROW(df) == 0) {
    return(d)
  }

  snus <- datapackr::getValidOrgUnits(d$info$cop_year) %>%
    dplyr::select(ou, country_name, snu1, psnu = ou_uid, psnu_uid = uid)

  df %<>%
    dplyr::inner_join(snus, by = c("psnuid" = "psnu_uid")) %>%
    dplyr::select(ou, country_name, snu1, psnu, indicator_code, Age, Sex, KeyPop, value) %>%
    dplyr::group_by(ou, country_name, snu1, psnu, indicator_code) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(indicator_code, ou, country_name, snu1, psnu)

  return(df)
}
