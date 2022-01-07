getExistingPrioritization <- function(psnus, cop_year, d2_session) {

  period <- paste0(cop_year, "Oct")
  dx <- "r4zbW3owX9n"
  ous <- paste(psnus, sep = "", collapse = ";")

  prios <- datimutils::getAnalytics(dx = "r4zbW3owX9n", pe_f = period, ou = ous, d2_session = d2_session)

  if (is.null(prios)) {
    return(data.frame("psnu_uid" = psnus, "prioritization" = "No Prioritization"))
  }

  #Check for invalid prioritization levels, and throw an error if this occurs
  if (!all(prios$Value %in% c(datapackr::prioritization_dict() %>% dplyr::pull(value), NA))) {
    stop("Invalid prioritization levels detected. Please contact DATIM support.")
  }

  prios %<>%
    dplyr::select(-Data) %>%
    dplyr::rename("psnu_uid" = "Organisation unit",
                  "value" = "Value") %>%
    dplyr::left_join(datapackr::prioritization_dict()) %>%
    dplyr::select(psnu_uid, "prioritization" = "name")

  return(prios)
}
