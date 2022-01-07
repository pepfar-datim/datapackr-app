comparePrioTables <- function(d) {
  #Comparison is not possible unless both tables exist
  #Normally, this would result because of data not existing already in DATIM
  if (is.null(d$data$prio_table) | is.null(d$data$memo$datim$prio)) {
    return(d)
  }

  prio_dp <- d$data$prio_table %>%
    tidyr::pivot_longer(., cols = 3:dim(.)[2], values_to = "Proposed", names_to = "Prioritization")


  prio_datim <- d$data$memo$datim$prio %>%
    tidyr::pivot_longer(., cols = 3:dim(.)[2], values_to = "Current", names_to = "Prioritization")

  df <- dplyr::full_join(prio_dp, prio_datim) %>%
    dplyr::mutate(Current = dplyr::case_when(is.na(Current) ~ 0,
                                             TRUE ~ Current),
                  Proposed = dplyr::case_when(is.na(Proposed) ~ 0,
                                              TRUE ~ Proposed)) %>%
    dplyr::mutate("Diff" = Proposed - Current,
                  "Identical" = as.character(dplyr::near(Proposed, Current, tol = 1e-5)),
                  "Percent diff" = round(Diff / Current * 100, digits = 1)) %>%
    tidyr::pivot_longer(., cols = -c(Indicator, Age, Prioritization, Identical), names_to = "Data Type") %>%
    dplyr::mutate(`Data Type` = factor(`Data Type`, levels = c("Proposed", "Current", "Diff", "Percent diff"))) %>%
    dplyr::arrange(Indicator, Age, Prioritization, `Data Type`)

  d$data$memo$compare$prio <- df

  return(d)
}
