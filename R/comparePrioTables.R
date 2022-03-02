comparePrioTables <- function(d) {
  #Comparison is not possible unless both tables exist
  #However, if we have data in the DP and no data in DATIM,
  #We can perform the comparison by assuming everything in DATIM is zero.

  if (is.null(d$data$memo$datim$prio) & !is.null(d$data$prio_table)) {
    d$data$memo$datim$prio <- d$data$prio_table %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.double), function(x) NA_real_))
  }
  #If both tables are NULL, then we have no data to compare at all.
  if (is.null(d$data$memo$datim$prio) & is.null(d$data$prio_table)) {
   return(d)
  }
  #The other case of data in DATIM, but no data in the DataPack does not really
  #seem feasible.

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
    dplyr::arrange(Indicator, Age, Prioritization, `Data Type`) %>%
    dplyr::rename("Value" = value)

  d$data$compare <- df

  return(d)
}
