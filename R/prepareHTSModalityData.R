prepareHTSModalityData <- function(d) {

  d <- d %>%
  purrr::pluck("data") %>%
  purrr::pluck("analytics") %>%
  dplyr::filter(!is.na(hts_modality)) %>%
  dplyr::filter(!stringr::str_detect(dataelement_name, "HTS_RECENT")) %>% #Temporary fix for DP-542
  dplyr::filter(!(resultstatus_specific %in% c("Known at Entry Positive", "Known Positives","Status Unknown")))  %>%
  dplyr::group_by(resultstatus_inclusive, hts_modality) %>%
  dplyr::summarise(value = sum(target_value)) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(resultstatus_inclusive, desc(resultstatus_inclusive)) %>%
  dplyr::mutate(resultstatus_inclusive = factor(resultstatus_inclusive, c("Unknown", "Negative", "Positive")))

  return(d)
}
