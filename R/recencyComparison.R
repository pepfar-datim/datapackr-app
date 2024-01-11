recencyComparison <- function(d) {

  d <- recencyComparison_COP23(d)

  d
}

recencyComparison_COP23 <- function(d) {

  hts_inds <- cop23_map_DataPack_DATIM_DEs_COCs %>%
    dplyr::select(indicator_code, hts_modality) %>%
    tidyr::drop_na() %>%
    dplyr::distinct()


  df <- d %>%
    purrr::pluck(., "data") %>%
    purrr::pluck(., "analytics") %>%
    dplyr::filter(resultstatus_inclusive == "Positive") %>%
    dplyr::filter(!(
      resultstatus_specific %in% c("Known at Entry Positive", "Status Unknown", "Known Positives")
    )) %>%
    dplyr::filter(indicator_code %in% c("HTS_RECENT.T", hts_inds$indicator_code)) %>%
    dplyr::mutate(indicator_code = dplyr::case_when(indicator_code == "HTS_RECENT.T"  ~ 'HTS_RECENT',
                                                    TRUE ~ 'HTS_TST_POS')) %>%
    dplyr::group_by(country_name, psnu, indicator_code) %>%
    dplyr::summarise(value = sum(target_value, na.rm = TRUE)) %>%
    tidyr::pivot_wider(names_from = indicator_code, values_from = value,
                       values_fill = list(value = 0))

  can_proceed <- NROW(df) > 0 &&
    setequal(names(df), c("country_name", "psnu", "HTS_RECENT", "HTS_TST_POS"))

  if (can_proceed) {
    d$data$recency <- df %>%
      dplyr::mutate("HTS_RECENT (%)" = HTS_RECENT / HTS_TST_POS * 100) %>%
      dplyr::mutate(
        HTS_RECENT = format(HTS_RECENT, big.mark = ",", scientific = FALSE),
        HTS_TST_POS = format(HTS_TST_POS, big.mark = ",", scientific = FALSE),
        `HTS_RECENT (%)` = format(round(`HTS_RECENT (%)`, 2), nsmall = 2))
  }


  return(d)

}

