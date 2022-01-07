recencyComparison <- function(d) {
  hts_mechs <-
    structure(
      .Data = list(
        indicator_code = c(
          "HTS_INDEX_COM.New.Pos.T",
          "HTS_INDEX_COM.New.Neg.T",
          "HTS_INDEX_FAC.New.Pos.T",
          "HTS_INDEX_FAC.New.Neg.T",
          "HTS_TST.Inpat.Pos.T",
          "HTS_TST.Inpat.Neg.T",
          "HTS_TST.Peds.Pos.T",
          "HTS_TST.Peds.Neg.T",
          "HTS_TST.Maln.Pos.T",
          "HTS_TST.Maln.Neg.T",
          "TB_STAT.N.New.Pos.T",
          "TB_STAT.N.New.Neg.T",
          "PMTCT_STAT.N.New.Pos.T",
          "PMTCT_STAT.N.New.Neg.T",
          "HTS_TST.PostANC1.Pos.T",
          "HTS_TST.PostANC1.Neg.T",
          "VMMC_CIRC.Pos.T",
          "VMMC_CIRC.Neg.T",
          "HTS_TST.STI.Pos.T",
          "HTS_TST.STI.Neg.T",
          "HTS_TST.EW.Pos.T",
          "HTS_TST.EW.Neg.T",
          "HTS_TST.VCT.Pos.T",
          "HTS_TST.VCT.Neg.T",
          "HTS_TST.MobileCom.Pos.T",
          "HTS_TST.MobileCom.Neg.T",
          "HTS_TST.Other.Pos.T",
          "HTS_TST.Other.Neg.T",
          "HTS_TST.OtherCom.Pos.T",
          "HTS_TST.OtherCom.Neg.T",
          "HTS_RECENT.IndexCom.T",
          "HTS_RECENT.IndexFac.T",
          "HTS_RECENT.Inpat.T",
          "HTS_RECENT.TB.T",
          "HTS_RECENT.PMTCT_STAT.T",
          "HTS_RECENT.PostANC1.T",
          "HTS_RECENT.VMMC.T",
          "HTS_RECENT.STI.T",
          "HTS_RECENT.EW.T",
          "HTS_RECENT.VCT.T",
          "HTS_RECENT.MobileCom.T",
          "HTS_RECENT.Other.T",
          "HTS_RECENT.OtherCom.T"
        ),
        hts_recency_compare = c(
          "Community - Index",
          "Community - Index",
          "Facility - Index",
          "Facility - Index",
          "Facility - Inpatient",
          "Facility - Inpatient",
          "Facility - Pediatric",
          "Facility - Pediatric",
          "Facility - Malnutrition",
          "Facility - Malnutrition",
          "Facility - TB Clinic",
          "Facility - TB Clinic",
          "Facility - PMTCT ANC1 Only",
          "Facility - PMTCT ANC1 Only",
          "Facility - PMTCT Post ANC1",
          "Facility - PMTCT Post ANC1",
          "Facility - VMMC",
          "Facility - VMMC",
          "Facility - STI Clinic",
          "Facility - STI Clinic",
          "Facility - Emergency Ward",
          "Facility - Emergency Ward",
          "Facility - VCT",
          "Facility - VCT",
          "Community - Mobile",
          "Community - Mobile",
          "Facility - Other PITC",
          "Facility - Other PITC",
          "Community - Other Services",
          "Community - Other Services",
          "Community - Index",
          "Facility - Index",
          "Facility - Inpatient",
          "Facility - TB Clinic",
          "Facility - PMTCT ANC1 Only",
          "Facility - PMTCT Post ANC1",
          "Facility - VMMC",
          "Facility - STI Clinic",
          "Facility - Emergency Ward",
          "Facility - VCT",
          "Community - Mobile",
          "Facility - Other PITC",
          "Community - Other Services"
        )
      ),
      names = c("indicator_code", "hts_recency_compare"),
      row.names = c(NA, 43L),
      class = "data.frame"
    )


  df <- d %>%
    purrr::pluck(., "data") %>%
    purrr::pluck(., "analytics") %>%
    dplyr::inner_join(., hts_mechs, by = "indicator_code") %>%
    dplyr::filter(resultstatus_inclusive == "Positive") %>%
    dplyr::filter(!(
      resultstatus_specific %in% c("Known at Entry Positive", "Status Unknown", "Known Positives")
    )) %>%
    dplyr::group_by(hts_recency_compare, indicator) %>%
    dplyr::summarise(value = sum(target_value)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(indicator, desc(indicator)) %>%
    dplyr::mutate(indicator = ifelse(indicator == "HTS_RECENT", "HTS_RECENT", "HTS_TST")) %>%
    dplyr::mutate(indicator = factor(
      indicator,
      c(
        "HTS_TST",
        "HTS_INDEX",
        "HTS_RECENT",
        "PMTCT_STAT",
        "TB_STAT"
      )
    )) %>%
    dplyr::rename(technical_area = indicator) %>%
    tidyr::pivot_wider(names_from = technical_area, values_from = value,
                       values_fill = list(value = 0))

  can_proceed <- NROW(df) > 0 &
    dplyr::setequal(names(df), c("hts_recency_compare", "HTS_TST", "HTS_RECENT"))

  if (can_proceed) {
    d$data$recency <- df %>%
      dplyr::select("Modality" = hts_recency_compare,
                    HTS_RECENT,
                    "HTS_TST_POS" = HTS_TST) %>%
      dplyr::arrange(Modality) %>%
      dplyr::mutate("HTS_RECENT (%)" = HTS_RECENT / HTS_TST_POS * 100) %>%
      dplyr::mutate(
        HTS_RECENT = format(HTS_RECENT, big.mark = ", ", scientific = FALSE),
        HTS_TST_POS = format(HTS_TST_POS, big.mark = ", ", scientific = FALSE),
        `HTS_RECENT (%)` = format(round(`HTS_RECENT (%)`, 2), nsmall = 2))
  }

  return(d)
}
