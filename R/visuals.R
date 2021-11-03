PSNUxIM_pivot <- function(d) {

  pivot <- d  %>%
    purrr::pluck("data") %>%
    purrr::pluck("analytics") %>%
    dplyr::select(indicator,
                  dataelement_name,
                  psnu,
                  prioritization,
                  mechanism_code,
                  partner = partner_desc,
                  agency = funding_agency,
                  value = target_value) %>%
    dplyr::group_by(indicator, dataelement_name, psnu, prioritization, mechanism_code, partner, agency) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(mechanism_code = ifelse(mechanism_code == "HllvX50cXC0", "default", mechanism_code))

  rpivotTable(data =   pivot, rows = c("dataelement_name"),
              vals = "value", aggregatorName = "Integer Sum", rendererName = "Table"
              , width = "70%", height = "700px")
}

memoStructure <- function(cop_year="2020") {

  if (cop_year == "2020") {
  row_order <- tibble::tribble(
    ~ind, ~options, ~in_partner_table,
    "HTS_INDEX", "<15", TRUE,
    "HTS_INDEX", "15+", TRUE,
    "HTS_INDEX", "Total", FALSE,
    "HTS_TST", "<15", TRUE,
    "HTS_TST", "15+", TRUE,
    "HTS_TST", "Total", FALSE,
    "HTS_TST_POS", "<15", TRUE,
    "HTS_TST_POS", "15+", TRUE,
    "HTS_TST_POS", "Total", FALSE,
    "TX_NEW", "<15", TRUE,
    "TX_NEW", "15+", TRUE,
    "TX_NEW", "Total", FALSE,
    "TX_CURR", "<15", TRUE,
    "TX_CURR", "15+", TRUE,
    "TX_CURR", "Total", FALSE,
    "TX_PVLS", "<15", TRUE,
    "TX_PVLS", "15+", TRUE,
    "TX_PVLS", "Total", FALSE,
    "CXCA_SCRN", "Total", TRUE,
    "OVC_SERV", "<18", TRUE,
    "OVC_SERV", "18+", TRUE,
    "OVC_SERV", "Total", FALSE,
    "OVC_HIVSTAT", "Total", TRUE,
    "PMTCT_STAT", "<15", TRUE,
    "PMTCT_STAT", "15+", TRUE,
    "PMTCT_STAT", "Total", FALSE,
    "PMTCT_STAT_POS", "<15", TRUE,
    "PMTCT_STAT_POS", "15+", TRUE,
    "PMTCT_STAT_POS", "Total", FALSE,
    "PMTCT_ART", "<15", TRUE,
    "PMTCT_ART", "15+", TRUE,
    "PMTCT_ART", "Total", FALSE,
    "PMTCT_EID", "Total", TRUE,
    "PP_PREV", "<15", TRUE,
    "PP_PREV", "15+", TRUE,
    "PP_PREV", "Total", FALSE,
    "KP_PREV", "Total", TRUE,
    "KP_MAT", "Total", TRUE,
    "VMMC_CIRC", "Total", TRUE,
    "HTS_SELF", "<15", TRUE,
    "HTS_SELF", "15+", TRUE,
    "HTS_SELF", "Total", FALSE,
    "PrEP_NEW", "Total", TRUE,
    "PrEP_CURR", "Total", TRUE,
    "TB_STAT", "<15", TRUE,
    "TB_STAT", "15+", TRUE,
    "TB_STAT", "Total", FALSE,
    "TB_ART", "<15", TRUE,
    "TB_ART", "15+", TRUE,
    "TB_ART", "Total", FALSE,
    "TB_PREV", "<15", TRUE,
    "TB_PREV", "15+", TRUE,
    "TB_PREV", "Total", FALSE,
    "TX_TB", "<15", TRUE,
    "TX_TB", "15+", TRUE,
    "TX_TB", "Total", FALSE,
    "GEND_GBV", "Total", TRUE)
    }

  if (cop_year == "2021") {
    row_order <- tibble::tribble(
      ~ind, ~options, ~in_partner_table,
      "HTS_INDEX", "<15", TRUE,
      "HTS_INDEX", "15+", TRUE,
      "HTS_INDEX", "Total", FALSE,
      "HTS_TST", "<15", TRUE,
      "HTS_TST", "15+", TRUE,
      "HTS_TST", "Total", FALSE,
      "HTS_TST_POS", "<15", TRUE,
      "HTS_TST_POS", "15+", TRUE,
      "HTS_TST_POS", "Total", FALSE,
      "TX_NEW", "<15", TRUE,
      "TX_NEW", "15+", TRUE,
      "TX_NEW", "Total", FALSE,
      "TX_CURR", "<15", TRUE,
      "TX_CURR", "15+", TRUE,
      "TX_CURR", "Total", FALSE,
      "TX_PVLS", "<15", TRUE,
      "TX_PVLS", "15+", TRUE,
      "TX_PVLS", "Total", FALSE,
      "CXCA_SCRN", "Total", TRUE,
      "OVC_SERV", "<18", TRUE,
      "OVC_SERV", "18+", TRUE,
      "OVC_SERV", "Total", FALSE,
      "OVC_HIVSTAT", "Total", TRUE,
      "PMTCT_STAT", "<15", TRUE,
      "PMTCT_STAT", "15+", TRUE,
      "PMTCT_STAT", "Total", FALSE,
      "PMTCT_STAT_POS", "<15", TRUE,
      "PMTCT_STAT_POS", "15+", TRUE,
      "PMTCT_STAT_POS", "Total", FALSE,
      "PMTCT_ART", "<15", TRUE,
      "PMTCT_ART", "15+", TRUE,
      "PMTCT_ART", "Total", FALSE,
      "PMTCT_EID", "Total", TRUE,
      "PP_PREV", "<15", TRUE,
      "PP_PREV", "15+", TRUE,
      "PP_PREV", "Total", FALSE,
      "KP_PREV", "Total", TRUE,
      "KP_MAT", "Total", TRUE,
      "VMMC_CIRC", "Total", TRUE,
      "HTS_SELF", "<15", TRUE,
      "HTS_SELF", "15+", TRUE,
      "HTS_SELF", "Total", FALSE,
      "PrEP_NEW", "Total", TRUE,
      "PrEP_CURR", "Total", TRUE,
      "TB_STAT", "<15", TRUE,
      "TB_STAT", "15+", TRUE,
      "TB_STAT", "Total", FALSE,
      "TB_ART", "<15", TRUE,
      "TB_ART", "15+", TRUE,
      "TB_ART", "Total", FALSE,
      "TB_PREV", "<15", TRUE,
      "TB_PREV", "15+", TRUE,
      "TB_PREV", "Total", FALSE,
      "TX_TB", "<15", TRUE,
      "TX_TB", "15+", TRUE,
      "TX_TB", "Total", FALSE,
      "GEND_GBV", "Total", TRUE,
      "AGYW_PREV", "Total", FALSE)
  }

col_order <- tibble::tribble(
    ~value, ~name, ~col_order,
    0, "No Prioritization", 7,
    1, "Scale-up: Saturation", 2,
    2, "Scale-up: Aggressive", 3,
    4, "Sustained", 4,
    5, "Centrally Supported", 5,
    6, "Sustained: Commodities", 6,
    7, "Attained", 1,
    8, "Not PEPFAR Supported", 8) %>%
  dplyr::mutate(Prioritization = paste0(value, " - ", name))

  list(row_order = row_order, col_order = col_order)
}

getMemoIndicators <- function(d, d2_session) {
  #Fetch indicators from the COP21 memo group
  #TODO: Make this work for both COP years.!

  if (d$info$cop_year == 2020) {
    ind_group <- "wWi08ToZ2gR"
  } else if (d$info$cop_year == 2021) {
    #TODO: Fix this with the real indicator group once it has been deployed to prod
    ind_group <- "TslxbFe3VUZ"
  } else {
    flog.info("Indicator group was not found")
    return(NULL)
  }
  inds <-
    datimutils::getIndicatorGroups(ind_group,
                                   d2_session = d2_session,
                                   fields = "indicators[id, name, numerator, denominator]")


  if (class(inds) != "data.frame") {
    stop("No indicator metadata  was returned from DATIM")
  }

    inds
}
#Should probably move this to datapackr
preparePrioTable <- function(d, d2_session) {

  df_cols <- memoStructure(cop_year = d$info$cop_year) %>%
    purrr::pluck("col_order")

  df_rows <- memoStructure(cop_year = d$info$cop_year) %>%
    purrr::pluck("row_order") %>%
    dplyr::select(ind, options) %>%
    dplyr::mutate(row_order = dplyr::row_number())

  df_base <- tidyr::crossing(df_rows, dplyr::select(df_cols, name)) %>%
    dplyr::arrange(ind, options, name) %>%
    dplyr::mutate(value = 0) %>%
    dplyr::select("Indicator" = ind,
                  Age = options,
                  prioritization = name,
                  value)

  inds <- getMemoIndicators(d, d2_session)

  df <- d  %>%
    purrr::pluck("data") %>%
    purrr::pluck("analytics") %>%
    dplyr::filter(!is.na(target_value)) %>%
    dplyr::select(dataelement_id,
                  categoryoptioncombo_id,
                  prioritization,
                  value = target_value) %>%
    dplyr::group_by(dataelement_id, categoryoptioncombo_id, prioritization) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::mutate(combi = paste0("#{", dataelement_id, ".", categoryoptioncombo_id, "}")) %>%
    plyr::ddply(., plyr::.(prioritization),
                    function(x)
                      evaluateIndicators(x$combi, x$value, inds))

  if (NROW(df) == 0) {
    return(d)
    }

  df %<>%
    dplyr::select(-id, -numerator, -denominator) %>%
    tidyr::complete(., prioritization, name, fill = list(value = 0)) %>%
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
    dplyr::group_by(Age, Indicator, prioritization) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(prioritization = case_when(is.na(prioritization) ~ "No Prioritization",
                                             TRUE ~ prioritization))

  df_total <- df %>%
    dplyr::filter(Age != "Total") %>%
    dplyr::select(-Age) %>%
    group_by(prioritization, Indicator) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Age = "Total") %>%
    dplyr::select(names(df))

      d$data$prio_table <- dplyr::bind_rows(df, df_total, df_base) %>%
        dplyr::group_by(Indicator, Age, prioritization) %>%
        dplyr::summarise(value = sum(value)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Age = factor(Age, levels = (unique(Age)))) %>%
        dplyr::left_join(df_rows, by = c("Indicator" = "ind", "Age" = "options")) %>%
        dplyr::left_join((df_cols %>%
        dplyr::select(name, col_order)), by = c("prioritization" = "name")) %>%
        dplyr::select(Indicator, Age, prioritization, value, row_order, col_order) %>%
    dplyr::arrange(col_order, row_order, Age) %>%
        dplyr::select(-row_order, -col_order) %>%
  tidyr::pivot_wider(names_from = prioritization, values_from = "value") %>%
    mutate("Total" = rowSums(across(where(is.numeric)))) %>%
    dplyr::select("Indicator", "Age", 3:dim(.)[2])


  return(d)


}

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

modalitySummaryChart <- function(d) {

  cop_year <- as.numeric(stringr::str_replace(d$info$cop_year, "^20", ""))
  chart_label <- paste0("COP", cop_year, "/FY", cop_year + 1, " Testing Targets")

  d %>%
    purrr::pluck("data") %>%
    purrr::pluck("analytics") %>%
    dplyr::filter(!is.na(hts_modality)) %>%
    dplyr::filter(!(resultstatus_specific %in% c("Known at Entry Positive", "Known Positives"))) %>%
    dplyr::group_by(resultstatus_inclusive, hts_modality) %>%
    dplyr::summarise(value = sum(target_value)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(resultstatus_inclusive, desc(resultstatus_inclusive)) %>%
    dplyr::mutate(resultstatus_inclusive = factor(resultstatus_inclusive, c("Unknown", "Negative", "Positive"))) %>%
    ggplot(aes(
      y = value,
      x = reorder(hts_modality, value, sum),
      fill = resultstatus_inclusive
)) +
    geom_col() +
    scale_y_continuous(labels = scales::comma) +
    coord_flip() +
    scale_fill_manual(values = c(	"#948d79", "#548dc0", "#59BFB3")) +
    labs(y = "", x = "",
         title = chart_label,
         subtitle = "modalities ordered by total tests") +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          text = element_text(color = "#595959", size = 14),
          plot.title = element_text(face = "bold"),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.x = element_line(color = "#595959"),
          panel.grid.minor.y = element_blank())

}

modalitySummaryTable <- function(d) {

  hts <- d %>%
    purrr::pluck("data") %>%
    purrr::pluck("analytics") %>%
    dplyr::filter(!is.na(hts_modality)) %>%
    dplyr::filter(!(resultstatus_specific %in% c("Known at Entry Positive", "Known Positives"))) %>%
    dplyr::group_by(resultstatus_inclusive, hts_modality) %>%
    dplyr::summarise(value = sum(target_value)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(resultstatus_inclusive, desc(resultstatus_inclusive)) %>%
    dplyr::mutate(resultstatus_inclusive = factor(resultstatus_inclusive, c("Unknown", "Negative", "Positive")))

  if (NROW(hts) == 0) {
    return(d)
    }

   structure_check <- hts %>%
      dplyr::group_by(resultstatus_inclusive) %>%
      dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
      dplyr::mutate(greater_than_zero = value > 0)

   is_ok <- Reduce("&", structure_check$greater_than_zero) &
     Reduce("&", c("Negative", "Positive") %in%  structure_check$resultstatus_inclusive)

    if (is_ok) {
      hts %<>%
       tidyr::pivot_wider(names_from = resultstatus_inclusive, values_from = value) %>%
       dplyr::mutate(yield = Positive / (Negative + Positive) * 100,
                     modality_share = Positive / sum(Positive) * 100,
                     Total = Positive + Negative) %>%
       dplyr::select(hts_modality, Positive, Total, yield, modality_share)

     hts_total <- hts %>%
       dplyr::select(Positive, Total) %>%
       dplyr::mutate(hts_modality = "Total") %>%
       dplyr::group_by(hts_modality) %>%
       dplyr::summarise_all(sum) %>%
       dplyr::mutate(yield = Positive / Total * 100,
                     modality_share = 100)

     d$data$modality_summary <- dplyr::bind_rows(hts, hts_total)

     d

   } else {
     return(d)
   }


}

formatModalitySummaryTable <- function(d) {
  df <- d$data$modality_summary

  if (is.null(df)) {
    return(NULL)
    }

    df %>% dplyr::mutate(
      Positive = format(Positive, big.mark = ", ", scientific = FALSE),
      Total = format(Total, big.mark = ", ", scientific = FALSE),
      yield = format(round(yield, 2), nsmall = 2),
      modality_share = format(round(modality_share, 2), nsmall = 2)) %>%
      dplyr::select(
        Modality = hts_modality,
        Positive,
        Total,
        "Yield (%)" = yield,
        "Percent of HTS_POS" = modality_share)
}

modalityYieldChart <- function(d) {

  df <- d$data$modality_summary
  cop_year <- as.numeric(stringr::str_replace(d$info$cop_year, "^20", ""))
  chart_label <- paste0("COP", cop_year, "/FY", cop_year + 1, " Testing Yields")

  if (NROW(df)  == 0) {
    return(NULL)
    }

  x_lim <- max(df$yield)

  df %>%
  dplyr::filter(hts_modality != "Total") %>% #Omit totals
    ggplot(aes(
      y = yield,
      x = reorder(hts_modality, yield)
)) +
    geom_col(fill = "#67A9CF") +
    geom_text(aes(label = percent(yield, accuracy = 0.1, scale = 1), hjust = -0.25)) +
    scale_y_continuous(limits = c(0, x_lim * 1.1), labels = percent_format(accuracy = 1, scale = 1)) +
    coord_flip() +
    scale_fill_manual(values = c("#2166AC")) +
    labs(y = "", x = "",
         title = chart_label,
         subtitle = "Modalities ordered by yield rates") +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          text = element_text(color = "#595959", size = 14),
          plot.title = element_text(face = "bold"),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.x = element_line(color = "#595959"),
          panel.grid.minor.y = element_blank())

}

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
        `HTS_RECENT (%)` = format(round(`HTS_RECENT (%)`, 2), nsmall = 2)
)
  }

  d

}

subnatPyramidsChart <- function(d, epi_graph_filter_results) {


  df <- d %>%
    purrr::pluck(., "data") %>%
    purrr::pluck(., "analytics")

  if (is.null(df)) {
    return(NULL)
    }

  if (length(epi_graph_filter_results) > 0 & !is.null(epi_graph_filter_results)) {
    df <- dplyr::filter(df, snu1 %in% epi_graph_filter_results)
  }

  if (NROW(df) == 0) {
     return(NULL)
     }

  df %<>%
    dplyr::filter(., indicator_code == "TX_CURR.T" |
                    indicator_code == "TX_PVLS.N.Routine.T" |
                    indicator_code == "PLHIV.T_1") %>%
    dplyr::select(age, sex, indicator_code, target_value) %>%
    dplyr::group_by(age, sex, indicator_code) %>%
    dplyr::summarise(value = sum(target_value)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(Age = age,
                  Sex = sex) %>%
    dplyr::arrange(indicator_code, desc(indicator_code)) %>%
    dplyr::mutate(indicator_code = ifelse(
      indicator_code == "PLHIV.T_1", "PLHIV", ifelse(
        indicator_code == "TX_CURR.T", "TX_CURR", ifelse(
          indicator_code == "TX_PVLS.N.Routine.T", "TX_PVLS", NA))))

  if (NROW(df) == 0) {
    return(NULL)
    }

  y_lim <- max(df$value)
  cop_year <- as.numeric(stringr::str_replace(d$info$cop_year, "^20", ""))
  chart_label <- paste0("COP", cop_year, "/FY", cop_year + 1, " Epidemic Cascade Age & Sex Pyramid")

  df %>%
    ggplot(aes(x = Age, y = value, fill = indicator_code)) +
    geom_bar(data = df %>% dplyr::filter(Sex == "Female") %>% dplyr::arrange(indicator_code),
             stat = "identity",
             position = "identity") +
    geom_bar(data = df %>% dplyr::filter(Sex == "Male") %>% dplyr::arrange(indicator_code),
             stat = "identity",
             position = "identity",
             mapping = aes(y = -value)) +
    coord_flip() +
    labs(x = "", y = "\u2190 Males | Females \u2192",
          title = chart_label,
          subtitle = "Comparison of Population with HIV, on Treatment, and Virally Suppressed") +
    geom_hline(yintercept = 0, size = 1) +
    scale_fill_manual(values = c(	"#B2182B", "#EF8A62", "#67A9CF")) +
    scale_y_continuous(limits = c(-y_lim, y_lim), labels = function(x) scales::comma(abs(x))) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          text = element_text(color = "#595959", size = 14),
          plot.title = element_text(face = "bold"),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.x = element_line(color = "#595959"),
          panel.grid.minor.y = element_blank())

}

kpCascadeChart <- function(d, kpCascadeInput_filter) {

  df <- d %>%
    purrr::pluck(., "data") %>%
    purrr::pluck(., "analytics")

  if (is.null(df)) {
    return(NULL)
    }

  if (length(kpCascadeInput_filter) > 0 & !is.null(kpCascadeInput_filter)) {
    df %<>% dplyr::filter(snu1 %in% kpCascadeInput_filter)
  }

  if (NROW(df) == 0) {
    return(NULL)
    }


  df %<>%
    dplyr::filter(dataelement_name == "IMPATT.PLHIV (N, SUBNAT, Age/Sex/HIVStatus) TARGET:" |
    dataelement_name == "KP_ESTIMATES (N, SUBNAT, PositiveEstimate/HIVStatus) TARGET: Estimated Key Pop" |
    dataelement_name == "TX_CURR (N, DSD, Age/Sex/HIVStatus) TARGET: Receiving ART" |
    dataelement_name == "TX_CURR (N, DSD, KeyPop/HIVStatus) TARGET: Receiving ART" |
    dataelement_name == "TX_PVLS (N, DSD, Age/Sex/Indication/HIVStatus) TARGET: Viral Load Documented"  |
    dataelement_name == "TX_PVLS (N, DSD, KeyPop/HIVStatus) TARGET: Viral Load Documented") %>%
    dplyr::mutate(indicator = ifelse(indicator == "KP_ESTIMATES", "PLHIV", indicator)) %>%
    dplyr::mutate(kp = ifelse(is.na(key_population), "GenPop", "KeyPop")) %>%
    dplyr::select(indicator, kp, target_value) %>%
    dplyr::group_by(indicator, kp) %>%
    dplyr::summarise(value = sum(target_value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(lbl = paste0(indicator, ".", kp))

  if (NROW(df) == 0) {
    return(NULL)
    }

  y_lim <- max(df$value)
  cop_year <- as.numeric(stringr::str_replace(d$info$cop_year, "^20", ""))
  chart_label <- paste0("COP", cop_year, "/FY", cop_year + 1, " Epidemic Cascade Age & Sex Pyramid")

  df %>%
    ggplot(aes(x = indicator, y = value, fill = lbl)) +
    geom_bar(data = df,
             stat = "identity",
             position = "identity") +
    geom_text(aes(label = scales::comma(value), vjust = -0.25)) +
    labs(x = "", y = "",
          title = chart_label,
          subtitle = "Comparison of General and Key Populations with HIV, on Treatment, and Virally Suppressed") +
    geom_hline(yintercept = 0, size = 1) +
    scale_fill_manual(values = c("#ceb966", "#9cb084", "#6bb1c9", "#6585cf", "#7e6bc9", "#a379bb")) +
    scale_y_continuous(limits = c(0, y_lim * 1.1), labels = scales::comma) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          text = element_text(color = "#595959", size = 14),
          plot.title = element_text(face = "bold"),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color = "#595959"))

}

vlsTestingChart <- function(df) {

  if (is.null(df)) {
    return(NULL)
    }

  df <- df %>%
    dplyr::filter(indicator == "TX_CURR" | indicator == "TX_PVLS") %>%
    dplyr::select(SNU1 = snu1, indicator, numerator_denominator, target_value) %>%
    dplyr::mutate(indicator = dplyr::case_when(indicator == "TX_CURR" ~ "TX_CURR",
    indicator == "TX_PVLS" & numerator_denominator == "Numerator" ~ "TX_PVLS (N)",
    indicator == "TX_PVLS" && numerator_denominator == "Denominator" ~ "TX_PVLS (D)",
    TRUE ~ NA_character_),
    SNU1 = ifelse(substr(SNU1, 0, 9) == "_Military", "Military", SNU1))  %>%
    dplyr::group_by(SNU1, indicator) %>%
    dplyr::summarise(value = sum(target_value)) %>%
    dplyr::mutate(freq = value / max(value)) %>%
    dplyr::mutate(sort_col = min(freq)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(desc(sort_col), indicator)

  if (NROW(df) == 0) {
    return(NULL)
    }

  y_lim <- (min(df$freq) %/% 0.1) / 10
  cop_year <- as.numeric(stringr::str_replace(df$info$cop_year, "^20", ""))
  chart_label <- paste0("COP", cop_year, "/FY", cop_year + 1, " Viral Load Testing Coverage")

  df %>%
    ggplot(aes(x = reorder(SNU1, sort_col), y = freq, fill = indicator)) +
    geom_bar(data = df,
             stat = "identity",
             position = "identity") +
    coord_flip(ylim = c(y_lim, 1)) +
    labs(x = "", y = "",
          title = chart_label,
          subtitle = "Percentage of Population Currently on Treatment Eligible and Targeted for VLS Testing") +
    geom_hline(yintercept = 0, size = 1) +
    scale_fill_manual(values = c(	"#B2182B", "#EF8A62", "#67A9CF")) +
    scale_y_continuous(labels = percent) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          text = element_text(color = "#595959", size = 14),
          plot.title = element_text(face = "bold"),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          panel.grid.major.x = element_line(color = "#595959"),
          panel.grid.minor.y = element_blank())

}

snuSelector <- function(df) {

  if (!inherits(df, "error") & !is.null(df)) {
    df  %>%
      purrr::pluck(., "data") %>%
      purrr::pluck(., "analytics") %>%
      dplyr::pull(., "snu1") %>%
      unique()

  } else {
    NULL
  }

}

prepareSNUSummaryTable <- function(d) {

  can_sum <- d$info$schema %>%
    dplyr::filter(value_type == "integer") %>%
    dplyr::pull(indicator_code) %>%
    unique(.)

  if (d$info$tool == "Data Pack") {
    df <- dplyr::bind_rows(d$data$MER, d$data$SUBNAT_IMPATT)
  } else if (d$info$tool == "OPU Data Pack") {
    if (d$info$cop_year == 2020) {
      df <- d$data$extract
    } else if (d$info$cop_year == 2021) {
      df <- d$data$SNUxIM
    }
  }
  

  if (NROW(df) == 0) {
    return(d)
    }

  snus <- datapackr::valid_PSNUs %>%
    dplyr::select(ou, country_name, snu1, psnu, psnu_uid)

  df %>%
    dplyr::inner_join(snus, by = c("psnuid" = "psnu_uid")) %>%
    dplyr::select(ou, country_name, snu1, psnu, indicator_code, Age, Sex, KeyPop, value) %>%
    dplyr::group_by(ou, country_name, snu1, psnu, indicator_code) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
    dplyr::arrange(indicator_code, ou, country_name, snu1, psnu)
}


memo_getPrioritizationTable <- function(d, d2_session, cop_year = "2020", include_no_prio = TRUE) {

  inds <- getMemoIndicators(d, d2_session = d2_session) %>%
    select(name, id)

  #TODO: Replace this with memoStructure
  df_cols <- tibble::tribble(
    ~id, ~shortName, ~col_name,
    "ATX2xv8PsrX", "PPG Attained", "Attained",
    "IzmZerN7tDN", "PPG Scale-up: Saturation", "Scale-up: Saturation",
    "AHMMjoPYta6", "PPG Scale up: Aggressive", "Scale-up: Aggressive",
    "b1X6pxMHgs6", "PPG Sustained", "Sustained",
    "pibJV72pMyW", "PPG Centrally Supported", "Centrally Supported",
    "CJYtvFbjeG2", "PPG No Prioritization", "No Prioritization",
    "p0JrTY2hLii", "PPG Not PEPFAR Supported", "Not PEPFAR Supported"
)

  df_rows <- memoStructure(d$info$cop_year) %>%
    purrr::pluck("row_order") %>%
    dplyr::select(ind, options)

  df_base <- tidyr::crossing(df_rows, dplyr::select(df_cols, col_name)) %>%
    dplyr::arrange(ind, options, col_name) %>%
    dplyr::mutate(Value = 0) %>%
    dplyr::rename("Indicator" = ind,
                  Age = options)


  psnus <- dplyr::bind_rows(datapackr::valid_PSNUs) %>%
    dplyr::filter(country_uid %in% d$info$country_uids) %>%
    dplyr::filter(!is.na(psnu_type)) %>%
    dplyr::pull(psnu_uid) %>%
    unique()


  #Break up into 2048 character URLS (approximately)
  n_requests <- ceiling(nchar(paste(psnus, sep = "", collapse = ";")) / 2048)
  n_groups <- split(sample(psnus), 1:n_requests)

  getPrioTable <- function(x) {
    datimutils::getAnalytics(ou = x,
                              dx = inds$id,
                              pe_f = paste0(d$info$cop_year, "Oct"),
                              d2_session = d2_session)
  }

  df <- n_groups  %>% purrr::map_dfr(function(x) getPrioTable(x))

  if (is.null(df) | NROW(df) == 0) {
    return(d)
    }

  prios <- n_groups %>% purrr::map_dfr(function(x) getExistingPrioritization(x, d$info$cop_year, d2_session))


  df <- df %>%
    dplyr::rename("psnu_uid" = `Organisation unit`) %>%
    dplyr::mutate(Value = as.numeric(Value))  %>%
    dplyr::inner_join(inds, by = c(`Data` = "id")) %>%
    dplyr::select(-Data) %>%
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
    dplyr::mutate(Age = case_when(Indicator %in% c("CXCA_SCRN", "OVC_HIVSTAT", "KP_PREV",
    "PMTCT_EID", "KP_MAT", "VMMC_CIRC", "PrEP_NEW", "PrEP_CURR", "GEND_GBV")  ~ "Total",
                                    TRUE ~ Age)) %>%
    dplyr::left_join(., prios, by = "psnu_uid") %>%
    dplyr::mutate(prioritization = as.character(prioritization)) %>%
    dplyr::mutate(prioritization = dplyr::case_when(is.na(prioritization) ~ "No Prioritization",
                                                    TRUE ~ prioritization)) %>%
    dplyr::group_by(`Indicator`, `Age`, `prioritization`) %>%
    dplyr::summarise(Value = sum(Value)) %>%
    dplyr::ungroup() %>%
    dplyr::rename("col_name" = "prioritization")

  df_totals <- df %>%
    dplyr::filter(Age != "Total") %>%
    group_by(Indicator, col_name) %>%
    dplyr::summarise(Value = sum(Value)) %>%
    dplyr::mutate(Age = "Total") %>%
    dplyr::ungroup() %>%
    dplyr::select(names(df))

  df_final <- dplyr::bind_rows(df, df_totals, df_base) %>%
    dplyr::group_by(Indicator, Age, col_name) %>%
    dplyr::summarise(Value = sum(Value)) %>%
    dplyr::distinct() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(col_name = factor(col_name, levels = df_cols$col_name)) %>%
    dplyr::mutate(Indicator = factor(Indicator, levels = unique(df_rows$ind))) %>%
    dplyr::arrange(Indicator, col_name) %>%
    tidyr::pivot_wider(names_from = col_name, values_from = "Value") %>%
    suppressWarnings()

  #Remove NOT pepfar supported if its only zeros, otherwise, show this, since its potentially problematic
  if (df_final %>%  dplyr::select("Not PEPFAR Supported") %>% sum(., na.rm = TRUE) == 0) {
    df_final <- df_final %>%  select(-`Not PEPFAR Supported`)
  }


  df_final %<>%
    mutate("Total" = rowSums(across(where(is.numeric)))) %>%
    dplyr::select("Indicator", "Age", 3:dim(.)[2])

  if (!include_no_prio & any("No Prioritization" %in% names(df_final))) {
    df_final  %<>% dplyr::select(-`No Prioritization`)
  }

  d$data$memo$datim$prio <- df_final

  return(d)


  }

getExistingPrioritization <- function(psnus, cop_year, d2_session) {

  period <- paste0(cop_year, "Oct")
  dx <- "r4zbW3owX9n"
  ous <- paste(psnus, sep = "", collapse = ";")

  prios <- datimutils::getAnalytics(dx = "r4zbW3owX9n", pe_f = period, ou = ous, d2_session = d2_session)


  if (is.null(prios)) {
    return(data.frame("psnu_uid" = psnus, "prioritization" = "No Prioritization"))
  }

  #Check for invalid prioritization levels, and throw an error if this occurs
  if (!all(prios$Value %in% c(datapackr::prioritization_dict() %>% dplyr::pull(value), NA)))  {
    stop("Invalid prioritization levels detected. Please contact DATIM support.")
  }


  prios %>%
    dplyr::select(-Data) %>%
    dplyr::rename("psnu_uid" = "Organisation unit",
                  "value" = "Value") %>%
    dplyr::left_join(datapackr::prioritization_dict()) %>%
    dplyr::select(psnu_uid, "prioritization" = "name")

}

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
