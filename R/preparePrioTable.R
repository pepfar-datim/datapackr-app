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
