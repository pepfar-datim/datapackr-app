modalitySummaryChart <- function(d) {

  cop_year <- as.numeric(stringr::str_replace(d$info$cop_year, "^20", ""))
  chart_label <- paste0("COP", cop_year, "/FY", cop_year + 1, " Testing Targets")

  chart_data <- d %>%
    purrr::pluck("data") %>%
    purrr::pluck("analytics") %>%
    dplyr::filter(!is.na(hts_modality)) %>%
    dplyr::filter(!(resultstatus_specific %in% c("Known at Entry Positive", "Known Positives"))) %>%
    dplyr::group_by(resultstatus_inclusive, hts_modality) %>%
    dplyr::summarise(value = sum(target_value)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(resultstatus_inclusive, desc(resultstatus_inclusive)) %>%
    dplyr::mutate(resultstatus_inclusive = factor(resultstatus_inclusive, c("Unknown", "Negative", "Positive")))

  gg <- chart_data %>%
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

  return(gg)
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
    Reduce("&", c("Negative", "Positive") %in% structure_check$resultstatus_inclusive)

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
  }

  return(d)
}

formatModalitySummaryTable <- function(d) {
  df <- d$data$modality_summary

  if (is.null(df)) {
    return(NULL)
  }

  df %<>% dplyr::mutate(
    Positive = format(Positive, big.mark = ",", scientific = FALSE),
    Total = format(Total, big.mark = ",", scientific = FALSE),
    yield = format(round(yield, 2), nsmall = 2),
    modality_share = format(round(modality_share, 2), nsmall = 2)) %>%
    dplyr::select(
      Modality = hts_modality,
      Positive,
      Total,
      "Yield (%)" = yield,
      "Percent of HTS_POS" = modality_share)

  return(df)
}

modalityYieldChart <- function(d) {

  df <- d$data$modality_summary
  cop_year <- as.numeric(stringr::str_replace(d$info$cop_year, "^20", ""))
  chart_label <- paste0("COP", cop_year, "/FY", cop_year + 1, " Testing Yields")

  if (NROW(df) == 0) {
    return(NULL)
  }

  x_lim <- max(df$yield)

  gg <- df %>%
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

  return(gg)
}
