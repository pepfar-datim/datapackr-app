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
    dplyr::filter(
      dataelement_name == "IMPATT.PLHIV (N, SUBNAT, Age/Sex/HIVStatus) TARGET:" |
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
  chart_label <- paste0("COP", cop_year, "/FY", cop_year + 1, " General Population vs. Key Population Epidemic Cascade")

  gg <- df %>%
    ggplot2::ggplot(aes(x = indicator, y = value, fill = lbl)) +
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

  return(gg)
}
