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

  # Aggregates age column for TX_CURR by regrouping
  df$age[df$indicator_code=="TX_CURR.T" &
           df$age %in% c("50-54","55-59","60-64","65+")] <- "50+"

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

  gg <- df %>%
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

  return(gg)
}
