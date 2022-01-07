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

  gg <- df %>%
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

  return(gg)
}
