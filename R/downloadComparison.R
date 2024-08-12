downloadComparison <- function(d, expanded = FALSE) {

  comparison_table <-
    d$memo$comparison %>%
    dplyr::rename(data_type = "Data Type") %>%
    dplyr::filter(data_type %in% c("Current", "Diff", "Proposed")) %>%
    dplyr::filter(Identical == "FALSE") %>%
    dplyr::select(!(Identical))

  if (NROW(comparison_table) == 0L || is.null(comparison_table)) {
    return(NULL)
  }

  wb <- openxlsx2::wb_workbook() %>%
    openxlsx2::wb_add_worksheet("Comparison data") %>%
    openxlsx2::wb_add_data(x = comparison_table)

  df <- openxlsx2::wb_data(wb)

  params <- list("colGrandTotals" = FALSE,
                 "rowGrandTotals" = FALSE)

  wb <- wb %>%
    openxlsx2::wb_add_worksheet(sheet = "By SNU1") %>%
    openxlsx2::wb_add_pivot_table(
      df,
      sheet = "By SNU1",
      dims = "A3",
      rows = c("Indicator", "snu1"),
      cols = "data_type",
      data = "value",
      params = params)

  wb <- wb %>%
    openxlsx2::wb_add_worksheet(sheet = "By Partner") %>%
    openxlsx2::wb_add_pivot_table(
      df,
      sheet = "By Partner",
      dims = "A3",
      rows = c("Indicator", "Partner"),
      cols = "data_type",
      data = "value",
      params = params
    )

  wb <- wb %>%
    openxlsx2::wb_add_worksheet(sheet = "By Agency") %>%
    openxlsx2::wb_add_pivot_table(
      df,
      sheet = "By Agency",
      dims = "A3",
      rows = c("Indicator", "Agency"),
      cols = "data_type",
      data = "value",
      params = params
    )

  wb <- wb %>%
    openxlsx2::wb_add_worksheet(sheet = "By Mechanism") %>%
    openxlsx2::wb_add_pivot_table(
      df,
      sheet = "By Mechanism",
      dims = "A3",
      rows = c("Indicator", "Mechanism"),
      cols = "data_type",
      data = "value",
      params = params
    )
}
