year2Pivot <- function(d) {


    pivot_data <- d %>%
      purrr::pluck("data") %>%
      purrr::pluck("Year2") %>%
    dplyr::select(Indicator = indicator_code,
                    Age = valid_ages.name,
                    Sex = valid_sexes.name,
                    KP = valid_kps.name,
                    value)

    rpivotTable(data = pivot_data, rows = c("Indicator","Age","Sex","KP"),
                vals = "value", aggregatorName = "Integer Sum", rendererName = "Table"
                , width = "70%", height = "700px")

}
