year2Pivot <- function(d) {



  pivot_data_year2 <- d %>%
    purrr::pluck("data") %>%
    purrr::pluck("Year2") %>%
    dplyr::select(Indicator = indicator_code,
                  Age = valid_ages.name,
                  Sex = valid_sexes.name,
                  KP = valid_kps.name,
                  `DataElement` = dataelementname,
                  `CatOptionCombo` = categoryoptioncomboname,
                  year2 = value)

  to_keep <- pivot_data_year2 %>%
    dplyr::select(DataElement,
                  CatOptionCombo) %>%
    dplyr::distinct()

  pivot_data_year1 <- d %>%
    purrr::pluck("data") %>%
    purrr::pluck("analytics") %>%
    dplyr::select(`DataElement` = dataelement_name,
                  `CatOptionCombo` = categoryoptioncombo_name,
                  value = target_value) %>%
    dplyr::inner_join(to_keep) %>%
    dplyr::group_by(DataElement, CatOptionCombo) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  dplyr::select(DataElement,
                CatOptionCombo,
                year1= value)


  pivot_data_year2 %<>%
    dplyr::left_join(pivot_data_year1) %>%
    tidyr::pivot_longer(cols = tidyr::starts_with("year"),
                        names_to = "Year",
                        names_prefix = "year",
                        values_to = "value")


    rpivotTable(data = pivot_data_year2, rows = c("DatElement","Age","Sex","KP"), cols = c("Year"),
                vals = "value", aggregatorName = "Integer Sum", rendererName = "Table"
                , width = "70%", height = "700px")

}
