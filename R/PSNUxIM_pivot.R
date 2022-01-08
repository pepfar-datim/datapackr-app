PSNUxIM_pivot <- function(d) {

  pivot <- d %>%
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

  rpivotTable(data = pivot, rows = c("dataelement_name"),
              vals = "value", aggregatorName = "Integer Sum", rendererName = "Table"
              , width = "70%", height = "700px")
}
