downloadCSOFlatPack <- function(d) {
  cso_indicators <- d$info$schema %>%
    dplyr::filter(value_type  == "integer") %>%
    dplyr::pull(indicator_code) %>%
    unique(.)

  cso_data <- d$data$analytics %>%
    dplyr::filter(indicator_code %in% cso_indicators) %>%
    dplyr::filter(stringr::str_detect(psnu, "_Military", negate = TRUE)) %>%
    dplyr::group_by(ou, country_name, snu1, psnu, indicator_code, dataelement_name, support_type,
                    hts_modality, age, sex, key_population, resultstatus_specific, resultstatus_inclusive, top_level) %>%
    dplyr::summarize(value = sum(target_value))

  wb  <-  openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "CSO export")
  openxlsx::writeData(wb = wb,
                      sheet = "CSO export", x = cso_data)

  return(wb)
}
