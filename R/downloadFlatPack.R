downloadFlatPack <- function(d) {
  #Create a new workbook
  wb <- openxlsx::createWorkbook()
  #Common to both OPUs and Datapacks
  openxlsx::addWorksheet(wb, "Analytics")
  openxlsx::writeDataTable(wb = wb,
                           sheet = "Analytics", x = d$data$analytics)
  #TODO. How to handle indicators which should not be summed
  snu_summary <- prepareSNUSummaryTable(d)
  openxlsx::addWorksheet(wb, "SNU Summary")
  openxlsx::writeDataTable(
    wb = wb,
    sheet = "SNU Summary", x = snu_summary
  )
  if (!is.null(d$data$recency)) {
    openxlsx::addWorksheet(wb, "HTS Recency")
    openxlsx::writeDataTable(
      wb = wb,
      sheet = "HTS Recency", x = d$data$recency
    )
  }
  if (!is.null(d$data$modality_summary)) {
    openxlsx::addWorksheet(wb, "HTS Summary")
    openxlsx::writeDataTable(
      wb = wb,
      sheet = "HTS Summary", x = formatModalitySummaryTable(d)
    )
  }
  if (!is.null(d$memo$datapack$by_prio)) {
    openxlsx::addWorksheet(wb, "Prioritization (DRAFT)")
    openxlsx::writeData(
      wb = wb,
      sheet = "Prioritization (DRAFT)", x = d$memo$datapack$by_prio
    )
  }
  #Datapack specific
  if (d$info$tool == "Data Pack") {

    mer_data <- d %>%
      purrr::pluck(., "data") %>%
      purrr::pluck(., "MER")

    subnat_impatt <- d %>%
      purrr::pluck(., "data") %>%
      purrr::pluck(., "SUBNAT_IMPATT")

    mer_data <- dplyr::bind_rows(mer_data, subnat_impatt)
    openxlsx::addWorksheet(wb, "MER Data")
    openxlsx::writeDataTable(wb = wb,
                             sheet = "MER Data", x = mer_data)

    has_psnu <- d %>%
      purrr::pluck(., "info") %>%
      purrr::pluck(., "has_psnuxim")

    if (has_psnu) {

      d$datim$MER$value <- as.character(d$datim$MER$value)
      d$datim$subnat_impatt$value <- as.character(d$datim$subnat_impatt$value)
      datim_export <- dplyr::bind_rows(d$datim$MER, d$datim$subnat_impatt)

      openxlsx::addWorksheet(wb, "DATIM export")
      openxlsx::writeData(wb = wb,
                          sheet = "DATIM export", x = datim_export)
    }
  }

  #OPU specific
  if (d$info$tool == "OPU Data Pack") {

    openxlsx::addWorksheet(wb, "DATIM export")
    openxlsx::writeData(wb = wb,
                        sheet = "DATIM export", x = d$datim$OPU)
  }

return(wb)
}
