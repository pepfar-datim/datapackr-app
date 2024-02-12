downloadFlatPack <- function(d) {
  #Create a new workbook
  wb <- openxlsx::createWorkbook()

  if (d$info$tool == "Data Pack") {

      mer_data <- d %>%
        purrr::pluck(., "data") %>%
        purrr::pluck(., "MER")

      subnat_impatt <- d %>%
        purrr::pluck(., "data") %>%
        purrr::pluck(., "SUBNAT_IMPATT")

      mer_data <- dplyr::bind_rows(mer_data, subnat_impatt)

  }

  if (d$info$tool %in% c("PSNUxIM", "OPU Data Pack")) {
    mer_data <- d$data$SNUxIM
  }

  if (!is.null(mer_data) && NROW(mer_data) > 0) {
    openxlsx::addWorksheet(wb, "MER Data")
    openxlsx::writeDataTable(wb = wb,
                             sheet = "MER Data", x = mer_data)

  }

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
    formatMST = formatModalitySummaryTable(d)

    openxlsx::addWorksheet(wb, "HTS Summary")
    openxlsx::writeDataTable(
      wb = wb,
      sheet = "HTS Summary",
      x = formatMST
    )
  }
  if (!is.null(d$memo$datapack$by_prio)) {
    openxlsx::addWorksheet(wb, "Prioritization (DRAFT)")
    openxlsx::writeData(
      wb = wb,
      sheet = "Prioritization (DRAFT)", x = d$memo$datapack$by_prio
    )
  }

  #Main export file
  openxlsx::addWorksheet(wb, "DATIM export")
  datim_export <- createDATIMExport(d)
  openxlsx::writeData(wb = wb,
                      sheet = "DATIM export", x = datim_export)

  #Add previous cop year's year 2 data for comparison
  previousY2 = fetchY2File(d$info$cop_year, d$info$sane_name)

  openxlsx::addWorksheet(wb, "Notional_FY25")
  openxlsx::writeDataTable(wb = wb,
                           sheet = "Notional_FY25", x = previousY2)

return(wb)
}
