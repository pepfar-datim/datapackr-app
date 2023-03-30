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

return(wb)
}
