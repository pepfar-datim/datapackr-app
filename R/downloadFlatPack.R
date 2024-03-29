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
    openxlsx::addWorksheet(wb, "Memo Tables")
    openxlsx::writeData(
      wb = wb,
      sheet = "Memo Tables", x = d$memo$datapack$by_prio
    )
  }

  #Main export file
  openxlsx::addWorksheet(wb, "DATIM export")
  datim_export <- datapackr::createPAWExport(d)
  openxlsx::writeData(wb = wb,
                      sheet = "DATIM export", x = datim_export)

  #Add previous cop year's year 2 data for comparison
  #Using try catch as a pseudo if else here, If an error is thrown, do not create the tab
  tryCatch({previousY2 = fetchY2File(d$info$cop_year, d$info$sane_name)

  openxlsx::addWorksheet(wb, paste0("Notional_FY", (d$info$cop_year + 1) %% 100))
  openxlsx::writeDataTable(wb = wb,
                           sheet = paste0("Notional_FY", (d$info$cop_year + 1) %% 100), x = previousY2)
  },
  error = function(e) {
    interactive_warning( paste0("No Year 2 data present from last year to populate",
                                " Notional_FY",
                                (d$info$cop_year + 1) %% 100, " tab"))
    return(NULL)
  }
  )

return(wb)
}
