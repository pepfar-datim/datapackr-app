downloadMemo <- function(d) {
  
  prio_table <- d %>%
    purrr::pluck("data") %>%
    purrr::pluck("prio_table")
  
  #Remove any columns which are all zeros to save space.
  column_filter  <-
    d$data$prio_table %>%
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = FALSE) !=  0)) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::filter(!V1) %>%
    row.names()
  
  prio_table %<>% dplyr::select(-column_filter)
  
  prio_table %<>% dplyr::mutate_if(is.numeric,
                                   function(x) ifelse(x  == 0, "-", formatC(x, format = "f", big.mark = ", ", digits = 0)))
  
  style_para_prio <- fp_par(text.align = "right",
                            padding.right = 0.04,
                            padding.bottom = 0,
                            padding.top = 0,
                            line_spacing = 1)
  
  style_header_prio <- fp_par(text.align = "center",
                              padding.right = 0,
                              padding.bottom = 0,
                              padding.top = 0,
                              line_spacing = 1)
  
  
  header_old <- names(prio_table)
  ou_name <- d$info$datapack_name
  header_new <- c(ou_name, ou_name, header_old[3:dim(prio_table)[2]])
  
  prio_table <- flextable(prio_table) %>%
    merge_v(., j = "Indicator") %>%
    delete_part(., part = "header") %>%
    add_header_row(., values = header_new) %>%
    add_header_row(., values = c(ou_name, ou_name, rep("SNU Prioritizations", (dim(prio_table)[2] - 2)))) %>%
    merge_h(., part = "header") %>%
    merge_v(., part = "header") %>%
    bg(., bg = "#CCC0D9", part = "header") %>%
    bg(., i = ~ Age  == "Total", bg = "#E4DFEC", part = "body") %>% #Highlight total rows
    bold(., i = ~ Age  == "Total", bold = TRUE, part = "body")  %>%
    bg(., j =  "Indicator", bg = "#FFFFFF", part = "body") %>%
    bold(., j = "Indicator", bold = FALSE) %>%
    bold(., bold = TRUE, part = "header") %>%
    fontsize(., size = 7, part = "all") %>%
    style(., pr_p = style_header_prio, part = "header") %>%
    style(., pr_p = style_para_prio, part = "body") %>%
    align(., j = 1:2, align = "center") %>%  #Align first two columns center
    flextable::add_footer_lines(., values =  paste("* Totals may be greater than the sum of categories",
                                                   "due to activities outside of the SNU prioritization areas outlined above"))
  
  fontname <- "Arial"
  if (gdtools::font_family_exists(fontname)) {
    prio_table  <-  font(prio_table, fontname = fontname, part = "all")
  }
  
  
  doc  <-  read_docx(path = "support_files/draft_memo_template.docx")
  doc <- body_add_flextable(doc, value = prio_table)
  doc <- body_add_break(doc, pos = "after")
  
  #Partners tables
  
  partners_table  <- d$data$partners_table %>%
    dplyr::mutate_if(is.numeric,
                     function(x) ifelse(x  == 0, "-", formatC(x, format = "f", big.mark = ", ", digits = 0)))
  
  sub_heading <- names(partners_table)[4:length(partners_table)] %>%
    stringr::str_split(., " ") %>%
    purrr::map(purrr::pluck(2)) %>%
    unlist() %>%
    c("Funding Agency", "Partner", "Mechanism",.)
  
  group_heading <- names(partners_table)[4:length(partners_table)] %>%
    stringr::str_split(., " ") %>%
    purrr::map(purrr::pluck(1)) %>%
    unlist() %>%
    c("Funding Agency", "Partner", "Mechanism", .)
  
  chunks <- list(c(1:15), c(1:3, 16:26), c(1:3, 27:35), c(1:3, 36:44))
  
  renderPartnerTable <- function(chunk) {
    
    partner_table <-  flextable(partners_table[, chunk]) %>%
      bg(., i = ~ Partner  == "", bg = "#E4DFEC", part = "body") %>%
      bold(., i = ~ Partner  == "", bold = TRUE) %>%
      delete_part(., part = "header") %>%
      add_header_row(., values = sub_heading[chunk]) %>%
      add_header_row(., top = TRUE, values = group_heading[chunk]) %>%
      merge_h(., part = "header") %>%
      merge_v(., part = "header")  %>%
      bg(., bg = "#CCC0D9", part = "header") %>%
      bold(., bold = TRUE, part = "header") %>%
      fontsize(., size = 7, part = "all") %>%
      style(., pr_p = style_para_prio, part = "body") %>%
      width(., j = 1:3, 0.75) %>%
      width(., j = 4:(length(chunk)), 0.4)
    
    fontname <- "Arial"
    if (gdtools::font_family_exists(fontname)) {
      partner_table  <-  font(partner_table, fontname = fontname, part = "all")
    }
    
    partner_table
  }
  
  for (i in seq_along(chunks)) {
    chunk <- chunks[[i]]
    partner_table_ft <- renderPartnerTable(chunk = chunk)
    doc <- body_add_flextable(doc, partner_table_ft)
    doc <- body_add_break(doc, pos = "after")
  }
  
  return(doc)
}


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

downloadFlatPack <- function(d) {
  #Create a new workbook
  wb  <-  openxlsx::createWorkbook()
  #Common to both OPUs and Datapacks
  openxlsx::addWorksheet(wb, "Analytics")
  openxlsx::writeDataTable(wb = wb,
                           sheet = "Analytics", x = d$data$analytics)
  #TODO. How to handle indicators which should not be summed
  snu_summary  <-  prepareSNUSummaryTable(d)
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
  if (!is.null(d$data$prio_table)) {
    openxlsx::addWorksheet(wb, "Prioritization (DRAFT)")
    openxlsx::writeData(
      wb = wb,
      sheet = "Prioritization (DRAFT)", x = d$data$prio_table
    )
  }
  #Datapack specific
  if (d$info$tool  == "Data Pack") {
    
    mer_data  <-  d %>%
      purrr::pluck(., "data") %>%
      purrr::pluck(., "MER")
    
    subnat_impatt  <-  d %>%
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
  if (d$info$tool  == "OPU Data Pack") {
    
    openxlsx::addWorksheet(wb, "DATIM export")
    openxlsx::writeData(wb = wb,
                        sheet = "DATIM export", x = d$datim$OPU)
    
  }
  
return(wb)
  

}

downloadDataPack <- function(d) {
  support_file <- fetchSupportFiles("/support_files/snuxim_model_data.rds")
  if (!file.exists(support_file)) {
    flog.error("Could not find model support file.")
    stop("WOMP!")
  }
  
  d  <-  writePSNUxIM(d, snuxim_model_data_path = support_file)
  unlink(support_file)

  return(d)
  
}