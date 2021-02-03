
pacman::p_load(shiny, shinyjs, shinyWidgets, magrittr, dplyr, datimvalidation,
               ggplot2, futile.logger, paws, datapackr, scales, DT, purrr,
               praise, rpivotTable, waiter)

source("./utils.R")
source("./visuals.R")

shinyServer(function(input, output, session) {

  ready <- reactiveValues(ok = FALSE)

  observeEvent(input$file1, {
    shinyjs::show("validate")
    shinyjs::enable("validate")
    ready$ok <- FALSE
  })

  observeEvent(input$validate, {
    shinyjs::disable("file1")
    shinyjs::disable("validate")
    ready$ok <- TRUE
  })

  observeEvent(input$reset_input, {
    shinyjs::reset("side-panel")
    shinyjs::enable("file1")
    shinyjs::disable("validate")
    shinyjs::disable("downloadFlatPack")
    #shinyjs::disable("downloadDataPack")
    shinyjs::disable("download_messages")
    #shinyjs::disable("send_paw")
    shinyjs::disable("downloadValidationResults")
    #shinyjs::disable("compare")
    ready$ok <- FALSE
  })

  observeEvent(input$send_paw, {
    d <- validation_results()
    r <- saveTimeStampLogToS3(d)
    timestampUploadUI(r)
    r <- sendMERDataToPAW(d)
    archiveDataPackErrorUI(r)
    r <- sendValidationSummary(d)
    validationSummaryUI(r)
    r <- saveDATIMExportToS3(d)
    datimExportUI(r)
  })

  observeEvent(input$login_button, {
    tryCatch({
      datimutils::loginToDATIM(base_url = Sys.getenv("BASE_URL"),
                               username = input$user_name,
                               password = input$password)
      },
      #This function throws an error if the login is not successful
      error = function(e) {
        sendSweetAlert(
          session,
          title = "Login failed",
          text = "Please check your username/password!",
          type = "error")
        flog.info(paste0("User ", input$user_name, " login failed."),
                  name = "datapack")
      })

    if (exists("d2_default_session"))  {

      user_input$authenticated <- TRUE
      user_input$d2_session <- d2_default_session$clone()
      flog.info(paste0("User ",
                       user_input$d2_session$me$userCredentials$username,
                       " logged in."),
                name = "datapack")
    }
  })

  epi_graph_filter <- reactiveValues(snu_filter = NULL)

  observeEvent(input$epiCascadeInput, {
    epi_graph_filter$snu_filter <- input$epiCascadeInput
  })

  kpCascadeInput_filter <- reactiveValues(snu_filter = NULL)

  observeEvent(input$kpCascadeInput, {
    kpCascadeInput_filter$snu_filter <- input$kpCascadeInput
  })

  observeEvent(input$logout, {
    flog.info(paste0("User ",
                     user_input$d2_session$me$userCredentials$username,
                     " logged out."))
    ready$ok <- FALSE
    user_input$authenticated <- FALSE
    user_input$d2_session <- NULL
    gc()

  })


  output$ui <- renderUI({

    if (user_input$authenticated == FALSE) {
      ##### UI code for login page
      fluidPage(
        fluidRow(
          column(width = 2, offset = 5,
                 br(), br(), br(), br(),
                 uiOutput("uiLogin"),
                 uiOutput("pass")
          )
        )
      )
    } else {

      wiki_url <- a("Datapack Wiki",
                    href = "https://github.com/pepfar-datim/Data-Pack-Feedback/wiki",
                    target = "_blank")

      fluidPage(
        tags$head(tags$style(".shiny-notification {
                             position: fixed;
                             top: 10%;
                             left: 33%;
                             right: 33%;}")),
        use_waiter(),
        sidebarLayout(
          sidebarPanel(
            shinyjs::useShinyjs(),
            id = "side-panel",
            tagList(wiki_url),
            tags$hr(),
            fileInput(
              "file1",
              "Choose DataPack (Must be XLSX!):",
              accept = c("application/xlsx",
                         ".xlsx"),
              width = "240px"
            ),
            actionButton("validate", "Validate"),
            tags$hr(),
            downloadButton("downloadFlatPack", "Download FlatPack"),
            tags$hr(),
            downloadButton("download_messages", "Validation messages"),
            tags$hr(),
             downloadButton("downloadValidationResults", "Validation report"),
            tags$hr(),
            actionButton("send_paw", "Send to PAW"),
            tags$hr(),
            downloadButton("downloadDataPack", "Regenerate PSNUxIM"),
            tags$hr(),
            downloadButton("compare", "Compare with DATIM"),
            tags$hr(),
            div(style = paste("display: inline-block;", "vertical-align:top;",
                              "width: 80 px;", sep = " "),
                actionButton("reset_input", "Reset inputs")),
            div(style = paste("display: inline-block;", "vertical-align:top;",
                              "width: 80 px;", sep = " "),
                actionButton("logout", "Logout"))
          ),

          mainPanel(tabsetPanel(
            id = "main-panel",
            type = "tabs",
            tabPanel("Messages", tags$ul(uiOutput("messages"))),
            tabPanel("Indicator summary", dataTableOutput("indicator_summary")),
            tabPanel("Validation rules", dataTableOutput("vr_rules")),
            tabPanel("HTS Summary Chart", plotOutput("modality_summary")),
            tabPanel("HTS Summary Table", dataTableOutput("modality_table")),
            tabPanel("HTS Yield", plotOutput("modality_yield")),
            tabPanel("HTS Recency", dataTableOutput("hts_recency")),
            tabPanel("VLS Testing", plotOutput("vls_summary")),
            tabPanel("Epi Cascade Pyramid",
                     pickerInput("epiCascadeInput", "SNU1",
                                 choices = "",
                                 options = list(`actions-box` = TRUE),
                                 multiple = T),
                     plotOutput("epi_cascade")),
            tabPanel("KP Cascade Pyramid",
                     pickerInput("kpCascadeInput", "SNU1",
                                 choices = "",
                                 options = list(`actions-box` = TRUE),
                                 multiple = T),
                     plotOutput("kp_cascade")),
            tabPanel("PSNUxIM Pivot", rpivotTableOutput({
              "pivot"
              }))

          ))
        ))
  }
})

  user_input <- reactiveValues(authenticated = FALSE,
                               status = "",
                               d2_session = NULL)

  # password entry UI componenets:
  #   username and password text fields, login button
  output$uiLogin <- renderUI({

    wellPanel(fluidRow(
      img(src = "pepfar.png", align = "center"), # TODO Fix centering of logo
      h4(paste("Welcome to the DataPack Validation App.",
               "Please login with your DATIM credentials:", sep = " "))
    ),
    fluidRow(
      textInput("user_name", "Username: ", width = "600px"),
      passwordInput("password", "Password:", width = "600px"),
      actionButton("login_button", "Log in!")
    ))
  })

  ## Validate Function --------------------------------------

  validate <- function() {

    shinyjs::disable("downloadFlatPack")
    #shinyjs::disable("downloadDataPack")
    shinyjs::disable("download_messages")
    #shinyjs::disable("send_paw")
    shinyjs::disable("downloadValidationResults")
    #shinyjs::disable("compare")
    
    if (!ready$ok) {
      shinyjs::disable("validate")
      return(NULL)
    }

    inFile <- input$file1
    messages <- ""

    if (is.null(inFile)) {
      return(NULL)
    }

    messages <- list()

    withProgress(message = "Validating file", value = 0, {

      shinyjs::disable("file1")
      shinyjs::disable("validate")
      incProgress(0.1, detail = ("Validating your DataPack"))


      d <- tryCatch({
        datapackr::unPackTool(inFile$datapath,
                              d2_session = user_input$d2_session)},
        error = function(e) {
          return(e)
        })

      print(names(d))
      if (!inherits(d, "error") & !is.null(d)) {
        print("FOO")
        d$info$sane_name <- paste0(stringr::str_extract_all(d$info$datapack_name,
                                                          "[A-Za-z0-9_]",
                                                          simplify = TRUE),
                                 sep = "", collapse = "")
        flog.info(d$info$sane_name)
        #All self-service datapacks should be marked as unapproved for PAW
        d$info$approval_status <- "UNAPPROVED"
        #Keep this until we can change the schema

        flog.info(paste0("Initiating validation of ", d$info$datapack_name,
                         " DataPack."), name = "datapack")
        if (d$info$tool == "Data Pack") {
          if (d$info$has_psnuxim) {
            flog.info("Datapack with PSNUxIM tab found.")

            incProgress(0.1, detail = ("Checking validation rules"))
            Sys.sleep(0.5)
            d <- validatePSNUData(d, d2_session = user_input$d2_session)
            incProgress(0.1, detail = "Validating mechanisms")
            Sys.sleep(0.5)
            d <- validateMechanisms(d, d2_session = user_input$d2_session)
            # incProgress(0.1, detail = ("Saving a copy of your submission to the archives"))
            # Sys.sleep(0.5)
            # r <- archiveDataPacktoS3(d,inFile$datapath)
            # archiveDataPackErrorUI(r)
            # incProgress(0.1, detail = (praise()))
            # Sys.sleep(1)
            d <- prepareFlatMERExport(d)

            shinyjs::enable("downloadFlatPack")
            shinyjs::enable("download_messages")
            #shinyjs::enable("send_paw")
            shinyjs::enable("downloadValidationResults")
            #shinyjs::enable("compare")
            # if ( d$info$missing_psnuxim_combos ) {
            #   shinyjs::enable("downloadDataPack")
            # }
            updatePickerInput(session = session, inputId = "kpCascadeInput",
                              choices = snuSelector(d))
            updatePickerInput(session = session, inputId = "epiCascadeInput",
                              choices = snuSelector(d))

          } else {
            #This should occur when there is no PSNUxIM tab and they want
            #to generate one.
            #shinyjs::enable("downloadDataPack")
            hideTab(inputId = "main-panel", target = "Validation rules")
            hideTab(inputId = "main-panel", target = "HTS Summary Chart")
            hideTab(inputId = "main-panel", target = "HTS Summary Table")
            hideTab(inputId = "main-panel", target = "HTS Yield")
            hideTab(inputId = "main-panel", target = "HTS Recency")
            hideTab(inputId = "main-panel", target = "VLS Testing")
            hideTab(inputId = "main-panel", target = "Epi Cascade Pyramid")
            hideTab(inputId = "main-panel", target = "KP Cascade Pyramid")
            hideTab(inputId = "main-panel", target = "PSNUxIM Pivot")
          }
        }
      }

      if (d$info$tool == "OPU Data Pack") {
        incProgress(0.1, detail = ("Checking validation rules"))
        Sys.sleep(0.5)
        d <- validatePSNUData(d, user_input$d2_session)
        incProgress(0.1, detail = "Validating mechanisms")
        Sys.sleep(0.5)
        d <- validateMechanisms(d, user_input$d2_session)

        shinyjs::enable("downloadFlatPack")
        shinyjs::enable("download_messages")
        # shinyjs::enable("send_paw")
        shinyjs::enable("downloadValidationResults")
        shinyjs::enable("compare")
        updatePickerInput(session = session, inputId = "kpCascadeInput",
                          choices = snuSelector(d))
        updatePickerInput(session = session, inputId = "epiCascadeInput",
                          choices = snuSelector(d))

        # hideTab(inputId = "main-panel", target = "Validation rules")
        # hideTab(inputId = "main-panel", target = "HTS Summary Chart")
        # hideTab(inputId = "main-panel", target = "HTS Summary Table")
        # hideTab(inputId = "main-panel", target = "HTS Yield")
        # hideTab(inputId = "main-panel", target = "HTS Recency")
        # hideTab(inputId = "main-panel", target = "VLS Testing")
        hideTab(inputId = "main-panel", target = "Epi Cascade Pyramid")
        hideTab(inputId = "main-panel", target = "KP Cascade Pyramid")
        # hideTab(inputId = "main-panel", target = "PSNUxIM Pivot")
      }

    })

    return(d)

  }

  ## VALIDATION RESULTS REACTIVE --------------------------------------

  validation_results <- reactive({
    validate()
  })

  ## EPI CASCADE GRAPH --------------------------------------

  output$epi_cascade <- renderPlot({

    vr <- validation_results()
    epi_graph_filter_results <- epi_graph_filter$snu_filter

    if (!inherits(vr, "error") & !is.null(vr)) {

      subnatPyramidsChart(vr, epi_graph_filter_results)

    } else {
      NULL
    }
  }, height = 600, width = 800)

  ## KP CASCADE GRAPH --------------------------------------
  output$kp_cascade <- renderPlot({

    vr <- validation_results()
    kpCascadeInput_filter_results <- kpCascadeInput_filter$snu_filter

    if (!inherits(vr, "error") & !is.null(vr)) {

      kpCascadeChart(vr, kpCascadeInput_filter_results)

    } else {
      NULL
    }
  }, height = 600, width = 800)

  ## PIVOT TABLE --------------------------------------

  output$pivot <- renderRpivotTable({
    vr <- validation_results()

    if (!inherits(vr, "error") & !is.null(vr)) {

      if (is.null(vr$data$analytics)) {
        return(NULL)
      }

      PSNUxIM_pivot(vr)

    } else {
      NULL
    }
  })

  ## HTS RECENCY TABLE  --------------------------------------

  output$hts_recency <- DT::renderDataTable({

    vr <- validation_results()

    if (!inherits(vr, "error") & !is.null(vr)) {

      if (is.null(vr$data$analytics)) {
        return(NULL)
      }
      r <- recencyComparison(vr)
      DT::datatable(r,
                    options = list(pageLength = 25, columnDefs = list(list(
                      className = "dt-right", targets = 2),
                      list(
                        className = "dt-right", targets = 3),
                      list(
                        className = "dt-right", targets = 4)
                    )))

    } else {
      NULL
    }
  })

  ## MODALITY GRAPH --------------------------------------

  output$modality_summary <- renderPlot({

    vr <- validation_results()

    if (!inherits(vr, "error") & !is.null(vr)) {
      analytics <-
        vr %>%
        purrr::pluck(., "data") %>%
        purrr::pluck(., "analytics")

      if (is.null(analytics)) {
        return(NULL)
      } else {
        modalitySummaryChart(analytics)
      }

    } else {
      NULL
    }

  }, height = 600, width = 800)

  ## MODALITY YIELD GRAPH --------------------------------------

  output$modality_yield <- renderPlot({

    vr <- validation_results()

    if (!inherits(vr, "error") & !is.null(vr)) {
      vr  %>%
        purrr::pluck(., "data") %>%
        purrr::pluck(., "analytics") %>%
        modalityYieldChart()
    } else {
      NULL
    }

  }, height = 400, width = 600)

  ## VLS SUMMARY GRAPH --------------------------------------

  output$vls_summary <- renderPlot({

    vr <- validation_results()

    if (!inherits(vr, "error") & !is.null(vr)) {
      vr %>%
        purrr::pluck(., "data") %>%
        purrr::pluck(., "analytics") %>%
        vlsTestingChart()

    } else {
      NULL
    }

  }, height = 600, width = 800)

  ## MODALITY TABLE --------------------------------------

  output$modality_table <- DT::renderDataTable({

    vr <- validation_results()

    if (!inherits(vr, "error") & !is.null(vr)) {
      hts_table <- modalitySummaryTable(vr$data$analytics)

      if (is.null(hts_table)) {
        return(NULL)
      }

      table_formatted <- hts_table %>%
        dplyr::mutate(
          Positive = format(Positive, big.mark = ",", scientific = FALSE),
          Total = format(Total, big.mark = ",", scientific = FALSE),
          yield = format(round(yield, 2), nsmall = 2),
          modality_share = format(round(modality_share, 2), nsmall = 2)) %>%
        dplyr::select(Modality = hts_modality,
                      Positive,
                      Total,
                      "Yield (%)" = yield,
                      "Percent of HTS_POS" = modality_share)

      DT::datatable(table_formatted,
                    options = list(pageLength = 25, columnDefs = list(list(
                      className = "dt-right", targets = 2),
                      list(
                        className = "dt-right", targets = 3),
                      list(
                        className = "dt-right", targets = 4),
                      list(
                        className = "dt-right", targets = 5)
                    )))
    } else {
      NULL
    }
  })

  ## INDICATOR SUMMARY TABLE --------------------------------------

  output$indicator_summary <- DT::renderDataTable({

    vr <- validation_results()

    if (!inherits(vr, "error") & !is.null(vr)) {

      if (vr$info$tool == "Data Pack") {
        vr %<>%
          purrr::pluck(., "data") %>%
          purrr::pluck(., "MER")
      } else if (vr$info$tool == "OPU Data Pack") {
        vr %<>%
          purrr::pluck(., "data") %>%
          purrr::pluck(., "extract")
      }

      vr %>%
        dplyr::group_by(indicator_code) %>%
        dplyr::summarise(value = format(round(sum(value)),
                                         big.mark = ",",
                                         scientific = FALSE)) %>%
        dplyr::arrange(indicator_code)

    } else {
      data.frame(message = "No data is available to display. An error may have occurred.")
    }
  })

  ## VALIDATION RULES TABLE  --------------------------------------

  output$vr_rules <- DT::renderDataTable({

    vr <- validation_results() %>%
      purrr::pluck(., "tests") %>%
      purrr::pluck(., "vr_rules_check")

    if (inherits(vr, "error")  | is.null(vr)) {
      data.frame(message = "No data is available to display. An error may have occurred.")
    }

    if (NROW(vr) == 0) {

      data.frame(message = "Congratulations! No validation rule issues found!")

    } else {
      vr
    }
  })

  ## SNU SELECTOR REACTIVE --------------------------------------

  snu_selector <- reactive({
    validation_results() %>%
      snuSelector()
    })

  ## WAITING SCREEN --------------------------------------
  waiting_screen_datapack <- tagList(
    spin_hourglass(),
    h4("Generating your SNUxIM tab. Please wait...")
  )

  ## DOWNLOAD DATA PACK BUTTON --------------------------------------

  output$downloadDataPack <- downloadHandler(

    filename = function() {
      d <- validation_results()
      prefix <- d$info$sane_name
      date <- format(Sys.time(), "%Y%m%d_%H%M%S")
      paste0(paste(prefix, date, sep = "_"), ".xlsx")

    },
    content = function(file) {

      d <- validation_results()
      shinyjs::disable("downloadDataPack")
      flog.info(
        paste0("Regeneration of Datapack requested for ", d$info$datapack_name),
        name = "datapack")
      waiter_show(html = waiting_screen_datapack,
                  color = "rgba(128,128,128,.8)")
      fetchSupportFiles()
      support_file <- paste0("./", Sys.getenv("MODEL_PATH"))
      d <- writePSNUxIM(d, snuxim_model_data_path = support_file)
      flog.info(
        paste0("Datapack reloaded for for ", d$info$datapack_name),
        name = "datapack")
      openxlsx::saveWorkbook(wb = d$tool$wb, file = file, overwrite = TRUE)
      waiter_hide()
    }

  )

  ## DOWNLOAD VALIDATION RESULTS BUTTON --------------------------------------

  output$downloadValidationResults <- downloadHandler(
    filename = function() {

      prefix <- "validation_results"

      date <- format(Sys.time(), "%Y%m%d_%H%M%S")

      paste0(paste(prefix, date, sep = "_"), ".xlsx")
    },
    content = function(file) {

      d <- validation_results()

      sheets_with_data <- d$tests[lapply(d$tests, NROW) > 0]

      if (length(sheets_with_data) > 0) {
        openxlsx::write.xlsx(sheets_with_data, file = file)
      } else {
        shinyjs::disable("downloadValidationResults")
        showModal(modalDialog(
          title = "Perfect score!",
          "No validation issues, so nothing to download!"
        ))
      }

    }
  )

  ## DOWNLOAD DATIM COMPARISON BUTTON --------------------------------------

  output$compare <- downloadHandler(
    filename = function() {

      prefix <- "comparison"

      date <- format(Sys.time(), "%Y%m%d_%H%M%S")

      paste0(paste(prefix, date, sep = "_"), ".xlsx")
    },
    content = function(file) {

      #Create a new workbook
      wb <- openxlsx::createWorkbook()

      d <- validation_results()

      if (d$info$tool == "Data Pack") {
        #Requires refactor to deal with http handles
        d_compare <- datapackr::compareData_DatapackVsDatim(d)

        openxlsx::addWorksheet(wb, "PSNUxIM without dedupe")
        openxlsx::writeDataTable(wb = wb,
                                 sheet = "PSNUxIM without dedupe",
                                 x = d_compare$psnu_x_im_wo_dedup)

        openxlsx::addWorksheet(wb, "PSNU with dedupe")
        openxlsx::writeDataTable(wb = wb,
                                 sheet = "PSNU with dedupe",
                                 x = d_compare$psnu_w_dedup)
      } else if (d$info$tool == "OPU Data Pack") {
        d_compare <- datapackr::compareData_OpuDatapackVsDatim(d)

        openxlsx::addWorksheet(wb, "Updated Targets")
        openxlsx::writeDataTable(wb = wb,
                                 sheet = "Updated Targets",
                                 x = d_compare$updates)


        openxlsx::addWorksheet(wb, "Deleted Targets")
        openxlsx::writeDataTable(wb = wb,
                                 sheet = "Deleted Targets",
                                 x = d_compare$deletes)

        openxlsx::addWorksheet(wb, "Deduplications")
        openxlsx::writeDataTable(wb = wb,
                                 sheet = "Deduplications",
                                 x = d_compare$dedupes)
      }

      datapack_name <- d$info$datapack_name

      flog.info(paste0("Comparison requested for ", datapack_name),
        name = "datapack")

      openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)

    }
  )

  ## DOWNLOAD FLATPACK BUTTON --------------------------------------

  output$downloadFlatPack <- downloadHandler(
    filename = function() {

      prefix <- "flatpack"

      date <- format(Sys.time(), "%Y%m%d_%H%M%S")

      paste0(paste(prefix, date, sep = "_"), ".xlsx")
    },
    content = function(file) {

      #Create a new workbook
      wb <- openxlsx::createWorkbook()

      d <- validation_results()

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

          openxlsx::addWorksheet(wb, "Distributed MER Data")
          openxlsx::writeDataTable(wb = wb,
                                   sheet = "Distributed MER Data",
                                   x = d$data$analytics)

          d$datim$MER$value <- as.character(d$datim$MER$value)
          d$datim$subnat_impatt$value <- as.character(d$datim$subnat_impatt$value)
          datim_export <- dplyr::bind_rows(d$datim$MER, d$datim$subnat_impatt)

          openxlsx::addWorksheet(wb, "DATIM export")
          openxlsx::writeData(wb = wb,
                              sheet = "DATIM export", x = datim_export)

          openxlsx::addWorksheet(wb, "Rounding diffs")
          openxlsx::writeData(wb = wb,
                              sheet = "Rounding diffs",
                              x = d$tests$PSNUxIM_rounding_diffs)

          openxlsx::addWorksheet(wb, "HTS Summary")
          hts_summary <- modalitySummaryTable(d$data$analytics)

          if (!is.null(hts_summary)) {
            openxlsx::writeData(wb = wb,
                                sheet = "HTS Summary", x = hts_summary)
          }
        }
      } else if (d$info$tool == "OPU Data Pack") {
        openxlsx::addWorksheet(wb,"Distributed MER Data")
        openxlsx::writeDataTable(wb = wb,
                                 sheet = "Distributed MER Data",
                                 x = d$data$extract)

        openxlsx::addWorksheet(wb,"Rounding diffs")
        openxlsx::writeData(wb = wb,
                            sheet = "Rounding diffs",
                            x = d$tests$PSNUxIM_rounding_diffs)

        openxlsx::addWorksheet(wb,"Analytics Summary")
        openxlsx::writeData(wb = wb,
                            sheet = "Analytics Summary",
                            x = modalitySummaryTable(d$data$analytics))
      }

      datapack_name <- d$info$datapack_name

      flog.info(paste0("Flatpack requested for ", datapack_name),
                name = "datapack")

      openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)

    }
  )

  ## VALIDATION MESSAGES UI --------------------------------------

  output$messages <- renderUI({

    vr <- validation_results()

    messages <- NULL

    if (is.null(vr)) {
      return(NULL)
    }

    if (inherits(vr, "error")) {
      return(paste0("ERROR! ", vr$message))

    } else {

      messages <- validation_results() %>%
        purrr::pluck(., "info") %>%
        purrr::pluck(., "warning_msg")

      if (!is.null(messages))  {
        lapply(messages, function(x) tags$li(x))
      } else {
        tags$li("No Issues with Integrity Checks: Congratulations!")
      }
    }
  })

  ## DOWNLOAD MESSAGES BUTTON --------------------------------------

  output$download_messages <- downloadHandler(
    filename = function() {
      paste("DataPack_Validation_Messages_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      vr <- validation_results()

      writeLines(vr$info$warning_msg, file)
    }
  )
  })
