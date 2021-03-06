
pacman::p_load(shiny,shinyjs,shinyWidgets,magrittr,dplyr,datimvalidation,ggplot2,
               futile.logger, paws, datapackr, scales, DT, purrr, praise,rpivotTable,waiter)


#Set the maximum file size for the upload file
options(shiny.maxRequestSize = 100 * 1024 ^ 2)
#Allow unsanitized error messages
options(shiny.sanitize.errors = FALSE)
#Initiate logging
logger <- flog.logger()

if (!file.exists(Sys.getenv("LOG_PATH"))) {
  file.create(Sys.getenv("LOG_PATH"))
}

flog.appender(appender.console(), name="datapack")


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
    shinyjs::disable("downloadDataPack")
    #shinyjs::disable("download_messages")
    shinyjs::disable("send_paw")
    shinyjs::disable("downloadValidationResults")
    shinyjs::disable("compare")
    ready$ok<-FALSE
  })
  
  
  waiting_screen_paw<-tagList(
    spin_ring(),
    h4("Transferring files to PAW. Please wait...")
  )
  
  observeEvent(input$send_paw, {
    waiter_show(html = waiting_screen_paw, color = "rgba(128,128,128,.8)" )
    d <- validation_results()
    r<-saveTimeStampLogToS3(d)
    timestampUploadUI(r)
    #r<-sendMERDataToPAW(d)
    archiveDataPackErrorUI(r)
    r<-sendValidationSummary(d,"validation_error")
    validationSummaryUI(r)
    r<-saveDATIMExportToS3(d)
    waiter_hide()
    datimExportUI(r)
  })

  observeEvent(input$login_button,
               {

                 tryCatch(  {  datimutils::loginToDATIM(base_url = Sys.getenv("BASE_URL"),
                                                        username = input$user_name,
                                                        password = input$password,
                                                d2_session_envir = parent.env(environment()))
                   },
                            #This function throws an error if the login is not successful
                            error=function(e) {
                              sendSweetAlert(
                                session,
                                title = "Login failed",
                                text = "Please check your username/password!",
                                type = "error")
                              flog.info(paste0("User ", input$user_name, " login failed."), name = "datapack")
                            } )

                 if ( exists("d2_default_session"))  {

                   user_input$authenticated<-TRUE
                   user_input$d2_session<-d2_default_session$clone()
                   flog.info(paste0("User ", user_input$d2_session$me$userCredentials$username, " logged in."), name = "datapack")

                 }

               })

  epi_graph_filter <- reactiveValues( snu_filter=NULL )

  observeEvent(input$epiCascadeInput,{
    epi_graph_filter$snu_filter<-input$epiCascadeInput
  })

  kpCascadeInput_filter <- reactiveValues( snu_filter=NULL )

  observeEvent(input$kpCascadeInput,{
    kpCascadeInput_filter$snu_filter<-input$kpCascadeInput
  })

  observeEvent(input$logout,{
    flog.info(paste0("User ", user_input$d2_session$me$userCredentials$username, " logged out."))
    ready$ok <- FALSE
    user_input$authenticated<-FALSE
    user_input$d2_session<-NULL
    gc()

  } )


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

      wiki_url <- a("Datapack User Guide",
                    href="https://apps.datim.org/datapack-userguide/",
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
            selectInput("downloadType","Download type",NULL),
            downloadButton("downloadOutputs","Download"),
            tags$hr(),
            actionButton("send_paw", "Send to PAW"),
            tags$hr(),
            div(style = "display: inline-block; vertical-align:top; width: 80 px;", actionButton("reset_input", "Reset inputs")),
            div(style = "display: inline-block; vertical-align:top; width: 80 px;", actionButton("logout", "Logout"))
          ),

          mainPanel(tabsetPanel(
            id = "main-panel",
            type = "tabs",
            tabPanel("Messages", tags$ul(uiOutput('messages'))),
            tabPanel("Analytics checks", tags$div(uiOutput('analytics_checks'))),
            tabPanel("Indicator summary", dataTableOutput("indicator_summary"),
                     tags$h4("Data source: Main DataPack tabs")),
            tabPanel("SNU-level summary", 
                     dataTableOutput("snu_summary"),
                     tags$h4("Data source: Main DataPack tabs")),
            tabPanel("Validation rules", 
                     dataTableOutput("vr_rules"),
                     tags$h4("Data source: PSNUxIM tab")),
            tabPanel("HTS Summary Chart", 
                     fluidRow(column(width=12,div(style="height:700px",plotOutput("modality_summary")))),
                     fluidRow(column(width=12,tags$h4("Data source: PSNUxIM tab")))),
            tabPanel("HTS Summary Table",
                     dataTableOutput("modality_table"),
                     tags$h4("Data source: PSNUxIM tab")),
            tabPanel("HTS Yield",
                     fluidRow(column(width=12,div(style="height:700px",plotOutput("modality_yield")))),
                     fluidRow(tags$h4("Data source: PSNUxIM tab"))),
            tabPanel("HTS Recency",
                     dataTableOutput("hts_recency"),
                     tags$h4("Data source: PSNUxIM tab")),
            tabPanel("VLS Testing",
                     fluidRow(column(width=12,div(style="height:700px",plotOutput("vls_summary")))),
                     fluidRow(column(width=12,tags$h4("Data source: PSNUxIM tab")))),
            tabPanel("Epi Cascade Pyramid",
                     pickerInput("epiCascadeInput","SNU1",
                                 choices= "",
                                 options = list(`actions-box` = TRUE),multiple = T),
                     fluidRow(column(width=12,div(style="height:700px",plotOutput("epi_cascade")))),
                     fluidRow(tags$h4("Data source: SUBNATT/IMPATT data & PSNUxIM tab"))),
            tabPanel("KP Cascade Pyramid",
                     pickerInput("kpCascadeInput","SNU1",
                                 choices= "",
                                 options = list(`actions-box` = TRUE),multiple = T),
                     fluidRow(column(width=12,div(style="height:700px",plotOutput("kp_cascade")))),
                     fluidRow(tags$h4("Data source: Data source: SUBNATT/IMPATT data & PSNUxIM tab"))),
            tabPanel("PSNUxIM Pivot",
                     fluidRow(column(width=12,div(rpivotTable::rpivotTableOutput({"pivot"})))),
                     fluidRow(tags$h4("Data source: PSNUxIM tab"))),
            tabPanel("Prioritization (DRAFT)",
                     DT::dataTableOutput("prio_table"),
                     h5("Note: This is a draft memo table. Final figures may differ."),
                     tags$h4("Data source: PSNUxIM tab"))

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
      img(src='pepfar.png', align = "center"),
      h4("Welcome to the DataPack Validation App. Please login with your DATIM credentials:")
    ),
    fluidRow(
      textInput("user_name", "Username: ",width = "600px"),
      passwordInput("password", "Password:",width = "600px"),
      actionButton("login_button", "Log in!")
    ),
    fluidRow(
      tags$hr(),
      tags$div(HTML('<ul><li><h4>Please be sure you fully populate the PSNUxIM tab when receiving a new DataPack. 
           Consult <a href="https://apps.datim.org/datapack-userguide/" target = "_blank" > the user guide</a> for further information!</h4></li>
                    <li><h4>See the latest updates to the app <a href="https://github.com/pepfar-datim/datapackr-app/blob/master/CHANGELOG.md" target ="_blank">here.</h4></a></li></ul>'))
    ),
    tags$hr(),
    fluidRow(HTML(getVersionInfo())))
  })

  validate<-function() {
    shinyjs::disable("downloadType")
    shinyjs::disable("downloadOutputs")
    shinyjs::disable("send_paw")

    if (!ready$ok) {
      shinyjs::disable("validate")
      return(NULL)
    }

    inFile <- input$file1
    messages<-""

    if ( is.null( inFile ) ) return( NULL )

    messages<-list()

    withProgress(message = 'Validating file', value = 0,{

      shinyjs::disable("file1")
      shinyjs::disable("validate")
      incProgress(0.1, detail = ("Unpacking your DataPack"))


      d<-tryCatch({
        datapackr::unPackTool(inFile$datapath,
                              d2_session = user_input$d2_session)},
        error = function(e){
          return(e)
        })

      if( inherits(d,"error") ) {
        return("An error occurred. Please contact DATIM support.")
      }
      
      if (!inherits(d,"error") & !is.null(d)) {
        #Create some additional metadadta for S3 tagging
        d$info$sane_name<-paste0(stringr::str_extract_all(d$info$datapack_name,"[A-Za-z0-9_]",
                                                          simplify = TRUE),sep="",collapse="")
        d$info$source_user<-user_input$d2_session$me$userCredentials$username
        #All self-service datapacks should be marked as unapproved for PAW
        d$info$approval_status<-"UNAPPROVED"
        #Generate a unique identifier
        d$info$uuid<-uuid::UUIDgenerate()
        d$info$operating_unit<-getOperatingUnitFromCountryUIDs(d$info$country_uids)



        flog.info(paste0("Initiating validation of ",d$info$datapack_name, " DataPack."), name="datapack")
        if (d$info$tool == "Data Pack") {
          
          d$info$needs_psnuxim <- d$info$missing_psnuxim_combos | (NROW(d$data$SNUxIM) == 1 & is.na(d$data$SNUxIM[[1,1]]))
          updateSelectInput(session = session, inputId="downloadType",
                            choices=downloadTypes(tool_type= d$info$tool,
                                                  needs_psnuxim = d$info$needs_psnuxim))
          if ( d$info$has_psnuxim & NROW(d$data$SNUxIM) > 0 ) {
            
            flog.info(paste(d$info$tool," with PSNUxIM tab found."))
            incProgress(0.1, detail = ("Checking validation rules"))
            Sys.sleep(0.5)
            d <- validatePSNUData(d, d2_session = user_input$d2_session)
            incProgress(0.1,detail="Validating mechanisms")
            Sys.sleep(0.5)
            d <- validateMechanisms(d, d2_session = user_input$d2_session)

            if (Sys.getenv("SEND_DATAPACK_ARCHIVE") == "TRUE" ) {
              incProgress(0.1, detail = ("Saving a copy of your submission to the archives"))
              Sys.sleep(0.5)
              r<-archiveDataPacktoS3(d,inFile$datapath)
              archiveDataPackErrorUI(r)
              Sys.sleep(1)
            }

            incProgress(0.1, detail = ("Preparing a prioritization table"))
            d<-preparePrioTable(d,d2_session = user_input$d2_session)
            Sys.sleep(1)
            incProgress(0.1, detail = ("Preparing a modality summary"))
            d<-modalitySummaryTable(d)
            Sys.sleep(1)
            incProgress(0.1, detail = ("Preparing a HTS recency analysis"))
            d<-recencyComparison(d)
            Sys.sleep(1)
            incProgress(0.1, detail = ("Performing analytics checks"))
            model_data_path<-"support_files/datapack_model_data.rds"
            full_model_path<-fetchModelFile(model_data_path)
            d<-checkAnalytics(d,model_data_path =full_model_path, d2_session = user_input$d2_session )
            Sys.sleep(1)
            incProgress(0.1, detail = ("Finishing up."))
            r<-sendValidationSummary(d,"app_analytics",include_timestamp = TRUE)
            validationSummaryUI(r)
            
            shinyjs::enable("downloadType")
            shinyjs::enable("downloadOutputs")
            shinyjs::enable("send_paw")

            # #TODO: The list of available download types needs to change here...
            # if (d$info$cop_year == 2020) {
            #   shinyjs::enable("compare")
            # } else {
            #   shinyjs::disable("compare")
            # }
            # 
            # if ( d$info$missing_psnuxim_combos ) {
            #   shinyjs::enable("downloadDataPack")
            # }

            updatePickerInput(session = session, inputId = "kpCascadeInput",
                              choices = snuSelector(d))
            updatePickerInput(session = session, inputId = "epiCascadeInput",
                              choices = snuSelector(d))

            showTab(inputId = "main-panel", target = "Validation rules")
            showTab(inputId = "main-panel", target = "HTS Summary Chart")
            showTab(inputId = "main-panel", target = "HTS Summary Table")
            showTab(inputId = "main-panel", target = "HTS Yield")
            showTab(inputId = "main-panel", target = "VLS Testing")
            showTab(inputId = "main-panel", target = "Epi Cascade Pyramid")
            showTab(inputId = "main-panel", target = "KP Cascade Pyramid")
            showTab(inputId = "main-panel", target = "PSNUxIM Pivot")
            showTab(inputId = "main-panel", target = "HTS Recency")
            showTab(inputId = "main-panel", target = "Prioritization (DRAFT)")

          } else if ( d$info$has_psnuxim & NROW(d$data$SNUxIM) == 0 ) {
            msg<-"ERROR! Your DataPack contains a PSNUxIM tab, but the formulas appear to be empty. Please ensure that the formulas have been properly populated in the PSNUxIM tab."
            d$info$warning_msg<-append(d$info$warning_msg,msg)
            

            shinyjs::enable("downloadType")
            shinyjs::enable("downloadOutputs")
            shinyjs::enable("send_paw")
            hideTab(inputId = "main-panel", target = "Validation rules")
            hideTab(inputId = "main-panel", target = "HTS Summary Chart")
            hideTab(inputId = "main-panel", target = "HTS Summary Table")
            hideTab(inputId = "main-panel", target = "HTS Yield")
            hideTab(inputId = "main-panel", target = "VLS Testing")
            showTab(inputId = "main-panel", target = "Epi Cascade Pyramid")
            hideTab(inputId = "main-panel", target = "KP Cascade Pyramid")
            hideTab(inputId = "main-panel", target = "PSNUxIM Pivot")
            hideTab(inputId = "main-panel", target = "HTS Recency")
            hideTab(inputId = "main-panel", target = "Prioritization (DRAFT)")
            
          } else  {
            #This should occur when there is no PSNUxIM tab and they want
            #to generate one.
            shinyjs::enable("downloadType")
            shinyjs::enable("downloadOutputs")
            shinyjs::enable("send_paw")
            hideTab(inputId = "main-panel", target = "Validation rules")
            hideTab(inputId = "main-panel", target = "HTS Summary Chart")
            hideTab(inputId = "main-panel", target = "HTS Summary Table")
            hideTab(inputId = "main-panel", target = "HTS Yield")
            hideTab(inputId = "main-panel", target = "VLS Testing")
            hideTab(inputId = "main-panel", target = "Epi Cascade Pyramid")
            hideTab(inputId = "main-panel", target = "KP Cascade Pyramid")
            hideTab(inputId = "main-panel", target = "PSNUxIM Pivot")
            hideTab(inputId = "main-panel", target = "HTS Recency")
            hideTab(inputId = "main-panel", target = "Prioritization (DRAFT)")
          }
        }
      }

      if (d$info$tool == "OPU Data Pack"){
        d$info$needs_psnuxim <- FALSE
        updateSelectInput(session = session, inputId="downloadType",
                          choices=downloadTypes(tool_type= d$info$tool,
                                                needs_psnuxim = d$info$needs_psnuxim))
        flog.info("Datapack with PSNUxIM tab found.")
        incProgress(0.1, detail = ("Checking validation rules"))
        Sys.sleep(0.5)
        d <- validatePSNUData(d, d2_session = user_input$d2_session)
        incProgress(0.1,detail="Validating mechanisms")
        Sys.sleep(0.5)
        d <- validateMechanisms(d, d2_session = user_input$d2_session)
        incProgress(0.1,detail="Updating prioritization levels from DATIM")
        Sys.sleep(0.5)
        #Move this to datapackr
        d<-updateExistingPrioritization(d,d2_session = user_input$d2_session)
        incProgress(0.1, detail = ("Preparing a prioritization table"))
        d<-preparePrioTable(d,d2_session = user_input$d2_session)
        incProgress(0.1, detail = ("Preparing a modality summary"))
        d<-modalitySummaryTable(d)
        Sys.sleep(1)
        incProgress(0.1, detail = ("Preparing a HTS recency analysis"))
        d<-recencyComparison(d)
        Sys.sleep(1)
        shinyjs::enable("downloadType")
        shinyjs::enable("downloadOutputs")
        shinyjs::disable("send_paw")

        
        # #TODO: Fix this once COP 2021 comparisons are functional
        # if (d$info$cop_year == 2020) {
        #   shinyjs::enable("compare")
        # } else {
        #   shinyjs::disable("compare")
        # }
        
        updatePickerInput(session = session, inputId = "kpCascadeInput",
                          choices = snuSelector(d))
        updatePickerInput(session = session, inputId = "epiCascadeInput",
                          choices = snuSelector(d))
        
        showTab(inputId = "main-panel", target = "Validation rules")
        showTab(inputId = "main-panel", target = "HTS Summary Chart")
        showTab(inputId = "main-panel", target = "HTS Summary Table")
        showTab(inputId = "main-panel", target = "HTS Yield")
        showTab(inputId = "main-panel", target = "VLS Testing")
        showTab(inputId = "main-panel", target = "Epi Cascade Pyramid")
        showTab(inputId = "main-panel", target = "KP Cascade Pyramid")
        showTab(inputId = "main-panel", target = "PSNUxIM Pivot")
        showTab(inputId = "main-panel", target = "HTS Recency")
        showTab(inputId = "main-panel", target = "Prioritization")
      }

    })

    return(d)

  }

  validation_results <- reactive({ validate() })

  output$epi_cascade<-renderPlot({

    vr<-validation_results()
    epi_graph_filter_results<-epi_graph_filter$snu_filter

    if (!inherits(vr,"error") & !is.null(vr)){

      subnatPyramidsChart(vr,epi_graph_filter_results)

    } else {
      NULL
    }
  },height = 600,width = 800)

  output$kp_cascade<-renderPlot({

    vr<-validation_results()
    kpCascadeInput_filter_results<-kpCascadeInput_filter$snu_filter

    if (!inherits(vr,"error") & !is.null(vr)){

      kpCascadeChart(vr,kpCascadeInput_filter_results)

    } else {
      NULL
    }
  },height = 600,width = 800)

  output$pivot <- renderRpivotTable({
    vr<-validation_results()

    if (!inherits(vr,"error") & !is.null(vr)){

      if ( is.null(vr$data$analytics) ) {return(NULL)}
      PSNUxIM_pivot(vr)

    } else {
      NULL
    }
  })

  output$prio_table <- DT::renderDataTable({
    prio_table<-validation_results() %>%
      purrr::pluck("data") %>%
      purrr::pluck("prio_table")

    if (!inherits(prio_table,"error") & !is.null(prio_table)){

          DT::datatable(prio_table,options = list(pageLength = 50,
                                         columnDefs = list(list(className = 'dt-right',
                                                                targets = 3:dim(prio_table)[2])))) %>%
            formatCurrency(3:dim(prio_table)[2], '',digits =0)


    } else {
      NULL
    }
  })

  output$hts_recency<-DT::renderDataTable({

    vr<-validation_results()

    if (!inherits(vr,"error") & !is.null(vr$data$recency)){

      DT::datatable(vr$data$recency,
                    options = list(pageLength = 25,columnDefs = list(list(
                      className = 'dt-right', targets = 2),
                      list(
                        className = 'dt-right', targets = 3),
                      list(
                        className = 'dt-right', targets = 4)
                    )))

    } else {
      NULL
    }
  })

  output$modality_summary <- renderPlot({

    vr<-validation_results()

    if (!inherits(vr,"error") & !is.null(vr)){
      analytics<-
        vr %>%
        purrr::pluck(.,"data") %>%
        purrr::pluck(.,"analytics")

      if (is.null(analytics)) {return(NULL)} else {
        modalitySummaryChart(vr)
      }

    } else {
      NULL
    }

  },height = 600,width = 800)

  output$modality_yield <- renderPlot({

    vr<-validation_results()

    if (!inherits(vr,"error") & !is.null(vr)){

        modalityYieldChart(vr)

    } else {
      NULL
    }

  },height = 400,width = 600)

  output$vls_summary <- renderPlot({

    vr<-validation_results()

    if (!inherits(vr,"error") & !is.null(vr)){
      vr  %>%
        purrr::pluck(.,"data") %>%
        purrr::pluck(.,"analytics") %>%
        vlsTestingChart()

    } else {
      NULL
    }

  },height = 600,width = 800)

  output$modality_table<-DT::renderDataTable({

    d<-validation_results()

    if (!inherits(d,"error") & !is.null(d$data$modality_summary)){

      DT::datatable(formatModalitySummaryTable(d),
                    options = list(pageLength = 25,columnDefs = list(list(
                      className = 'dt-right', targets = 2),
                      list(
                        className = 'dt-right', targets = 3),
                      list(
                        className = 'dt-right', targets = 4),
                      list(
                        className = 'dt-right', targets = 5)
                    )))
    } else {
      NULL
    }
  })

  output$indicator_summary<-DT::renderDataTable({

    vr<-validation_results()

    if (!inherits(vr,"error") & !is.null(vr)){

         prepareSNUSummaryTable(vr) %>% 
          dplyr::group_by(indicator_code) %>%
          dplyr::summarise(value = format( round(sum(value,na.rm=TRUE)) ,big.mark=',', scientific=FALSE)) %>%
          dplyr::arrange(indicator_code)

    } else {
      NULL
    }
  })

  output$snu_summary <- DT::renderDataTable({

    vr <- validation_results()

    if (!inherits(vr, "error") & !is.null(vr)) {

      prepareSNUSummaryTable(vr) %>% 
        dplyr::mutate(value = format( round_trunc(value), big.mark=',',scientific=FALSE))

    } else {
      data.frame(message = "No data is available to display. An error may have occurred.")
    }
  })

  output$vr_rules<-DT::renderDataTable({

    vr<-validation_results() %>%
      purrr::pluck(.,"tests") %>%
      purrr::pluck(.,"vr_rules_check")

    if ( inherits(vr,"error")  | is.null(vr) ){

      return(NULL)
    }

    if (NROW(vr) == 0 ) {

      data.frame(message="Congratulations! No validation rule issues found!")

    } else {
      vr
    }
  })

  snu_selector <- reactive({ validation_results() %>% snuSelector() })

  output$messages <- renderUI({

    vr<-validation_results()

    messages<-NULL

    if ( is.null(vr)) {
      return(NULL)
    }

    if ( inherits(vr,"error") ) {
      return( paste0("ERROR! ",vr$message) )

    } else {

      messages <- validation_results() %>%
        purrr::pluck(., "info") %>%
        purrr::pluck(., "warning_msg")

      if (!is.null(messages))  {
        lapply(messages, function(x)
          tags$li(x))
      } else
      {
        tags$li("No Issues with Integrity Checks: Congratulations!")
      }
    }
  })
  
  output$analytics_checks <- renderUI({
    
    vr<-validation_results()
    
    messages<-NULL
    
    if ( is.null(vr)) {
      return(NULL)
    }
    
    if ( inherits(vr,"error") ) {
      return( paste0("ERROR! ",vr$message) )
      
    } else {
      
      messages <- vr %>%
        purrr::pluck(., "info") %>%
        purrr::pluck(., "analytics_warning_msg") %>% 
        purrr::map(.,function(x) HTML(x))
      
      if (!is.null(messages))  {
        shiny::HTML(ansistrings::ansi_to_html(messages))
      } else
      {
        tags$li("No Issues with Analytics Checks: Congratulations!")
      }
    }
  })
  
  
  waiting_screen_datapack <- tagList(
    spin_hourglass(),
    h4("Generating your SNUxIM tab. Please wait...")
  )
  
  waiting_screen_comparison<-tagList(
    spin_hourglass(),
    h4("Generating a comparison to DATIM. Please wait...")
  )
  
  waiting_screen_flatpack<-tagList(
    spin_hourglass(),
    h4("Generating your FlatPack™. Please wait...")
  )
  

  output$downloadOutputs <- downloadHandler(
    filename = function(){
      prefix <- input$downloadType
      date <- date<-format(Sys.time(),"%Y%m%d_%H%M%S")
      suffix <- if (input$downloadType %in% c("messages")) {
        ".txt"
      } else {
        ".xlsx"
      }
      
      paste0(prefix,"_",date,suffix)
    },
    content = function(file){
      
      d<-validation_results()
      
      if (input$downloadType == "messages") {
        writeLines(d$info$warning_msg, file)
      }
      
      if (input$downloadType =="cso_flatpack") {

        cso_indicators<-d$info$schema %>% 
          dplyr::filter(value_type == "integer") %>% 
          dplyr::pull(indicator_code) %>% unique(.)
        
        cso_data<-d$data$analytics %>%
          dplyr::filter(indicator_code %in% cso_indicators) %>% 
          dplyr::filter(stringr::str_detect(psnu,"_Military",negate = TRUE)) %>% 
          dplyr::group_by(ou,country_name,snu1,psnu,indicator_code,dataelement_name,support_type,hts_modality,age,sex,key_population,resultstatus_specific,resultstatus_inclusive,top_level) %>% 
          dplyr::summarize(value = sum(target_value))
        
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb,"CSO export")
        openxlsx::writeData(wb = wb,
                            sheet = "CSO export",x = cso_data)
        openxlsx::saveWorkbook(wb,file=file,overwrite = TRUE)
      }
      
      if (input$downloadType =="flatpack") {
        
        waiter_show(html = waiting_screen_flatpack, color = "rgba(128,128,128,.8)" )
        #Create a new workbook
        wb <- openxlsx::createWorkbook()
        #Common to both OPUs and Datapacks
        openxlsx::addWorksheet(wb,"Analytics")
        openxlsx::writeDataTable(wb = wb,
                                 sheet = "Analytics",x = d$data$analytics)
        #TODO. How to handle indicators which should not be summed
        snu_summary <- prepareSNUSummaryTable(d)
        openxlsx::addWorksheet(wb,"SNU Summary")
        openxlsx::writeDataTable(wb = wb,
                                 sheet = "SNU Summary",x = snu_summary)
        if (!is.null(d$data$recency)) {
          openxlsx::addWorksheet(wb,"HTS Recency")
          openxlsx::writeDataTable(wb = wb,
                                   sheet = "HTS Recency", x = d$data$recency)
        }
        if (!is.null(d$data$modality_summary)) {
          openxlsx::addWorksheet(wb,"HTS Summary")
          openxlsx::writeDataTable(wb = wb,
                                   sheet = "HTS Summary", x = formatModalitySummaryTable(d))
        }
        if (!is.null(d$data$prio_table)) {
          openxlsx::addWorksheet(wb,"Prioritization (DRAFT)")
          openxlsx::writeData(wb = wb,
                              sheet = "Prioritization (DRAFT)",x = d$data$prio_table) }
        #Datapack specific
        if (d$info$tool == "Data Pack") {
          
          mer_data <- d %>%
            purrr::pluck(.,"data") %>%
            purrr::pluck(.,"MER")
          
          subnat_impatt <- d %>%
            purrr::pluck(.,"data") %>%
            purrr::pluck(.,"SUBNAT_IMPATT")
          
          mer_data<-dplyr::bind_rows(mer_data,subnat_impatt)
          openxlsx::addWorksheet(wb,"MER Data")
          openxlsx::writeDataTable(wb = wb,
                                   sheet = "MER Data",x = mer_data)

          has_psnu<-d %>%
            purrr::pluck(.,"info") %>%
            purrr::pluck(.,"has_psnuxim")
          
          if (has_psnu) {
            
            d$datim$MER$value<-as.character(d$datim$MER$value)
            d$datim$subnat_impatt$value<-as.character(d$datim$subnat_impatt$value)
            datim_export<-dplyr::bind_rows(d$datim$MER,d$datim$subnat_impatt)
            
            openxlsx::addWorksheet(wb,"DATIM export")
            openxlsx::writeData(wb = wb,
                                sheet = "DATIM export",x = datim_export)
          }
        }
        
        #OPU specific 
        if (d$info$tool == "OPU Data Pack") {
          
          openxlsx::addWorksheet(wb,"DATIM export")
          openxlsx::writeData(wb = wb,
                              sheet = "DATIM export",x = d$datim$OPU)
          
        }
        
        datapack_name <-d$info$datapack_name
        
        flog.info(
          paste0("Flatpack requested for ", datapack_name)
          ,
          name = "datapack"
        )
        
        openxlsx::saveWorkbook(wb,file=file,overwrite = TRUE)
        waiter_hide()
      }
      
      if (input$downloadType == "vr_rules") {
        
        sheets_with_data<-d$tests[lapply(d$tests,NROW) > 0]
        
        if (length(sheets_with_data)>0) {
          openxlsx::write.xlsx(sheets_with_data, file = file)
        } else {
          showModal(modalDialog(
            title = "Perfect score!",
            "No validation issues, so nothing to download!"
          ))
        }
      }
      
      if (input$downloadType == "datapack") {
        
        flog.info(
          paste0("Regeneration of Datapack requested for ", d$info$datapack_name)
          ,
          name = "datapack")
        waiter_show(html = waiting_screen_datapack, color = "rgba(128,128,128,.8)" )
        flog.info("Fetching support files")
        
        support_file<-fetchSupportFiles("/support_files/snuxim_model_data.rds")
        if (!file.exists(support_file)) {
          flog.error("Could not find model support file.")
          stop("WOMP!")
        }
        
        d <- writePSNUxIM(d,snuxim_model_data_path = support_file )
        unlink(support_file)
        flog.info(
          paste0("Datapack reloaded for for ", d$info$datapack_name) ,
          name = "datapack")
        openxlsx::saveWorkbook(wb = d$tool$wb, file = file, overwrite = TRUE)
        waiter_hide()
      }
      
      if (input$downloadType =="comparison") {
        waiter_show(html = waiting_screen_comparison, color = "rgba(128,128,128,.8)" )
        #Create a new workbook
        wb <- openxlsx::createWorkbook()
        d<-validation_results()
        if (d$info$tool == "OPU Data Pack") {
          d_compare<-datapackr::compareData_OpuDatapackVsDatim(d, d2_session = user_input$d2_session) 
          d_compare<-lapply(d_compare,function(x) adorn_import_file(x,
                                                                    cop_year = d$info$cop_year,
                                                                    d2_session = user_input$d2_session))
        } else if (d$info$tool == "Data Pack") {
          d_compare<-datapackr::compareData_DatapackVsDatim(d,d2_session = user_input$d2_session)
        }
    
          for(name in names(d_compare)){
            foo <- d_compare %>% purrr::pluck(name)
            openxlsx::addWorksheet(wb,name)
            openxlsx::writeDataTable(wb = wb,
                                     sheet = name,x = foo) }
            
        datapack_name <-d$info$datapack_name
        
        flog.info(
          paste0("Comparison requested for ", datapack_name)
          ,
          name = "datapack"
        )
        
        openxlsx::saveWorkbook(wb,file=file,overwrite = TRUE)
        waiter_hide()
      }
    }
  )
  
  
  
  })
