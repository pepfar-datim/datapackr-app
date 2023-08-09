
pacman::p_load(shiny, shinyjs, shinyWidgets, magrittr, dplyr,
               datimvalidation, ggplot2, datimutils,
               futile.logger, paws, datapackr, scales,
               DT, purrr, rpivotTable, waiter,
               flextable, officer, gdtools, digest, fansi)

# js ----
# allows for using the enter button
jscode_login <- '$(document).keyup(function(e) {
    var focusedElement = document.activeElement.id;
    console.log(focusedElement);
    if (e.key == "Enter" && focusedElement == "user_name") {
    $("#password").focus();
    } else if (e.key == "Enter" && focusedElement == "password") {
    $("#login_button").click();
    }
});'

#Set the maximum file size for the upload file
options(shiny.maxRequestSize = 150 * 1024 ^ 2)
#Allow unsanitized error messages
options(shiny.sanitize.errors = FALSE)
#Initiate logging
logger <- flog.logger()

if (!file.exists(Sys.getenv("LOG_PATH"))) {
  file.create(Sys.getenv("LOG_PATH"))
}

flog.appender(appender.console(), name = "datapack")

################ OAuth Client information #####################################
if (interactive()) {
  # testing url
  options(shiny.port = 3123)
  APP_URL <- "http://127.0.0.1:3123/"# This will be your local host path
} else {
  # deployed URL
  APP_URL <- Sys.getenv("APP_URL") #This will be your shiny server path
}

{

  oauth_app <- httr::oauth_app(Sys.getenv("OAUTH_APPNAME"),
                               key = Sys.getenv("OAUTH_KEYNAME"),        # dhis2 = Client ID
                               secret = Sys.getenv("OAUTH_SECRET"), #dhis2 = Client Secret
                               redirect_uri = APP_URL
  )

  oauth_api <- httr::oauth_endpoint(base_url = paste0(Sys.getenv("BASE_URL"),"uaa/oauth"),
                                    request=NULL,# Documentation says to leave this NULL for OAuth2
                                    authorize = "authorize",
                                    access="token"
  )

  oauth_scope <- "ALL"
}

has_auth_code <- function(params) {

  return(!is.null(params$code))
}



shinyServer(function(input, output, session) {

  validation_results <- reactive({ validate() }) # nolint

  ready <- reactiveValues(ok = FALSE)

  user_input <- reactiveValues(authenticated = FALSE,
                                 status = "",
                                 d2_session = NULL,
                                 memo_authorized = FALSE,
                                 file1_state  = NULL,
                                 file2_state = NULL,
                                 uuid = NULL)

  epi_graph_filter <- reactiveValues(snu_filter = NULL)

  kpCascadeInput_filter <- reactiveValues(snu_filter = NULL)

  snu_selector <- reactive({
    validation_results() %>% snuSelector()
  })



  observeEvent(input$file1, {

    shinyjs::show("validate")
    user_input$file1_state <- 'uploaded'
    ready$ok <- FALSE
    shinyjs::enable("validate")
  })

  observeEvent(input$file2, {

    shinyjs::show("validate")
    user_input$file2_state <- 'uploaded'
    ready$ok <- FALSE
    shinyjs::enable("validate")
  })

  observeEvent(input$validate, {
    shinyjs::disable("file1")
    shinyjs::disable("file2")
    shinyjs::disable("validate")
    ready$ok <- TRUE
  })

  observeEvent(input$reset_input, {
    shinyjs::reset("side-panel")
    shinyjs::reset("file1")
    shinyjs::reset("file2")
    shinyjs::enable("file1")
    shinyjs::enable("file2")
    shinyjs::disable("validate")
    shinyjs::disable("downloadDataPack")
    shinyjs::disable("download_messages")
    shinyjs::disable("send_paw")
    shinyjs::disable("downloadValidationResults")
    shinyjs::disable("compare")
    user_input$file1_state <- 'reset'
    user_input$file2_state <- 'reset'
    ready$ok <- FALSE
  })

  file_input <- reactive({

    file1_state <- user_input$file1_state
    file2_state <- user_input$file2_state

    if (is.null(file1_state) && is.null(file2_state)) {
      return(NULL)
    }

    input_files <- list()

    if (!is.null(file1_state)) {
      if (file1_state == "uploaded") {
        input_files$file1 <- input$file1
      }

      if (file1_state == "reset") {
        input_files$file1 <- NULL
      }
    }

    if (!is.null(file2_state)) {
      if (file2_state == "uploaded") {
        input_files$file2 <- input$file2
      }
      if (file2_state == "reset") {
        input_files$file2 <- NULL
      }
    }

    input_files
  })


  observeEvent(input$send_paw, {
     waiter_show(html = waiting_screen_paw, color = "rgba(128, 128, 128, .8)")
     d <- validation_results()
     r <- sendTimeStampLogToS3(d)
     timestampUploadUI(r)
     sendDataPackErrorUI(r)
     r <- sendDATIMExportToS3(d)
     if (!is.null(d$datim$year2)) {
       r <- sendYear2ExportToS3(d)
     }

     sendEventToS3(d, "PAW_EXPORT")
     waiter_hide()
     datimExportUI(r)

  })


  observeEvent(input$epiCascadeInput, {
    epi_graph_filter$snu_filter <- input$epiCascadeInput
  })

  observeEvent(input$kpCascadeInput, {
    kpCascadeInput_filter$snu_filter <- input$kpCascadeInput
  })

  observeEvent(input$logout, {
    req(input$logout)
    # Gets you back to the login without the authorization code at top
    updateQueryString("?",mode="replace",session=session)
    flog.info(paste0("User ", user_input$d2_session$me$userCredentials$username, " logged out."))
    ready$ok <- FALSE
    user_input$authenticated <- FALSE
    user_input$user_name <- ""
    user_input$authorized <- FALSE
    user_input$d2_session <- NULL
    d2_default_session <- NULL
    gc()
    session$reload()

  })

  output$ui <- renderUI({

    if (user_input$authenticated == FALSE) {
      ##### UI code for login page
      fluidPage(
        fluidRow(
          column(width = 2, offset = 5,
                 br(), br(), br(), br(),
                 uiOutput("uiLogin")
          )
        )
      )
    } else {
      uiOutput("authenticated")
    }
  })


  # Username and password text fields, login button
  output$uiLogin <- renderUI({

    wellPanel(fluidRow(
      #img(src = "pepfar.png", align = "center"),
      tags$head(tags$script(HTML(jscode_login))), # enter button functionality for login button
      tags$div(HTML('<center><img src="pepfar.png"></center>')),
      h4("Welcome to the Target Setting Validation App. Please login with your DATIM credentials:")
    ),
    fluidRow(
      actionButton("login_button_oauth","Log in with DATIM"),
      uiOutput("ui_hasauth"),
      uiOutput("ui_redirect")
    ),
    fluidRow(
      tags$hr(),
      tags$div(HTML("<ul><li><h4>Please be sure you fully populate the PSNUxIM",
                    " tab when receiving a new PSNUxIM Tool. Consult <a href = ",
                    "\"https://apps.datim.org/datapack-userguide/\" target = ",
                    "\"blank\" > the user guide</a> for further information!",
                    "</h4></li><li><h4>See the latest updates to the app <a href =",
                    "\"https://github.com/pepfar-datim/datapackr-app/blob/master/CHANGELOG.md\"",
                    "target = \"blank\">here.</h4></a></li></ul>"))
    ),
    tags$hr(),
    fluidRow(HTML(getVersionInfo())))
  })

  output$authenticated <- renderUI({
    wiki_url <- a("Target Setting Tool User Guide",
                  href = "https://apps.datim.org/datapack-userguide/",
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
            "Choose a Target Setting Tool (Must be an XLSX file!)",
            accept = c("application/xlsx",
                       ".xlsx"),
            width = "440px"
          ),
          fileInput(
            "file2",
            "Choose a PSNUxIM Tool (Must be an XLSX file!):",
            accept = c("application/xlsx",
                       ".xlsx"),
            width = "440px"),
          actionButton("validate", "Validate"),
          tags$hr(),
          selectInput("downloadType", "Download type", NULL),
          downloadButton("downloadOutputs", "Download"),
          tags$hr(),
          actionButton("send_paw", "Send to PAW"),
          tags$hr(),
          div(style = "display: inline-block; vertical-align:top; width: 80 px;",
              actionButton("reset_input", "Reset inputs")),
          div(style = "display: inline-block; vertical-align:top; width: 80 px;",
              actionButton("logout", "Logout"))
        ),

        mainPanel(tabsetPanel(
          id = "main-panel",
          type = "tabs",
          tabPanel("Messages", tags$ul(uiOutput("messages"))),
          tabPanel("Analytics checks", tags$div(uiOutput("analytics_checks"))),
          tabPanel("Indicator summary", dataTableOutput("indicator_summary"),
                   tags$h4("Data source: Main DataPack tabs")),
          tabPanel("SNU-level summary",
                   dataTableOutput("snu_summary"),
                   tags$h4("Data source: Main DataPack tabs")),
          tabPanel("Validation rules",
                   dataTableOutput("vr_rules"),
                   tags$h4("Data source: PSNUxIM tab")),
          tabPanel("HTS Summary Chart",
                   fluidRow(column(width = 12, div(style = "height:700px", plotOutput("modality_summary")))),
                   fluidRow(column(width = 12, tags$h4("Data source: PSNUxIM tab")))),
          tabPanel("HTS Summary Table",
                   dataTableOutput("modality_table"),
                   tags$h4("Data source: PSNUxIM tab")),
          tabPanel("HTS Yield",
                   fluidRow(column(width = 12, div(style = "height:700px", plotOutput("modality_yield")))),
                   fluidRow(tags$h4("Data source: PSNUxIM tab"))),
          tabPanel("HTS Recency",
                   dataTableOutput("hts_recency"),
                   tags$h4("Data source: PSNUxIM tab")),
          tabPanel("VLS Testing",
                   fluidRow(column(width = 12, div(style = "height:700px", plotOutput("vls_summary")))),
                   fluidRow(column(width = 12, tags$h4("Data source: PSNUxIM tab")))),
          tabPanel("Epi Cascade Pyramid",
                   pickerInput("epiCascadeInput", "SNU1",
                               choices = "",
                               options = list(`actions-box` = TRUE), multiple = T),
                   fluidRow(column(width = 12, div(style = "height:700px", plotOutput("epi_cascade")))),
                   fluidRow(tags$h4("Data source: SUBNATT/IMPATT data & PSNUxIM tab"))),
          tabPanel("KP Cascade Pyramid",
                   pickerInput("kpCascadeInput", "SNU1",
                               choices = "",
                               options = list(`actions-box` = TRUE), multiple = T),
                   fluidRow(column(width = 12, div(style = "height:700px", plotOutput("kp_cascade")))),
                   fluidRow(tags$h4("Data source: Data source: SUBNATT/IMPATT data & PSNUxIM tab"))),
          tabPanel("PSNUxIM Pivot",
                   fluidRow(column(width = 12, div(rpivotTable::rpivotTableOutput({"pivot"})))), # nolint
                   fluidRow(tags$h4("Data source: PSNUxIM tab"))),
          tabPanel("Year 2 Pivot",
                   fluidRow(column(width = 12, div(rpivotTable::rpivotTableOutput({"year2_pivot"})))), # nolint
                   fluidRow(tags$h4("Data source: Year 2 Tab"))),
          tabPanel(
            "Memo Tables",
            fluidRow(
              pickerInput(
                "memo_pivot_style",
                label = "Table Style",
                choices = c("Prioritization", "By Agency", "By Partner", "Comparison"),
                selected = "Prioritization"
              )
            ),
            fluidRow(column(width = 12,
                                   div(dataTableOutput({"memo_compare"})))), # nolint
                   fluidRow(tags$h4("Data source: PSNUxIM tab & DATIM")))

        ))
      ))
  })

   output$ui_redirect = renderUI({
    #print(input$login_button_oauth) useful for debugging
    if(!is.null(input$login_button_oauth)){
      if(input$login_button_oauth>0){
        url <- httr::oauth2.0_authorize_url(oauth_api, oauth_app, scope = oauth_scope)
        redirect <- sprintf("location.replace(\"%s\");", url)
        tags$script(HTML(redirect))
      } else NULL
    } else NULL
  })

   ### Login Button oauth Checks
   observeEvent(input$login_button_oauth > 0,{

     #Grabs the code from the url
     params <- parseQueryString(session$clientData$url_search)
     #Wait until the auth code actually exists
     req(has_auth_code(params))

     #Manually create a token
     token <- httr::oauth2.0_token(
       app = oauth_app,
       endpoint = oauth_api,
       scope = oauth_scope,
       use_basic_auth = TRUE,
       oob_value = APP_URL,
       cache = FALSE,
       credentials = httr::oauth2.0_access_token(endpoint = oauth_api,
                                                 app = oauth_app,
                                                 code = params$code,
                                                 use_basic_auth = TRUE)
     )

     loginAttempt <- tryCatch({
       user_input$uuid <- uuid::UUIDgenerate()
       datimutils::loginToDATIMOAuth(base_url =  Sys.getenv("BASE_URL"),
                                     token = token,
                                     app = oauth_app,
                                     api = oauth_api,
                                     redirect_uri= APP_URL,
                                     scope = oauth_scope,
                                     d2_session_envir = parent.env(environment())
       ) },
       # This function throws an error if the login is not successful
       error = function(e) {
         flog.info(paste0("User ", input$user_name, " login failed. ", e$message), name = "datapack")
       }
     )

     if (exists("d2_default_session")) {

       user_input$authenticated  <-  TRUE
       user_input$d2_session  <-  d2_default_session$clone()
       d2_default_session <- NULL

       #Need to check the user is a member of the PRIME Data Systems Group, COP Memo group, or a super user
             user_input$memo_authorized <-
               grepl("VDEqY8YeCEk|ezh8nmc4JbX", user_input$d2_session$me$userGroups) |
               grepl(
                 "jtzbVV4ZmdP",
                 user_input$d2_session$me$userCredentials$userRoles
               )
             flog.info(
               paste0(
                 "User ",
                 user_input$d2_session$me$userCredentials$username,
                 " logged in."
               ),
               name = "datapack"
             )
             sendEventToS3(NULL, "LOGIN", user_input = user_input)

       flog.info(
         paste0(
           "User ",
           user_input$d2_session$me$userCredentials$username,
           " logged in."
         ),
         name = "datapack"
       )
     }

   })

  output$epi_cascade <- renderPlot({

    vr <- validation_results()
    epi_graph_filter_results <- epi_graph_filter$snu_filter

    if (!inherits(vr, "error") & !is.null(vr)) {

      subnatPyramidsChart(vr, epi_graph_filter_results)

    } else {
      NULL
    }
  }, height = 600, width = 800)

  output$kp_cascade <- renderPlot({

    vr <- validation_results()
    kpCascadeInput_filter_results <- kpCascadeInput_filter$snu_filter

    if (!inherits(vr, "error") & !is.null(vr)) {

      kpCascadeChart(vr, kpCascadeInput_filter_results)

    } else {
      NULL
    }
  }, height = 600, width = 800)

  output$pivot  <-  rpivotTable::renderRpivotTable({
    vr  <-  validation_results()

    if (!inherits(vr, "error") & !is.null(vr)) {
      if (is.null(vr$data$analytics)) {
        return(NULL)
      }
      PSNUxIM_pivot(vr)

    } else {
      NULL
    }
  })

  output$year2_pivot  <-  rpivotTable::renderRpivotTable({
    vr  <-  validation_results()

    if (!inherits(vr, "error") & !is.null(vr)) {
      if (is.null(vr$data$Year2)) {
        return(NULL)
      }
      year2Pivot(vr)

    } else {
      NULL
    }
  })


  output$memo_compare  <-  DT::renderDataTable({
    vr <- validation_results()

    if (!inherits(vr, "error") & !is.null(vr)) {
      p_data <- switch(
        input$memo_pivot_style,
        "Prioritization" = vr$memo$datapack$by_prio,
        "By Agency" = vr$memo$datapack$by_agency,
        "By Partner" = vr$memo$datapack$by_partner,
        "Comparison" = (

          if ( NROW(vr$memo$comparison) > 0) {
            vr$memo$comparison %>%
            dplyr::select("Indicator", "Age", "Data Type", "value") %>%
            dplyr::filter(`Data Type` != "Percent diff") %>%
            dplyr::group_by(Indicator, Age, `Data Type`) %>%
            dplyr::summarise(value = sum(value), .groups = "drop") %>%
            tidyr::pivot_wider(
            id_cols = c(Indicator, Age),
            names_from = `Data Type` ) %>%
            dplyr::filter(Diff != "0") } else {
              data.frame(message="No differences detected")
            }
        )
      )

      if (!is.null(p_data)) {
        table_options <-
          list(columnDefs = list(list(
            targets = as.vector(which(sapply(p_data,"class") == "numeric")), class = "dt-right"
          )))

        p_data <- p_data %>%
          dplyr::mutate_if(is.numeric, function(x)
            prettyNum(x, big.mark = ","))

        DT::datatable(p_data,
                      option = table_options)
      } else {

        NULL
      }
    } else {
      NULL
    }
  })

  output$hts_recency <- DT::renderDataTable({

    vr <- validation_results()

    if (!inherits(vr, "error") & !is.null(vr$data$recency)) {

      DT::datatable(vr$data$recency,
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
        modalitySummaryChart(vr)
      }

    } else {
      NULL
    }

  }, height = 600, width = 800)

  output$modality_yield <- renderPlot({

    vr <- validation_results()

    if (!inherits(vr, "error") & !is.null(vr)) {

      modalityYieldChart(vr)

    } else {
      NULL
    }

  }, height = 400, width = 600)

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

  output$modality_table <- DT::renderDataTable({

    d <- validation_results()

    if (!inherits(d, "error") & !is.null(d$data$modality_summary)) {

      DT::datatable(formatModalitySummaryTable(d),
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

  output$indicator_summary <- DT::renderDataTable({

    vr <- validation_results()

    if (!inherits(vr, "error") & !is.null(vr)) {

      prepareSNUSummaryTable(vr) %>%
        dplyr::group_by(indicator_code) %>%
        dplyr::summarise(value = format(round(sum(value, na.rm = TRUE)), big.mark = ",", scientific = FALSE)) %>%
        dplyr::arrange(indicator_code)

    } else {
      NULL
    }
  })

  output$snu_summary <- DT::renderDataTable({

    vr <- validation_results()

    if (!inherits(vr, "error") & !is.null(vr)) {

      prepareSNUSummaryTable(vr) %>%
        dplyr::mutate(value = format(round_trunc(value), big.mark = ",", scientific = FALSE))

    } else {
      data.frame(message = "No data is available to display. An error may have occurred.")
    }
  })

  output$vr_rules <- DT::renderDataTable({

    vr <- validation_results() %>%
      purrr::pluck(., "tests") %>%
      purrr::pluck(., "vr_rules_check")

    if (inherits(vr, "error") | is.null(vr)) {

      return(NULL)
    }

    if (NROW(vr) == 0) {

      data.frame(message = "Congratulations! No validation rule issues found!")

    } else {
      vr %>%
        dplyr::filter(!`Valid`) %>%
        dplyr::select(1:6)

    }
  })

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
        purrr::pluck(., "messages")


      if (length(messages$message) > 0) {

        class(messages) <- "data.frame"

        messages %<>%
          dplyr::mutate(level = factor(level, levels = c("ERROR", "WARNING", "INFO"))) %>%
          dplyr::arrange(level) %>%
          dplyr::mutate(msg_html =
                          dplyr::case_when(
                            level == "ERROR" ~ paste('<li><p style = "color:red"><b>', message, "</b></p></li>"),
                            TRUE ~ paste("<li><p>", message, "</p></li>")
                          ))

        messages_sorted <-
          paste0("<ul>", paste(messages$msg_html, sep = "", collapse = ""), "</ul>")

        shiny::HTML(messages_sorted)
      } else {
        tags$li("No Issues with Integrity Checks: Congratulations!")
      }
    }
  })

  output$analytics_checks <- renderUI({

    vr <- validation_results()

    messages <- NULL

    if (is.null(vr)) {
      return(NULL)
    }

    if (inherits(vr, "error")) {
      return(paste0("ERROR! ", vr$message))
    } else {

      messages <- vr %>%
        purrr::pluck(., "info") %>%
        purrr::pluck(., "analytics_warning_msg") %>%
        purrr::map(., function(x) fansi::to_html(fansi::html_esc(x))) %>%
        purrr::map(., function(x) paste("<li><p>", x, "</p></li>")) %>%
        paste(., collapse = "") %>%
        stringr::str_replace_all("\n", "<p/>") %>%
        stringr::str_replace_all("\t", "&emsp;")

      if (!is.null(messages)) {
        shiny::HTML(paste("<ul>", messages, "</ul>"))
      } else {
        tags$li("No Issues with Analytics Checks: Congratulations!")
      }
    }
  })

  output$downloadOutputs <- downloadHandler(
    filename = function() {
      d <- validation_results()

      sane_name <- d$info$sane_name

      prefix <- switch(input$downloadType,
                       "messages" = "Messages",
                        "cso_flatpack" = "CSO_Flatpack",
                        "flatpack" = "Flatpack",
                        "vr_rules" = "Validation_report",
                        "datapack" = ifelse(d$info$cop_year == 2023, "PSNUxIM", "Datapack"),
                        "update_psnuxim_targets" = ifelse(d$info$cop_year == 2023, "PSNUxIM", "Datapack"),
                        "missing_psnuxim_targets" = "PSNUxIM_Missing_Targets",
                        "append_missing_psnuxim_targets" = ifelse(d$info$cop_year == 2023, "PSNUxIM", "Datapack"),
                        "comparison" = "Comparison",
                        "memo" = paste("COP", substring(d$info$cop_year,first = 3,last = 4), "_Memo"),
                         "Other"
                        )

      date <- date <- format(Sys.time(), "%Y%m%d_%H%M%S")

      suffix <- if (input$downloadType %in% c("messages")) {
        ".txt"
      } else if (input$downloadType %in% c("memo")) {
        ".docx"
      } else {
        ".xlsx"
      }

      paste0(prefix, "_", sane_name, "_", date, suffix)
    },
    content = function(file) {

      d <- validation_results()

      if (input$downloadType == "messages") {
        sendEventToS3(d, "MESSAGE_DOWNLOAD")
        writeLines(d$info$messages$message, file)
      }

      if (input$downloadType == "cso_flatpack") {
        sendEventToS3(d, "CSO_FLATPACK_DOWNLOAD")
        wb <- downloadCSOFlatPack(d)
        openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
      }

      if (input$downloadType == "flatpack") {
        sendEventToS3(d, "FLATPACK_DOWNLOAD")
        waiter_show(html = waiting_screen_flatpack, color = "rgba(128, 128, 128, .8)")
        datapack_name <- d$info$datapack_name
        flog.info(
          paste0("Flatpack requested for ", datapack_name),
          name = "datapack"
        )

        wb <- downloadFlatPack(d)
        openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
        waiter_hide()
      }

      if (input$downloadType == "vr_rules") {
        #The exact structure of the tests is unknown, but filter out anything
        #which is not a data frame.

        is_data_frame <- unlist(lapply(lapply(d$tests,class) , function(x) "data.frame" %in% x))

        d$tests <- d$tests[is_data_frame]

        sheets_with_data <- d$tests[lapply(d$tests, NROW) > 0 ] %>%
         #Limit the number of rows to the maximum in Excel
          purrr::map(.,~ dplyr::slice(.x,1:1048575)) %>%
          #Collapses nested lists to a string which will fit inside of excel
          purrr::map(., ~ .x %>% dplyr::mutate_if(is.list,function(x)  paste(as.character(x[[1]]),sep="",collapse=","))) %>%
          #Convert everything to characters and apply Excel limits
          purrr::map(., ~ .x %>% dplyr::mutate_if(is.character,function(x) substring(as.character(x),0,36766)))


        if (length(sheets_with_data) > 0) {
          sendEventToS3(d, "VR_RULES_DOWNLOAD")
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
        waiter_show(html = waiting_screen_datapack, color = "rgba(128, 128, 128, .8)")

        d <- downloadDataPack(d,
                              d2_session = user_input$d2_session,
                              append = FALSE,
                              use_template = TRUE
                              )

        openxlsx::saveWorkbook(wb = d$tool$wb, file = file, overwrite = TRUE)
        sendEventToS3(d, "DATAPACK_DOWNLOAD")
        flog.info(
          paste0("Datapack reloaded for ", d$info$datapack_name),
          name = "datapack")
        waiter_hide()
      }


      if (input$downloadType == "update_psnuxim_targets") {
        flog.info(
          paste0("Updating PSNUxIM target values from Main Tabs... ", d$info$datapack_name)
          ,
          name = "datapack")
        waiter_show(html = waiting_screen_datapack, color = "rgba(128, 128, 128, .8)")

        d <- datapackr::updatePSNUxIMTargetValues(d)

        openxlsx::saveWorkbook(wb = d$tool$wb, file = file, overwrite = TRUE)
        sendEventToS3(d, "UPDATED_TARGETS_DOWNLOAD")
        flog.info(
          paste0("PSNUxIM targets updated for ", d$info$datapack_name),
          name = "datapack")
        waiter_hide()
      }

      if (input$downloadType == "missing_psnuxim_targets") {

        flog.info(
          paste0("Generation of missing PSNUxIM targets requested for ", d$info$datapack_name)
          ,
          name = "datapack")
        waiter_show(html = waiting_screen_datapack, color = "rgba(128, 128, 128, .8)")

        d <- downloadDataPack(d,
                                           d2_session = user_input$d2_session,
                                           append = TRUE,
                                           use_template = TRUE
                                           )

        openxlsx::saveWorkbook(wb = d$tool$wb, file = file, overwrite = TRUE)
        sendEventToS3(d, "MISSING_PSNUXIM_DOWNLOAD")
        flog.info(
          paste0("Missing PSNUxIM targets generated reloaded for ", d$info$datapack_name),
          name = "datapack")
        waiter_hide()
      }

      if (input$downloadType == "append_missing_psnuxim_targets") {

        flog.info(
          paste0("Appending PSNUxIM targets requested for ", d$info$datapack_name)
          ,
          name = "datapack")
        waiter_show(html = waiting_screen_datapack, color = "rgba(128, 128, 128, .8)")

        d <- downloadDataPack(d,
                                           d2_session = user_input$d2_session,
                                           append = TRUE,
                                           use_template = FALSE
        )

        openxlsx::saveWorkbook(wb = d$tool$wb, file = file, overwrite = TRUE)
        sendEventToS3(d, "APPEND_PSNUXIM_DOWNLOAD")
        flog.info(
          paste0("Missing PSNUxIM targets generated reloaded for ", d$info$datapack_name),
          name = "datapack")
        waiter_hide()
      }

      if (input$downloadType == "comparison") {
        waiter_show(html = waiting_screen_comparison, color = "rgba(128, 128, 128, .8)")
        #Create a new workbook
        flog.info(
          paste0("Comparison requested for ", d$info$datapack_name)
          ,
          name = "datapack"
        )

        # wb <- openxlsx::createWorkbook()
        # openxlsx::addWorksheet(wb, "Comparison")
        # openxlsx::writeData(wb = wb,
        #                     sheet = "Comparison", x = d$memo$comparison)
        #
        # openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
        comparison_table <-
          d$memo$comparison %>%
          dplyr::rename(data_type = "Data Type") %>%
          dplyr::filter(data_type %in% c("Current", "Diff", "Proposed"))

        if (NROW(comparison_table) == 0L || is.null(comparison_table)) {
          return(NULL)
        }

        wb <- openxlsx2::wb_workbook() %>%
          openxlsx2::wb_add_worksheet("Comparison data") %>%
          openxlsx2::wb_add_data(x = comparison_table)

        df <- openxlsx2::wb_data(wb)

        params <- list("colGrandTotals" = FALSE,
                       "rowGrandTotals" = FALSE)

        wb <- wb %>%
          openxlsx2::wb_add_worksheet(sheet = "By SNU1") %>%
          openxlsx2::wb_add_pivot_table(
            df,
            sheet = "By SNU1",
            dims = "A3",
            rows = c("Indicator", "snu1"),
            cols = "data_type",
            data = "value",
            params = params)

        wb <- wb %>%
          openxlsx2::wb_add_worksheet(sheet = "By Partner") %>%
          openxlsx2::wb_add_pivot_table(
            df,
            sheet = "By Partner",
            dims = "A3",
            rows = c("Indicator", "Partner"),
            cols = "data_type",
            data = "value",
            params = params
          )

        wb <- wb %>%
          openxlsx2::wb_add_worksheet(sheet = "By Agency") %>%
          openxlsx2::wb_add_pivot_table(
            df,
            sheet = "By Agency",
            dims = "A3",
            rows = c("Indicator", "Agency"),
            cols = "data_type",
            data = "value",
            params = params
          )

        wb <- wb %>%
          openxlsx2::wb_add_worksheet(sheet = "By Mechanism") %>%
          openxlsx2::wb_add_pivot_table(
            df,
            sheet = "By Mechanism",
            dims = "A3",
            rows = c("Indicator", "Mechanism"),
            cols = "data_type",
            data = "value",
            params = params
          )

        openxlsx2::wb_save(wb, file = file)

        sendEventToS3(d, "COMPARISON_DOWNLOAD")
        waiter_hide()
      }

      if (input$downloadType == "memo") {
        sendEventToS3(d, "MEMO_DOWNLOAD")
        doc <- datapackr::generateApprovalMemo(d,
                                               memo_type = "datapack",
                                               draft_memo = TRUE )
        print(doc, target = file)

      }
    }
  )

  validate <- function() {

    shinyjs::disable("downloadType")
    shinyjs::disable("downloadOutputs")
    shinyjs::disable("send_paw")

    if (!ready$ok) {
      shinyjs::disable("validate")
      return(NULL)
    }

    input_files <- file_input()
    inFile <- input_files$file1
    inFile2 <- input_files$file2
    messages <- ""

    if (is.null(inFile) && is.null(inFile2)) {
      return(NULL)
    }

    messages <- list()

    withProgress(message = "Validating file", value = 0, {

      shinyjs::disable("file1")
      shinyjs::disable("validate")
      incProgress(0.1, detail = ("Unpacking your DataPack"))

     if (!is.null(inFile$datapath) && is.null(inFile2$datapath)){

       d <- tryCatch({
         datapackr::unPackTool(inFile$datapath,
                               d2_session = user_input$d2_session)},
         error = function(e) {
           return(e)
         })
     }

      if (is.null(inFile$datapath) && !is.null(inFile2$datapath)){

        d <- tryCatch({
          datapackr::unPackTool(inFile2$datapath,
                                d2_session = user_input$d2_session)},
          error = function(e) {
            return(e)
          })
      }

      if (!is.null(inFile$datapath) && !is.null(inFile2$datapath)){

        d <- tryCatch({
          datapackr::unPackToolSet(d1 = inFile$datapath,
                                   d2 = inFile2$datapath,
                                d2_session = user_input$d2_session)},
          error = function(e) {
            return(e)
          })
      }


      if (inherits(d, "error")) {
        return("An error occurred. Please contact DATIM support.")
      }

      if (!inherits(d, "error") & !is.null(d)) {
        #All self-service datapacks should be marked as unapproved for PAW
        d$info$approval_status <- "UNAPPROVED"
        #Generate a unique identifier
        d$info$uuid <- user_input$uuid

        #Log the validation to S3
        sendEventToS3(d, "VALIDATE")
        flog.info(paste0("Initiating validation of ", d$info$datapack_name, " DataPack."), name = "datapack")
        #Data Packs
        if (d$info$tool %in% c("Data Pack", "PSNUxIM")) {



          updateSelectInput(session = session, inputId = "downloadType",
                            choices = downloadTypes(d,
                                                    memo_authorized = user_input$memo_authorized))

          if (is.null(d$data$Year2)) {
            hideTab(inputId = "main-panel", target = "Year 2 Pivot")
            }

          if ((d$info$has_psnuxim & NROW(d$data$SNUxIM) > 0) | d$info$cop_year %in% c("2022","2023")) {

            flog.info(paste("COP", d$info$cop_year, d$info$tool, " found."))
            incProgress(0.1, detail = ("Checking validation rules"))
            Sys.sleep(0.5)
            d <- datapackr::checkPSNUData(d)

            # if (Sys.getenv("SEND_DATAPACK_ARCHIVE") == "TRUE") {
            #   incProgress(0.1, detail = ("Saving a copy of your submission to the archives"))
            #   Sys.sleep(0.5)
            #   r <- sendDataPackToS3(d, inFile$datapath)
            #   sendDataPackErrorUI(r)
            #   Sys.sleep(1)
            # }

            #Users which have a dimension restriction
            #Are not going to be able to retrieve
            #prior data from DATIM. In this case, prepare
            #the memo, and warn the user of this situation.
            #The use of the comparison table really only makes
            #sense of we are dealing with a DataPack OPU but
            #at the moment, we do not have an easy way to determine
            incProgress(0.1, detail = ("Preparing COP memo data"))
            if (user_input$memo_authorized) {
              d <-
                datapackr::prepareMemoData(
                  d,
                  memo_type = "comparison",
                  include_no_prio = TRUE,
                  d2_session = user_input$d2_session
                )
            } else {
              d <-
                datapackr::prepareMemoData(
                  d,
                  memo_type = "datapack",
                  include_no_prio = TRUE,
                  d2_session = user_input$d2_session
                )
            }

            d <- datapackr::generateComparisonTable(d)
            Sys.sleep(1)

            incProgress(0.1, detail = ("Preparing a modality summary"))
            d <- modalitySummaryTable(d)
            Sys.sleep(1)
            incProgress(0.1, detail = ("Preparing a HTS recency analysis"))
            d <- recencyComparison(d)
            Sys.sleep(1)
            incProgress(0.1, detail = ("Performing analytics checks"))
            model_data_path <- "support_files/datapack_model_data.rds"
            full_model_path <- fetchModelFile(model_data_path)
            d <- datapackr::checkAnalytics(d, model_data_path  = full_model_path, d2_session = user_input$d2_session)
            Sys.sleep(1)
            incProgress(0.1, detail = ("Finishing up."))
            flog.info("Sending validation summary")
            r <- sendValidationSummaryToS3(d, "validation_error_summary", include_timestamp = TRUE)
            validationSummaryUI(r)

            shinyjs::enable("downloadType")
            shinyjs::enable("downloadOutputs")
            shinyjs::enable("send_paw")
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

          } else if (d$info$has_psnuxim & NROW(d$data$SNUxIM) == 0) {
            msg <- paste("ERROR! Your DataPack contains a PSNUxIM tab, but the formulas appear to be empty.,
            Please ensure that the formulas have been properly populated in the PSNUxIM tab.")
            d$info$warning_msg <- append(d$info$warning_msg, msg)
            #Enable the UI
            shinyjs::enable("downloadType")
            shinyjs::enable("downloadOutputs")
            shinyjs::enable("send_paw")
            #Hide tabs that do not make sense
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

          } else {
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

        #COP22 OPU DataPacks AKA PSNUxIM Tabs
        if (d$info$tool %in% c("OPU Data Pack", "PSNUxIM")) {

          updateSelectInput(session = session, inputId = "downloadType",
                            choices = downloadTypes(d,
                                                    memo_authorized = user_input$memo_authorized))

          flog.info("Datapack with PSNUxIM tab found.")
          incProgress(0.1, detail = ("Checking validation rules"))
          Sys.sleep(0.5)
          d <- datapackr::checkPSNUData(d)
          incProgress(0.1, detail = ("Preparing COP memo data"))
          #Only execute the comparison if the user has proper authorization
          #Global agency users cannot retrieve prioritization data
          #from the DATIM analytics API
          d <-
            datapackr::prepareMemoData(
              d,
              memo_type = "comparison",
              include_no_prio = TRUE,
              d2_session = user_input$d2_session
            )
          d <- datapackr::generateComparisonTable(d)
          Sys.sleep(1)
          incProgress(0.1, detail = ("Preparing a modality summary"))
          d <- modalitySummaryTable(d)
          Sys.sleep(1)
          incProgress(0.1, detail = ("Preparing a HTS recency analysis"))
          d <- recencyComparison(d)
          Sys.sleep(1)
          shinyjs::disable("send_paw")
          shinyjs::enable("downloadType")
          shinyjs::enable("downloadOutputs")
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
        }
      }



    })

    return(d)

  }

})
