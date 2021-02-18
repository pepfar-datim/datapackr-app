
#Set the maximum file size for the upload file
options(shiny.maxRequestSize = 100 * 1024 ^ 2)

#Initiate logging
logger <- flog.logger()

if (!file.exists(Sys.getenv("LOG_PATH"))) {
  file.create(Sys.getenv("LOG_PATH"))
}

flog.appender(appender.console(), name="datapack")


fetchSupportFiles <- function() {
  

    s3 <- paws::s3()
    s3_object <-
      s3$get_object(Bucket = Sys.getenv("AWS_S3_BUCKET"),
                    Key = paste0("/support_files/snuxim_model_data.rds"))
    s3_object_body <- s3_object$Body
    
    file_name2 <- paste0(getwd(),"/support_files/snuxim_model_data.rds")
    if (file.exists(file_name2)) {
      unlink(file_name2)
    }
    
    con<-file(file_name2,"wb")
    writeBin(s3_object_body, con = con)
    close(con)
    flog.info(paste0("Retreived model file to ", file_name2))
    if(!file.exists(file_name2)) {stop("Could not retreive support file.")}
  
  return(file_name2)
}

validatePSNUData <- function(d,d2_session) {
  
  if (d$info$tool == "Data Pack") {
    vr_data<-d$datim$MER
  }
  
  if (d$info$tool == "OPU Data Pack") {
    vr_data<-d$datim$OPU
  }
    
  

  if (is.null(vr_data) | NROW(vr_data) == 0 ) {return(d)}
  
  # We need ALL mechanisms to be in DATIM before remapping....TODO
  vr_data$attributeOptionCombo <-
    datimvalidation::remapMechs(vr_data$attributeOptionCombo,
                                d2_session$user_orgunit,
                                "code",
                                "id",
                                d2session = d2_session)
  datasets_uid <- 
    if ( d$info$cop_year == "2020" ) {
      c("Pmc0yYAIi1t", "s1sxJuqXsvV")
    } else if  ( d$info$cop_year == "2021" ) {
      c("YfZot37BbTm", "Pmc0yYAIi1t")
    }

  if ( Sys.info()["sysname"] == "Linux") {
    ncores <- parallel::detectCores() - 1
    doMC::registerDoMC( cores = ncores )
    is_parallel <- TRUE
  } else {
    is_parallel <- FALSE
  }
  
  vr_violations <- datimvalidation::validateData(vr_data,
                                                 datasets = datasets_uid,
                                                 parallel = is_parallel,
                                                 return_violations_only = TRUE,
                                                 d2session = d2_session)
  
  # rules_to_keep <- c(
  #   "L76D9NGEPRS",
  #   "rVVZmdG1KTb",
  #   "zEOFo6X436M",
  #   "oVtpQHVVeCV",
  #   "r0CC6MQW5zc",
  #   "vrS3kAtlJ4F",
  #   "WB338HNucS7",
  #   "tiagZGzSh6G",
  #   "vkFHYHgfqCf",
  #   "coODsuNsoXu",
  #   "qOnTyseQXv8",
  #   "Ry93Kc34Zwg",
  #   "g0XwMGLB5XP",
  #   "eb02xBNx7bD",
  #   "SNzoIyNuanF",
  #   "Ry93Kc34Zwg",
  #   "WiRJutVpAq4"
  # )
  
  rules_to_ignore<-c("RLXOqAeHN04","KdqWm8ZvWoO")
  
  vr_violations<-vr_violations[!(vr_violations$id %in% rules_to_ignore),]
  
  if ( NROW(vr_violations) > 0 ) {
    
    # vr_violations <-
    #   vr_violations[vr_violations$id %in% rules_to_keep,] } else {
    #     d$datim$vr_rules_check <- NULL
    #     return(d)
    #   }
    
    diff <- gsub(" [<>]= ", "/", vr_violations$formula)
    vr_violations$diff <- sapply( diff, function(x) { round( ( eval( parse( text = x ) ) - 1) * 100, 2 ) })
    vr_violations$diff <-ifelse(vr_violations$rightSide.expression == 0 | vr_violations$leftSide.expression == 0,
                                NA,
                                vr_violations$diff)
    
    #vr_violations %<>% dplyr::filter(diff >= 5)
    
    diff <- gsub(" [<>]= ", "-", vr_violations$formula)
    vr_violations$abs_diff <- sapply( diff, function(x) { abs( eval( parse( text = x ) ) ) })
    
    
    if (NROW(vr_violations) > 0) {
      
      d$tests$vr_rules_check <- vr_violations  %>%
        dplyr::select(name, ou_name, mech_code, formula, diff,abs_diff) %>%
        dplyr::rename("Validation rule" = name,
                      "PSNU" = ou_name,
                      "Mechanism" = mech_code,
                      "Formula" = formula,
                      "Diff (%)" = diff,
                      "Diff (Absolute)" = abs_diff)
      
      warning_message<-     
        paste0(
          NROW(vr_violations),
          " validation rule issues found in ",
          d$info$datapack_name,
          " DataPack."
        )
      d$info$warning_msg<-append(d$info$warning_msg,warning_message)
      flog.info(
        warning_message,
        name = "datapack"
      )
    } else {
      d$tests$vr_rules_check <- NULL
      flog.info(
        paste0(
          "No validation rule issues found in ",
          d$info$datapack_name,
          " DataPack."
        ),
        name = "datapack"
      )
      return(d)
    }
  }
  
  d
  
}

#TODO: Move this back to the DataPackr....
validateMechanisms<-function(d, d2_session) {
  
  
  vr_data <- d$data$analytics%>%
    dplyr::pull(mechanism_code) %>%
    unique()
  
  #TODO: Remove hard coding of time periods and 
  #filter for the OU as well
  mechs<-datapackr::getMechanismView(d2_session = d2_session) %>%
    dplyr::filter(!is.na(startdate)) %>%
    dplyr::filter(!is.na(enddate)) %>%
    dplyr::filter(startdate <= as.Date('2020-10-01')) %>%
    dplyr::filter(enddate >= as.Date('2021-09-30')) %>%
    dplyr::pull(mechanism_code)
  
  #Allow for the dedupe mechanisms in COP20 OPU Data Packs
  if (d$info$tool == "OPU Data Pack" & d$info$cop_year == 2020 ) {
    mechs <- append(c("00000","00001"),mechs)
  }
  
  #Allow for the dedupe mechanisms in COP21 Data packs
  if (d$info$tool == "Data Pack" & d$info$cop_year == 2021 ) {
    mechs <- append(c("00000","00001"),mechs)
  }

  
  bad_mechs<-vr_data[!(vr_data %in% mechs)]
  
  if (length(bad_mechs) > 0 ) {
    
    msg <- paste0("ERROR!: Invalid mechanisms found in the PSNUxIM tab. 
                  These MUST be reallocated to a valid mechanism
                  ",paste(bad_mechs,sep="",collapse=","))
    d$tests$bad_mechs<-bad_mechs
    d$info$warning_msg<-append(msg,d$info$warning_msg)
    d$info$had_error<-TRUE
  }
  
  d
  
}

getCountryNameFromUID<-function(uid) {
  
  
  paste0(getOption("baseurl"),"api/organisationUnits/",uid,"?fields=shortName") %>%
    URLencode(.) %>%
    httr::GET(., handle = user_input$d2_session$handle) %>%
    httr::content(.,"text") %>%
    jsonlite::fromJSON(.) %>% 
    purrr::pluck(.,"shortName")
}

archiveDataPacktoS3<-function(d,datapath) {
  
  #Write an archived copy of the file
  s3<-paws::s3()
  object_tags<-createS3BucketTags(d)
  object_name<-paste0("datapack_archives/",gsub(" ","_",d$info$sane_name),"_",format(Sys.time(),"%Y%m%d_%H%m%s"),".xlsx")
  # Load the file as a raw binary
  read_file <- file(datapath, "rb")
  raw_file <- readBin(read_file, "raw", n = file.size(datapath))
  close(read_file)
  
  r<-tryCatch({
    foo<-s3$put_object(Bucket = Sys.getenv("AWS_S3_BUCKET"),
                       Body = raw_file,
                       Key = object_name,
                       Tagging = object_tags,
                       ContentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
    flog.info("Datapack Archive sent to S3", name = "datapack")
    TRUE
  },
  error = function(err) {
    flog.info("Datapack could not be archived",name = "datapack")
    flog.info(err, name = "datapack")
    FALSE
  })
  
  return(r)
  
}

archiveDataPackErrorUI <- function(r) {
  if (!r) {
    showModal(modalDialog(title = "Error",
                          "The DataPack could not be archived."))
  }
}

saveTimeStampLogToS3<-function(d) {


  #Write an archived copy of the file
  s3<-paws::s3()
  object_tags<-createS3BucketTags(d)
  object_name <-
    paste0("processed/",
           gsub("^20", "cop", d$info$cop_year),
           "/",
           d$info$sane_name,
           ".csv")
  #Save a timestamp of the upload
  timestamp_info<-list(
    ou=d$info$datapack_name,
    ou_id=d$info$country_uids,
    country_name=d$info$datapack_name,
    country_uids=paste(d$info$country_uids,sep="",collapse=","),
    upload_timestamp=strftime(as.POSIXlt(Sys.time(), "UTC") , "%Y-%m-%d %H:%M:%S"),
    filename=object_name
  )
  
  tmp<-tempfile()
  write.table(
    as.data.frame(timestamp_info),
    file = tmp,
    quote = FALSE,
    sep = "|",
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
  # Load the file as a raw binary
  read_file <- file(tmp, "rb")
  raw_file <- readBin(read_file, "raw", n = file.size(tmp))
  close(read_file)
  object_name <-
    paste0(
      "upload_timestamp/",
      gsub("^20", "cop", d$info$cop_year),
      "/",
      d$info$sane_name,
      ".csv"
    )
  
  r<-tryCatch({
    foo<-s3$put_object(Bucket = Sys.getenv("AWS_S3_BUCKET"),
                       Body = raw_file,
                       Key = object_name,
                       Tagging = object_tags,
                       ContentType = "text/csv")
    flog.info("Timestamp log sent to S3", name = "datapack")
    TRUE
  },
  error = function(err) {
    flog.error("Timestamp log could not be saved to S3",name = "datapack")
    FALSE
  })
  unlink(tmp)
  return(r)
}

timestampUploadUI<-function(r) {
  if (!r) {
    showModal(modalDialog(title = "Error",
                          "Timestamp log could not be saved to S3."))
  }
  
}

sendMERDataToPAW<-function(d) {
  
  
  
  #Write the combined DATIM export for MER and SUBNATT data
  tmp <- tempfile()
  mer_data<-dplyr::bind_rows(d$datim) %>% 
    dplyr::mutate(categoryOptionCombo = case_when(is.na(categoryOptionCombo) ~ "HllvX50cXC0",
                                                   TRUE ~categoryOptionCombo )) %>% 
    tidyr::drop_na()
  
  #Need better error checking here
  write.table(
    mer_data,
    file = tmp,
    quote = FALSE,
    sep = "|",
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
  
  # Load the file as a raw binary
  read_file <- file(tmp, "rb")
  raw_file <- readBin(read_file, "raw", n = file.size(tmp))
  close(read_file)
  
  object_tags<-createS3BucketTags(d)
  object_name<-paste0("datim_export/cop21/",d$info$sane_name,".csv")
  
  svc<-paws::s3()
  
  r<-tryCatch({
    foo<-svc$put_object(Bucket = Sys.getenv("AWS_S3_BUCKET"),
                       Body = raw_file,
                       Key = object_name,
                       Tagging = object_tags,
                       ContentType = "text/csv")
    flog.info("Flatpack sent to AP", name = "datapack")
    TRUE
  },
  error = function(err) {
    flog.info("Flatpack cannot be sent to AP",name = "datapack")
    flog.info(err, name = "datapack")
    FALSE
  })
  
  unlink(tmp)
  
  return(r)
}

sendValidationSummary<-function(d) {
  

  validation_summary<-validationSummary(d)
  
  tmp <- tempfile()
  #Need better error checking here I think. 
  write.table(
    validation_summary,
    file = tmp,
    quote = FALSE,
    sep = "|",
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
  # Load the file as a raw binary
  read_file <- file(tmp, "rb")
  raw_file <- readBin(read_file, "raw", n = file.size(tmp))
  close(read_file)
  
  object_tags<-createS3BucketTags(d)
  object_name<-paste0("validation_error/",gsub("^20","cop",d$info$cop_year),"/",d$info$sane_name,".csv")
  s3<-paws::s3()
  
  r<-tryCatch({
    foo<-s3$put_object(Bucket = Sys.getenv("AWS_S3_BUCKET"),
                       Body = raw_file,
                       Key = object_name,
                       Tagging = object_tags,
                       ContentType = "text/csv")
    
    TRUE
  },
  error = function(err) {
    flog.info("Validation summary could not be sent to AP",name = "datapack")
    flog.info(err, name = "datapack")
    FALSE
  })
  
  unlink(tmp)
  
  return(r)
  
}

validationSummaryUI<-function(r) {
  if (!r) {
    showModal(modalDialog(title = "Error","Validation summary could not be sent to AP."))
  } 
}

saveDATIMExportToS3<-function(d) {
  #Write the flatpacked output
  tmp <- tempfile()
  d$datim$MER$value<-as.character(d$datim$MER$value)
  d$datim$subnat_impatt$value<-as.character(d$datim$subnat_impatt$value)
  datim_export<-dplyr::bind_rows(d$datim$MER,d$datim$subnat_impatt)
  
  #Need better error checking here I think. 
  write.table(
    datim_export,
    file = tmp,
    quote = FALSE,
    sep = "|",
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
  
  # Load the file as a raw binary
  read_file <- file(tmp, "rb")
  raw_file <- readBin(read_file, "raw", n = file.size(tmp))
  close(read_file)
  
  
  object_tags<-createS3BucketTags(d)
  
  object_name<-paste0("datim_export/",gsub("^20","cop",d$info$cop_year),"/",d$info$sane_name,".csv")
  s3<-paws::s3()
  
  r<-tryCatch({
    foo<-s3$put_object(Bucket = Sys.getenv("AWS_S3_BUCKET"),
                       Body = raw_file,
                       Key = object_name,
                       Tagging = object_tags,
                       ContentType = "text/csv")
    flog.info("DATIM Export sent to S3", name = "datapack")
    TRUE
  },
  error = function(err) {
    flog.info("DATIM Export could not be sent to  S3",name = "datapack")
    flog.info(err, name = "datapack")
    FALSE
  })
  
  unlink(tmp)
  
  return(r)
  
}

datimExportUI<-function(r) {
  if (!r) {
    showModal(modalDialog(title = "Error",
                          "DATIM Export could not be sent to S3"))
  } else {
    showModal(modalDialog(title = "Congrats!",
                          "Export to PAW was successful."))
  }
}


evaluateIndicators<-function(combis,values,inds) {
  
  indicators_empty<-data.frame("Indicator" = character(),
                               "N_OR_D" = character(),
                               "Age" = character(),
                               id = character(),
                               numerator = numeric(),
                               denominator = numeric(),
                               value  = numeric())
  
  this.des <-
    vapply(combis, function(x) {
      unlist(strsplit(x, "\\."))[[1]]
    }, FUN.VALUE = character(1))
  totals_df<-data.frame(exp = this.des,values=values,stringsAsFactors = FALSE) %>% 
    dplyr::group_by(exp) %>% 
    dplyr::summarise(values = sum(values)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(exp=paste0(exp,"}"))
  matches_indicator <- function(x) {
    agrepl(x, inds$numerator) |
      agrepl(x, inds$denominator)
  }
  this.des<-unique(this.des)  
  matches_v <- lapply(this.des,matches_indicator) %>% Reduce("|",.)
  matches <-inds[matches_v,]
  #Return something empty here if we have no indicator matches
  
  if (nrow(matches) == 0) {return(indicators_empty)}
  matches$numerator <-
    stringi::stri_replace_all_fixed(matches$numerator,
                                    combis, values, vectorize_all =
                                      FALSE)
  matches$denominator <-
    stringi::stri_replace_all_fixed(matches$denominator,
                                    combis, values,
                                    vectorize_all =
                                      FALSE)
  
  #Substitute totals
  matches$numerator<-stringi::stri_replace_all_fixed(matches$numerator,
                                                     totals_df$exp, totals_df$values, vectorize_all=FALSE)
  matches$denominator<-stringi::stri_replace_all_fixed(matches$denominator,
                                                       totals_df$exp, totals_df$values,
                                                       vectorize_all=FALSE)
  expression.pattern<-"#\\{[a-zA-Z][a-zA-Z0-9]{10}(\\.[a-zA-Z][a-zA-Z0-9]{10})?\\}"
  matches$numerator <-
    gsub(expression.pattern, "0", matches$numerator)
  matches$denominator <-
    gsub(expression.pattern, "0", matches$denominator)
  matches$numerator<-vapply(matches$numerator,function(x) {eval(parse(text=x))},FUN.VALUE=double(1))
  matches$denominator<-vapply(matches$denominator,function(x) {eval(parse(text=x))},FUN.VALUE=double(1))
  matches$value<-matches$numerator/matches$denominator
  
  matches
}


createS3BucketTags<-function(d) {
  d$info$country_uids<-paste0(d$info$country_uids,sep="",collapse=",")
  tags<-c("tool","country_uids","cop_year","has_error","sane_name","approval_status","source_user")
  object_tags<-d$info[names(d$info) %in% tags]
  object_tags<-URLencode(paste(names(object_tags),object_tags,sep="=",collapse="&"))
  
  return(object_tags)
}
