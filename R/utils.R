downloadTypes <- function(tool_type="Data Pack",needs_psnuxim=FALSE, memo_authorized = FALSE) {
  
  if (is.null(needs_psnuxim)) { needs_psnuxim<-FALSE}
  
  download_names <-
    c(
      "FlatPack",
      "CSO Flatpack",
      "Messages",
      "Validation report",
      "New PSNUxIM",
      "Comparison",
      "COP Memo"
    )
  download_types <-
    c("flatpack",
      "cso_flatpack",
      "messages",
      "vr_rules",
      "datapack",
      "comparison",
      "memo")
  
  
  names(download_types) <- download_names
  
  if (tool_type == "OPU Data Pack" | !needs_psnuxim ) {
    download_types<- download_types[!(download_types %in% c("datapack"))]
  }
  
  if (!memo_authorized) {
    download_types<- download_types[!(download_types %in% c("memo"))]
  }
  
  
  download_types
}

getVersionInfo<-function() {
  

  currDCF <- read.dcf("DESCRIPTION")
  currVersion <- currDCF[1,"Version"]
  
  #paste0("Version: ",gert::git_branch(),"@",substr(gert::git_log(max=1)$commit,0,10)) %>% 
  paste0("Version: ",currVersion) %>% 
    paste('<div style="font-size:small;text-align: center;"><p>',.) %>% 
    paste(.,"</p></div>")
}

fetchModelFile<-function(model_path="support_files/datapack_model_data.rds") {
  

  can_read_file <- file.access(model_path, 4) == 0
  can_write_file <-file.access(dirname(model_path), 2) == 0
  max_cache_age <- "1 day"

  if (file.exists(model_path) & can_read_file) {
    is_fresh <-
      lubridate::as.duration(lubridate::interval(file.info(model_path)$mtime,Sys.time())) < lubridate::duration(max_cache_age)
  } else{
    is_fresh<-FALSE
  }
  
  if (!is_fresh & can_write_file) {
    interactive_print("Fetching new model file from S3")
    dest_file<-fetchSupportFiles(model_path)

  } else {
    interactive_print("Found cached model file.")
    dest_file<-paste0(getwd(),"/",model_path)
  }
  
  return(dest_file)

  
}

fetchSupportFiles <- function(path) {
  

    s3 <- paws::s3()
    s3_object <-
      s3$get_object(Bucket = Sys.getenv("AWS_S3_BUCKET"),
                    Key = path)
    s3_object_body <- s3_object$Body
    
    #Hmm...use of getwd() is really not a good idea.
    file_name2 <- paste0(getwd(),"/",path)
    if (file.exists(file_name2)) {
      unlink(file_name2)
    }
    
    con<-file(file_name2,"wb")
    writeBin(s3_object_body, con = con)
    close(con)
    flog.info(paste0("Retreived support file to ", file_name2))
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
  
  #Deal with negative values in dedupe
  vr_data %<>% dplyr::mutate(value = case_when(attributeOptionCombo %in% c("00000","00001") ~ abs(value),
                                               TRUE ~ value))
  
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
  
  vr_rules<-readRDS("support_files/cop_validation_rules.rds") %>% 
    purrr::pluck(.,as.character(d$info$cop_year))
  
  vr_violations <- datimvalidation::validateData(vr_data,
                                                 parallel = is_parallel,
                                                 return_violations_only = TRUE,
                                                 vr = vr_rules,
                                                 d2session = d2_session)

  
  if ( NROW(vr_violations) > 0 ) {
    
    diff <- gsub(" [<>]= ", "/", vr_violations$formula)
    vr_violations$diff <- sapply( diff, function(x) { round( ( eval( parse( text = x ) ) - 1) * 100, 2 ) })
    vr_violations$diff <-ifelse(vr_violations$rightSide.expression == 0 | vr_violations$leftSide.expression == 0,
                                NA,
                                vr_violations$diff)
    
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
  
  
  mechs_data <- unique(d$datim$MER$attributeOptionCombo)
  
  period_info<-datimvalidation::getPeriodFromISO(paste0(d$info$cop_year,"Oct"))
  
  operating_unit<-getOperatingUnitFromCountryUIDs(d$info$country_uids)

  mechs_datim<-datapackr::getMechanismView(d2_session = d2_session,
                                     update_stale_cache = TRUE,
                                     cached_mechs_path = "support_files/mechs.rds") %>%
    dplyr::filter( ou == operating_unit$ou) %>% 
    dplyr::filter(!is.na(startdate)) %>%
    dplyr::filter(!is.na(enddate)) %>%
    dplyr::filter(startdate <= period_info$startDate) %>%
    dplyr::filter(enddate >= period_info$endDate ) %>%
    dplyr::pull(mechanism_code)
  
  #Allow for the default mechanism
  mechs_datim<-append("HllvX50cXC0",mechs_datim)
  
  #Allow for the dedupe mechanisms in COP20 OPU Data Packs
  if (d$info$tool == "OPU Data Pack" & d$info$cop_year == 2020 ) {
    mechs_datim <- append(c("00000","00001"),mechs_datim)
  }
  
  #Allow for the dedupe mechanisms in COP21 Data packs
  if (d$info$tool == "Data Pack" & d$info$cop_year == 2021 ) {
    mechs_datim <- append(c("00000","00001"),mechs_datim)
  }

  
  bad_mechs<-mechs_data[!(mechs_data %in% mechs_datim)]
  
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

getOperatingUnitFromCountryUIDs<-function(country_uids) {
  ou<-datapackr::valid_PSNUs %>% 
    dplyr::select(ou,ou_id,country_name,country_uid) %>%  
    dplyr::distinct() %>% 
    dplyr::filter(country_uid %in% country_uids) %>% 
    dplyr::select(ou,ou_id) %>% 
    dplyr::distinct()
  
  if ( NROW(ou) != 1) {  stop("Datapacks cannot belong to multiple operating units") }
  
  ou
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
  #options(digits.secs=6)
  timestamp_info<-list(
    ou=d$info$operating_unit$ou,
    ou_id=d$info$operating_unit$ou_id,
    country_name=d$info$datapack_name,
    country_uids=paste(d$info$country_uids,sep="",collapse=","),
    upload_timestamp=strftime(as.POSIXlt(Sys.time(), "UTC") , "%Y-%m-%d %H:%M:%S"),
    #uuid=d$info$uuid,
    #upload_timestamp=strftime(as.POSIXlt(Sys.time(), "UTC") , "%Y-%m-%d %H:%M:%OS"),
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

validationSummary2<-function (d) {
  tests_rows <- purrr::map(d$tests, NROW) %>% plyr::ldply(., 
                                                          data.frame) %>% `colnames<-`(c("test_name", "count"))
  tests_names <- purrr::map(d$tests, function(x) attr(x, "test_name")) %>% 
    plyr::ldply(., data.frame) %>% `colnames<-`(c("test_name", 
                                                  "validation_issue_category"))
  dplyr::left_join(tests_names, tests_rows, by = "test_name") %>% 
    dplyr::mutate(ou = d$info$operating_unit$ou, 
                  ou_id = d$info$operating_unit$ou_id, 
                  country_name = d$info$datapack_name, 
                  country_uid = paste(d$info$country_uids,sep="",collapse=",")) %>% 
    dplyr::filter(count > 0)
}

sendValidationSummary<-function(d,s3_folder,include_timestamp=FALSE) {
  

  validation_summary<-validationSummary2(d)
  
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
  if (include_timestamp) {
    object_name <-
      paste0(
        s3_folder,
        "/",
        gsub("^20", "cop", d$info$cop_year),
        "/",
        d$info$sane_name,
        "_",
        format(Sys.time(), "%Y%m%d_%H%M%OS"),
        ".csv"
      )
  } else {
    object_name <-
      paste0(s3_folder,
             "/",
             gsub("^20", "cop", d$info$cop_year),
             "/",
             d$info$sane_name,
             ".csv")
  }

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


  datim_export<-dplyr::bind_rows(d$datim$subnat_impatt,
  d$datim$fy22_prioritizations,
  d$datim$MER) %>% 
  dplyr::mutate(value = as.character(value))
  
  #Need better error checking here.
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
  

  matches <- this.des %>% 
    unique(.) %>% 
    purrr::map(.,matches_indicator) %>% 
    Reduce("|",.) %>% 
    dplyr::filter(inds,.)
  
  #Return something empty here if we have no indicator matches
  
  if (nrow(matches) == 0) {return(indicators_empty)}
  
  
  replaceCombisWithValues<-function(x,combis.this=combis,values.this=values) {
    stringi::stri_replace_all_fixed(x,
                                    combis.this, values.this, vectorize_all =
                                      FALSE)
  }
  
  replaceTotalsWithValues<-function(x) replaceCombisWithValues(x,combis=totals_df$exp,values=totals_df$values)
  
  replaceExpressionsWithZeros<-function(x) {
    expression.pattern<-"#\\{[a-zA-Z][a-zA-Z0-9]{10}(\\.[a-zA-Z][a-zA-Z0-9]{10})?\\}"
    gsub(expression.pattern, "0", x)
  }
  
  evaluateExpression<-function(exp) {
    vapply(exp,function(x) {eval(parse(text=x))},FUN.VALUE=double(1))
  }
  
  
  matches %>% purrr::modify_at(.,c("numerator","denominator"),replaceCombisWithValues) %>% 
    purrr::modify_at(.,c("numerator","denominator"),replaceTotalsWithValues) %>% 
    purrr::modify_at(.,c("numerator","denominator"),replaceExpressionsWithZeros) %>% 
    purrr::modify_at(.,c("numerator","denominator"),evaluateExpression) %>% 
    dplyr::mutate(value = numerator / denominator)
  


}

createS3BucketTags<-function(d) {
  d$info$country_uids<-paste0(d$info$country_uids,sep="",collapse="_")
  #d$info$country_uids<-d$info$operating_unit$ou_id
  tags<-c("tool","country_uids","cop_year","has_error","sane_name","approval_status","source_user")
  object_tags<-d$info[names(d$info) %in% tags]
  object_tags<-URLencode(paste(names(object_tags),object_tags,sep="=",collapse="&"))
  
  return(object_tags)
}

updateExistingPrioritization<-function(d,d2_session) {
  

psnus<-   d$data$analytics$psnu_uid %>% unique() %>% unlist()

#Break up into 2048 character URLS (approximately)
n_requests<-ceiling(nchar(paste(psnus,sep="",collapse=";"))/2048)
n_groups<-split(sample(psnus),1:n_requests)


getPrioTable<-function(x) {
  datimutils::getAnalytics(dx="r4zbW3owX9n",
                           pe_f =paste0( (d$info$cop_year),"Oct"),
                           ou = x,
                           d2_session = d2_session ) }

prios<-n_groups %>% purrr::map_dfr(getPrioTable)

if (is.null(prios)) {
  interactive_print("No prioritization information found. Skipping update.")
  return(d)
  }

prios %<>% 
  dplyr::select(-Data) %>% 
  dplyr::rename("psnu_uid" = "Organisation unit",
                "value" = "Value") %>% 
  dplyr::left_join(datapackr::prioritization_dict()) %>% 
  dplyr::select(psnu_uid,
                "prioritization" = "name")

d$data$analytics %<>% 
  dplyr::select(-prioritization) %>% 
  dplyr::left_join(prios,by="psnu_uid")

d
}