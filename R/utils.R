getVersionInfo <- function() {

  currDCF <- read.dcf("DESCRIPTION")
  currVersion <- currDCF[1, "Version"]

  paste0("Version: ", currVersion) %>%
    paste('<div style="font-size:small;text-align: center;"><p>', .) %>%
    paste(., "</p></div>")
}

fetchModelFile <- function(model_path="support_files/datapack_model_data.rds") {

  can_read_file <- file.access(model_path, 4) == 0
  can_write_file <- file.access(dirname(model_path), 2) == 0
  max_cache_age <- "1 day"

  if (file.exists(model_path) & can_read_file) {
    cache_age <- lubridate::interval(file.info(model_path)$mtime, Sys.time())
    is_fresh <-
      lubridate::as.duration(cache_age) < lubridate::duration(max_cache_age)
  } else{
    is_fresh <- FALSE
  }

  if (!is_fresh & can_write_file) {
    datapackr::interactive_print("Fetching new model file from S3")
    dest_file <- fetchSupportFiles(model_path)

  } else {
    datapackr::interactive_print("Found cached model file.")
    dest_file <- paste0(getwd(), "/", model_path)
  }

  return(dest_file)
}

fetchSupportFiles <- function(path, locally=T) {

  s3 <- paws::s3()
  s3_object <-
    s3$get_object(Bucket = Sys.getenv("AWS_S3_BUCKET"),
                  Key = path)
  s3_object_body <- s3_object$Body

  if (locally==T) {
  #Hmm...use of getwd() is really not a good idea.
  file_name2 <- paste0(getwd(), "/", path)
  if (file.exists(file_name2)) {
    unlink(file_name2)
  }

  con <- file(file_name2, "wb")
  writeBin(s3_object_body, con = con)
  close(con)
  futile.logger::flog.info(paste0("Retreived support file to ", file_name2))
  if (!file.exists(file_name2)) {
    stop("Could not retreive support file.")
  }

  } else{
    file_name2 <- s3_object_body %>%
      rawToChar %>%
      read.csv(text = ., sep = "|")

    futile.logger::flog.info("Retreived support file in memory")
  }

  return(file_name2)
}

fetchY2File <- function(Y2_path= paste0("datim_export/cop", (d$info$cop_year - 1) %% 100 , "/", d$info$sane_name, "_Y2.csv")) {

  datapackr::interactive_print("Fetching last COP year's Year 2 data from S3")
  Y2File <- fetchSupportFiles(Y2_path, locally=F)

  return(Y2File)
}

sendDataPackErrorUI <- function(r) {
  if (!is_null(r)) {
    if (!r) {
      showModal(modalDialog(title = "Error",
                            "The Data Pack could not be archived."))
    }
  }

}

timestampUploadUI <- function(r) {
  if (!is.null(r)) {
    if (!r) {
      showModal(modalDialog(title = "Error",
                            "Timestamp log could not be saved to S3."))
    }
  }

}

validationSummaryUI <- function(r) {
  if (!is.null(r)) {
    if (!r) {
      shiny::showModal(shiny::modalDialog(title = "Error", "Validation summary could not be sent to AP."))
    }
  }

}

datimExportUI <- function(r) {
  if (!is.null(r)) {
    if (!r) {
      shiny::showModal(shiny::modalDialog(title = "Error",
                                          "DATIM Export could not be sent to S3"))
    } else {
      shiny::showModal(shiny::modalDialog(title = "Congrats!",
                                          "Export to PAW was successful."))
    }
  }

}

createS3BucketTags <- function(d) {
  d$info$country_uids <- paste(d$info$country_uids, sep = "", collapse = "_")
  tags <- c("tool", "country_uids", "cop_year", "has_error", "sane_name", "approval_status", "source_user")
  object_tags <- d$info[names(d$info) %in% tags]
  object_tags <- URLencode(paste(names(object_tags), object_tags, sep = "=", collapse = "&"))

  return(object_tags)
}
