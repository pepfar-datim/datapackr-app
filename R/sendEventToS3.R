sendEventToS3 <- function(d=NULL, event_type, user_input=NULL) {

  if (is.null(d)) {
    d <- list(info = list(
      tool = NA,
      datapack_name = NA,
      cop_year = NA,
      uuid  = user_input$uuid,
      source_user = "unknown"))
  }

  s3 <- paws::s3()
  tm <- as.POSIXlt(Sys.time(), "UTC")
  ts_file <- strftime(tm, "%Y_%m_%d_%H_%M_%s")

  #will need to update the above for this PR
  object_name <-
    paste0("datapackr_app_events/", ts_file, ".csv")

  event_info <- list(
    event_type = event_type,
    tool = d$info$tool,
    datapack_name = d$info$datapack_name,
    cop_year = d$info$cop_year,
    uuid = d$info$uuid,
    user = digest(d$info$source_user, "md5", serialize = FALSE),
    ts = strftime(tm, "%Y-%m-%dT%H:%M:%S%z")
  )

  tmp <- tempfile()
  write.table(
    as.data.frame(event_info),
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

  r <- tryCatch({
    foo <- s3$put_object(Bucket = Sys.getenv("AWS_S3_BUCKET"),
                         Body = raw_file,
                         Key = object_name,
                         ContentType = "text/csv")
  },
  error = function(err) {
    futile.logger::flog.error("Event could not be saved to S3", name = "datapack")
    FALSE
  })

  unlink(tmp)

  return(r)
}
