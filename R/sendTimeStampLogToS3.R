sendTimeStampLogToS3 <- function(d) {

  #Write an archived copy of the file
  s3 <- paws::s3()
  object_tags <- createS3BucketTags(d)

  object_name <-
    paste0("processed/",
           gsub("^20", "cop", d$info$cop_year),
           ifelse(d$info$cop_year==2021,"_opu",""),
           "/",
           d$info$sane_name,
           ".csv")

  timestamp_info <- list(
    ou = d$info$operating_unit$ou,
    ou_id = d$info$operating_unit$ou_id,
    country_name = d$info$datapack_name,
    country_uids = paste(d$info$country_uids, sep = "", collapse = ", "),
    upload_timestamp = strftime(as.POSIXlt(Sys.time(), "UTC"), "%Y-%m-%d %H:%M:%S"),
    filename = object_name
  )

  tmp <- tempfile()
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
      ifelse(d$info$cop_year==2021,"_opu",""),
      "/",
      d$info$sane_name,
      ".csv"
    )

  r <- tryCatch({
    foo <- s3$put_object(Bucket = Sys.getenv("AWS_S3_BUCKET"),
                         Body = raw_file,
                         Key = object_name,
                         Tagging = object_tags,
                         ContentType = "text/csv")
    futile.logger::flog.info("Timestamp log sent to S3", name = "datapack")
    TRUE
  },
  error = function(err) {
    futile.logger::flog.error("Timestamp log could not be saved to S3", name = "datapack")
    FALSE
  })
  unlink(tmp)
  return(r)
}
