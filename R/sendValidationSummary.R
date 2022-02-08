sendValidationSummaryToS3 <- function(d, s3_folder, include_timestamp=FALSE) {

  validation_summary <- validationSummary(d) %>%
    # Adds columns that were a part of `validationSummary2`
    dplyr::mutate(ts = strftime(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                  uuid = d$info$uuid)

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

  object_tags <- createS3BucketTags(d)
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

  s3 <- paws::s3()

  r <- tryCatch({
    foo <- s3$put_object(Bucket = Sys.getenv("AWS_S3_BUCKET"),
                         Body = raw_file,
                         Key = object_name,
                         Tagging = object_tags,
                         ContentType = "text/csv")

    TRUE
  },
  error = function(err) {
    flog.info("Validation summary could not be sent to AP", name = "datapack")
    flog.info(err, name = "datapack")
    FALSE
  })

  unlink(tmp)

  return(r)
}
