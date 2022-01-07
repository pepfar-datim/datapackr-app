sendMERDataToPAW <- function(d) {



  #Write the combined DATIM export for MER and SUBNATT data
  tmp  <-  tempfile()
  mer_data <- dplyr::bind_rows(d$datim) %>%
    dplyr::mutate(categoryOptionCombo = case_when(is.na(categoryOptionCombo) ~ "HllvX50cXC0",
                                                  TRUE ~categoryOptionCombo)) %>%
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
  read_file  <-  file(tmp, "rb")
  raw_file  <-  readBin(read_file, "raw", n = file.size(tmp))
  close(read_file)

  object_tags <- createS3BucketTags(d)
  object_name <- paste0("datim_export/cop21/", d$info$sane_name, ".csv")

  svc <- paws::s3()

  r <- tryCatch({
    foo <- svc$put_object(Bucket = Sys.getenv("AWS_S3_BUCKET"),
                          Body = raw_file,
                          Key = object_name,
                          Tagging = object_tags,
                          ContentType = "text/csv")
    flog.info("Flatpack sent to AP", name = "datapack")
    TRUE
  },
  error = function(err) {
    flog.info("Flatpack cannot be sent to AP", name = "datapack")
    flog.info(err, name = "datapack")
    FALSE
  })

  unlink(tmp)

  return(r)
}
