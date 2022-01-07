saveDATIMExportToS3 <- function(d) {
  #Write the flatpacked output
  tmp  <-  tempfile()

  datim_export <- dplyr::bind_rows(d$datim$subnat_impatt,
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
  read_file  <-  file(tmp, "rb")
  raw_file  <-  readBin(read_file, "raw", n = file.size(tmp))
  close(read_file)

  object_tags <- createS3BucketTags(d)

  object_name <- paste0("datim_export/", gsub("^20", "cop", d$info$cop_year), "/", d$info$sane_name, ".csv")
  s3 <- paws::s3()

  r <- tryCatch({
    foo <- s3$put_object(Bucket = Sys.getenv("AWS_S3_BUCKET"),
                         Body = raw_file,
                         Key = object_name,
                         Tagging = object_tags,
                         ContentType = "text/csv")
    flog.info("DATIM Export sent to S3", name = "datapack")
    TRUE
  },
  error = function(err) {
    flog.info("DATIM Export could not be sent to  S3", name = "datapack")
    flog.info(err, name = "datapack")
    FALSE
  })

  unlink(tmp)

  return(r)

}
