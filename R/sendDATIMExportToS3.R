sendDATIMExportToS3 <- function(d, job_type) {

  raw_file <-
    datapackr::writePDAPExportCSV(d, job_type = job_type)


  file_location <-
    datapackr::uploadDATIMExportToPDAP(raw_file = raw_file,
                                       job_type = job_type,
                                       content_type = "text/csv")
  resp <-
    datapackr::initiatePDAPJob(
      job_type = job_type,
      datim_export = file_location,
      org_unit_id = d$info$country_uids,
      period = paste0(d$info$cop_year, "Oct")
    )
  if (resp$status_code == 200L) {
    futile.logger::flog.info(paste0("Job type ", job_type, " successfully initiated."), name = "datapack")
    return(TRUE)
  } else {
    futile.logger::flog.error(paste0("Failed to inititate ",  job_type, "job type."), name = "datapack")
    return(FALSE)
  }

# sendDATIMExportToS3 <- function(d) {
#   #Write the flatpacked output
#   tmp <- tempfile()
#
#   # Selects the correct dataset to send to PAW if the tool has a PSNUxIM tab
#   datim_export <- datapackr::createPAWExport(d)
#
#   #Need better error checking here.
#   write.table(
#     datim_export,
#     file = tmp,
#     quote = FALSE,
#     sep = "|",
#     row.names = FALSE,
#     na = "",
#     fileEncoding = "UTF-8"
#   )
#
#   # Load the file as a raw binary
#   read_file <- file(tmp, "rb")
#   raw_file <- readBin(read_file, "raw", n = file.size(tmp))
#   close(read_file)
#
#   object_tags <- createS3BucketTags(d)
#
#   object_name <- paste0("datim_export/",gsub("^20", "cop",d$info$cop_year),
#                         "/",d$info$sane_name,".csv")
#
#   s3 <- paws::s3()
#
#   r <- tryCatch({
#     foo <- s3$put_object(Bucket = Sys.getenv("AWS_S3_BUCKET"),
#                          Body = raw_file,
#                          Key = object_name,
#                          Tagging = object_tags,
#                          ContentType = "text/csv")
#     futile.logger::flog.info("DATIM Export sent to S3", name = "datapack")
#     TRUE
#   },
#   error = function(err) {
#     futile.logger::flog.info("DATIM Export could not be sent to S3", name = "datapack")
#     futile.logger::flog.info(err, name = "datapack")
#     FALSE
#   })
#
#   unlink(tmp)
#
#   return(r)
# }
