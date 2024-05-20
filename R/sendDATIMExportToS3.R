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

}
