downloadDataPack <- function(d, d2_session) {

  if (d$info$cop_year == "2021") {
    support_file <- fetchSupportFiles("support_files/psnuxim_model_data_21.rds")
  }

  if (d$info$cop_year == "2022") {
    support_file <- fetchSupportFiles("support_files/psnuxim_model_data_22.rds")
  }

  if (!file.exists(support_file)) {
    futile.logger::flog.error("Could not find model support file.")
    stop("WOMP!")
  }

  d <- datapackr::writePSNUxIM(d, snuxim_model_data_path = support_file, d2_session = d2_session, append = TRUE)
  unlink(support_file)

  return(d)
}

downloadMissingPSNUxIMTargets <- function(d, d2_session) {

  if (d$info$cop_year == "2021") {
    support_file <- fetchSupportFiles("support_files/psnuxim_model_data_21.rds")
  }

  if (d$info$cop_year == "2022") {
    support_file <- fetchSupportFiles("support_files/psnuxim_model_data_22.rds")
  }

  if (!file.exists(support_file)) {
    futile.logger::flog.error("Could not find model support file.")
    stop("WOMP!")
  }

  d <- datapackr::writePSNUxIM(d, snuxim_model_data_path = support_file, d2_session = d2_session, append = FALSE)
  unlink(support_file)

  return(d)
}
