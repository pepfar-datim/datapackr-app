downloadDataPack <- function(d,
                             append = TRUE,
                             use_template = FALSE,
                             d2_session) {

  if (d$info$cop_year == "2022") {
    support_file <- fetchSupportFiles("support_files/psnuxim_model_data_22.rds")
  }

  if (d$info$cop_year == "2023") {
    support_file <- fetchSupportFiles("support_files/psnuxim_model_data_23.rds")
  }

  if (d$info$cop_year == "2024") {
    support_file <- fetchSupportFiles("support_files/psnuxim_model_data_24.rds") #NOTE this doesn't exist yet
  }

  if (!file.exists(support_file)) {
    futile.logger::flog.error("Could not find model support file.")
    stop("WOMP!")
  }

  d <- datapackr::writePSNUxIM(d,
                               snuxim_model_data_path = support_file,
                               d2_session = d2_session,
                               append = append,
                               use_template = use_template)
  unlink(support_file)

  return(d)
}

