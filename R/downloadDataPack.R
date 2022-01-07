downloadDataPack <- function(d) {

  if (d$info$cop_year == "2021") {
    support_file <- fetchSupportFiles("data/psnuxim_model_data_21.rds")
  }

  if (d$info$cop_year == "2022") {
    support_file <- fetchSupportFiles("data/psnuxim_model_data_22.rds")
  }

  if (!file.exists(support_file)) {
    flog.error("Could not find model support file.")
    stop("WOMP!")
  }

  d <- writePSNUxIM(d, snuxim_model_data_path = support_file)
  unlink(support_file)

  return(d)
}
