downloadTypes <- function(d,
                          memo_authorized = FALSE) {
  #Common types which should always be there.
  download_names <-
    c("FlatPack",
      "CSO Flatpack",
      "Messages",
      "Validation report",
      "Comparison")

  download_types <-
    c("flatpack",
      "cso_flatpack",
      "messages",
      "vr_rules",
      "comparison")
#A completely new PSNUxIM
#

  if (!d$info$has_psnuxim){
    download_types <- c(download_types, "datapack")
    download_names <- c(download_names, "New PSNUxIM")
  }

#Two ways to "append". Either to the existing datapack
#or only get the missing rows
  if ( NROW(d$data$missingCombos) > 0 ) {
    download_types <- c(download_types, "missing_psnuxim_targets")
    download_names <- c(download_names,  "Only Missing PSNUxIM Targets")

    download_types <- c(download_types, "append_missing_psnuxim_targets")
    download_names <- c(download_names,  "Append Missing PSNUxIM Targets")
  }

  if ( NROW(d$tests$non_equal_targets) > 0 ) {
    download_types <- c(download_types, "update_psnuxim_targets")
    download_names <- c(download_names,  "Update PSNUxIM Targets")
  }

  if (memo_authorized) {
    download_types <- c(download_types, "memo")
    download_names <- c(download_names, "COP Memo")
  }

  names(download_types) <- download_names

  return(download_types)
}
