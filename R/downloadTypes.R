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


  #Here are two situations when we need a new PSNUxIM
  # 1) This is a COP23 datapack with no associated PSNUxIM tab
  # 2) A COP23 datapack which has a PSNUxIM tab attached, but which has missing combos

  if (is.null(d$data$PSUxIM) || (NROW(d$data$missingCombos) > 0)) {
    download_types <- c(download_types, "datapack")
    download_names <- c(download_names, "New PSNUxIM")
  }

  #If there are no non-equal targets, they can choose to get only the missing PSNUxIM
  #Targets and append them
  if (NROW(d$data$missingCombos) > 0 &&
      NROW(d$tests$non_equal_targets == 0)) {
    download_types <- c(download_types, "missing_psnuxim_targets")
    download_names <- c(download_names,  "Missing PSNUxIM Targets")
  }


  if (memo_authorized) {
    download_types <- c(download_types, "memo")
    download_names <- c(download_names, "COP Memo")
  }

  names(download_types) <- download_names

  return(download_types)
}
