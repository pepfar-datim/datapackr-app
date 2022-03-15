downloadTypes <- function(tool_type="Data Pack", needs_psnuxim=FALSE, memo_authorized = FALSE, has_comments_issue = FALSE) {

  if (is.null(needs_psnuxim)) {
    needs_psnuxim <- FALSE
  }

  download_names <-
    c(
      "FlatPack",
      "CSO Flatpack",
      "Messages",
      "Validation report",
      "New PSNUxIM",
      "Missing PSNUxIM Targets",
      "Comparison",
      "COP Memo"
    )
  download_types <-
    c("flatpack",
      "cso_flatpack",
      "messages",
      "vr_rules",
      "datapack",
      "missing_psnuxim_targets",
      "comparison",
      "memo")

  names(download_types) <- download_names

  if (tool_type == "OPU Data Pack" | !needs_psnuxim) {
    download_types <- download_types[!(download_types %in% c("datapack","missing_psnuxim_targets"))]
  }

  if (!memo_authorized) {
    download_types <- download_types[!(download_types %in% c("memo"))]
  }

  #Remove the PSNUxIM download if they have comments
  if (has_comments_issue) {
    download_types <- download_types[!(download_types %in% c("datapack"))]
  }

  return(download_types)
}
