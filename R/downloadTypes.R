downloadTypes  <-  function(tool_type="Data Pack", needs_psnuxim=FALSE, memo_authorized = FALSE) {

  if (is.null(needs_psnuxim)) {
    needs_psnuxim <- FALSE
  }

  download_names  <-
    c(
      "FlatPack",
      "CSO Flatpack",
      "Messages",
      "Validation report",
      #"New PSNUxIM",
      "Comparison",
      "COP Memo"
    )
  download_types  <-
    c("flatpack",
      "cso_flatpack",
      "messages",
      "vr_rules",
      # "datapack",
      "comparison",
      "memo")

  names(download_types)  <-  download_names

  if (tool_type == "OPU Data Pack" | !needs_psnuxim) {
    download_types <-  download_types[!(download_types %in% c("datapack"))]
  }

  if (!memo_authorized) {
    download_types <-  download_types[!(download_types %in% c("memo"))]
  }

  return(download_types)
}
