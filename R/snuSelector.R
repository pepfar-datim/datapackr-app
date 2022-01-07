snuSelector <- function(df) {

  if (inherits(df, "error") || is.null(df)) {
    return(NULL)
  }

  snu_list <- df  %>%
    purrr::pluck(., "data") %>%
    purrr::pluck(., "analytics") %>%
    dplyr::pull(., "snu1") %>%
    unique()

  return(snu_list)
}
