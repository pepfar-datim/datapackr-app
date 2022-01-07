snuSelector <- function(df) {

  if (!inherits(df, "error") & !is.null(df)) {
    df  %>%
      purrr::pluck(., "data") %>%
      purrr::pluck(., "analytics") %>%
      dplyr::pull(., "snu1") %>%
      unique()

  } else {
    NULL
  }

}
