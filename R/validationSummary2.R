validationSummary2 <- function(d) {
  tests_rows <- purrr::map(d$tests, NROW) %>%
    plyr::ldply(., data.frame) %>%
    `colnames<-`(c("test_name", "count"))

  tests_names <- purrr::map(d$tests, function(x) attr(x, "test_name")) %>%
    plyr::ldply(., data.frame) %>%
    `colnames<-`(c("test_name", "validation_issue_category")) # nolint

  dplyr::left_join(tests_names, tests_rows, by = "test_name") %>%
    dplyr::mutate(ou = d$info$datapack_name,
                  ou_id = d$info$country_uids,
                  country_name = d$info$datapack_name,
                  country_uid = paste(d$info$country_uids, sep = "", collapse = ", "),
                  ts = strftime(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                  uuid = d$info$uuid) %>%
    dplyr::filter(count > 0)
}
