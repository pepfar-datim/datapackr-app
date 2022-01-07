evaluateIndicators <- function(combis, values, inds) {

  indicators_empty <- data.frame("Indicator" = character(),
                                 "N_OR_D" = character(),
                                 "Age" = character(),
                                 id = character(),
                                 numerator = numeric(),
                                 denominator = numeric(),
                                 value = numeric())

  this.des <-
    vapply(combis, function(x) {
      unlist(strsplit(x, "\\."))[[1]]
    }, FUN.VALUE = character(1))

  totals_df <- data.frame(exp = this.des, values = values, stringsAsFactors = FALSE) %>%
    dplyr::group_by(exp) %>%
    dplyr::summarise(values = sum(values)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(exp = paste0(exp, "}"))

  matches_indicator <- function(x) {
    agrepl(x, inds$numerator) |
      agrepl(x, inds$denominator)
  }

  matches <- this.des %>%
    unique(.) %>%
    purrr::map(., matches_indicator) %>%
    Reduce("|", .) %>%
    dplyr::filter(inds, .)

  #Return something empty here if we have no indicator matches
  if (nrow(matches) == 0) {
    return(indicators_empty)
  }

  replaceCombisWithValues <- function(x, combis.this=combis, values.this=values) {
    stringi::stri_replace_all_fixed(x,
                                    combis.this, values.this, vectorize_all =
                                      FALSE)
  }

  replaceTotalsWithValues <- function(x) replaceCombisWithValues(x, combis = totals_df$exp, values = totals_df$values)

  replaceExpressionsWithZeros <- function(x) {
    expression.pattern <- "#\\{[a-zA-Z][a-zA-Z0-9]{10}(\\.[a-zA-Z][a-zA-Z0-9]{10})?\\}"
    gsub(expression.pattern, "0", x)
  }

  evaluateExpression <- function(exp) {
    vapply(exp, function(x) eval(parse(text = x)), FUN.VALUE = double(1))
  }


  matches %>%
    purrr::modify_at(., c("numerator", "denominator"), replaceCombisWithValues) %>%
    purrr::modify_at(., c("numerator", "denominator"), replaceTotalsWithValues) %>%
    purrr::modify_at(., c("numerator", "denominator"), replaceExpressionsWithZeros) %>%
    purrr::modify_at(., c("numerator", "denominator"), evaluateExpression) %>%
    dplyr::mutate(value = numerator / denominator)



}
