getMemoIndicators <- function(d, d2_session) {
  #Fetch indicators from the COP21 memo group
  #TODO: Make this work for both COP years.!
  ind_group <- switch(as.character(d$info$cop_year),
                      "2020" = "wWi08ToZ2gR",
                      "2021" = "TslxbFe3VUZ",
                      "2022" = "TslxbFe3VUZ",
                      NULL) #TODO: Fix this once we get the COP22 indicator group

  inds <-
    datimutils::getIndicatorGroups(ind_group,
                                   d2_session = d2_session,
                                   fields = "indicators[id, name, numerator, denominator]")

  if (class(inds) != "data.frame") {
    stop("No indicator metadata  was returned from DATIM")
  }

  inds
}
