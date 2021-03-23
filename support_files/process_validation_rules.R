require(jsonlite)
require(purrr)
require(dplyr)
require(magrittr)

new<-"support_files/newValidationRules.json"
old<-"support_files/oldValidationRules.json"
modified<-"support_files/modifiedValidationRules.json"

rules<-list(new,old,modified)

processValidationRules<-function(r) {
  expression.pattern<-"[a-zA-Z][a-zA-Z0-9]{10}(\\.[a-zA-Z][a-zA-Z0-9]{10})?"
  vr<-jsonlite::fromJSON(r,flatten=TRUE)$validationRules
  #Static predefined map of operators
  op.map<-data.frame(x=c("greater_than_or_equal_to","greater_than","equal_to","not_equal_to","less_than_or_equal_to","less_than","exclusive_pair","compulsory_pair"),
                     y=c(">=",">","==","!=","<=","<","|","&"),stringsAsFactors=F)
  #Strategies
  strat.map<-data.frame(x=c("SKIP_IF_ANY_VALUE_MISSING","SKIP_IF_ALL_VALUES_MISSING","NEVER_SKIP"))
  #Remap the operators
  vr$operator<-plyr::mapvalues(vr$operator,op.map$x,op.map$y,warn_missing=FALSE)
  #Count the left and right side operators
  vr$rightSide.ops<-stringr::str_count(vr$rightSide.expression,expression.pattern)
  vr$leftSide.ops<-stringr::str_count(vr$leftSide.expression,expression.pattern)
  #Remove any line breaks
  vr$leftSide.expression<-stringr::str_replace(vr$leftSide.expression,pattern = "\n","")
  vr$rightSide.expression<-stringr::str_replace(vr$rightSide.expression,pattern = "\n","")
  
  vr
  
}



vr_cop20<-purrr::map_dfr(rules,processValidationRules) %>% 
  dplyr::filter(stringr::str_detect(description,"TARGET")) 



require(datimvalidation)
require(datimutils)
loginToDATIM("~/.secrets/datim-prod.json")

vr_cop21<-getValidationRules() %>% 
  dplyr::filter(stringr::str_detect(description,"TARGET"))


vr<-list("2020"=vr_cop20,"2021"=vr_cop21)

saveRDS(vr,"support_files/cop_validation_rules.rds")