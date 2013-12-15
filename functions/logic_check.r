#' Check data.frame for logical errors.
#' 
#' @param data Data.frame to work with.
#' @param rules Named list with rules to check.
#' 
#' @return Data.frame containing indentificators of rows with errors,
#' error values and their context.
#' 

logic_check <- function(data, rules) {
  require(plyr)
  require(stringr)
  check_cond <- function(cond, df) eval(parse(text=cond), df)
  result <- laply(rules, check_cond, data)
  result <- t(result)
  if(length(names(rules))==0) {
    rulesnames <- str_replace_all(unlist(rules), ' ', '')
    rulesnames <- make.names(rulesnames)
  } else rulesnames <- names(rules)
  colnames(result) <- rulesnames
  result
}


