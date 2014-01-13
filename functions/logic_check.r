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
  
  # Function to run one rule inside data.frame
  check_cond <- function(cond, df) eval(parse(text=cond), df)
  
  # Run all rules for every row in data
  result <- laply(rules, check_cond, data)
  result <- t(result)
  
  # If no names in rules list generate them from rules
  if(length(names(rules))==0) {
    rulesnames <- str_replace_all(unlist(rules), ' ', '')
    rulesnames <- make.names(rulesnames)
  } else rulesnames <- names(rules)
  colnames(result) <- rulesnames
  
  # Check the rule has at least one trigger
  brokenrules <- aaply(result, .(2), function(x) sum(x) < length(x))
  
  # Check the row has at least one error
  rowswitherror <- aaply(result, .(1), function(x) sum(x) < length(x))
  
  # What data columns take part in rules
  columnsincheck <- laply(names(data), 
                          function(x) sum(str_detect(unlist(rules), x)) > 0)
  
  # Drop passed rules
  result <- result[, brokenrules]
  
  # cbind result with original DF but only used columns
  result <- cbind(result, data[, columnsincheck])
  
  # TODO: Also remove columns what in rules but without errors 
  
  # Drop passed rows
  result <- result[rowswitherror,]
  
  result
}


