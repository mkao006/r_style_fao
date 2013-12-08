#' Get data from FAO statistical working system (SWS).
#' 
#' @param area numeric or character vector with countries' ids (numeric) or 
#'   names (character).
#' @param item numeric or character vector with items' ids (numeric) or names 
#'   (character).
#' @param ele numeric or character vector with elements' ids (numeric) or names 
#'   (character).
#' @param year numeric vector with years to fetch.
#' @param symb optional character vector with symbols (flags) to fetch data type
#'   of.
#' @param melted logical, TRUE by default. Should the result data be returned in
#'   long format (instead of one column per every year two columns: year and 
#'   corresponded value).
#' @param value.names logical, TRUE by default. Should identificational vectors 
#'   (area, element, etc.) be converted from numeric ids to names.
#' @param stringsAsFactors logical. Should character 
#'   identificational vectors be converted to factors.
#' @param dbquery optional string with SQL-query to request from SWS.
#' @param class.path optional string with path to Oracle Java Data Base
#'   Connectivity library. By default 'ojdbc14.jar' in working directory.
#' @param user optional string with name of SWS DB user instead of default user 
#'   'demo'.
#' @param pass optional string with password of SWS DB user instead of default 
#'   password 'demo'.
#'   
#' @import RJDBC stringr reshape2
#'   
#' @return data.frame with results from SWS DB.


sws_query <- function(area, item, ele, year, symb, melted = TRUE, 
                      value.names = T, 
                      stringsAsFactors = default.stringsAsFactors(),
                      dbquery, class.path = 'ojdbc14.jar',
                      user = 'demo', pass = 'demo') {

# Check for ojdbc14.jar
if(!file.exists(class.path)) 
  stop("Oracle JDBC class not found. Please, put file ojdbc14.jar
into the working directory or specify full path with class.path argument.")

# Check for the internal connection
# Temrorary disable the checking
is.local.conn <- T

if(!is.local.conn)
  stop("SWS DB allows only internal connections. Please get a cable and find 
the nearest ethernet socket :)")

# Packages
library(RJDBC)
library(stringr)
library(reshape2)
       
return()

}