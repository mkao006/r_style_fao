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
#'   (area and item) be converted from numeric ids to names.
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


sws_query <- function(area, item, ele, year, symb = T, melted = TRUE, 
                      value.names = T, 
                      stringsAsFactors = default.stringsAsFactors(),
                      dbquery, class.path = 'ojdbc14.jar',
                      user = 'demo', pass = 'demo') {
  
  
  
  # Check for ojdbc14.jar
  if(!file.exists(class.path)) 
    stop("Oracle JDBC class not found. Please, put file ojdbc14.jar
into the working directory or specify full path with class.path argument.")
  
  # Check for the internal connection
  # Source of ping function:
  # http://stackoverflow.com/questions/7012796/ping-a-website-in-r
  ping <- function(x,stderr=FALSE,stdout=FALSE,...){
    pingvec <- system2("ping",x,
                       stderr=FALSE,
                       stdout=FALSE,...)
    if (pingvec == 0) TRUE else FALSE
  }
  
  if(!ping("lprdbwo1.fao.org"))
    stop("SWS DB allows only internal connections. Please get a cable and find 
the nearest ethernet socket :)")
  
  # Packages
  library(RJDBC)

  
  drv <- JDBC(driverClass = "oracle.jdbc.driver.OracleDriver",
              classPath = class.path)
  conn <- dbConnect(drv, "jdbc:oracle:thin:@lprdbwo1:3310:fstp",
                    user = user, password = pass)
  
  if(!missing(dbquery)) {
    dboutput <- dbGetQuery(conn, dbquery)
    dbDisconnect(conn)
    return(dboutput)
  }

  library(stringr)
  library(reshape2)
  
  if(!missing(area)) area <- str_c(area, collapse=', ')
  if(!missing(item)) item <- str_c(item, collapse=', ')
  if(!missing(ele)) ele <- str_c(ele, collapse=', ')
  if(!missing(year) & symb) flag <- 
    str_c('SYMB_', formatC(year - 1959, width=2, format='d', flag='0'),
          collapse = ', ')
  if(!missing(year)) year <- 
    str_c('NUM_', 
          formatC(year - 1959, width=2, format='d', flag='0'), collapse=', ')

  
  # Constructing query
  dbmain <- 'TS_ICS_WORK_YR'
  # WHAT
  if(value.names) 
    whatsql <- str_c('area.name_e', 'item.name_e', sep = ', ') else
      whatsql <- str_c('area', 'item', sep = ', ')
  
  
  whatsql <- str_c(whatsql, 'ele', sep = ', ')
  
  if(!missing(year)) whatsql <- str_c(whatsql, year, sep=', ')
  if(!missing(year) & symb) whatsql <- str_c(whatsql, flag, sep=', ')

  
  # FROM
  fromsql <- dbmain
  if(value.names) fromsql <- str_c(str_c('FAOSTAT.',fromsql),
                                   'FAOSTAT.AREA, FAOSTAT.ITEM', sep = ', ')
  
  # WHERE
  wheresql <- list()
  if(!missing(area)) wheresql[length(wheresql) + 1] <- 
    str_c(dbmain, '.area in (', area, ') ')
  if(!missing(item)) wheresql[length(wheresql) + 1] <-
    str_c(dbmain, '.item in (', item, ') ')
  if(!missing(ele)) wheresql[length(wheresql) + 1] <-
    str_c(dbmain, '.ele in (', ele, ') ')
#   if(length(wheresql == 0)) wheresql[1] <- '*'
  
  if(value.names) wheresql[length(wheresql) + 1] <- 
    str_c('AREA.AREA = ', dbmain, '.AREA and item.item = ', dbmain, '.item')
  
  wheresql <- str_c(unlist(wheresql), collapse=' and ')
  
  
  constrdbquery <- str_c('select ', whatsql, ' from ', 
                         fromsql, ' where ', wheresql
  )

# For debugging of query construction  
#   return(constrdbquery)
  
  dboutput <- sws_query(class.path=class.path, dbquery=constrdbquery)
  
  
  
  dboutput
  
}