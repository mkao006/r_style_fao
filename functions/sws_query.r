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
    pingvec <- system2("ping",paste('-n 1', x),
                       stderr=FALSE,
                       stdout=FALSE,...)
    if (pingvec == 0) TRUE else FALSE
  }
  
  if(!ping("lprdbwo1.fao.org"))
    stop("SWS DB accepts only internal connections. Please get a cable and find 
the nearest ethernet socket :)")
  
  # Packages
  library(RJDBC)

  
  drv <- JDBC(driverClass = "oracle.jdbc.driver.OracleDriver",
              classPath = class.path)
  conn <- dbConnect(drv, "jdbc:oracle:thin:@lprdbwo1:3310:fstp",
                    user = user, password = pass)
  
  # This is the exclusive request to DB in the function.
  # All others just invoke sws_query with dbquery argument.
  if(!missing(dbquery)) {
    dboutput <- dbGetQuery(conn, dbquery)
    dbDisconnect(conn)
    return(dboutput)
  }

  library(stringr)
  library(reshape2)
  
  # Function to convert year in colnames, e.g. from 00 to 1960
  convertyear <- function(x) {
    # Vectorizing the function
    if(length(x) > 1) {
      require(plyr)
      return(unlist(llply(x, convertyear)))
    }
    
    require(stringr)
    if(!str_detect(x, '[0-9]{2}$')) return(x)
    orignumb <- as.numeric(str_extract(x, '[0-9]{2}$'))
    corryear <- orignumb + 1959
    corrname <- str_c(str_replace(x, '(^.*)([0-9]{2}$)', '\\1'), corryear)
    corrname
  }
  
  
  # convert vectors in arguments to collapsed strings.
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
  # Name of data base table. In case of using tsv_ics_work_yr it's not
  # require to convert years from 00 to 1960. But possibly you need to 
  # remove totals.
  dbmain <- 'TS_ICS_WORK_YR'
  
  # WHAT part of query
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

  # Ask the DB with constructed query
  dboutput <- sws_query(class.path=class.path, dbquery=constrdbquery)
  
  colnames(dboutput) <- tolower(colnames(dboutput))
  colnames(dboutput)[1:2] <- c('area', 'item')
  
  # Converting colnames with years from 00 to 1960
  if(tolower(dbmain) == 'ts_ics_work_yr') colnames(dboutput) <-
    convertyear(colnames(dboutput))
  
  # Converting from wide format to long.
  if(melted) {
    
    # Selecting part with values only (without symbols/flags)
    valueswithoutsymb <- dboutput[, colnames(dboutput)[
      str_detect(colnames(dboutput), perl('^(?!symb)'))]]
    
    # Melting
    valueswithoutsymb <- 
      melt(valueswithoutsymb, measure.vars=
             names(valueswithoutsymb[str_detect(names(valueswithoutsymb),
                                          '^num_')]),
           variable.name = 'year')
    
    # Convert character vector with year to numeric
    valueswithoutsymb$year <- as.numeric(str_replace(valueswithoutsymb$year,
                                                     '^num_', ''))
    
    # Converting part with symbols/flags
    if(symb) {
      flags <- dboutput[, colnames(dboutput)[
        str_detect(colnames(dboutput), perl('^(?!num)'))]]
      
      flags <- 
        melt(flags, measure.vars=
               names(flags[str_detect(names(flags),
                                                  '^symb_')]),
             variable.name = 'year', value.name = 'flag')
      
      flags$year <- as.numeric(str_replace(flags$year, '^symb_', ''))
      
      # Joining values and flags
      dboutput <- join(valueswithoutsymb, flags, by = c('area', 'item',
                                                        'ele', 'year'))
      
    } else dboutput <- valueswithoutsymb
    


  }
  
  dboutput
  
}