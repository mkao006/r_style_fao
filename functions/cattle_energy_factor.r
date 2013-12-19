# source('C:/Users/Matrunich/Documents/r_style_fao/functions/sws_query.r')


cattle_energy_factor <- function(area, year) {
  
  if(length(year) > 1) {
    library(plyr)
    return(ldply(year, cattle_energy_factor, area = area))
  }
  
  vars <- list(heads = c(11, 866), carcass = c(41, 867), milk = c(51, 882))
 

  
  
  
  data <- sws_query(area = area, year = year, 
                     pairs = vars)

  within(data, {
    Carcass.Wt <- Carcass.Wt / 10
    milkpercow <- Production * 1000 / Stocks
    energy <- (365 * (8.3 + 0.091 * Carcass.Wt * 2) + 5 * milkpercow) / 35600
  })

  
}

