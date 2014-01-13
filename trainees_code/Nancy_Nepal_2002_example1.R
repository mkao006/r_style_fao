## This routine created by NC, JAM and MAV at FAO, Rome on 10/12/2013

## Last updated on 10/12/13

## This routine selects a sample by Probability Proportional to Size (PPS)
## from a Census listing data set from the Nepal 2002 Census

# Remove everything preivously stored in the environment
rm(list = ls())

# Loading the necessary libraries
library(XLConnect)
library(stringr)


# Read file from MS Excel file in xlsx format
nepal2002censusdata <- readWorksheetFromFile("data.xlsx", sheet = 1)


# Show the structure of the structure of data once imported into R
str(nepal2002censusdata)

# Covert id variable from character format to numeric value format
nepal2002censusdata$wd_id <- as.numeric( nepal2002censusdata$Wd_Id)

# Create a new unique id by joining multiple variables
nepal2002censusdata$id <- str_c(nepal2002censusdata$District_ID, 
                                nepal2002censusdata$Wd_Id,
                                nepal2002censusdata$EA_No,
                                nepal2002censusdata$House_No,
                                nepal2002censusdata$Hhold_no
                                )

# Create new logical varirable based previous binary (0 or 1) variable
## that describes if the family are involved in agriculture
nepal2002censusdata$do_agri <- as.logical(nepal2002censusdata$Op_Ag.)

# Create a factor variable based on binary variable that also describes
## if the family are involved in agriculture
nepal2002censusdata$do_agri_fac <- factor(nepal2002censusdata$Op_Ag.,
                                          levels = 0:1,
                                          labels = c("No agri", "Do agri"))

# Tabulation of logical variable to show how many of each we now have
table(nepal2002censusdata$do_agri)

# Tabulation of factor variable to show how many of each we now have
table(nepal2002censusdata$do_agri_fac)

# As an example, pull out a subset with rows where do_agri value is TRUE 
## and columns House_no and cow
nepal2002censusdata[nepal2002censusdata$do_agri, c("House_No", "Cow")]

# Pull out subset with rows where do_agri value is not TRUE (FALSE)
nepal2002censusdata[!nepal2002censusdata$do_agri, c("House_No", "Cow")]

# Get subset with rows where do_agri_fac factor is "Do agri"
nepal2002censusdata[nepal2002censusdata$do_agri_fac == "Do agri",
                    c("House_No", "Cow")]

# Remove all records for families that do not perform agriculture,
## ie. where do_agri is false
do_agri <- nepal2002censusdata[nepal2002censusdata$do_agri,]

# Create new variable in a column that calculates the 
## cummulative sum of area of land (Area_ha)
do_agri$ha_cum <- cumsum(do_agri$Area_ha)


# Elegant test to check and see if the sum of the land area column
## equals that of the cumulative sum column that we've just created
sum(do_agri$Area_ha) == do_agri$ha_cum[length(do_agri$ha_cum)]

# Setting new variable
sample_size <- 10

# One more variable with sample interval
sample_int <- sum(do_agri$Area_ha) / sample_size

# "Freeze" the random number generator
set.seed(123)

# Getting start point 
start_ha <- runif(n=1, min=0, max=sample_int)

# Create empty vector
do_agri$ha_cum_lower <- NA

# Put 0 in the first element of new vector
do_agri$ha_cum_lower[1] <- 0

# Put ha_cum value from from previous row to ha_cum_lower of current row
do_agri$ha_cum_lower[2:length(do_agri$ha_cum_lower)] <-
  do_agri$ha_cum[1:length(do_agri$ha_cum_lower) - 1]


# Get vector of values for sampling
sample_ha_all <- start_ha + (0:(sample_size - 1) * sample_int)

# Create first row of future sampled data.frame
do_agri_sampled <- do_agri[do_agri$ha_cum >= sample_ha_all[1] & 
                             do_agri$ha_cum_lower < sample_ha_all[1],]

# Adding other elements to sampled data.frame
for (i in sample_ha_all[2:length(sample_ha_all)]) {
  x <- do_agri[do_agri$ha_cum >= i & do_agri$ha_cum_lower < i,]
  do_agri_sampled <- rbind(do_agri_sampled, x)
}

