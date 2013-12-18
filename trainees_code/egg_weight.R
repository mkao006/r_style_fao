## This script was produced by Thomasz Filipczuk, Alex Matrunich, and Jim Maas
## at FAO, Rome on 16/12/2013

## Last edited on 18/12/2013

## This is a small script to demonstrate soem simple methods to download some
## data from the FAO working system, do some preliminary checking for outliers
## that may not be valid data.  This example downloads data on eggs produced in
## Canada to see if they total weights and numbers of eggs produced correspond
## to each other

## load necessary libraries
library(ggplot2)

## load the R file that performs the connection to the FAO Statistical Working
## System (SWS) database.  It requires the additional jarfile listed.
source("sws_query.r")
jarfile <- "ojdbc14.jar"


## Download the total egg production data, in metric tonnes, for Canada for
## years 1961-2012

canada_eggs_tonnes <- sws_query(area = 33, item= 1062, ele=51, year=1961:2012, 
                         class.path= jarfile)
       
## Download the total egg production data, in number of individual eggs, for
## Canada for years 1961-2012

canada_eggs <- sws_query(area = 33, item=1067, ele=51, year=1961:2012, 
                         class.path= jarfile)

## Divide the two values to calculate the mean weight per egg produced

canada_eggs$weight <- canada_eggs_tonnes$value / canada_eggs$value

## Change instances of either 0 or Inf in the data file to NA so that
## R will treat them as missing values

canada_eggs$weight[canada_eggs$weight==0 | canada_eggs$weight==Inf ] <- NA


## Do a quick check to see how often the calculated weight per egg was greater
## than 55 g and less than 60 g

canada_eggs$passed <- canada_eggs$weight < .06 & canada_eggs$weight > .055

## Run a couple of plots to view the data of eggs that fit within the acceptable
## weights
ggplot(canada_eggs[canada_eggs$passed, ], aes(weight)) + geom_density()
ggplot(canada_eggs[canada_eggs$passed, ], aes(weight)) + geom_bar()


## Now perform a similar analysis on all countries

## Download the total egg production data, in metric tonnes, for all countries
## for years 1961-2012

eggs_tonnes <- sws_query(1:299, item= 1062, ele=51, year=1961:2012, 
                                class.path= jarfile)

eggs <- sws_query(1:299, item=1067, ele=51, year=1961:2012, 
                         class.path= jarfile)

## Give some column names to the two individual data files
names(eggs)[5] <- 'eggs_number'
names(eggs_tonnes)[5] <- 'eggs_tonns'

## Combine the two individual data files
eggs1 <- join(eggs, eggs_tonnes, by=c('area', 'year', 'ele'))

## Calculate the egg weights again
eggs1$weight <- eggs1$eggs_tonns / eggs1$eggs_number

## Convert zeros and Inf characters to NA for missing data
eggs1$weight[eggs1$weight==0 | eggs1$weight==Inf ] <- NA


## Plot the overall data for all countries
ggplot(eggs1, aes(eggs_number, eggs_tonns)) + geom_point() +
  scale_y_continuous(limits=c(0, 7000)) + scale_x_continuous(limits=c(0,
  200000))

ggplot(eggs1, aes(eggs_number, weight)) + geom_point()
scale_y_continuous(limits=c(0, 7000)) + scale_x_continuous
(limits = c (0,200000))

## Extract the data for country/years when mean egg weight falls between 45g and
## 70g

eggs1$passed <- eggs1$weight < .07 & eggs1$weight > .045

## Run a couple of plots to view the data of eggs that fit within the acceptable
## weights, from all countries

ggplot(eggs1[eggs1$passed, ], aes(weight)) + geom_density()
ggplot(eggs[eggs$passed, ], aes(weight)) + geom_bar()

## Get list of coutries that failed the test
head(eggs1[!eggs1$passed, ])

## A plot of this data
ggplot(eggs1, aes(year, eggs_number, group=area)) + geom_line()
eggs1[eggs1$eggs_number==max(eggs1$eggs_number),]
