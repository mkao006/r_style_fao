## This code produced by JAM at Norwich on 20/12/2013

## Last edited on 13/1/2014

##This little script uses rollapply (zoo) to do weighted means, and will deal
## with missing values (NA).

library(zoo)

## First create a small example data set, this particular set is complete with
## no missing (NA) values

input.data1 <- data.frame(
  Country = rep(c(1, 5000), each = 10),
  Year = factor(rep(1990:1999, 2)),
  Values = sample(x =  1:20, size = 20, replace = TRUE),
  Weights = sample(x = seq(0,50,10), size = 20, replace =TRUE)
)

## time-series zoo object split up by Countries
input.data1.zoo <- read.zoo(input.data1, index = "Year", split = "Country",
  FUN = identity)

## ========================
## Main function
## lapply across countries, then rollapply across years

## first, get uniqe list of countries contained in the original dataset
cntry <- unique(input.data1$Country)

weighted.means <- lapply(cntry, function(x) rollapply (
  data = input.data1.zoo[ , paste(c ("Values", "Weights"), x, sep = ".")],
  width = 3, by.column = FALSE,
  FUN = function(y) weighted.mean(y[, 1], y[, 2], na.rm = TRUE) ) )

names(weighted.means) <- cntry

## merge into one series
final.weighted.means <- do.call("merge", weighted.means)

## do a manual check to see if calculations are producing what we think they
## are!

weighted.mean.check5 <- weighted.mean(input.data1.zoo$Values.1[5 + 0:3],
                                      input.data1.zoo$Weights.1[5 + 0:3],
                                      na.rm = TRUE)

## build a generic function out of this code
weight.mean.jam1 <- function (cntry, input.data, mean.width) {
    results <- lapply(cntry, function(x) rollapply (
        data = input.data[ , paste(c ("Values", "Weights"), x, sep = ".")],
        width = mean.width, by.column = FALSE,
        FUN = function(y) weighted.mean(y[, 1], y[, 2], na.rm = TRUE) ) )
    names(results) <- cntry
    final.results <- do.call("merge", results)
}

## Trial of function with mean width = 3
trial.results1 <- weight.mean.jam1 (cntry, input.data1.zoo, 3)
trial.results1

## Another trial of function, this time with mean width = 4
trial.results2 <- weight.mean.jam1 (cntry, input.data1.zoo, 4)
trial.results2

## Thus it appears to work fine when there are no missing data so now create
## another data set containing a few missing data as random NA and see if it
## continues to work.  In this case the NA for Value does not correlate with a
## NA for the corresponding Weight

input.data2 <- data.frame (
    Country = rep(c(1, 5000), each = 10),
    Year = factor(rep(1990:1999, 2)),
    Values = rep(c(1:6,NA,8:10), 2),
    Weights = rep(c(NA,40,30,50,10,20,40,30,50,10), 2) )

input.data2.zoo <- read.zoo(input.data2, index = "Year", split = "Country",
  FUN = identity)

cntry2 <- unique(input.data2$Country)

## Trial of function with mean width = 3
trial.results3 <- weight.mean.jam1 (cntry2, input.data2.zoo, 3)
trial.results3

## Another trial of function, this time with mean width = 4
trial.results4 <- weight.mean.jam1 (cntry2, input.data2.zoo, 4)
trial.results4

## This does not work and produces outputs of NA.  Therefore create another data
## set where the NA values for Value correspond to a NA for the appropriate
## Weight.

input.data3 <- data.frame (
    Country = rep(c(1, 5000), each = 10),
    Year = factor(rep(1990:1999, 2)),
    Values = c(8,10,15,17,12,11,19,NA,8,7,
               12,14,11,7,3,NA,12,10,12,14),
    Weights = c(40,30,20,40,30,20,10,NA,50,40,
               20,40,10,30,30,NA,10,40,20,10))

input.data3.zoo <- read.zoo(input.data3, index = "Year", split = "Country",
  FUN = identity)

cntry3 <- unique(input.data3$Country)

## Trial of function with mean width = 3
trial.results5 <- weight.mean.jam1 (cntry3, input.data3.zoo, 3)
trial.results5

## Another trial of function, this time with mean width = 4
trial.results6 <- weight.mean.jam1 (cntry3, input.data3.zoo, 4)
trial.results6

## It appears that this works out so a routine must be built in such that NA's
## correspond in both Values and Weights
