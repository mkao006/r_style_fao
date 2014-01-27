## This code written by Franck Cachia and JAM at FAO, Rome on 19/12/2013

## Last edited on 24/1/2014

## This is a simple piece of code writen to compare looping methodology vs using
## "vectorized" code and "apply" functions.  There are several advantages to
## writing R code in this way including 1) it is usually faster, 2) It is easier
## to write generalizable functions that can be recycled and reused.  This
## particular example is to calculate some cost price index indices, in this
## case for North America only.


## Comparison FAO Food Price Indices and Regional CPIs ##

## set start time to measure how long the total procedure takes
start.time <- proc.time()

## load necessary libraries
library(dynlm)
library(tseries)
library(zoo)
library(timeSeries)
library(plyr)
source('functions.r')

## This function converts the date, in the input csv data file, which is in the
## format of "2000M1", which corresponds to year 2000, month 1 (January) and
## converts it to a proper R time series (ts) format of the form "Jan 2000".
## The syntax "%Y M %m" corresponds to POSIX Year with century, the placeholder
## "M", and then the POSIX month in decimal number (1-12).  So the M is only a
## placeholder
Read.date.function <- function(x) as.yearmon(x, "%Y M %m")

## Read in the csv data file.
time.series.data.zoo <- read.zoo("CPIandFPI.csv", header = TRUE,
                     check.names = FALSE, sep = ",",
                     FUN = Read.date.function)

## First find the oldest year in the series to use as an index year, this is the
## year that the values of subsequent years will be "indexed" against
index.year <- format (min (index (time.series.data.zoo)), "%Y")

## Query which rows in the zoo data object match the selected "index.year" as
## determined previously line of code
index.year.rows <- format ( index (time.series.data.zoo), "%Y") == index.year

## Now get column means of all columns of the "coredata" of the zoo object that
## correspond to the index year only, by including the rows from
## "index.year.rows" only
index.year.means <- colMeans ( time.series.data.zoo [index.year.rows, ] )

## Now to get the mean of all values for the oldest year, in this case is the
## year 2000 in this particular time series object.  This uses a plyr package
## function called "aaply"
index.values <- aaply (time.series.data.zoo, 1, "/", index.year.means) * 100

## Convert it back to a ts object with the correct indexs for years/months
Prices.ind <- ts(data = index.values, freq=12,
                 start=c(as.numeric(index.year), 1))

## This code is copied directly from FC's original

## Variables
## Endogenous and exogeneous variables
lfpi <- log(Prices.ind[,"Food Price Index"])

## dlcpi <- diff.ts(lcpi)
dlfpi <- diff.ts(lfpi)

## North-America
nam.lcpi <- log.cpi(Prices.ind, "Northern America")
eq.lt.NAM <- long.lm(ts.endo=nam.lcpi, ts.exo=lfpi, F)

## Short-Term relationship
nam.dlcpi <- diff.ts(nam.lcpi)
plot.ts(nam.dlcpi,ylim=c(-0.15,0.1))
lines(dlfpi,col="red")

eq.st.NAM <- dynlm(nam.dlcpi~L(nam.dlcpi,c(5))+L(dlfpi,c(7))+
                   L(residuals(eq.lt.NAM),1))
summary(eq.st.NAM)

res.st.NAM <- residuals(eq.st.NAM)
plot.ts(res.st.NAM)
plot(cbind(density(res.st.NAM)$x,density(res.st.NAM)$y))

## Forecasting
## Static
nam.dlcpi_s <- fitted(eq.st.NAM)

## Dynamic

## All identical to this stage, have the correct objects and values
nam.lcpi_d <- nam.lcpi

## Now this is an attempt to replace the looping structure, with vectorized
## apply statements to improve speed.  The challenge is getting the indexing
## correct

## Create the lfpi [2*156] matrix required for last line of FC's loop
lfpi.matrix <- rbind (rep(1,156), lfpi[2:157])

## Do the calculations to produce the vector of values relative to lfpi and
## using the eq.lt.NAM coefficients from earlier regression
vector4 <- coefficients(eq.lt.NAM) %*% lfpi.matrix

## Produce the entire coeficients matrix to multiply by the eq.st.NAM
## coefficients

coefs.matrix <- rbind (rep(1,156),
                       c( diff(nam.lcpi_d, 1), 0),
                       c(dlfpi[8:length(dlfpi)], rep(0, 156 - (length(dlfpi) - 7))),
                       vector4
                       )

## Produce the final vector of values from coefficients of eq.st.NAM 
vector5 <- coefficients(eq.st.NAM) %*% coefs.matrix

vector6 <- c(nam.lcpi_d[2:156], 0) + vector5

## offset by 13 values as was done in the original loop
nam.lcpi_d <- c(nam.lcpi_d[1:13], vector6[14:156])

## create the ts object of final values.
nam.lcpi_d <- ts(data = nam.lcpi_d, freq=12,
                 start=c(as.numeric(index.year), 1))

## check the time to see how long it took
total.time <- proc.time() - start.time 

total.time
