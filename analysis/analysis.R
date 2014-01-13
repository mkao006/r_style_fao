########################################################################
## Title: Imputation example
## Date: 2013-11-13
## Notes: This is only an example script, it does not RUN!
########################################################################

## Load library and source scripts
library(data.table)
library(reshape2)
library(FAOSTAT)
library(lme4)
source("../codes/naiveImputation.R")
source("../codes/computeYield.R")
source("../swsToDataFrame.R")
source("../swsRmImputation.R")
source("../toLowerCamel.R")
source("../swsToImputationDataTable.R")
source("../codes/swsImputation.R")
source("../codes/meanlme4.R")
source("../splitNACountry.R")
source("../codes/impDiag.R")
source("../codes/impFit.R")
source("../codes/shocklme4.R")
source("../codes/predict.shocklme4.R")

## Data manipulation
grape.dt = swsToImputationDataTable(file = "grapeSUA.csv",
    denormalizer = "Element.Code")
grapeSub.dt = grape.dt[Year %in% 1994:2012, ]

## Imputation
grapeImputed.lst = swsImputation(grapeSub.dt, area = "areaNum",
    prod = "productionNum", yield = "yieldNum",
    country = "areaName", region = "unsdSubReg",
    year = "Year", tol = 1e-3, EMverbose = TRUE,
    meanType = "shocklme4")

## Examine the imputation
impDiag(object = grapeImputed.lst, yieldObsVar = "yieldNum",
        countryVar = "areaName", savePlots = FALSE)

## Examine the fit
impFit(grapeImputed.lst, productionObsVar = "productionNum",
       areaObsVar = "areaNum", yieldObsVar = "yieldNum",
       countryVar = "areaCode", itemCode = "itemCode",
       file = "grapeImputationFit.pdf")

## Simulation
n.sim = 2000
sim.df = data.frame(propSim = runif(n.sim, 0.05, 1 - 1e-05),
    propReal = rep(NA, n.sim), MAPE = rep(NA, n.sim),
    coverage = rep(NA, n.sim))

## pdf(file = "checkImputation.pdf")
for(i in 1:n.sim){
  print(paste0("Simulation Number: ", i))
  tmp.dt = grapeSub.dt
  prop = sim.df[i, "propSim"]
  ## set.seed(587)
  simMissArea = sample(which(tmp.dt$areaSymb %in% c(" ", "*")),
    length(which(tmp.dt$areaSymb %in% c(" ", "*"))) * prop)
  tmp.dt[, simArea := areaNum]
  tmp.dt[simMissArea, "simArea"] = NA
  ## set.seed(587)
  simMissProd = sample(which(tmp.dt$productionSymb %in% c(" ", "*")),
    length(which(tmp.dt$productionSymb %in% c(" ", "*"))) * prop)
  tmp.dt[, simProd := productionNum]
  tmp.dt[simMissProd, "simProd"] = NA 
  tmp.dt[, simYield := computeYield(simProd, simArea)]
  sim.df[i, "propReal"] = tmp.dt[, sum(is.na(simYield))/length(simYield)]
  impSim.dt = try(swsImputation(data = tmp.dt, area = "simArea",
    prod = "simProd", yield = "simYield", country = "areaCode",
    region = "unsdSubReg", year = "Year", tol = 1e-3,
      EMverbose = FALSE, meanType = "shocklme4")$imputed)
  if(!inherits(impSim.dt, "try-error")){
    ## Check the MAPE of the imputation
    sim.df[i, "MAPE"] = 
      impSim.dt[1:nrow(impSim.dt) %in% simMissProd &
                productionNum != 0 & !is.na(productionNum) &
                !is.na(imputedProd),
                sum(abs((productionNum - imputedProd)/
                        (productionNum)))/length(imputedProd)]
    sim.df[i, "coverage"] =
        NROW(impSim.dt[1:nrow(impSim.dt) %in% simMissProd &
                       productionNum != 0 & !is.na(productionNum) &
                       !is.na(imputedProd)])/length(simMissProd)
  } else {
    sim.df[i, "MAPE"] = NA
  }
}


## Melt and change colnames
meltgrapeSim.df =
    melt(sim.df[, c("propReal", "MAPE", "coverage")],
         id.vars = "propReal")
meltgrapeSim.df$variable = ifelse(meltgrapeSim.df$variable == "MAPE",
    "Mean Absolute Percentage Error (%)", "Imputation Coverage Rate (%)")

## Plot simulation results
xyplot(value * 100 ~ propReal|variable, data = meltgrapeSim.df,
       panel = function(x, y){
           panel.grid(h = 5)           
           panel.points(x, y, cex = 0.5, pch = 19,
                        col = rgb(red = 70, green = 130, blue = 180,
                            maxColorValue = 255, alpha = 100))
           panel.loess(x, y, col = "darkblue", lwd = 2.5,
                       span = 0.2)
       }, layout = c(1, 2), xlab = "Proportion of Missing Values",
       ylab = "", main = "Simulation Result for Grape (1994-2011)",
       xlim = c(0, 1), ylim = c(0, 100))
