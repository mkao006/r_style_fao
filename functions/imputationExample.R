##' This function performs imputation by linear mixed model and EM-algorithm
##'
##'
##' @param formula See the formula of lme4
##' @param groupVar The grouped effect of the model, the mean are
##' computed based on this formula
##' @param countryVar The variable which defines the country, which
##' act as the conditional variable in the random effect.
##' @param data the data.frame or data.table containing the data
##' @param n.iter The number of iteration for the EM-algorithm for
##' estimating the grouped average effect.
##' @param tol The tolerance, stopping criteria for the likelihood.
##' @param EMverbose logical, whether the likelihood in the EM step
##' should be returned.
##' @param includeChange logical, whether the grouped mean should be
##' used.
##' @param allow.new.levels logical, whether observation from new
##' levels can be predicted.
##' 
##' @seealso \code{\link{FAOProductionImpute}}
##' @export


shocklme4 = function(formula, groupVar, countryVar, data,
    n.iter, tol, EMverbose = TRUE, includeChange = TRUE,
    allow.new.levels = FALSE){
    require(lme4)
    
    ## Initialization
    dataCopy = copy(data.table(data))
    ## ll = vector(mode = "numeric", length = 1 + n.iter)
    ll = rep(NA, length = 1 + n.iter)
    ll[1] = -Inf
    
    ## Extract the response variable
    y = as.character(nlme:::getResponseFormula(formula))[2]

    dataCopy[, eval(parse(text =
                          paste0("imputedValue := naiveImputation(",
                          y, ")"))), by = countryVar]
    
    ## Update the formula to include the group mean
    if(includeChange){
        formula = update(formula,
            paste0(". ~ . + (1 + Year + groupedChange|",
            countryVar, ")"))
    } else {
        formula = update(formula, paste0(". ~ . + (1 + Year|",
            countryVar, ")"))
    }

    ## Identify the location of the missing values
    dataCopy[, eval(parse(text =
                          paste0("missInd := is.na(", y, ")"))), ]
    
    ## Start EM mean imputation
    formula = update(formula, imputedValue ~ .)
    for(i in 1:n.iter){
        if(i == n.iter)
            print("maximum iteration reached, model may have not converged")
        if(includeChange){
            dataCopy[, shock := c(0, sign(diff(imputedValue))),
                     by = countryVar]
            dataCopy[, groupedChange := mean(shock),
                     by = groupVar]
        }
        
        EMMean.fit = try(
            do.call("lmer",
                    list(formula = formula,
                         data = dataCopy, REML = FALSE)
                    ),
            silent = TRUE
            )
        if(EMverbose)
            cat("Iteration", i, ":", logLik(EMMean.fit), "\n")
        

        if(is.finite(logLik(EMMean.fit))){            
            ll[i + 1] = logLik(EMMean.fit)
            if(ll[i + 1] - ll[i] > tol){
                dataCopy[missInd == TRUE,
                         imputedValue :=
                         predict(EMMean.fit, dataCopy[missInd == TRUE, ],
                                 allow.new.levels = allow.new.levels)]
                
            } else {
                break
            }
        } else {
            break
        }   
    }
    
    ## Return the model
    if(includeChange){
        list(model = update(EMMean.fit, REML = TRUE),
             groupedChange = dataCopy[, groupedChange], ll = ll)
    } else {
        list(model = update(EMMean.fit, REML = TRUE), ll = ll)
    }
}
