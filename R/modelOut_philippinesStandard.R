#' Philippines - Standard Residual; Published Fit
#'
#' Code to produce model for Philippines - Standard Redisual; Published Fit
#' @param year year of experiment
#' @param season season of experiment
#' @param site site name of site where experiment was performed
#' @return model values
#' @export
modelOut_philippinesStandard = function(year, season, site, compare=F, compareA=T, residFuncName = "standard", filepath){

    #==============================================================================#
    # Fit model to epidemiological data using nls.lm (Sharp, 2017)
    #==============================================================================#

    library(stats4)

    env <- new.env()
    setup_env(year, season, site, compare=F, compareA=T, residFuncName = "standard", filepath, env)
    
    fitval = fitOut_philippinesStandard(year, season, site, filepath=filepath, env=env)
    
    fitFunc = env$fitFunc
    plotSum = env$plotSum
    tColStr = env$tColStr
    modelFunc = env$modelFunc
    args = env$args
    N0 = env$N0
    solved = env$solved
    
    optParams=opt_get_fitFuncParams(fitFunc,fitval)
    rk45dp7 = solve_opt1D(
        params=optParams,
        DAT=plotSum[,tColStr],
        tColStr=tColStr,
        func=modelFunc,
        args=args,
        N0=N0, solved=solved
    )
    return(rk45dp7)
}
