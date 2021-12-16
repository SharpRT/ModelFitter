#' @importFrom stats nls
NULL

#' Performs non-linear least squares parameter optimisation.
#'
#' Performs non-linear least squares parameter optimisation.
#' @param initParams initial guess parameter values for optimiser to improve upon
#' @param plotSum target experimental data
#' @param tColStr name of matrix's column to be used as measure of time, e.g. "DAT"
#' @param lower lower bound of parameter values for optimiser to search
#' @param upper upper bound of parameter values for optimiser to search
#' @param modelFuncRes function containing the resistant variety model to be solved
#' @param modelFuncSus function containing the susceptible variety model to be solved
#' @param control nls control object
#' @param ... unmodified parameters (incl. isNonDim, N0, args(optional), plotArea and residFunc) to be passed to \code{opt_residuals()}
#' @return non-linear least squares (nls) object
#' @export
opt_paramsNLS = function(
    initParams, lower=NULL, upper=NULL,
    plotSum, tColStr,
    modelFunc=NULL,
    modelFuncRes=NULL, modelFuncSus=NULL, control=nls.control(), algorithm="port",
    ...
){
    print(control)

    #split the data according to plot type.
    if(!is.null(modelFuncRes) && is.null(modelFuncSus)){
        plotSum = plotSum[grepl("res", plotSum$Plot_Type),]
    } else if(!is.null(modelFuncSus) && is.null(modelFuncRes)){
        plotSum = plotSum[grepl("sus", plotSum$Plot_Type),]
    }

    # Calculate Sum of Squares (SS) of model produced with initParams.
    residuals = opt_residuals(
        params=initParams, plotSum=plotSum, tColStr=tColStr,
        modelFuncRes=modelFuncRes, modelFuncSus=modelFuncSus, modelFunc=modelFunc,
        ...
    )
    # fitFunc=fitFunc,
    # lower=lower,upper=upper,
    # control=control,
    initSS = sum((residuals)^2,na.rm = T)

    #Wrapper to make nls call compatible with residFunc.
    residWrapper = function(numTillers, DAT, Plot_Type, Plot_f, site, Line, params){
        return(opt_residuals(
            plotSum=data.frame(numTillers, DAT, Plot_Type, Plot_f, site, Line),
            params=params, tColStr=tColStr,
            modelFuncRes=modelFuncRes, modelFuncSus=modelFuncSus, modelFunc=modelFunc,
            ...
        ))
    }

    #To generalise nls code to accept different parameter sets.
    formulaString=paste0(
        "~residWrapper(",
        "numTillers=numTillers,",
        "DAT=", tColStr, ",",
        "Plot_Type=Plot_Type,",
        "Plot_f=Plot_f,",
        "site=site,",
        "Line=Line,",
        "params=cbind(",
            paste0(names(initParams),collapse=", "),
        ")[1,])"
    )

    fitval = nls(
        formula=as.formula(formulaString),
        start=initParams,
        lower=lower,
        upper=upper,
        data = plotSum,
        control = control,
        trace=T
        , algorithm = algorithm
    )

    # Determine relative improvement compared to initParams (finalSS/initSS<1 = improvement)
    finalSS = fitval$m$deviance()
    print(paste0("SE = ", summary(fitval)$sigma,"; SS = ",finalSS,"; finalSS/initSS = ",finalSS/initSS))

    # Return the results of the fitting routine.
    return(fitval)
}
