#' Performs non-linear least squares parameter optimisation, dealing with common convergence errors.
#'
#' Performs non-linear least squares parameter optimisation, dealing with common convergence errors.
#' @param fitFunc optimisation routine to use
#' @param initParams initial guess parameter values for optimiser to improve upon
#' @param control list of control parameters to be passed to nls
#' @param recCount the number of recursive calls of this function made so far
#' @param maxRecs a limit on the number of recursive calls that can be made
#' @param ... unmodified parameters (incl. isNonDim, lower, upper, tColStr, plotSum, N0, modelFuncSus, modelFuncRes, args(optional) and residFunc) to be passed to \code{opt_paramsNLS()}
#' @return non-linear least squares (nls) object
#' @export
opt_tryFit = function(fitFunc, initParams, control, recCount=0, maxRecs=5, ...){
    print(paste0("Attempt ",recCount," ------------------------------------------"))
    fitval = opt_run_fitFunc(fitFunc=fitFunc,initParams=initParams, control=control, ...)

    if(!opt_is_converged(fitFunc=fitFunc, fitval=fitval) && recCount<maxRecs){
        if(opt_is_stop(fitFunc=fitFunc, fitval=fitval)){
            return(fitval)
        } else if(opt_is_maxIterError(fitFunc=fitFunc, fitval=fitval)){
            return(opt_tryFit(
                fitFunc=fitFunc,
                initParams=initParams,
                control=opt_set_doubleMaxIter(fitFunc=fitFunc, control=control),
                recCount=recCount+1,
                maxRecs = maxRecs,
                ...
            ))
        } else if(opt_is_retryable(fitFunc=fitFunc, fitval=fitval)){
            return(opt_tryFit(
                fitFunc=fitFunc,
                initParams = fitval$m$getAllPars(),
                control=control,
                recCount=recCount+1,
                maxRecs = maxRecs,
                ...
            ))

        } else{
            print("unimplemented case")
            return(fitval)
        }
    } else{
        if(recCount==maxRecs) warning("max number of recursive calls reached")
        return(fitval)
    }
}
