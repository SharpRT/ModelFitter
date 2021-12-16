#' Run chosen fitting routine
#'
#' Run chosen fitting routine.
#' @param fitFunc the function to use for optimisation
#' @param ... unmodified parameters to be passed to the chosen fitting routine.
#' @return the object returned from the fitting routine
#' @export
opt_run_fitFunc = function(fitFunc, modelFunc=NULL, ...){
    if(identical(fitFunc,stats:::nls)){
        return(opt_paramsNLS(modelFunc=modelFunc, ...))
    } else{
        stop("Method not implemented.")
    }
}

#' Set and return a fitting routine's control object
#'
#' Set and return a fitting routine's control object.
#' @param fitFunc the function to use for optimisation
#' @param ... unmodified parameters to be passed to the fitting routine's control function
#' @return a fitting routine's control object
#' @export
opt_get_control = function(fitFunc, ...){
    if(identical(fitFunc,stats:::nls)){
        return(nls.control(..., warnOnly = T))
    } else{
        stop("Method not implemented.")
    }
}

#' Get the optimised parameters from the fitting routine's output
#'
#' Get the optimised parameters from the fitting routine's output.
#' @param fitFunc the function to use for optimisation
#' @param fitval the object returned from the fitting routine
#' @export
opt_get_fitFuncParams = function(fitFunc, fitval){
    if(identical(fitFunc,stats:::nls)){
        return(fitval$m$getAllPars())
    } else{
        stop("Method not implemented.")
    }
}

#' Determine in the fitting routine successfully converged
#'
#' Determine in the fitting routine successfully converged.
#' @param fitFunc the function to use for optimisation
#' @param fitval the object returned from the fitting routine
#' @return boolean, true if routine converged
#' @export
opt_is_converged = function(fitFunc, fitval){
    if(identical(fitFunc,stats:::optim)){
        return(fitval$convergence==0)
    } else if(identical(fitFunc,stats:::nls)){
        return(fitval$convInfo$isConv)
    } else if(identical(fitFunc,minpack.lm:::nls.lm)){
        return(fitval$info %in% c(1,2,3,4))
    } else if(identical(fitFunc,stats4:::mle)){
        return(fitval@details$convergence==0)
    } else{
        stop("Method not implemented.")
    }
}
