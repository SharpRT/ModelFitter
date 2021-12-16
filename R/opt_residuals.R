#' Calculate residuals
#'
#' Calculates residuals between model and data.
#' @param params parameters to be passed to model
#' @param args parameters to be fixed throughout optimisation
#' @param isNonDim boolean to determine whether data requires nondimensionalisation
#' @param plotSum experimental data
#' @param modelFuncRes function containing the resistant variety model to be solved
#' @param modelFuncSus function containing the susceptible variety model to be solved
#' @param defaultResidual a large default sum of squares to be used when [?]
#' @param tColStr name of matrix's column to be used as measure of time, e.g. "DAT"
#' @param ... unmodified parameters (incl. tColStr, N0, args(optional), plotArea and residFunc) to be passed to \code{opt_get_compModDat()}
#' @return sum of squares
#' @export
opt_residuals = function(
    params, args=NULL, isNonDim=F, plotSum, modelFuncRes=NULL, modelFuncSus=NULL, modelFunc=NULL, joint,
    defaultResidual=.Machine$double.xmax, residFunc=opt_residFunc_standard, ...
){

    if(is.null(residFunc)) residFunc=opt_residFunc_standard
    if(is.matrix(params)) params = params[1,]

    if(isNonDim){
        plotSum=opt_nonDimPlotSum(plotSum=plotSum, modelFuncRes=modelFuncRes, modelFuncSus=modelFuncSus, tColStr=tColStr, params=params, args=args)
    }

    if(!is.null(modelFuncRes)){
        #split the data according to plot type.
        plotSumFiltered = plotSum[grepl("res", plotSum$Plot_Type),]
        compModDat = opt_get_compModDat(
            params=params, args=args,
            data=plotSumFiltered,
            func=modelFuncRes, ...
        )

        if(!is.null(modelFuncSus)){
            compModDat_Res = compModDat
        }
    }

    if(!is.null(modelFuncSus)){
        #split the data according to plot type.
        plotSumFiltered = plotSum[grepl("sus", plotSum$Plot_Type),]
        compModDat = opt_get_compModDat(
            params=params, args=args,
            data=plotSumFiltered,
            func=modelFuncSus, ...
        )

        if(!is.null(modelFuncRes)){
            compModDat = rbind(compModDat_Res,compModDat)
            plotSumFiltered = plotSum
        }
    }

    if(!is.null(modelFunc)){
        #leave the data be and calc compModDat immediately
        plotSumFiltered = plotSum
        if(joint){
            compModDat = opt_get_compModDatJoint(
                params=params, args=args,
                data=plotSumFiltered,
                func=modelFunc, ...
            )
        } else{
            compModDat = opt_get_compModDat(
                params=params, args=args,
                data=plotSumFiltered,
                func=modelFunc, ...
            )
        }
    }

    invalidParam=F
    if(isNonDim){
        compModDat=opt_dimCompModDat(
            compModDat=compModDat, params=params, args=args, modelFuncRes=modelFuncRes, modelFuncSus=modelFuncSus, tColStr=tColStr
        )
        if(!is.null(modelFuncRes)) invalidParam = params["iR_dim"]==0
    }

    # Evaluate predicted vs experimental residual
    # nls.lm requires a function that returns a vector of residuals
    # i.e. r_i = data_i-model_i, that nls will then square and sum.
    if(dim(compModDat)[1]<dim(plotSumFiltered)[1] || invalidParam){
        warning(paste0("Returning defaultResidual (",defaultResidual,")"))
        return(defaultResidual)
    } else{
        return(residFunc(compModDat))
    }
}
