#' Get model and data comparisons for residual calculation.
#'
#' Get model and data comparisons for residual calculation.
#' @param params parameters to be passed to model
#' @param args parameters to be fixed throughout optimisation
#' @param data experimental data
#' @param tColStr name of matrix's column to be used as measure of time, e.g. "DAT"
#' @param ... unmodified parameters (incl. func, N0) to be passed to \code{solve_opt1D()}
#' @return table storing independent variable and the dependent variable from both model and data.
#' @export
opt_get_compModDatJoint=function(params, args=NULL, data, tColStr, aOrder=NULL,...){

    params = opt_conjugateParams(params, aOrder)

    m = solve_opt1D(
        params=params,
        args=args,
        DAT=data[,tColStr],
        tColStr=tColStr,
        ...
    )

    m = as.data.frame(m)

    #Need to convert m and d from wide to long format. Before merging.
    m = reshape(
        m, direction = "long",
        varying = list(names(m)[2:3]),
        v.names = "numTillers",
        idvar = c("DAT"),
        timevar = "Plot_Type", times = c("res", "sus"),
        new.row.names = 1:(dim(m)[1] * 2)
    )

    #Merge d and m.
    compModDat = merge(
        data[,c(tColStr, "Plot_Type", "numTillers")],
        m,
        by=c(tColStr, "Plot_Type"),
        suffixes = c(".dat", ".mod")
    )

    return(compModDat)
}
