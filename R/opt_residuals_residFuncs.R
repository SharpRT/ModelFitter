#' Calculate standard residuals, d-m
#'
#' Calculate standard residuals, d-m
#' @param compModDat table storing independent variable and the dependent variable from both model and data.
#' @return standard residuals, d-m
#' @export
opt_residFunc_standard = function(compModDat){

    d=compModDat$numTillers.dat
    m=compModDat$numTillers.mod

    return(d-m)
}