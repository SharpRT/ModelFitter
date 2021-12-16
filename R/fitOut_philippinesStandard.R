#' Philippines - Standard Residual; Published Fit
#'
#' Code to produce model for Philippines - Standard Redisual; Published Fit
#' @param year year of experiment
#' @param season season of experiment
#' @param site site name of site where experiment was performed
#' @return model values
#' @export
fitOut_philippinesStandard = function(year, season, site, compare=F, compareA=T, residFuncName = "standard", filepath, env=NULL){
  
  #==============================================================================#
  # Fit model to epidemiological data using nls.lm (Sharp, 2017)
  #==============================================================================#
  
  library(stats4)
  
  if(is.null(env)){
    env <- new.env()
    setup_env(year, season, site, filepath=filepath, env=env)
  }
  
  fitFunc = env$fitFunc
  initParams = env$initParams
  lower = env$lower
  upper = env$upper
  tColStr = env$tColStr
  plotSum = env$plotSum
  N0 = env$N0
  args = env$args
  modelFunc = env$modelFunc
  joint = env$joint
  solved = env$solved
  aOrder = env$aOrder
  control = env$control
  residFunc = env$residFunc
  
  fitval = opt_tryFit(
    fitFunc=fitFunc,initParams=initParams,lower=lower,upper=upper,tColStr=tColStr,plotSum=plotSum,N0=N0,args=args,
    modelFunc=modelFunc, joint=joint, solved=solved,
    aOrder=aOrder,
    control=control,
    residFunc=residFunc
    ,maxRecs = 10
  )

  return(fitval)
}
