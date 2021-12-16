#' Set up parameters for fitOut and modelOut routines.
#'
#' Set up parameters for fitOut and modelOut routines.
#' @param year year of experiment
#' @param season season of experiment
#' @param site site name of site where experiment was performed
#' @return model values
#' @export
setup_env = function(year, season, site, compare=F, compareA=T, residFuncName = "standard", filepath, env){

  #LOAD DATA----------------------------------------------------------------------------------------------------
  
  #Specify which dataset to work with.]
  country = "Philippines"#"Thailand"
  experiment = "Field"
  tColStr = "DAT" #Specify which measure of time to use. #DAT recommended.
  plotSum = calc_sumPlotWeek(country=country, experiment=experiment, site=site, year=year, season=season, sumVar="numTillers", filepath=filepath) #Load the data
  #If you get an error message re. a corrupt file here, it may be because R has not been restarted since installing the package.
  
  plotSum=plotSum[!is.na(plotSum$numTillers),] #Remove NA values.
  plotSum = plotSum[plotSum$type=="I",] #Analyse just the infected count data.
  
  key = paste0(year, "_", season, "_", site)
  
  #Remove certain data from model fit (outliers).
  if(any(key==c(#------------------------------rmLastWeek
    "2016_Wet_Pila",
    "2016_Wet_Victoria",
    "2016_Wet_Malayantoc",
    "2017_Wet_Pila",
    "2017_Wet_Victoria",
    "2017_Wet_Maligaya",
    "2018_Dry_Maligaya",
    "2018_Wet_Victoria"
  ))){
    plotSum = plotSum[plotSum$Week!=max(plotSum$Week),]
  } else if(any(key==c(#--------------------------rmWeek4
    "2017_Dry_Maligaya"
  ))){
    plotSum = plotSum[plotSum$Week!=4,]
  } else if(any(key==c(#---------------------rmLast2Weeks
    "2018_Wet_Malayantoc"
  ))){
    plotSum = plotSum[plotSum$Week!=max(plotSum$Week),]
    plotSum = plotSum[plotSum$Week!=max(plotSum$Week),]
  } else if(any(key==c(#---------------------rmLast3Weeks
    "2018_Wet_Maligaya"
  ))){
    plotSum = plotSum[plotSum$Week!=max(plotSum$Week),]
    plotSum = plotSum[plotSum$Week!=max(plotSum$Week),]
    plotSum = plotSum[plotSum$Week!=max(plotSum$Week),]
  } else if(any(key==c(#--------------------rmFirst3Weeks
    "2018_Dry_Malayantoc"
  ))){
    plotSum = plotSum[plotSum$Week!=min(plotSum$Week),]
    plotSum = plotSum[plotSum$Week!=min(plotSum$Week),]
    plotSum = plotSum[plotSum$Week!=min(plotSum$Week),]
  }

  #FIT MODEL----------------------------------------------------------------------------------------------------
  
  #Set initial values.
  init.eps = 0.1
  init.a = init.eps*max(plotSum$numTillers,na.rm=T)
  init.tau = min(plotSum[,tColStr])
  init.r = 0.09
  
  eps = data.frame(row.names = "eps", initl = init.eps, lower = 0, upper = Inf)
  tau = data.frame(row.names = "tau", initl = init.tau, lower = 0, upper = init.tau)
  aS = data.frame(row.names = "a_sus", initl = init.a, lower = 0, upper = Inf)
  rS = data.frame(row.names = "r_sus", initl = init.r, lower = 0, upper = Inf)
  
  if(compare){ #for comparing differences between parameters
    splitType = "Plot_Type"
    aOrder=vector("list",length(unique(plotSum[,splitType])))
    names(aOrder) = unique(plotSum[,splitType])
    if(compareA){
      rR = data.frame(row.names = "r_RS", initl = init.r, lower = 0, upper = Inf) #standard
      aR = data.frame(row.names = "a_res", initl = init.a, lower = -Inf, upper = Inf) #modded
      aOrder[[1]] = c("a_sus", "a_res")
      aOrder[[2]] = c("a_sus")
    } else{ #compareR
      aR = data.frame(row.names = "a_res", initl = init.a, lower = 0, upper = Inf) #standard
      rR = data.frame(row.names = "r_RS", initl = init.r, lower = -Inf, upper = Inf) #modded
      aOrder[[1]] = c("r_sus", "r_RS")
      aOrder[[2]] = c("r_sus")
    }
  } else{
    aR = data.frame(row.names = "a_res", initl = init.a, lower = 0, upper = Inf)
    rR = data.frame(row.names = "r_RS", initl = init.r, lower = 0, upper = Inf)
    
    #what happens when null? model runs without amalgamation of parameters.
    splitType = NULL
    aOrder = NULL
  }
  
  #-------------------------------------Fitval
  
  solved=F
  if(solved){
    modelFunc=model_expJoint2_solved
    N0 = c(I_R=0, I_S=0) #THE SOLVED EQUATIONS ASSUME I_R(0), I_S(0) = 0
  } else{
    modelFunc=model_resExpJoint2
    N0 = c(I_R=0, I_S=0) #this needs to both be 0
  }
  joint = T
  fitFunc = nls
  control = opt_get_control(fitFunc=fitFunc, maxiter=100)
  
  residFuncStr = paste0("opt_residFunc_",residFuncName)
  residFunc = get(residFuncStr)
  
  if(residFuncName=="standard"){
    if(any(key==c(#---------------------------------------------------Full
      "2016_Wet_Malayantoc", #fullWorks, allPars used
      "2018_Dry_Victoria", #fullWorks, allPars used
      "2018_Dry_Maligaya", #fullWorks, allPars used
      "2018_Wet_Malayantoc" #fullWorks, allPars used
    ))){
      params=rbind(aR, aS, eps, tau, rR, rS)
      args = list(theta.a=0, theta.r=0, theta.eps=0, theta.tau=0)
      modelName = "fitval2A"
    } else if(any(key==c(#-------------------------------------------noEps
      "2017_Wet_Maligaya" #full->eps=0
      ,"2016_Wet_Victoria" #noFull, noEps best(SS&SE)
      ,"2018_Dry_Malayantoc" #noFull, noEps best(SS&SE)
    ))){
      params=rbind(aR, aS, tau, rR, rS)
      args = list(theta.a=0, theta.r=0, theta.eps=0, theta.tau=0, eps=0, eps_res=0, eps_sus=0)
      modelName = "fitval2Alin"
    } else if(any(key==c(#-------------------------------------------noRRS
      "2017_Dry_Malayantoc", #full->rRS=0
      "2017_Dry_Maligaya", #full->rRS=0
      "2018_Wet_Victoria" #full->rRS=0
    ))){
      params=rbind(aR, aS, eps, tau, rS)
      args = list(theta.a=0, theta.r=0, theta.eps=0, theta.tau=0, r_RS=0)
      modelName = "fitval2A-rRS"
    } else if(any(key==c(#--------------------------------------------noRS
      "2017_Wet_Pila" #full->rS=0
    ))){
      params=rbind(aR, aS, eps, tau, rR)
      args = list(theta.a=0, theta.r=0, theta.eps=0, theta.tau=0, r_sus=0)
      modelName = "fitval2A-rS"
    } else if(any(key==c(#--------------------------------------------noAR
    ))){
      params=rbind(aS, eps, tau, rR, rS)
      args = list(theta.a=0, theta.r=0, theta.eps=0, theta.tau=0, a_res=0)
      modelName = "fitval2A-aR"
    } else if(any(key==c(#-------------------------------------------noTau
    ))){
      params=rbind(aR, aS, eps, rR, rS)
      args = list(theta.a=0, theta.r=0, theta.eps=0, theta.tau=0, tau=0, tau_sus=0, tau_res=0)
      modelName = "fitval2A-tau"
    } else if(any(key==c(#----------------------------------------noEpsTau
      "2016_Wet_Maligaya" #full->eps,tau=0
      ,"2016_Wet_Pila" #noFull, noEps best->tau=0(SS&SE)
      ,"2017_Wet_Malayantoc" #noFull, noEps best(SS), noTau->noEps(best SE)
    ))){
      params=rbind(aR, aS, rR, rS)
      args = list(theta.a=0, theta.r=0, theta.eps=0, theta.tau=0, eps=0, tau=0, eps_res=0, eps_sus=0, tau_sus=0, tau_res=0)
      modelName = "fitval2Alin-tau"
    } else if(any(key==c(#----------------------------------------noRRSEps
      "2018_Wet_Pila" #noFull, noEps best(SS), noRRS->eps=0(best SE)
    ))){
      params=rbind(aR, aS, tau, rS)
      args = list(theta.a=0, theta.r=0, theta.eps=0, theta.tau=0, eps=0, r_RS=0, eps_res=0, eps_sus=0)
      modelName = "fitval2Alin-rRS"
    } else if(any(key==c(#--------------------------------------noTauEpsAR
      "2018_Dry_Pila" #noFull, noRS best(SS), noTau->eps,aR=0(best SE)
    ))){
      params=rbind(aS, rR, rS)
      args = list(theta.a=0, theta.r=0, theta.eps=0, theta.tau=0, eps=0, eps_res=0, eps_sus=0, a_res=0, tau=0, tau_sus=0, tau_res=0)
      modelName = "fitval2Alin-aR-tau"
    } else if(any(key==c(#--------------------------------------noRRSEpsRS(Linear)
      "2018_Wet_Maligaya" #noFull, noRS best(SS), noRRS->eps,rS=0(best SE)
    ))){
      params=rbind(aR, aS, tau)
      args = list(theta.a=0, theta.r=0, theta.eps=0, theta.tau=0, eps=0, r_RS=0, r_sus=0, eps_res=0, eps_sus=0)
      modelName = "fitval2Alin-rS-rRS"
    } else if(any(key==c(#---------------------------------------noRSEpsAR
      "2017_Wet_Victoria" #noFull, noAR best(SS), noRS->eps,aR=0(best SE)
    ))){
      params=rbind(aS, tau, rR)
      args = list(theta.a=0, theta.r=0, theta.eps=0, theta.tau=0, eps=0, a_res=0, r_sus=0, eps_res=0, eps_sus=0)
      modelName = "fitval2Alin"
    } else{#---------------------------------------------------------ERROR
      stop("NO MATCH")
    }
  } else if(residFuncName=="log"){
    if(any(key==c(
      "2018_Dry_Victoria"
    ))){
      params=rbind(aR, aS, tau, rR, rS)
      args = list(theta.a=0, theta.r=0, theta.eps=0, theta.tau=0, eps=0)
      modelName = "fitval2Alin"
    } else{
      stop("NO MATCH")
    }
  }
  
  initParams = t(params["initl"])[1,]
  lower = t(params["lower"])[1,]
  upper = t(params["upper"])[1,]
  
  env$fitFunc=fitFunc
  env$initParams=initParams
  env$lower=lower
  env$upper=upper
  env$tColStr=tColStr
  env$plotSum=plotSum
  env$N0=N0
  env$args=args
  env$aOrder=aOrder
  env$control=control
  env$residFunc=residFunc
  env$modelFunc=modelFunc
  env$joint=joint
  env$solved=solved
  
}