#' @importFrom deSolve rk
NULL

#' RK45DP7 solver for 1D ODE optimisation routine.
#'
#' RK45DP7 solver for 1D ODE optimisation routine reporting at time points determined by sampling dates.
#' Initial condition I=0.
#' Parameters to be excluded-from/fixed-in optimisation routine given in \code{args}.
#' @param params named vector of parameters
#' @param DAT experimental data to obtain sampling dates
#' @param tColStr name of matrix's column to be used as measure of time, e.g. "DAT"
#' @param N0 initial conditions of the model's state variables
#' @param args parameters to be fixed throughout optimisation
#' @param ... unmodified parameters (incl. func) to be passed to the model
#' @return solution of deterministic model at given time points
#' @export
solve_opt1D = function(params,DAT,tColStr,N0,plotArea=1,args=NULL,func,solved){

    if('theta.tau' %in% names(params)){
        theta.tau = params["theta.tau"]
    } else{
        theta.tau = args[["theta.tau"]]
    }

    if(!('tau' %in% names(params))){
        tau = args[["tau"]]
    } else{
        tau = params["tau"]
        tau = (1-theta.tau)*tau

        tau_res = tau
        tau_sus = tau
    }

    if(!('tau_res' %in% names(params))){
        tau_res = args[["tau_res"]]
    } else{
        tau_res = params["tau_res"]
    }

    if(!('tau_sus' %in% names(params))){
        tau_sus = args[["tau_sus"]]
    } else{
        tau_sus = params[["tau_sus"]]
    }

    if('tau_res' %in% names(params) & 'tau_sus' %in% names(params)){
        tau = min(tau_res,tau_sus)
    }

    t0 = unique(DAT)-tau
    t = t0[t0>=0]                   #removes -ve elements (decreases size of array)
    if(t[1]!=0) t = c(0,t)          #add an initial 0 value if not already present

    if(solved){
        m = func(t=t, params=params, args=args)
    } else{
        m = rk(N0, t, parms=params, method="rk45dp7",args=args, verbose=F, func=func) #args=arguments kind of works, but renames all params "args."param
    }

    td = unique(DAT)[match(t,t0)] #(DAT, data scale) to ensure that original values are used for later matching
    td[is.na(td)] = t[is.na(td)] + tau #if 0 was added to t, reset as tau.
    m[,1] = td #change model time to DAT time.
    if(plotArea!=1) stop("Is this code correct?")
    m[,2:dim(m)[2]] = m[,2:dim(m)[2]]*plotArea #results are for m2, am interested in 0.04m2
    colnames(m) = c(tColStr, names(N0))
    colnames(m)[colnames(m)=="I"] = "numTillers"

    return(m)
}
