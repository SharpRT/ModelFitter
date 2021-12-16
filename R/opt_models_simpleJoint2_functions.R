#' Deterministic 1D ODE model of infection of resistant variety with joint term.
#'
#' Deterministic model of form \deqn{\frac{dI_R}{dt} = a \exp(-\epsilon t) + r_SR I_S}{dI_R/dt = a exp(-eps t) + r_SR I_S}
#' This model is designed to be run before \code{model_susExpJoint}
#' @param t time vector
#' @param N.vector initial conditions
#' @param params named vector of parameters (a; eps)
#' @param args parameters to be fixed throughout optimisation
#' @return values of state variables of deterministic model at given time points
#' @export
model_resExpJoint2 = function(t, N.vector, params, args=NULL){

    if(!('a_sus' %in% names(params))){
        a_sus = args[["a_sus"]]
    }

    if(!('a_res' %in% names(params))){
        a_res = args[["a_res"]]
    }

    if(!('r_RS' %in% names(params))){
        r_RS = args[["r_RS"]]
    }

    if(!('r_sus' %in% names(params))){
        r_sus = args[["r_sus"]]
    }

    if(!('eps_res' %in% names(params))){
        eps_res = args[["eps_res"]]
    }

    if(!('eps_sus' %in% names(params))){
        eps_sus = args[["eps_sus"]]
    }

    if(!('eps' %in% names(params))){
        eps = args[["eps"]]
    } else{
        eps_res = params[["eps"]]
        eps_sus = params[["eps"]]
    }

    if(!('tau_res' %in% names(params))){
        tau_res = args[["tau_res"]]
    }

    if(!('tau_sus' %in% names(params))){
        tau_sus = args[["tau_sus"]]
    }

    if(!('tau' %in% names(params))){
        tau = args[["tau"]]
    } else{
        tau_res = params[["tau"]]
        tau_sus = params[["tau"]]
    }

    if(!('theta.a' %in% names(params))){
        theta.a = args[["theta.a"]]
    }

    if(!('theta.eps' %in% names(params))){
        theta.eps = args[["theta.eps"]]
    }

    if(!('theta.r' %in% names(params))){
        theta.r = args[["theta.r"]]
    }

    if(!('theta.tau' %in% names(params))){
        theta.tau = args[["theta.tau"]]
    }

    if(!('phi.r' %in% names(params))){
        phi.r = args[["phi.r"]]
    }

    with(as.list(c(params,N.vector)),{

        if(theta.a>0 || theta.eps>0 || theta.r>0 || theta.tau>0) stop("This case not implemented!")

        if(min(tau_res,tau_sus)==tau_sus){ #then t = tS
            ts = t
            tr = ts - (tau_res-tau_sus)
        } else if(min(tau_res,tau_sus)==tau_res){ #then t = tR
            tr = t
            ts = tr - (tau_sus-tau_res)
        }

        if(tr<0){ #shouldnt happen
            I_R.prime = 0
        } else{
            I_R.prime = (1-theta.a)*a_res*exp(-(1-theta.eps)*eps_res*tr) + (1-theta.r)*r_RS*I_S;
        }
        
        if(ts<0){ # shouldnt happen
            I_S.prime = 0
        } else{
            I_S.prime = model_susExpJoint2(
                t=ts,
                N.vector=c(I=I_S),
                params=c(a=a_sus, eps=eps_sus, r=r_sus, theta.a=theta.a, theta.eps=theta.eps, theta.r=theta.r)
            )[[1]]#[["I"]]
        }

        return(list(c(I_R.prime, I_S.prime)))
    })
}

#' Deterministic 1D ODE model of infection of susceptible variety.
#'
#' Deterministic model of form \deqn{\frac{dI}{dt} = \theta a \exp(-\epsilon t) + r_S I}{dI/dt = theta a exp(-eps t) + r_S I}
#' @param t time vector
#' @param N.vector initial conditions
#' @param params named vector of parameters (theta; a; eps; r)
#' @param args parameters to be fixed throughout optimisation
#' @return values of state variables of deterministic model at given time points
#' @export
model_susExpJoint2 = function(t, N.vector, params, args=NULL){
    with(as.list(c(params,N.vector)),{
        I.prime = (1-theta.a)*a*exp(-(1-theta.eps)*eps*t) + (1-theta.r)*r*I;
        return(list(c(I.prime)))
    })
}
