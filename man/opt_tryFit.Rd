% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/opt_tryFit.R
\name{opt_tryFit}
\alias{opt_tryFit}
\title{Performs non-linear least squares parameter optimisation, dealing with common convergence errors.}
\usage{
opt_tryFit(fitFunc, initParams, control, recCount = 0, maxRecs = 5, ...)
}
\arguments{
\item{fitFunc}{optimisation routine to use}

\item{initParams}{initial guess parameter values for optimiser to improve upon}

\item{control}{list of control parameters to be passed to nls}

\item{recCount}{the number of recursive calls of this function made so far}

\item{maxRecs}{a limit on the number of recursive calls that can be made}

\item{...}{unmodified parameters (incl. isNonDim, lower, upper, tColStr, plotSum, N0, modelFuncSus, modelFuncRes, args(optional) and residFunc) to be passed to \code{opt_paramsNLS()}}
}
\value{
non-linear least squares (nls) object
}
\description{
Performs non-linear least squares parameter optimisation, dealing with common convergence errors.
}
