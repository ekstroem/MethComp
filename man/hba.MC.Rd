% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MethComp-package.R
\docType{data}
\name{hba.MC}
\alias{hba.MC}
\title{A MCmcmc object from the hba1c data}
\format{
The format is a \code{\link{MCmcmc}} object.
}
\description{
This object is included for illustrative purposes. It is a result of a
5-hour run using MCmcmc, with \code{n.iter=100000}.
}
\details{
The data are the venous measurements from the \code{\link{hba1c}} dataset,
using the day of analysis as replicate. Measurements are taken to be linked
within replicate (=day of analysis).
}
\examples{

data(hba.MC)
attr(hba.MC,"mcmc.par")
# print.MCmcmc(hba.MC)
# One of the chains is really fishy (it's the first one)
# trace.MCmcmc(hba.MC)
# trace.MCmcmc(hba.MC,"beta")
# Try to have a look, excluding the first chain
# hba.MCsub <- subset.MCmcmc(hba.MC,chains=-1)
# trace.MCmcmc(hba.MCsub)
# trace.MCmcmc(hba.MCsub,"beta")
# A MCmcmc object also has class mcmc.list, so we can use the
# coda functions for covergence diagnostics:
# acfplot( subset.MCmcmc(hba.MC, subset="sigma"))

}
\keyword{datasets}
