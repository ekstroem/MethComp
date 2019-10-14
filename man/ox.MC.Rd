\name{ox.MC}
\alias{ox.MC}
\docType{data}
\title{A MCmcmc object from the oximetry data.}

\description{
  This object is included for illustrative purposes. It is a result of
  using \code{\link{MCmcmc}}, with \code{n.iter=20000}.
  } 

\usage{data(ox.MC)}

\format{The format is a \code{\link{MCmcmc}} object.
  }

\details{
  The data are the \code{\link{ox}} dataset, where measurements are
  linked within replicate (=day of analysis).
  }

\examples{ 
data(ox.MC)
attr(ox.MC,"mcmc.par")
\dontrun{
print.MCmcmc(ox.MC)
trace.MCmcmc(ox.MC)
trace.MCmcmc(ox.MC,"beta")
 post.MCmcmc(ox.MC)
 post.MCmcmc(ox.MC,"beta") }
# A MCmcmc object also has class mcmc.list, so we can use the
# coda functions for covergence diagnostics:
\dontrun{ acfplot( subset.MCmcmc(ox.MC, subset="sigma")) }
}

\keyword{datasets}
