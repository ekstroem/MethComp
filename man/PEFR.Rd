\name{PEFR}
\alias{PEFR}
\docType{data}
\title{Peak Expiratory Flow Rate (PEFR) measurements with Wright peak
  flow and mini Wright peak flow meter.}
\description{Measurement of PEFR with Wright peak flow and mini Wright
  peak flow meter on 17 individuals.
}
\usage{data(PEFR)}
\format{
  A data frame with 68 observations on the following 3 variables.
  \describe{
    \item{\code{meth}}{a factor with levels
          \code{Wright} and \code{Mini},
            representing measurements by a Wright peak flow
	    meter and a mini Wright meter respectively, in random order.}
    \item{\code{item}}{Numeric vector, the person ID.}
    \item{\code{y}}{Numeric vector, the measurements, i.e. PEFR for the
      two measurements with a Wright peak flow meter and a mini Wright
      meter respectively. The measurement unit is l/min.}
    \item{\code{repl}}{Numeric vector, replicate number. Replicates are
      exchangeable within item.}
  }
}
\source{J. M. Bland and D. G. Altman (1986) Statistical Methods for Assessing
   Agreement Between Two Methods of Clinical Measurement, Lancet. 1986
   Feb 8;1(8476):307-10. 
}
\examples{
data(PEFR)
PEFR <- Meth(PEFR)
summary(PEFR)
plot(PEFR)
plot(perm.repl(PEFR))
}
\keyword{datasets}
