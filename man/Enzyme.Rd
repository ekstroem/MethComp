\name{Enzyme}
\alias{Enzyme}
\docType{data}
\title{ Enzyme activity data}
\description{Three measurement of enzyme activity on 24 patients. The
  measurements is of the enzymes sucrase and alkaline phosphatase. The
  interest is to compare the 'homogenate' and 'pellet' methods.}
\usage{data(Enzyme)}
\format{
  A data frame with 72 observations on the following 3 variables.
  \describe{
    \item{\code{meth}}{a factor with levels \code{SucHom} \code{SucPel}
      \code{Alkphos}, representing three different measurements, i.e.
     homogenate and pellet values of sucrase, as well as homogenate values
     of alkaline.}
    \item{\code{item}}{a numeric vector, the person ID for the 24 patients}
    \item{\code{y}}{a numeric vector, the measurements on the enzyme activity.}
  }
}
\source{R. L. Carter; Restricted Maximum Likelihood Estimation of
  Bias and Reliability in the Comparison of Several Measuring Methods;
  Biometrics, Dec., 1981, Vol. 37, No. 4, pp. 733-741.
}
\examples{
data(Enzyme)
Enzyme <- Meth( Enzyme )
summary( Enzyme )
# plot( Enzyme )
}
\keyword{datasets}

