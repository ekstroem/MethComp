\name{CardOutput}
\Rdversion{1.1}
\alias{CardOutput}
\docType{data}
\title{
  Measurements of Cardiac output.
}
\description{
  Two different ways of measuring cardiac output and oxygen saturation in
  15 critically ill persons.
}
\usage{data(CardOutput)}
\format{
  A data frame with 15 observations on the following 8 variables.
  \describe{
    \item{\code{Age}}{Patient age}
    \item{\code{Diag}}{Diagnosis,
      a factor with levels \code{sepsis}, \code{cardiogenic}, \code{hypothermia}}
    \item{\code{VO2}}{Oxygen consumption}
    \item{\code{Svo2}}{Mixed venous O2 saturation}
    \item{\code{Scvo2}}{Central venous oxygen saturation}
    \item{\code{TCO}}{Thermodilution-derived cardiac output}
    \item{\code{FCO}}{Fick-derived cardiac output.}
    \item{\code{Sex}}{Sex, a factor with levels \code{F}, \code{M}}
  }
}
\source{
Avi A. Weinbroum, Philippe Biderman,
Dror Soffer, Joseph M. Klausner & Oded Szold:

Reliability of cardiac output
calculation by the fick principle
and central venous oxygen saturation
in emergency conditions.

Journal of Clinical Monitoring and Computing (2008) 22: 361-366
}
\examples{
data(CardOutput)
}
\keyword{datasets}
