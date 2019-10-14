\name{fat}
\alias{fat}
\docType{data}
\title{Measurements of subcutaneous and visceral fat}
\description{
43 persons had Subcutaneous and Visceral fat thickness measured at
Steno Diabetes Center in 2006 by two observers; all measurements were done
three times. The interest is to compare the measurements by the two
observers. Persons are items, observers are methods, the three replicates
are exchangeable within (person,observer)=(item,method)
}
\usage{data(fat)}
\format{
  A data frame with 258 observations on the following 6 variables.
  \describe{
    \item{\code{Id}}{Person id.}
    \item{\code{Obs}}{Observers, a factor with levels \code{KL} and \code{SL}.}
    \item{\code{Rep}}{Replicate --- exchangeable within person and observer.}
    \item{\code{Sub}}{Subcutaneous fat measured in cm.}
    \item{\code{Vic}}{Visceral fat measured in cm.}
  }
}
\examples{
data(fat)
str(fat)
vic <- Meth( fat, meth=2, item=1, repl="Rep", y="Vic" )
str(vic)
BA.est( vic, linked=FALSE )
}
\keyword{datasets}
