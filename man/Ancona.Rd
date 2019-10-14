\name{Ancona}
\alias{Ancona}
\docType{data}
\title{
Data from a rating experiment of recorgnizing point counts.
}
\description{
At the course "Statsitical Analysis of Method Comparison Studies" ai the
SISMEC conference in Ancona, on 28 September 2011, the participants on
the course were used as raters of ten pictures of points. Pitures were
shown 3 times each to the participants, and they assessed the number of
points in each.
}
\usage{data(Ancona)}
\format{
  A data frame with 510 observations on the following 4 variables.
  \describe{
    \item{\code{rater}}{a factor with 17 levels}
    \item{\code{item}}{a numeric vector indicating the pictures
      shown. The value is the actual number of points.}
    \item{\code{repl}}{a numeric vector, replicate number}
    \item{\code{score}}{a numeric vector, the number of points in  \code{item}}
  }
}
\source{
  The course "Statsitical Analysis of Method Comparison Studies" ai the
SISMEC conference in Ancona, on 28 September 2011.
}
\examples{
library( MethComp )
data( Ancona )
Anc <- Meth( Ancona, 1, 2, 3, 4 )
}
\keyword{datasets}
