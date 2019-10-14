\name{predict.PBreg}
\alias{predict.PBreg}
\title{Passing-Bablok regression - predict method}
\description{
  A predict method for the \code{"PBreg"} class object, that is a
  result of Passing-Bablok regression.
}
\usage{
\method{predict}{PBreg}(object, newdata = object$model$x, interval="confidence",
  level=0.95,...)
}
\arguments{
  \item{object}{an object of class \code{"PBreg"}}
  \item{newdata}{an optional vector of new values of \code{x} to make predictions for.
    If omitted, the fitted values will be used.}
  \item{interval}{type of interval calculation - either \code{confidence} or \code{none}.
    The former is the default.}
  \item{level}{tolerance/confidence level.}
  \item{...}{Not used.}
}
\value{
  If \code{interval} is \code{"confidence"} this function returns a data frame with
  three columns: "fit", "lwr" and "upr" - similarly to \code{predict.lm}.

  If \code{interval} is \code{"none"} a vector of predicted values is returned.
}
\references{Passing, H. and Bablok, W. (1983), A New Biometrical Procedure
  for Testing the Equality of Measurements from Two Different Analytical
  Methods. \emph{Journal of Clinical Chemistry and Clinical Biochemistry},
  \bold{Vol 21}, 709--720}
\author{Michal J. Figurski \email{mfigrs@gmail.com}}
\seealso{
  \code{\link{PBreg}, \link{Deming}}.
}
\examples{
  ## Model data frame generation
  a <- data.frame(x=seq(1, 30)+rnorm(mean=0, sd=1, n=30),
                  y=seq(1, 30)*rnorm(mean=1, sd=0.4, n=30))

  ## Call to PBreg
  x <- PBreg(a)
  print(x)
  predict(x, interval="none")

  ## Or the same using "Meth" object
  a <- Meth(a, y=1:2)
  x <- PBreg(a)
  print(x)
  predict(x)
}
