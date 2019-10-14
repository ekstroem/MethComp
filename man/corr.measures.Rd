\name{corr.measures}
\alias{corr.measures}
\alias{middle}
\alias{ends}
\title{Correlation measures for method comparison studies. Please don't
  use them!}
\description{
  Computes correlation, mean squared difference, concordance correlation
  coefficient and the association coefficient.
  \code{middle} and \code{ends} are useful utilities for illustrating the
  shortcomings of the association measures, see the example.
  }
\usage{
  corr.measures(x, y)
  middle(w, rm = 1/3)
    ends(w, rm = 1/3)
  }
\arguments{
  \item{x}{vector of measurements by one method.}
  \item{y}{vector of measurements by another method.}
  \item{w}{numerical vector.}
  \item{rm}{fraction of data to remove.}
  }
\details{
  These measures are all flawed since they are based on the correlation in
  various guises. They fail to address the relevant problem of AGREEMENT.
  It is recommended NOT to use them.
  The example gives an example, illustrating what happens when increasingly
  large chunks of data in the middle are removed.
  }
\value{
  \code{corr.measures} return a vector with 4 elements.
  \code{middle} and \code{ends} return a logical vector pointing to the middle
  or the ends of the \code{w} after removing a fraction of \code{rm} from data.
  }
\references{Shortly...}
\author{
  Bendix Carstensen, Steno Diabetes Center,
  \url{http://BendixCarstensen.com}
  }
\seealso{
  \code{\link{MCmcmc}}.
  }
\examples{
cbind( zz <- 1:15, middle(zz), ends(zz) )
data( sbp )
bp <- subset( sbp, repl==1 & meth!="J" )
bp <- Meth( bp )
summary( bp )
plot( bp )
bw <- to.wide( bp )
with( bw, corr.measures( R, S ) )
# See how it gets better with less and less data:
summ.corr <-
rbind(
with( subset( bw, middle( R+S, 0.6 ) ), corr.measures( R, S ) ),
with( subset( bw, middle( R+S, 0.4 ) ), corr.measures( R, S ) ),
with(         bw                      , corr.measures( R, S ) ),
with( subset( bw,   ends( R+S, 0.3 ) ), corr.measures( R, S ) ),
with( subset( bw,   ends( R+S, 0.4 ) ), corr.measures( R, S ) ),
with( subset( bw,   ends( R+S, 0.6 ) ), corr.measures( R, S ) ),
with( subset( bw,   ends( R+S, 0.8 ) ), corr.measures( R, S ) ) )
rownames( summ.corr ) <- c("middle 40\%",
                           "middle 60\%",
                           "total",
                           "outer 70\%",
                           "outer 60\%",
                           "outer 40\%",
                           "outer 20\%")
summ.corr
  }
\keyword{models}
\keyword{regression}
