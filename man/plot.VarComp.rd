\name{plot.VarComp}
\alias{plot.VarComp}
\title{Plot the a posteriori densities for variance components}
\description{
  When a method comparison model i fitted and stored in a
  \code{\link{MCmcmc}} object, then the posterior distributions
  of the variance components are plotted, in separate displays for
  method.
  }
\usage{
\method{plot}{VarComp}( x,
                    which,
                 lwd.line = rep(2, 4),
                 col.line = c("red", "green", "blue", "black"),
                 lty.line = rep(1, 4),
                     grid = TRUE,
                 col.grid = gray(0.8),
                      rug = TRUE,
                    probs = c(5, 50, 95),
                  tot.var = FALSE,
                  same.ax = TRUE,
               meth.names = TRUE,
                 VC.names = "first",
                      ... )
  }
\arguments{
  \item{x}{A \code{MCmcmc} object.}
  \item{which}{For which of the compared methods should the plot be made?}
  \item{lwd.line}{Line width for drawing the density.}
  \item{col.line}{Color for drawing the densities.}
  \item{lty.line}{Line type for drawing the densities.}
  \item{grid}{Logical. Should a vertical grid be set up? If numeric it is
              set up at the values specified. If \code{same.ax}, the range of
              the grid is taken to be the extent of the x-axis for all plots.}
  \item{col.grid}{The color of the grid.}
  \item{rug}{Should a small rug at the bottom show posterior quantiles?}
  \item{probs}{Numeric vector with numbers in the range from 0 to 100,
               indicating the posterior percentiles to be shown in the rug.}
  \item{tot.var}{Should the posterior of the total variance also be shown?}
  \item{same.ax}{Should the same axes be used for all methods?}
  \item{meth.names}{Should the names of the methods be put on the plots?}
  \item{VC.names}{Should the names of the variance components be put on
                  the first plot (\code{"first"}), the last (\code{"last"}),
                  all (\code{"all"}) or none (\code{"none"}). Only the first
                  letter is needed.}
  \item{\dots}{Parameters passed on the \code{\link{density}} furnction that
               does the smoothing of the posterior samples.}
  }
\details{The function generates a series of plots, one for each method compared
  in the \code{MCmcmc} object supplied (or those chosen by \code{which=}).
  Therefore the user must take care to set \code{mfrow} or \code{mfcol} to
  capture all the plots.}
\value{
  A list with one element for each method.
  Each element of this is a list of densities, i.e. of objects of class
  \code{density}, one for each variance component.
  }
\author{Bendix Carstensen, \url{www.biostat.ku.dk/~bxc}}
\seealso{\code{\link{plot.MCmcmc}},
         \code{\link{MCmcmc}},
         \code{\link{check.MCmcmc}}}
\examples{
  data( ox.MC )
  par( mfrow=c(2,1) )
  plot.VarComp( ox.MC, grid=c(0,15) )
  }
\keyword{models}
\keyword{design}
\keyword{regression}
