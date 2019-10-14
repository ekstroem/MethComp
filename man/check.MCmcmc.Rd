\name{check.MCmcmc}
\alias{check.MCmcmc}
\alias{trace.MCmcmc}
\alias{post.MCmcmc}
\alias{pairs.MCmcmc}
% \alias{acf.MCmcmc}
\title{Functions to graphically assess the convergence of the
  MCMC-simulation in a MCmcmc object}
\description{
   These functions display traces, posterior densities and
   autocorrelation functions for the relevant subset of the
   parameters in a MCmcmc object.
   }
\usage{
\method{trace}{MCmcmc}( obj, what = "sd",
                           scales = c("same", "free"),
                           layout = "col",
                           aspect = "fill", ...)
%  acf.MCmcmc( obj, what = "sd",
%                   scales = c("same", "free"),
%                   layout = "col",
%                   aspect = "fill", ...)
\method{post}{MCmcmc}( obj, what ="sd",
                           check = TRUE,
                          scales = "same",
                          layout = "row",
                             lwd = 2,
                             col,
                     plot.points = FALSE,
                          aspect = "fill", ... )
\method{pairs}{MCmcmc}( x, what = "sd",
                         subset,
                            col = NULL,
                            pch = 16,
                            cex = 0.2,
                         scales = "free", ... )
}
\arguments{
  \item{obj}{A \code{MCmcmc} object.}
  \item{x}{A \code{MCmcmc} object.}
  \item{what}{Character indicating what parameters to plot.
              Possible values are \code{"sd"} or \code{"var"} which
              gives plots for the variance components (on the sd. scale),
              \code{"beta"} or \code{"slope"}, which gives plots for
              slope parameters and \code{"alpha"} or \code{"int"}, which
              gives plots for the intercept parameters.}
  \item{scales}{Character vector of length two, with possible values "same" or
                "free", indicating whether x- and y-axes of the plots should be
                constrained to be the same across panels. For \code{pairs} only
                the first element is used to decide whether all panles should
                have the same axes.}
  \item{layout}{Character. If \code{"col"} parameters are displayed columnwise
                by method, if \code{"row"} they are displayed row-wise.}
  \item{aspect}{How should the panels be scaled. Default (\code{"fill"}) is to
                make a panels take up as much place as possible.}
  \item{check}{Logical. Should the density plots be separate for each chain
               (in order to check convergence) or should the chains be merged.}
  \item{lwd}{Width of the lines used for plotting of the posterior densities.}
  \item{col}{Color of the lines points used for plotting of the posterior densities.}
  \item{plot.points}{Logical. Should a rug with actual data points be
                     plotted beneath the density.}
  \item{pch}{Plot symbol for the points.}
  \item{subset}{Character or numerical indicating the columns of the posterior
                that should be plotted by \code{pairs}.}
  \item{cex}{Plot character size for points in \code{pairs}.}
  \item{\dots}{Further aruments passed on to the \code{\link{Lattice}}
    function called:
    \code{trace} calls \code{\link{xyplot}} from the \code{coda} package,
    \code{post} calls \code{\link{densityplot}} from the \code{coda} package,
%    \code{acf} calls \code{\link{acfplot}},
    \code{pairs} calls \code{\link{pairs}} from the \code{graphics} package.}
}
\details{
  A \code{Lattice} plot is returned, which means that it must
  \code{print}ed when these functions are called in a batch program or
  inside another function or for-loop.

  \code{trace} plots traces of the sampled chains,
  \code{post} plots posterior densities of the parameters and
  \code{pairs} plots a scatter-plot matrix of bivariate marginal
  posterior distributions.
  }
\value{
  A \code{\link{Lattice}} plot.
}
\author{
  Bendix Carstensen, Steno Diabetes Center,
  \email{bxc@steno.dk},
  \url{http://BendixCarstensen.com}.
}
\seealso{\code{\link{MCmcmc}},
         \code{\link{plot.MCmcmc}},
         \code{\link{ox.MC}},
         \code{\link{sbp.MC}}}
\examples{
# Load a provided MCmcmc object
data( ox.MC )
trace.MCmcmc( ox.MC, what="beta" )
pairs.MCmcmc( ox.MC, what="sd" )
}
\keyword{models}

