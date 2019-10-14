\name{BA.plot}
\alias{BA.plot}
\title{
  Bland-Altman plot of differences versus averages.
  }
\description{
  For two vectors of equal length representing measurements of the same
  quantity by two different methods, the differences are plotted versus the
  average. The limits of agreement (prediction limits for the differences)
  are plotted, optionally a regression of differences of means is given
  too. Works with \code{\link{Meth}} and \code{\link{MethComp}}
  objects too.
  }
\usage{
BA.plot( y1, y2, meth.names = NULL,
                     wh.comp = 1:2,
                     pl.type = "BA",
                    dif.type = "const",
                     sd.type = "const",
                       model = if( inherits(y1,"Meth") & has.repl(y1) ) "exch"
                               else NULL,
                        eqax = FALSE,
                       axlim = if( is.data.frame(y1) ) range(y1$y) else range(c(y1,y2)),
                      diflim = NULL,
                        grid = TRUE,
                      N.grid = 10,
                    col.grid = grey(0.9),
                      points = TRUE,
                  col.points = "black",
                  cex.points = 1,
                  pch.points = 16,
                         lwd = c(3,1,1),
                   col.lines = "blue",
                   repl.conn = FALSE,
                    col.conn = "gray",
                    lwd.conn = 1,
                        xlab = NULL,
                        ylab = NULL,
                         eqn = FALSE,
                     col.eqn = col.lines,
                    font.eqn = 2,
                      digits = 2,
                   Transform = if( mult ) "log" else NULL,
                        mult = FALSE,
                       alpha = NULL,
                         ... )
 }
\arguments{
  \item{y1}{Numerical vector of measurements by 1st method. Can also be a
            \code{\link{Meth}} or a \code{\link{MethComp}} object, see details.}
  \item{y2}{Numerical vector of measurements by 2nd method. Must of same
            length as \code{x}. Ignored if a \code{\link{Meth}} or a
	    \code{\link{MethComp}} objects is given for \code{y1}.}
  \item{meth.names}{Label for the method names.}
  \item{wh.comp}{Which methods should be compared. Either numerical or
    character.}
  \item{   pl.type}{What type of plot should be made, \code{"BA"} for
    differences versus averages, \code{"conv"} for method 1 versus
    method 2.}
  \item{  dif.type}{How should difference depend on the
    averages. \code{"const"} or \code{"lin"}.}
  \item{   sd.type}{How should the standard deviation depend on the
    averages. \code{"const"} or \code{"lin"}.}
  \item{     model}{Should a variance component model be used to compute
  the limits of agreement? If \code{NULL} a simple analysis is made;
  other possibilities are \code{"exch"} or \code{"linked"} for
  exchangeable or linked replicates.}
  \item{      eqax}{Should the axes be identical? If a Bland-Altman plot
  is drawn, the axis for the differences will have the same extent as the
  axis for the averages, but centered on 0 (see \code{diflim}).}
  \item{     axlim}{The limits of the axes.}
  \item{    diflim}{The limits of the difference axis.}
  \item{      grid}{Should a grid be drawn? If numeric it indicates the
    places where the grid is drawn.}
  \item{    N.grid}{How many grid-lines should be drawn.}
  \item{  col.grid}{What should be the color of the grid?}
  \item{    points}{Logical. Should the observed points be drawn?}
  \item{col.points}{What color should they have?}
  \item{cex.points}{How large should they be?}
  \item{pch.points}{What plot character for the points}
  \item{       lwd}{Numerical vector of 3, giving the width of the
    conversion line (mean difference) and the limits of agreement.}
  \item{ col.lines}{What color should the lines have.}
  \item{ repl.conn}{Should replicate measurements be connected (within items)?}
  \item{  col.conn}{Color of connecting lines.}
  \item{  lwd.conn}{Width of connecting lines.}
  \item{      xlab}{x-axis label.}
  \item{      ylab}{y-axis label.}
  \item{       eqn}{Logical. Should the equations linking the methods be
    shown on the plot? If a Bland-Altman plot is made, both the equations
  linking the methods and the equation for the differences versus the
  averages are shown.}
  \item{   col.eqn}{Color for equations}
  \item{  font.eqn}{Font for equations}
  \item{    digits}{How many digits after the decimal point should be
    used when showing the equations.}
  \item{ Transform}{Transformation applied to data prior to
    analysis. Plots are made on the original scale after
    back-transformation.}
  \item{      mult}{Logical. If TRUE, ratios of measurement instead of
    differences will be plotted in the Bland-Altman plot on a logarithmic
    axis, and limits of agreement will be given on this scale?
    This gives the same analysis as using \code{Transform="log"}, but a different
    plot. Using another transformation than the log is accommodated, but
    no LoA is shown on the axis.}
  \item{     alpha}{1 minus the confidence level. If \code{NULL} a
    multiplier of 2 is used for constructing prediction limits,
    otherwise a t-quantile with d.f. equal th number of items minus 1.}
  \item{...}{Further parameters passed on to \code{\link{plot.MethComp}}}
}
\details{A plot of the relationship between the methods is produced;
  either a Bland-Altman plot of the differences versus averages, or a 45
  degree rotation as a conversion between the methods. If
  \code{model=NULL} a simple regression of averages on differences is
  made by calling \code{DA.reg}, and the specified conversion plotted.
}
\value{
  An object of class \code{\link{MethComp}} and either \code{DA.reg} (if
  \code{model=NULL}) or \code{BA.est} (if \code{model} is character).
  }
\references{
JM Bland and DG Altman: Statistical methods for assessing agreement between
  two methods of clinical measurement, Lancet, i, 1986, pp. 307-310.

JM Bland and DG Altman. Measuring agreement in method comparison studies.
  Statistical Methods in Medical Research, 8:136-160, 1999.

B Carstensen: Comparing methods of measurement: Extending the LoA by
  regression. Stat Med. 2010 Feb 10;29(3):401-10.
  }
\author{
  Bendix Carstensen \email{bxc@steno.dk},
  \url{http://BendixCarstensen.com}.
  }
\seealso{
  \code{\link{BA.est}}, \code{\link{DA.reg}}, \code{\link{MCmcmc}}.
}
\examples{
data( ox )
ox <- Meth( ox )
# The simplest possible Bland-Altman plot
BA.plot( ox )

## With bells and whistles, comparing the naive and model
par( mfrow=c(2,2) )
BA.plot( ox, model=NULL, repl.conn=TRUE, col.lines="blue",
         axlim=c(0,100), diflim=c(-50,50), xaxs="i", yaxs="i",
         las=1, eqn=TRUE, dif.type="lin", pl.type="BA", sd.type="lin",
         grid=1:9*10, digits=3,font.eqn=1)
par(new=TRUE)
BA.plot( ox, model="linked", repl.conn=TRUE, col.lines="red",
         axlim=c(0,100), diflim=c(-50,50), xaxs="i", yaxs="i",
         las=1, eqn=FALSE, dif.type="lin", pl.type="BA", sd.type="lin",
        grid=1:0*10, digits=3)
BA.plot( ox, model=NULL, repl.conn=TRUE, col.lines="blue",
         axlim=c(0,100), diflim=c(-50,50), xaxs="i", yaxs="i",
         las=1, eqn=TRUE, dif.type="lin", pl.type="conv", sd.type="lin",
        grid=1:9*10, digits=3,font.eqn=1)
par(new=TRUE)
BA.plot( ox, model="linked", repl.conn=TRUE, col.lines="red",
         axlim=c(0,100), diflim=c(-50,50), xaxs="i", yaxs="i",
         las=1, eqn=FALSE, dif.type="lin", pl.type="conv", sd.type="lin",
         grid=1:9*10, digits=3)
# The same again, but now logit-transformed
BA.plot( ox, model=NULL, repl.conn=TRUE, col.lines="blue",
         axlim=c(0,100), diflim=c(-50,50), xaxs="i", yaxs="i",
         las=1, eqn=TRUE, dif.type="lin", pl.type="BA", sd.type="lin",
         grid=1:9*10, digits=3,font.eqn=1,Transform="pctlogit")
par(new=TRUE)
BA.plot( ox, model="linked", repl.conn=TRUE, col.lines="red",
         axlim=c(0,100), diflim=c(-50,50), xaxs="i", yaxs="i",
         las=1, eqn=FALSE, dif.type="lin", pl.type="BA", sd.type="lin",
         grid=1:0*10, digits=3,Transform="pctlogit")
BA.plot( ox, model=NULL, repl.conn=TRUE, col.lines="blue",
         axlim=c(0,100), diflim=c(-50,50), xaxs="i", yaxs="i",
         las=1, eqn=TRUE, dif.type="lin", pl.type="conv", sd.type="lin",
         grid=1:9*10, digits=3,font.eqn=1,Transform="pctlogit")
par(new=TRUE)
BA.plot( ox, model="linked", repl.conn=TRUE, col.lines="red",
         axlim=c(0,100), diflim=c(-50,50), xaxs="i", yaxs="i",
         las=1, eqn=FALSE, dif.type="lin", pl.type="conv", sd.type="lin",
         grid=1:9*10, digits=3,Transform="pctlogit")
}
\keyword{models}
\keyword{design}
