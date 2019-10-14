\name{BlandAltman}
\alias{BlandAltman}
\alias{AB.plot}
\title{
  Bland-Altman plot of differences versus averages. Deprecated, formerly
  known as BA.plot, likely to disappear from the package.
}
\description{
  For two vectors of equal length representing measurements of the same
  quantity by two different methods, the differences are plotted versus the
  average. The limits of agreement (prediction limits for the differences)
  are plotted, optionally a regression of differences of means is given too.
}
\usage{
  BlandAltman(x, y,
            x.name = NULL,
            y.name = NULL,
           maintit = "",
               cex = 1,
               pch = 16,
        col.points = "black",
         col.lines = "blue",
              limx = NULL,
              limy = NULL,
              ymax = NULL,
              eqax = FALSE,
              xlab = NULL,
              ylab = NULL,
             print = TRUE,
          reg.line = FALSE,
            digits = 2,
              mult = FALSE,
             alpha,
               ... )
  AB.plot( y1, y2,
      meth.names = NULL,
       mean.repl = FALSE,
       conn.repl = !mean.repl,
        lwd.conn = 1,
        col.conn = "black",
     comp.levels = 2:1,
              ... )
}
\arguments{
  \item{x}{Numerical vector of measurements by 1st method.}
  \item{y}{Numerical vector of measurements by 2nd method. Must of same
           length as \code{x}.}
  \item{x.name}{Label for the 1st method (\code{x}).}
  \item{y.name}{Label for the 2nd method (\code{y}).}
  \item{maintit}{Main title for the plot}
  \item{cex}{Character expansion for the points.}
  \item{pch}{Plot symbol for points.}
  \item{col.points}{Color for the points.}
  \item{col.lines}{Color for the lines indicating limits of agreement.}
  \item{limx}{x-axis limits.}
  \item{limy}{y-axis limits.}
  \item{ymax}{Scalar. The y-axis will extend from -ymax to +ymax.}
  \item{eqax}{Logical. Should the range on x- and y- axes be the same?}
  \item{xlab}{x-axis label.}
  \item{ylab}{y-axis label.}
  \item{print}{Logical: Should the limits of agreement and the c.i.s of these
               be printed?}
  \item{reg.line}{If \code{TRUE}, the regression line of \code{x-y} on
                  \code{(x+y)/2} is drawn. If numerical the regression
                  equation is printed with the given number of digits
                  after the decimal points.}
  \item{digits}{How many decimal places should be used when printing limits of agreement?
                Used both for the printing of results and for annotation of the plot.}
  \item{mult}{Logical. Should data be log-transformed and reporting be on a
              multiplicative scale?}
  \item{alpha}{1 minus confidence level used when computing confidence
               intervals and limits of agreement, i.e. the t(1-alpha/2)
               quantile is used. If not supplied the standard value of 2
               is used for computing LoA.}
	     \item{y1}{Measurements by method 1. Alternatively a
	       \code{\link{Meth}} object or a dataframe with columns
           \code{meth}, \code{item}, \code{y}, and possibly \code{repl}.}
  \item{y2}{Corresponding measurements by method 2. Ignored if \code{y1}
            is a dataframe.}
  \item{meth.names}{Names for the two methods. Used for annotation of the plot.
                    If not supplied and \code{y1} is a dataframe names are
                    derived from the factor level names of \code{meth}.}
  \item{mean.repl}{Logical. If there are replicate measurements by each method
                   should the means by \code{item} and \code{meth} be formed
                   before further ado. WARNING: This will give too narrow
                   limits of agreement.}
  \item{conn.repl}{Logical. Should replicates from the same item be connected?}
  \item{lwd.conn}{Line width of connecting lines}
  \item{col.conn}{Color of connecting lines}
  \item{comp.levels}{Levels of the \code{meth} factor to compare. May be used
                     to switch the order of the methods compared by specifying
                     \code{comp.meth=2:1}.}
  \item{...}{Further arguments passed on from \code{BA.plot} to
             \code{BlandAltman} and possibly further to the \code{plot}
             function. The arguments passed to \code{\link{BlandAltman}} are
             used for fine-tuning the appearance of the plot.}
}
\value{
  An object of class \code{BA.check}; list with 3 elements:
  \item{LoA}{A vector of length 3 with Limits of Agreement.}
  \item{p.value}{P-values for three hypothese: 1) Constant variance - this is
                 the test of 0 slope in the regression of absolute residuals
                 on averages. 2) Constant difference - this is the test of 0
                 slop in the regression of differences on averages. 3)
                 Difference equal to 0 - this is usually a lame thing to
		 use.}
  \item{reg.res}{A \eqn{3\times 4}{3x4} matrix with (in the first row)
    the results from regressing the averages on the means, and in the
    two other rows the derived relationships
    between methods. In each line the intercept (\code{alpha}), slope
    (\code{beta}), the prediction standard deviation (\code{pr.sd}) and
    half the width of the prdiction interval (\code{pr.int}).}
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
  \code{\link{BA.plot}}, \code{\link{MCmcmc}}.
}
\examples{
data( ox )
par( mfrow=c(1,2) )
# Wrong to use mean over replicates
mtab <- with( ox, tapply( y, list(item, meth), mean ) )
CO <- mtab[,"CO"]
pulse <- mtab[,"pulse"]
BlandAltman( CO, pulse )

# (almost) Right to use replicates singly
par( mfrow=c(1,1) )
oxw <- to.wide( ox )
CO <- oxw[,"CO"]
pulse <- oxw[,"pulse"]
BlandAltman( CO, pulse, mult=TRUE )
BlandAltman( CO, pulse, eqax=TRUE )

data( plvol )
BA.plot( plvol )
BA.plot( plvol, reg.line=TRUE )
BA.plot( plvol, reg.line=2 )
}
\keyword{models}
\keyword{design}
