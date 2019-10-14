\name{plot.PBreg}
\alias{plot.PBreg}
\title{Passing-Bablok regression - plot method}
\description{
  A plot method for the \code{"PBreg"} class object, that is a
  result of Passing-Bablok regression.
}
\usage{
\method{plot}{PBreg}(x,
               pch=21, bg="#2200aa33",
               xlim=c(0, max(x$model)), ylim=c(0, max(x$model)),
               xlab=x$meths[1], ylab=x$meths[2], subtype=1, colors =
               list(CI = "#ccaaff50", fit = "blue", ref = "#99999955", 
               bars = "gray", dens = "#8866aaa0", ref2 = c("#1222bb99", 
               "#bb221299")), \ldots)
}
\arguments{
  \item{x}{an object of class \code{"PBreg"}}
  \item{pch}{Which plotting character should be used for the points.}
  \item{bg}{Background colour for the plotting character.}
  \item{xlim}{Limits for the x-axis.}
  \item{ylim}{Limits for the y-axis.}
  \item{xlab}{Label on the x-axis.}
  \item{ylab}{Label on the y-axis.}
  \item{subtype}{a numeric value or vector, that selects the desired
    plot subtype.  Subtype \bold{1} is an x-y plot of raw data with regression
    line and confidence boundaries for the fit as a shaded area.  This
    is the default.  Subtype \bold{2} is a ranked residuals plot.  Subtype
    \bold{3} is the "Cusum" plot useful for assessing linearity of the
    fit. Plot subtypes 1 through 3 are standard plots from the 1983 paper
    by Passing and Bablok - see the reference.  Plot subtype \bold{4} is
    a histogram (with overlaid density line) of the individual slopes.
    The range of this plot is limited to 5 x IQR for better visibility.}
  \item{colors}{A list of 6 elements allowing customization of colors of various 
    plot elements. For plot subtype 1: "CI" is the color of the shaded confidence 
    interval area; and "fit" is the color of fit line. For plot subtypes 2 & 3: 
    "ref" is the color of the horizontal reference line. For plot subtype 4:
    "bars" is the bar background color, "dens" is the color of the density line, and
    "ref2" is a vector of two colors for lines indicating the median and confidence
    limits.}
  \item{\ldots}{other parameters as in \code{"plot"}, some of which are
    pre-defined for improved appearance. This affects only the subtype 1 plot.}
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
  par(mfrow=c(2,2))
  plot(x, s=1:4)

  ## Or the same using "Meth" object
  a <- Meth(a, y=1:2)
  x <- PBreg(a)
  print(x)
  par(mfrow=c(2,2))
  plot(x, s=1:4)
}
