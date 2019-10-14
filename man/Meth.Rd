\name{Meth}
\alias{Meth}
\alias{summary.Meth}
\alias{plot.Meth}
\alias{mean.Meth}
\alias{median.Meth}
\alias{min.Meth}
\alias{max.Meth}
\alias{sort.Meth}
\alias{subset.Meth}
\alias{sample.Meth}
\alias{transform.Meth}
\title{Create a Meth object representing a method comparison study}
\description{
  Creates a dataframe with columns \code{meth}, \code{item}, (\code{repl}) and
  \code{y}.
  }
\usage{
Meth( data=NULL,
      meth="meth", item="item", repl=NULL, y="y",
      print=!is.null(data), keep.vars=!is.null(data) )
\method{summary}{Meth}( object, ... )
\method{plot}{Meth}(x, which = NULL,
          col.LoA = "blue", col.pt = "black", cex.name = 2,
        var.range,
       diff.range,
        var.names = FALSE,
              pch = 16,
              cex = 0.7,
        Transform,
              ... )
\method{mean}{Meth}(x, na.rm=TRUE, simplify=TRUE, ... )
\method{sort}{Meth}(x, ... )
\method{subset}{Meth}(x, ... )
\method{sample}{Meth}( x,
                     how = "random",
                       N = if( how=="items" ) nlevels( x$item ) else nrow(x),
                     ... )
\method{transform}{Meth}(`_data`, ... )
}
\arguments{
  \item{data}{A dataframe.}
  \item{meth}{Vector of methods, numeric, character or factor.
              Can also be a number or character referring to a column in
              \code{data}.}
  \item{item}{Vector of items, numeric, character or factor.
              Can also be a number or character referring to a column in
              \code{data}.}
  \item{repl}{Vector of replicate numbers, numeric, character or factor.
              Can also be a number or character referring to a column in
              \code{data}.}
  \item{y}{Vector of measurements. Can also be a character
    or numerical vector pointing to columns in \code{data} which
    contains the measurements by different methods or a dataframe with
    columns representing  measurements by different methods. In this
    case the argument \code{meth} is ignored, and the names of the
    columns are taken as method names.}
  \item{print}{Logical: Should a summary result be printed?}
  \item{keep.vars}{Logical. Should the remaining variables from the dataframe
                   \code{data} be transferred to the \code{Meth} object.}
  \item{object}{A \code{Meth} object.}
  \item{x}{A \code{Meth} object.}
  \item{which}{A vector of indices or names of methods to plot. If
    \code{NULL} all methods in the object are plotted.}
  \item{col.LoA}{What color should be used for the limits of agreement.}
  \item{col.pt}{What color should be used for the points.}
  \item{cex.name}{Character expansion factor for plotting method names }
  \item{var.range}{The range of both axes in the scatter plot and
                   the x-axis in the Bland-Altman plot be?}
  \item{diff.range}{The range of yaxis in the Bland-Altman plot. Defaults to
                    a range as the x-axis, but centered around 0.}
  \item{var.names}{If logical: should the individual panels be labelled with the
                   variable names?. If character, then the values of the character
                   will be used to label the methods.}
  \item{pch}{Plot character for points.}
  \item{cex}{Plot charcter expansion for points.}
  \item{Transform}{Transformation used to the measurements prior to
           plotting. Function or character, see
           \code{\link{choose.trans}} for possible values.}
  \item{na.rm}{Logical. Should \code{NA}s be removed before calculations?}
  \item{simplify}{Should a \code{Meth} object with one row per
           (meth,item) be returned?}
  \item{how}{Character. What sampling strategy should be used, one of
             \code{"random"}, \code{"linked"} or \code{"item"}. Only the first
             letter is significant. See details for explanation.}
  \item{N}{How many observations should be sampled?}
  \item{_data}{A \code{Meth} object.}
  \item{...}{Ignored by the \code{Meth} and the \code{summary} and \code{sample}
             functions. In the \code{plot} function, parameters passed
           on to both the panel function plotting methods against each
           other, as well as to those plotting differences against means.}
 }
\details{
  In order to perform analyses of method comparisons it is convenient to have a
  dataframe with classifying factors, \code{meth}, \code{item}, and possibly
  \code{repl} and the response variable \code{y}. This function creates such a
  dataframe, and gives it a class, \code{Meth}, for which there is a number of
  methods: \code{summary} - tabulation, \code{plot} - plotting and a couple of
  analysis methods.

  If there are replicates in the values of \code{item} it is assumed
  that those observations represent replicate measurements and different
  replicate numbers are given to those.

  \code{sample.Meth} samples a \code{\link{Meth}} object with
  replacement.  If \code{how=="random"}, a random sample of the rows are
  sampled, the existing values of \code{meth}, \code{item} and \code{y}
  are kept but new replicate numbers are generated.  If
  \code{how=="linked"}, a random sample of the linked observations (i.e.
  observations with identical \code{item} and \code{repl} values) are
  sampled with replacement and replicate numbers are kept. If
  \code{how=="item"}, items are sampled with replacement, and their
  observations are included the sampled numner of times.
  }

\value{ The \code{Meth} function returns a \code{Meth} object which is a
  dataframe with columns \code{meth}, \code{item}, (\code{repl}) and \code{y}.
  \code{summary.Meth} returns a table classified by method and no. of
  replicate measurements, extended with columns of the total number of items,
  total number of observations and the range of the measurements.

  The \code{mean.Meth} returns a \code{Meth} object where means have
  been computed over replicates, and put in a variable \code{mean.y}. If
  \code{simplify=TRUE}, a smaller \code{Meth} object will be returned
  with only one row per (meth,item), and the means in the variable
  \code{y}. This is useful if the definition of a particular measurement
  method is the mean of a specified number of replicate measuements.
  The functions \code{mean.Meth}, \code{median.Meth}, \code{max.Meth}, and
  \code{min.Meth} behaves similarly, whereas \code{sort.Meth} just sorts
  the replicates within each (meth,item), and puts the results in a
  variable \code{sort.y} added \code{Meth} object.

  The \code{subset.Meth} returns a subset of the \code{Meth} rows. If a
  subset of the methods is selected, the new \code{meth} variable will
  have levels equal to the actually present levels of \code{meth} in the
  new \code{Meth} object. This is not the case if subsetting is done
  using "\code{[}".
  }
\author{Bendix Carstensen, \email{bxc@steno.dk} }
\examples{
data(fat)
# Different ways of selecting columns and generating replicate numbers
Sub1 <- Meth(fat,meth=2,item=1,repl=3,y=4,print=TRUE)
Sub2 <- Meth(fat,2,1,3,4,print=TRUE)
Sub3 <- Meth(fat,meth="Obs",item="Id",repl="Rep",y="Sub",print=TRUE)
summary( Sub3 )
plot( Sub3 )

# Use observation in different columns as methods
data( CardOutput )
head( CardOutput )
sv <- Meth( CardOutput, y=c("Svo2","Scvo2") )
# Note that replicates are generated if a non-unique item-id is used
sv <- Meth( CardOutput, y=c("Svo2","Scvo2"), item="Age" )
str( sv )
# A summary is not created if the the first argument (data=) is not used:
sv <- Meth( y=CardOutput[,c("Svo2","Scvo2")], item=CardOutput$VO2 )
summary(sv)

# Sample items
ssv <- sample.Meth( sv, how="item", N=8 )

# More than two methods
data( sbp )
plot( Meth( sbp ) )
# Creating non-unique replicate numbers per (meth,item) creates a warning:
data( hba1c )
hb1  <- with( hba1c,
              Meth( meth=dev, item=item, repl=d.ana-d.samp, y=y, print=TRUE ) )
hb2  <- with( subset(hba1c,type=="Cap"),
              Meth( meth=dev, item=item, repl=d.ana-d.samp, y=y, print=TRUE ) )
  }

\keyword{manip}