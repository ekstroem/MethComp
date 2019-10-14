\name{DA.reg}
\alias{DA.reg}
\alias{DA2y}
\alias{y2DA}
\title{Make a regression of differences on averages}
\description{
  For each pair of methods in \code{data}, a regression of the differences on the
  averages between methods is made and a linear relationship between methods
  with prediction standard deviations is derived.
  }
\usage{
  DA.reg(data,
     Transform = NULL,
     trans.tol = 1e-6,
         print = TRUE,
 random.raters = FALSE,
      DA.slope = TRUE )
  DA2y( a=0, b=0, s=NA )
  y2DA( A=0, B=1, S=NA )
  }
\arguments{
  \item{data}{A \code{\link{Meth}} object. May also be a data frame with columns
              \code{meth}, \code{item} and \code{y}.}
  \item{Transform}{A character string, or a list of two functions, each other's
                   inverse. The measurements are transformed by this before
                   analysis. Possibilities are: "exp", "log", "logit",
                   "pctlogit" (transforms percentages by the logit), "sqrt",
                   "sq" (square), "cll" (complementary log-minus-log), "ll"
                   (log-minus-log). For further details see
                   \code{\link{choose.trans}}.}
  \item{trans.tol}{The tolerance used to check whether the supplied
                   transformation and its inverse combine to the identity.
                   Only used if \code{Transform} is a list of two
		   functions.}
  \item{print}{Should the results be printed?}
  \item{random.raters}{If methods really are a random selection of
    raters, neither intercept nor slope different from 0 are sensible, so
    if this is \code{TRUE}, intercept and slope in the regression of
    difference on averages are fixed to 0. Meaning that we are
    essentially looking at the raw differences as residuals.}
  \item{DA.slope}{If this is TRUE, a slope of the differences in the verages
        is estimated, otherwise the relationship is assumed constant.}
  \item{a}{Intercept in the linear relation of the differences
    \code{y1-y2} to the averages \code{(y1+y2)/2}. If a vector of length>1, this is
    used instead of \code{a}, \code{b} and \code{s}, and \code{b} and
    \code{s} are ignored.}
  \item{b}{Slope in the linear relstion of the differences to the averages.}
  \item{s}{SD from the regression of the differences in the
    averages. Can be \code{NA}.}
  \item{A}{Intercept in the linear relation of y1 on y2.}
  \item{B}{Slope in the linear relation of y1 on y2.}
  \item{S}{SD for the linear relation of y1 on y2. Can be \code{NA}.}
}
\details{
  If the input object contains replicate measurements these are taken as separate
  items in the order they appear in the dataset.

  The functions \code{DA2y} and \code{y2DA} are convenience functions
  that convert the estimates of intercept, slope and sd from the
  regression of \eqn{D=y_1-y_2}{D=y1-y2} on
  \eqn{A=(y_1+y_2)/2}{A=(y1+y2)/2}, back and forth to the resulting
  intercept, slope and sd in the relationship between \eqn{y_1}{y1} and
  \eqn{y_2}{y2}, cf. Carstensen (2010), equation 6.

  \code{DA2y} takes intercept(\code{a}), slope(\code{b}) and
  sd(\code{s}) from the relationship \code{(y1-y2)=a+b((y1+y2)/2)+e}
  with sd(\code{e})=\code{s}, and returns a two by 3 matrix with columns
  \code{"int","slope","sd"} and rows \code{"y1|2","y2|1"}.

  \code{y2DA} takes intercept(\code{A}), slope(\code{B}) and
  sd(\code{S}) from the relationship \code{y1=A+B y2 + E}
  with sd(\code{E})=\code{E}, and returns a vector of length 3 with names
  \code{"int(t-f)","slope(t-f)","sd(t-f)"}, where \code{t} refers to
  "to" (\code{y1} and \code{f} to "from" \code{y2}.
  }
  \value{\code{DA.reg} returns a \code{\link{MethComp}} object, i.e. a
       list with three components, \code{Conv}, \code{VarComp}, and
       \code{data}. \code{Conv} is a three-dimensional array, with
       dimensions \code{To}, \code{From} (both with levels equal to the
       methods in \code{data}) and an unnamed dimension with levels
       \code{"alpha"}, \code{"beta"}, \code{"sd.pred"}, \code{"beta=1"},
       referring to the linear relationship of \code{To} to \code{From},
       \code{"int(t-f)"}, \code{"slope(t-f)"}, \code{"sd(t-f)"},
       referring to the regression of the differences on the averages,
       and \code{"int(sd)"}, \code{"slope(sd)"}, and \code{"s.d.=K"},
       referring to the regression of the absoulte residuals on the
       averages, and \code{LoA-lo}, \code{LoA-hi}, the limits of
       agreement.

       Converting from method \eqn{l} to method \eqn{k} using
       \deqn{y_{k|l}=\alpha+\beta y_l} with prediction standard
       deviation \eqn{\sigma}, just requires the entries
       \code{[k,l,c("alpha","beta","sd.pred")]}, if we assume the s.d. is
       constant.

       The next entry is the p-values for the hypothesis \eqn{\beta=1},
       intercept and slope of the SD of the differences as a linear
       function of the average and finally p-value of the hypothesis
       that standard errors are constant over the range. The latter
       three are derived by regressing the absolute values of the
       residuals on the averages, and can be used to produce LoA where
       the s.d. increases (or decreases) by the mean, using the function \code{DA2y}.

       The \code{VarComp} element of the list is \code{NULL}, and only present
       for compatibility with the print method for \code{MethComp} objects.

       The \code{data} element is the input dataframe. The measurements
       in \code{y} are left un-transformed, even if data are transformed (i.e. if
       the \code{Transform} attribute of the object is non-null).

       \code{DA2y} returns a 2 by 3 matrix with rownames
       \code{c("y1|2","y2|1")} and columnnames \code{c("int","slope","sd")},
       calculated under the assumption that the differences were formed as
       \code{D <- y1 - y2}.

       \code{y2DA} returns a 3-component vector with names
       \code{c("DA-int","DA-slope","DA-sd")}, referring to differences
       \code{D=y1-y2} as a linear function of \code{A=(y1+y2)/2}.
     }
\references{
  B. Carstensen:
  Comparing methods of measurement: Extending the LoA by regression.
  Stat Med, 29:401-410, 2010.
}
\author{Bendix Carstensen, Steno Diabetes Center, \code{bxc$steno.dk},
  \url{http://BendixCarstensen.com/MethComp}
}
\examples{
data( milk )
DA.reg( milk )
data( sbp )
print( DA.reg(sbp), digits=3 )
# Slope, intercept : y1 = 0.7 + 1.2*y2 (0.4)
A <- c(0.7,1.2,0.4)
( y2DA( A ) )
( DA2y( y2DA( A ) ) )
}
\keyword{models}
\keyword{regression}
