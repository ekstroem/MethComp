#' Make a regression of differences on averages
#' 
#' For each pair of methods in \code{data}, a regression of the differences on
#' the averages between methods is made and a linear relationship between
#' methods with prediction standard deviations is derived.
#' 
#' If the input object contains replicate measurements these are taken as
#' separate items in the order they appear in the dataset.
#' 
#' @param data A \code{\link{Meth}} object. May also be a data frame with
#' columns \code{meth}, \code{item} and \code{y}.
#' @param Transform A character string, or a list of two functions, each
#' other's inverse. The measurements are transformed by this before analysis.
#' Possibilities are: "exp", "log", "logit", "pctlogit" (transforms percentages
#' by the logit), "sqrt", "sq" (square), "cll" (complementary log-minus-log),
#' "ll" (log-minus-log). For further details see \code{\link{choose.trans}}.
#' @param trans.tol The tolerance used to check whether the supplied
#' transformation and its inverse combine to the identity.  Only used if
#' \code{Transform} is a list of two functions.
#' @param print Should the results be printed?
#' @param random.raters If methods really are a random selection of raters,
#' neither intercept nor slope different from 0 are sensible, so if this is
#' \code{TRUE}, intercept and slope in the regression of difference on averages
#' are fixed to 0. Meaning that we are essentially looking at the raw
#' differences as residuals.
#' @param DA.slope If this is TRUE, a slope of the differences in the verages
#' is estimated, otherwise the relationship is assumed constant.
#' @return \code{DA.reg} returns a \code{\link{MethComp}} object, i.e. a list
#' with three components, \code{Conv}, \code{VarComp}, and \code{data}.
#' \code{Conv} is a three-dimensional array, with dimensions \code{To},
#' \code{From} (both with levels equal to the methods in \code{data}) and an
#' unnamed dimension with levels \code{"alpha"}, \code{"beta"},
#' \code{"sd.pred"}, \code{"beta=1"}, referring to the linear relationship of
#' \code{To} to \code{From}, \code{"int(t-f)"}, \code{"slope(t-f)"},
#' \code{"sd(t-f)"}, referring to the regression of the differences on the
#' averages, and \code{"int(sd)"}, \code{"slope(sd)"}, and \code{"s.d.=K"},
#' referring to the regression of the absoulte residuals on the averages, and
#' \code{LoA-lo}, \code{LoA-hi}, the limits of agreement.
#' 
#' Converting from method \eqn{l} to method \eqn{k} using
#' \deqn{y_{k|l}=\alpha+\beta y_l} with prediction standard deviation
#' \eqn{\sigma}, just requires the entries
#' \code{[k,l,c("alpha","beta","sd.pred")]}, if we assume the s.d. is constant.
#' 
#' The next entry is the p-values for the hypothesis \eqn{\beta=1}, intercept
#' and slope of the SD of the differences as a linear function of the average
#' and finally p-value of the hypothesis that standard errors are constant over
#' the range. The latter three are derived by regressing the absolute values of
#' the residuals on the averages, and can be used to produce LoA where the s.d.
#' increases (or decreases) by the mean, using the function \code{DA2y}.
#' 
#' The \code{VarComp} element of the list is \code{NULL}, and only present for
#' compatibility with the print method for \code{MethComp} objects.
#' 
#' The \code{data} element is the input dataframe. The measurements in \code{y}
#' are left un-transformed, even if data are transformed (i.e. if the
#' \code{Transform} attribute of the object is non-null).
#' 
#' \code{DA2y} returns a 2 by 3 matrix with rownames \code{c("y1|2","y2|1")}
#' and columnnames \code{c("int","slope","sd")}, calculated under the
#' assumption that the differences were formed as \code{D <- y1 - y2}.
#' 
#' \code{y2DA} returns a 3-component vector with names
#' \code{c("DA-int","DA-slope","DA-sd")}, referring to differences
#' \code{D=y1-y2} as a linear function of \code{A=(y1+y2)/2}.
#' @author Bendix Carstensen, Steno Diabetes Center, \email{bendix.carstensen@@regionh.dk},
#' \url{https://BendixCarstensen.com/MethComp/}
#' @references B. Carstensen: Comparing methods of measurement: Extending the
#' LoA by regression.  Stat Med, 29:401-410, 2010.
#' @keywords models regression
#' @examples
#' 
#' data( milk )
#' DA.reg( milk )
#' data( sbp )
#' print( DA.reg(sbp), digits=3 )
#' # Slope, intercept : y1 = 0.7 + 1.2*y2 (0.4)
#' A <- c(0.7,1.2,0.4)
#' ( y2DA( A ) )
#' ( DA2y( y2DA( A ) ) )
#' 
#' @export 
DA.reg <-
function( data,
     Transform = NULL,    # Transformation to be applied to y
     trans.tol = 1e-6,
         print = TRUE,
 random.raters = FALSE,
      DA.slope = TRUE )
{
# This function makes regression of differences on averages for all pairs
# of methods and makes ad-hoc test for slope=1 and constant variance
# If DA.slope is FALSE, the model is constrained to slope=0
# If random.raters is TRUE, the model is constrained to slope=0 and intercept=0

# Check that the supplied data is actually a Meth object
dfr <- data <- Meth( data, print=FALSE )

# Transform the response if required
Transform <- choose.trans( Transform )
if( !is.null(Transform) )
  {
  check.trans( Transform, data$y, trans.tol=trans.tol )
  data$y <- Transform$trans( data$y )
  }

# Names and number of methods
Mnam <-  levels( data$meth )
Nm   <- nlevels( data$meth )

# Array to hold the conversion parameters
dnam <- list( "To:" = Mnam,
            "From:" = Mnam,
                      c("alpha","beta","sd.pred","beta=1",
                        "int(t-f)", "slope(t-f)", "sd(t-f)",
                        "int(sd)","slope(sd)","sd=K",
                        "LoA-lo", "LoA-up") )
conv <- array( NA, dim=sapply(dnam,length), dimnames=dnam )

# Fill in the array; first the diagonal
for( i in 1:Nm ) conv[i,i,] <- c(0,1,NA,NA,0,0,rep(NA,6))
# Note that the consistency of ordering comes from the fact the
# calculations are always done uinsg the first occurring factor
# level minus the later occurring (see inside do.Da.reg)
for( i in 1:(Nm-1) ) for( j in (i+1):Nm )
   {
#  Note we need to use Meth() here, in order to reduce the no. of
#  levels of mteh in the subsetted dataframe
   sb <- Meth( data[data$meth %in% Mnam[c(i,j)],c("meth","item","repl","y")], print=FALSE )
   cf <- do.DA.reg( sb, random.raters = random.raters,
                             DA.slope = DA.slope )
   cv <- cbind( DA2y(cf[1:3]), rbind(c(cf[4], cf[1:3],cf[5:7], cf[8:9]),
                                     c(cf[4],-cf[1:3],cf[5:7],-cf[9:8])) )
   conv[i,j,] <- cv[1,]
   conv[j,i,] <- cv[2,]
   }
# Collect the results
res <- list( Conv = conv,
          VarComp = NULL,
             data = dfr )

class( res ) <- c("MethComp","DA.reg")
attr( res, "Transform" ) <- Transform
attr( res, "RandomRaters" ) <- random.raters
res
}

do.DA.reg <-
function( data,
 random.raters = FALSE,
      DA.slope = TRUE )
{
Mnam <- levels( data$meth )
wd   <- to.wide( data, warn=FALSE )
wd   <- wd[complete.cases(wd),]
if( nrow(wd)==0 ) return( rep(NA,10) )
else
{
D <-  wd[,Mnam[1]]-wd[,Mnam[2]]
A <- (wd[,Mnam[1]]+wd[,Mnam[2]])/2
m0 <- if( random.raters ) lm( D ~ -1 )
      else if( DA.slope ) lm( D ~  A )
           else           lm( D ~  1 )
mc <- lm( D ~ 1 )
ms <- lm( abs(residuals(m0)) ~ A )
cf <- if( random.raters ) cbind(c(0,0),matrix(NA,2,3))
      else if( DA.slope ) summary(m0)$coef
           else rbind( summary(m0)$coef, c(0,NA,NA,1) )
res <- c(cf[,1],                              # a, b regressing D on A
         summary(m0)$sigma,                   # residual sd
         cf[2,4],                             # pvalue for b=0
         summary(ms)$coef[1:2,1]*sqrt(pi/2),  # alpha, beta for sd regressed on means
         summary(ms)$coef[2,4],               # pvalue for beta=0
         mc$coef + c(-2,2)*summary(mc)$sigma) # Limits of agreement from mc
return( invisible( res ) )
}
}


#' Convert DA to (classical) regression
#' 
#' The functions \code{DA2y} and \code{y2DA} are convenience functions that
#' convert the estimates of intercept, slope and sd from the regression of
#' \eqn{D=y_1-y_2}{D=y1-y2} on \eqn{A=(y_1+y_2)/2}{A=(y1+y2)/2}, back and forth
#' to the resulting intercept, slope and sd in the relationship between
#' \eqn{y_1}{y1} and \eqn{y_2}{y2}, cf. Carstensen (2010), equation 6.
#' 
#' \code{DA2y} takes the intercept(\code{a}), slope(\code{b}) and sd(\code{s}) from
#' the relationship \code{(y1-y2)=a+b((y1+y2)/2)+e} with sd(\code{e})=\code{s},
#' and returns a two by 3 matrix with columns \code{"int","slope","sd"} and
#' rows \code{"y1|2","y2|1"}.
#' 
#' @param a Intercept in the linear relation of the differences \code{y1-y2} to
#' the averages \code{(y1+y2)/2}. If a vector of length>1, this is used instead
#' of \code{a}, \code{b} and \code{s}, and \code{b} and \code{s} are ignored.
#' @param b Slope in the linear relstion of the differences to the averages.
#' @param s SD from the regression of the differences in the averages. Can be
#' \code{NA}.
#' 
#' @return \code{DA2y} returns a 2 by 3 matrix with rownames \code{c("y1|2","y2|1")}
#' and columnnames \code{c("int","slope","sd")}, calculated under the
#' assumption that the differences were formed as \code{D <- y1 - y2}.
#' @author Bendix Carstensen, Steno Diabetes Center, \email{bendix.carstensen@@regionh.dk},
#' \url{https://BendixCarstensen.com/MethComp/}
#' @references B. Carstensen: Comparing methods of measurement: Extending the
#' LoA by regression.  Stat Med, 29:401-410, 2010.
#'
#' @examples
#' data( milk )
#' DA.reg( milk )
#' data( sbp )
#' print( DA.reg(sbp), digits=3 )
#' # Slope, intercept : y1 = 0.7 + 1.2*y2 (0.4)
#' A <- c(0.7,1.2,0.4)
#' ( y2DA( A ) )
#' ( DA2y( y2DA( A ) ) )
#'
#' @export
DA2y <-
function( a=0, b=0, s=NA )
{
# Convert from D = (y1-y2) = a + b*(y1+y2)/2 (s)
# to the linear relationships between y1 and y2
if( length(a)>1 )
  {
  s <- a[3]
  b <- a[2]
  a <- a[1]
  }
res <- rbind( c(  a, 1+b/2, s ) / (1-b/2),
              c( -a, 1-b/2, s ) / (1+b/2) )
rownames( res ) <- c("y1|2","y2|1")
colnames( res ) <- c("int","slope","sd")
invisible( res )
}

#' Convert DA to (classical) regression
#' 
#' The functions \code{DA2y} and \code{y2DA} are convenience functions that
#' convert the estimates of intercept, slope and sd from the regression of
#' \eqn{D=y_1-y_2}{D=y1-y2} on \eqn{A=(y_1+y_2)/2}{A=(y1+y2)/2}, back and forth
#' to the resulting intercept, slope and sd in the relationship between
#' \eqn{y_1}{y1} and \eqn{y_2}{y2}, cf. Carstensen (2010), equation 6.
#' 
#' #' \code{y2DA} takes intercept(\code{A}), slope(\code{B}) and sd(\code{S}) from
#' the relationship \code{y1=A+B y2 + E} with sd(\code{E})=\code{E}, and
#' returns a vector of length 3 with names
#' \code{"int(t-f)","slope(t-f)","sd(t-f)"}, where \code{t} refers to "to"
#' (\code{y1} and \code{f} to "from" \code{y2}.
#' 
#' @param A Intercept in the linear relation of y1 on y2.
#' @param B Slope in the linear relation of y1 on y2.
#' @param S SD for the linear relation of y1 on y2. Can be \code{NA}.
#' 
#' @return \code{y2DA} returns a 3-component vector with names
#' \code{c("DA-int","DA-slope","DA-sd")}, referring to differences
#' \code{D=y1-y2} as a linear function of \code{A=(y1+y2)/2}.
#' @author Bendix Carstensen, Steno Diabetes Center, \email{bendix.carstensen@@regionh.dk},
#' \url{https://BendixCarstensen.com/MethComp/}
#' @references B. Carstensen: Comparing methods of measurement: Extending the
#' LoA by regression.  Stat Med, 29:401-410, 2010.
#'
#' @examples
#' data( milk )
#' DA.reg( milk )
#' data( sbp )
#' print( DA.reg(sbp), digits=3 )
#' # Slope, intercept : y1 = 0.7 + 1.2*y2 (0.4)
#' A <- c(0.7,1.2,0.4)
#' ( y2DA( A ) )
#' ( DA2y( y2DA( A ) ) )
#'
#' @export
y2DA <-
function( A=0, B=1, S=NA )
{
# Convert from the linear relationship y1 = A + B * y2 (S)
# to the linear relationship between D=y1-y2 and A=(y1+y2)/2
if( length(A)>1 )
  {
  S <- A[3]
  B <- A[2]
  A <- A[1]
  }
res <- c( 2*A, 2*(B-1), 2*S )/(B+1)
names( res ) <- c("int(t-f)","slope(t-f)","sd(t-f)")
invisible( res )
}
