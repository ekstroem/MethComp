#' Regression with errors in both variables (Deming regression)
#'
#' The formal model underlying the procedure is based on a so called functional
#'  relationship:
#'  \deqn{x_i=\xi_i + e_{1i}, \qquad y_i=\alpha + \beta \xi_i + e_{2i}}{x_i=k_i + e_1i, y_i=alpha + beta k_i + e_2i}
#'  with \eqn{\mathrm{var}(e_{1i})=\sigma}{var(e_1i)=s},
#'       \eqn{\mathrm{var}(e_{2i})=\lambda\sigma}{var(e_2i)=VR*s},
#'  where \eqn{\lambda}{VR} is the known variance ratio.
#'
#'  The estimates of the residual variance is based on a weighting of
#'  the sum of squared deviations in both directions, divided by \eqn{n-2}{n-2}.
#'  The ML estimate would use \eqn{2n}{2n} instead, but in the model we actually
#'  estimate \eqn{n+2}{n+2} parameters --- \eqn{\alpha, \beta}{alpha, beta} and
#'  the \eqn{n}{n} \eqn{\xi s}{k_i's}.
#'  This is not in Peter Sprent's book (see references).
#' 
#' @param x a numeric variable 
#' @param y a numeric variable
#' @param vr The assumed known ratio of the (residual) variance of the \code{y}s relative to that of the \code{x}s. Defaults to 1.
#' @param sdr do. for standard deviations. Defaults to 1. \code{vr} takes precedence if both are given.
#' @param boot Should bootstrap estimates of standard errors of parameters be done? If \code{boot==TRUE}, 1000 bootstrap samples are done, if \code{boot} is numeric, \code{boot} samples are made.
#' @param keep.boot Should the 4-column matrix of bootstrap samples be returned? If \code{TRUE}, the summary is printed, but the matrix is returned invisibly. Ignored if \code{boot=FALSE}
#' @param alpha What significance level should be used when displaying confidence intervals?
#'
#' @return If \code{boot==FALSE} a named vector with components
#'  \code{Intercept}, \code{Slope}, \code{sigma.x}, \code{sigma.y}, where \code{x}
#'  and \code{y} are substituted by the variable names.
#'
#'  If \code{boot==TRUE} a matrix with rows \code{Intercept},
#'  \code{Slope}, \code{sigma.x}, \code{sigma.y}, and colums giving the estimates,
#'  the bootstrap standard error and the bootstrap estimate and c.i. as the 0.5,
#'  \eqn{\alpha/2}{alpha/2} and \eqn{1-\alpha/2}{1-alpha/2} quantiles of the sample.
#'  
#'  If \code{keep.boot==TRUE} this summary is printed, but a matrix with columns
#'  \code{Intercept},
#'  \code{Slope}, \code{sigma.x}, \code{sigma.y} and \code{boot} rows is returned.
#'
#' @references Peter Sprent: Models in Regression, Methuen & Co., London 1969, ch.3.4.
#'  
#'  WE Deming: Statistical adjustment of data, New York: Wiley, 1943.
#'
#' @author Bendix Carstensen, Steno Diabetes Center, \email{bendix.carstensen@@regionh.dk}, \url{https://BendixCarstensen.com}
#'
#' @examples
#'
#' 
#' # 'True' values 
#' M <- runif(100,0,5)
#' # Measurements:
#' x <- M + rnorm(100)
#' y <- 2 + 3 * M + rnorm(100,sd=2)
#' # Deming regression with equal variances, variance ratio 2.
#' Deming(x,y)
#' Deming(x,y,vr=2)
#' Deming(x,y,boot=TRUE)
#' bb <- Deming(x,y,boot=TRUE,keep.boot=TRUE)
#' str(bb)
#' # Plot data with the two classical regression lines
#' plot(x,y)
#' abline(lm(y~x))
#' ir <- coef(lm(x~y))
#' abline(-ir[1]/ir[2],1/ir[2])
#' abline(Deming(x,y,sdr=2)[1:2],col="red")
#' abline(Deming(x,y,sdr=10)[1:2],col="blue")
#' # Comparing classical regression and "Deming extreme"
#' summary(lm(y~x))
#' Deming(x,y,vr=1000000)
#'
#' @importFrom stats complete.cases var cov quantile
#' @export
Deming <- function( x, y, vr=sdr^2, sdr=sqrt(vr), boot=FALSE, keep.boot=FALSE, alpha=0.05 ) {
if( missing( vr ) & missing( sdr ) ) var.ratio <- 1
else var.ratio <- vr
vn <- c( deparse( substitute( x ) ),
         deparse( substitute( y ) ) )
pn <- c( "Intercept", "Slope", paste( "sigma", vn, sep="." ) )

alfa <- alpha
dfr <- data.frame( x=x, y=y )
dfr <- dfr[complete.cases(dfr),]
x <- dfr$x
y <- dfr$y
n <- nrow( dfr )
SSDy <- var( y )*(n-1)
SSDx <- var( x )*(n-1)
SPDxy <- cov( x, y )*(n-1)
beta <- ( SSDy - var.ratio*SSDx +
          sqrt( ( SSDy - var.ratio*SSDx )^2 +
                4*var.ratio*SPDxy^2 ) ) / ( 2*SPDxy)
alpha <- mean( y ) - mean( x ) * beta
ksi <- ( var.ratio*x + beta*(y-alpha) )/(var.ratio+beta^2)
sigma.x <- ( var.ratio*sum( (x-ksi)^2 ) +
                       sum( (y-alpha-beta*ksi)^2 ) ) /
# The ML-estiamtes requires 2*n at this point bu we do not think we have that
# many observations so we stick to (n-2). Any corroboation from litterature?
           ( (n-2)*var.ratio )
sigma.y <- var.ratio*sigma.x
sigma.x <- sqrt( sigma.x )
sigma.y <- sqrt( sigma.y )
if( !boot ){
res <- c( alpha, beta, sigma.x, sigma.y )
names( res ) <- pn
res
}
else
{
if( is.numeric( boot ) ) N <- boot else N <- 1000
res <- matrix( NA, N, 4 )
for( i in 1:N )
   {
   wh <- sample( 1:n, n, replace=TRUE )
   res[i,] <- Deming( x[wh], y[wh], vr=var.ratio, boot=FALSE )
   }
ests <- cbind( c(alpha,beta,sigma.x, sigma.y),
               se <- sqrt( diag( cov( res ) ) ),
               t( apply( res, 2, quantile, probs=c(0.5,alfa/2,1-alfa/2 ), na.rm=T ) ) )
colnames( res ) <- rownames( ests ) <- pn
colnames( ests )<- c("Estimate", "S.e.(boot)", colnames(ests)[3:5] )
if(keep.boot)
  {
  print( ests )
  invisible( res )
  }
else
  {
  cat( vn[2], " = alpha + beta*", vn[1], "\n" )
  ests
  }
}
}