#' Compute Lin's Total deviation index
#'
#' This index calculates a value such that a certain fraction of difference between methods will be numerically smaller than this. The TDI is a measure which esentially is a number K such that the interval [-K,K] contains the limits of agreement.
#'
#' If \code{boot==FALSE} a single number, the TDI is returned.
#' If \code{boot} is a number, the median and the 1-\code{alpha}/2 central interval
#' based on \code{boot} resamples are returned too, in a named vector of length 4.
#'
#' @param y1 Measurements by one method.
#' @param y2 Measurements by the other method.
#' @param p The fraction of items with differences numerically exceeding the TDI
#' @param boot If numerical, this is the number of bootstraps. If \code{FALSE} no confidence interval for the TDI is produced.
#' @param alpha 1 - confidende degree.
#'
#' @return A list with 3 components. The names of the list are preceeded by the
#'  criterion percentage, i.e. the percentage of the population that the TDI is
#'  devised to catch.
#'  \item{TDI}{The numerically computed value for the TDI. If \code{boot} is
#'             numeric, a vector of median and a bootstrap c.i. is appended.}
#'  \item{TDI}{The approximate value of the TDI}
#'  \item{Limits of Agreement}{Limits of agreement}
#' @references LI Lin: Total deviation index for measuring individual agreement with applications in laboratory performance and bioequivalence, Statistics in Medicine, 19, 255-270 (2000)
#' @author Bendix Carstensen, \email{bendix.carstensen@@regionh.dk}
#'
#' @examples
#'
#' data(plvol)
#' pw <- to.wide(plvol)
#' with(pw,TDI(Hurley,Nadler))
#'
#' @importFrom stats qnorm pnorm uniroot
#' @export
TDI <-
function( y1, y2, p = 0.05, boot=1000, alpha = 0.05 )
{
  if( length(y1) != length(y2) )
    stop( "Lengths of y1 and y2 must be the same!" )

  # Analytical approximation
  mu  <- mean(y1-y2)
  sigma <- sd(y1-y2)
  TDI.appr <- qnorm(1-p/2)*sqrt(mu^2+sigma^2)
  names( TDI.appr ) <- "(approximate)"

  # Limits of agreement
  LoA <- mu + c(-1,1)*qnorm(1-p/2)*sigma
  names( LoA ) <- c("lower","upper")

  # Numerical calculation
  FF <- function( x ) pnorm( (x-mu)/sigma ) - pnorm( (-x-mu)/sigma ) - (1-p)
  int <- 2 * max(abs(LoA)) * c(-1,1)
  TDI.num <- uniroot( FF, int )$root
  names( TDI.num ) <- "(numeric)"

  # Bootstrap c.i.
  if( is.numeric(boot) )
    {
    nn <- length(y1)
    tdi <- numeric(boot)
    for( i in 1:boot )
       {
       wh <- (1:nn)[sample(1:nn,nn,replace=T)]
       tdi[i] <- TDI( y1[wh], y2[wh], p=p, boot=FALSE )[[1]]
       }
    TDI.num <- c(TDI.num, quantile( tdi, c(0.5,alpha/2,1-alpha/2) ))
    }

  # Put it all together
  res <- list( c(TDI.appr,TDI.num), LoA )
  names( res ) <- c( paste((1-p)*100,"% TDI",sep=""),
                     paste((1-p)*100,"% Limits of Agreement",sep="") )
  return( res )
}