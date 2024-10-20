#' Add regression lines to a plot
#' 
#' Add the regression lines of \eqn{y}{y} on \eqn{x}{x} AND \eqn{x}{x} on
#' \eqn{y}{y} to the plot. Optionally add the line obtained by allowing errors
#' in both variables (Deming regression).
#' 
#' 
#' @param x Numeric vector
#' @param y Numeric vector
#' @param Dem Logical. Should the Deming regression line be added too?
#' @param sdr Numeric. The assumed ratio of standard deviations used in the
#' Deming regression.
#' @param col Colour of the lines. Can be a vector of up to 3 elements, one for
#' each line.
#' @param \dots Additional arguments passed on to \code{\link{abline}}, which
#' does the actual plotting.
#' @return None.
#' @author Bendix Carstensen, Steno Diabetes Center,
#' \url{https://BendixCarstensen.com}
#' @seealso \code{\link{abline}}.
#' @keywords manip
#' @examples
#' 
#' data( ox )
#' oxw <- to.wide(ox)
#' attach( oxw )
#' plot( CO, pulse )
#' abline(0,1)
#' bothlines( CO, pulse, Dem=TRUE, col=rainbow(3), lwd=2 )
#' plot( CO, pulse,pch=16 )
#' abline(0,1, col=gray(0.7), lwd=2)
#' bothlines( CO, pulse, Dem=TRUE, col=c(rep("transparent",2),"black"), lwd=2 )
#' 
#' @export bothlines
bothlines <-
function( x, y, Dem=FALSE, sdr=1, col="black", ... )
{
clr <- rep( col, 3 )
if( inherits( x, "lm" ))
  {
  y <- x$model[[1]]
  x <- x$model[[2]]
  }
abline( lm( y ~ x ), col=clr[1], ... )
ic <- coef( lm( x ~ y ) )
abline( -ic[1]/ic[2], 1/ic[2], col=clr[2], ... )
if( Dem )
  {
  Dm <- Deming( x, y, sdr=sdr )
  abline( Dm[1], Dm[2], col=clr[3], ... )
  }
}
