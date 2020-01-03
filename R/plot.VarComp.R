#' Plot the posterior densities for variance components
#' 
#' When a method comparison model i fitted and stored in a \code{\link{MCmcmc}}
#' object, then the posterior distributions of the variance components are
#' plotted, in separate displays for method.
#' 
#' The function generates a series of plots, one for each method compared in
#' the \code{MCmcmc} object supplied (or those chosen by \code{which=}).
#' Therefore the user must take care to set \code{mfrow} or \code{mfcol} to
#' capture all the plots.
#' 
#' @param x A \code{MCmcmc} object.
#' @param which For which of the compared methods should the plot be made?
#' @param lwd.line Line width for drawing the density.
#' @param col.line Color for drawing the densities.
#' @param lty.line Line type for drawing the densities.
#' @param grid Logical. Should a vertical grid be set up? If numeric it is set
#' up at the values specified. If \code{same.ax}, the range of the grid is
#' taken to be the extent of the x-axis for all plots.
#' @param col.grid The color of the grid.
#' @param rug Should a small rug at the bottom show posterior quantiles?
#' @param probs Numeric vector with numbers in the range from 0 to 100,
#' indicating the posterior percentiles to be shown in the rug.
#' @param tot.var Should the posterior of the total variance also be shown?
#' @param same.ax Should the same axes be used for all methods?
#' @param meth.names Should the names of the methods be put on the plots?
#' @param VC.names Should the names of the variance components be put on the
#' first plot (\code{"first"}), the last (\code{"last"}), all (\code{"all"}) or
#' none (\code{"none"}). Only the first letter is needed.
#' @param ... Parameters passed on the \code{\link{density}} furnction that
#' does the smoothing of the posterior samples.
#' @return A list with one element for each method.  Each element of this is a
#' list of densities, i.e. of objects of class \code{density}, one for each
#' variance component.
#' @importFrom graphics mtext lines
#' @importFrom grDevices grey
#' @importFrom stats density 
#'
#' @rdname plot
#' @export
plot.VarComp <-
function( x,
        which,
     lwd.line = rep(2,4),
     col.line = c("red","green","blue","black"),
     lty.line = rep(1,4),
         grid = TRUE, col.grid=gray(0.8),
          rug = TRUE, probs=c(5,50,95),
      tot.var = FALSE,
      same.ax = TRUE,
   meth.names = TRUE,
     VC.names = "first",
          ... )
{
Got.coda <- requireNamespace( "coda" )
if( !Got.coda )
  stop( "Using the plot.VarComp function requires that\n",
        "the package 'coda' is installed.\n",
        "All installed packages are shown if you type 'library()'." )

# A function to plot the posterior densities of the
# estimated variance components.
Mnames <- attr( x, "methods" )
     if( missing(      which ) ) wh <- 1:length(Mnames)
else if( is.numeric(   which ) ) wh <- which
else if( is.character( which ) ) wh <- match( which, Mnames )
Nm <- length( wh )
VN <- toupper( substr( VC.names, 1, 1 ) )
dens <- list()
quan <- list()

# Convert the MCmcmc object to a matrix
if( is.list( x ) ) x <- as.matrix( x[[1]] )
              else x <- as.matrix( x )

# First loop extracts all the densities and the summary quantiles
for( i in 1:Nm )
   {
   wh.col <- intersect( grep( "sigma", colnames( x ) ),
                        grep( Mnames[i], colnames( x ) ) )
   to.col <- intersect( grep( "sigma.tot", colnames( x ) ),
                        grep( Mnames[i], colnames( x ) ) )
   if( !tot.var ) wh.col <- setdiff( wh.col, to.col )
   dns <- list()
   qnt <- list()
   for( j in wh.col )
      {
      dns <- c( dns, list( density( x[,j], ... ) ) )
      qnt <- c( qnt, list( quantile( x[,j], probs=probs/100 ) ) )
      }
   names( dns ) <-
   names( qnt ) <- colnames( x )[wh.col]
   dens <- c( dens, list( dns ) )
   quan <- c( quan, list( qnt ) )
   }
names( dens ) <- Mnames[wh]
Xr <- pmax( 0, range( sapply(dens, function(z) sapply( z, function(x) range(x$x) ) ) ))
if( is.numeric( grid ) ) Xr <- range(grid)
Yr <- c(0,max( sapply(dens, function(z) sapply( z, function(x) max(x$y) ) ) ))
for( i in 1:Nm )
   {
   dns <- dens[[i]]
   qnt <- quan[[i]]
   xr <- pmax(0,range(sapply( dns, function(x) range(x$x) )))
   yr <- c(0,max(sapply( dns, function(x) max(x$y) )))
   plot( NA, axes=FALSE, xlab="", ylab="",
             xlim=if(same.ax) Xr else xr,
             ylim=if(same.ax) Yr else yr )
   if( is.logical(grid) )
     if( grid ) abline( v=pretty(xr), col=col.grid )
   if( is.numeric( grid ) ) abline( v=grid, col=col.grid )
   for( j in 1:length(dns) )
      {
      lines( dns[[j]], lwd=lwd.line[j], lty=lty.line[j], col=col.line[j] )
      if( rug ) axis( side=1, at=qnt[[j]],
                      labels=FALSE, tcl=0.5, col=col.line[j], lwd=lwd.line[j] )
      }
   if( ( VN=="F" & i==1 ) |
       ( VN=="L" & i==Nm ) |
       ( VN=="A" ) )
     {
     xl <- par("usr")[2]
     yl <- par("usr")[3:4]
     for( j in 1:length(dns) )
        text( xl, yl %*% c(j,15-j)/15,
              switch( substr(names(dns),7,9)[j],
                      "mir" = "Residual",
                      "mi[" = "Meth-Item",
                      "ir[" = "Item-Repl",
                      "tot" = "Total" ),
              col=col.line[j], adj=1, font=2 )
     }
   axis( side=1 )
   if( meth.names ) mtext( Mnames[i], side=3, adj=1, font=2 )
   }
invisible( dens )
}