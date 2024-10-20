#' Plot Meth obejct
#'
#' @param x A \code{Meth} object.
#' @param which A vector of indices or names of methods to plot. If \code{NULL}
#' all methods in the object are plotted.
#' @param col.LoA What color should be used for the limits of agreement.
#' @param col.pt What color should be used for the points.
#' @param cex.name Character expansion factor for plotting method names
#' @param var.range The range of both axes in the scatter plot and the x-axis
#' in the Bland-Altman plot be?
#' @param diff.range The range of yaxis in the Bland-Altman plot. Defaults to a
#' range as the x-axis, but centered around 0.
#' @param var.names If logical: should the individual panels be labelled with
#' the variable names?. If character, then the values of the character will be
#' used to label the methods.
#' @param pch Plot character for points.
#' @param cex Plot charcter expansion for points.
#' @param Transform Transformation used to the measurements prior to plotting.
#' Function or character, see \code{\link{choose.trans}} for possible values.
#' @param ... parameters passed on
#' to both the panel function plotting methods against each other, as well as
#' to those plotting differences against means.
#' @return A plot as a side effect
#' @author Bendix Carstensen, \email{bendix.carstensen@@regionh.dk}
#'
#' @importFrom stats sd
#' @importFrom graphics abline points text box axis plot par
#' @rdname plot
#' @export
plot.Meth <-
function( x,
      which = NULL,
    col.LoA = "blue",
     col.pt = "black",
   cex.name = 2,
  var.range,
 diff.range,
  var.names = FALSE,
        pch = 16,
        cex = 0.7,
  Transform,
        ... )
{
# Should we transform data?
if( !missing(Transform) )
  {
  if( is.character(Transform) ) Transform <- choose.trans(Transform)$trans
  if( !is.function(Transform) ) stop( "Transform= must be of mode character or function\n" )
  x$y <- Transform( x$y )
  }
# Drop all unneeded variables in order to avoid warnings
x <- x[,c("meth","item","repl","y")]
# Wide-ify data to allow plotting
data <- to.wide( x )
if( is.null(which) )
    which <- match( levels(x$meth), names(data) )

# Should we plot the variable names
if( is.logical( var.names ) )
  {
  plot.names <- var.names
  if( is.character( which ) ) var.names <- which
  else var.names <- names( data )[which]
  }
else plot.names <- TRUE

# Functions to plot the upper and lower panels
pnl <-
function( x, y, ... )
{
abline( 0, 1, col="black" )
points( x, y, pch=pch, cex=cex, col=col.pt, ... )
}

pnu <-
function( x, y, ... )
{
sdd <-   sd( (y-x), na.rm=TRUE )
mnd <- mean( (y-x), na.rm=TRUE )
abline( h=mnd+(-1:1)*2.00*sdd, col=col.LoA )
abline( h=0,col="black" )
points( (x+y)/2, y-x, pch=pch, cex=cex, col=col.pt, ... )
}

pldat <- data[,which]
nvar <- ncol( pldat )
if( missing(var.range) )
  rg <- range( pldat, na.rm=TRUE )
  else rg <- var.range
if( missing(diff.range) )
  dif.rg <- c(-1,1)*diff(rg)/2
  else if( length( diff.range )==1 ) dif.rg <- c(-1,1)*abs(diff.range)
       else dif.rg <- diff.range

# Make a grid of plots preserving the old plotting paramters
oma <- par("mar")
oldpar <- par( mfrow=c(nvar,nvar), mar=c(0,0,0,0), oma=c(4,4,4,4), las=1 )
on.exit( par ( oldpar ) )
for( ir in 1:nvar ) for( ic in 1:nvar )
   {
   if( ir == ic )
     {
     plot( 0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE )
     text( 0.5, 0.5, names( pldat )[ir], font=2, cex=cex.name )
     box()
     }
   if( ir < ic )
     {
     plot( 0:1, 0:1, xlim=rg, ylim=dif.rg,
           type="n", xlab="", ylab="", axes=FALSE )
     pnu( pldat[,ir], pldat[,ic], ... )
     if( plot.names )
       {
       text( rg[1], dif.rg[2], paste(var.names[ic],"-",var.names[ir]), adj=c(0,1) )
       text( rg[2], dif.rg[1], paste("(",var.names[ic],"+",var.names[ir],")/2"), adj=c(1,0) )
       }
     if( ir == nvar ) axis( side=1 )
     if( ic == 1 )    axis( side=2 )
     if( ir == 1 )    axis( side=3 )
     if( ic == nvar ) axis( side=4 )
     box()
     }
   if( ir > ic )
     {
     plot( 0:1, 0:1, xlim=rg, ylim=rg,
           type="n", xlab="", ylab="", axes=FALSE )
     pnl( pldat[,ic], pldat[,ir], ... )
     if( plot.names )
       {
       text( rg[1], rg[2], var.names[ir], adj=c(0,1) )
       text( rg[2], rg[1], var.names[ic], adj=c(1,0) )
       }
     if( ir == nvar ) axis( side=1 )
     if( ic == 1 )    axis( side=2 )
     if( ir == 1 )    axis( side=3 )
     if( ic == nvar ) axis( side=4 )
     box()
     }
   }
}
