#' Summarize conversion equations and prediction intervals between methods.
#' 
#' Takes the results from \code{\link{BA.est}}, \code{\link{DA.reg}},
#' \code{\link{AltReg}} or \code{\link{MCmcmc}} and returns a \code{MethComp}
#' object, suitable for displaying the relationship between methods in print pr
#' graphic form.
#' 
#' Using \code{MethComp} on the results from \code{\link{BA.est}} or
#' \code{\link{AltReg}} is not necessary, as these two functions already return
#' objetcs of class \code{MethComp}.
#' 
#' @param obj A \code{MethComp} or \code{\link{MCmcmc}} object.
#' @return \code{MethComp} returns a \code{MethComp} object, which is a list
#' with three elements, \code{Conv}, a three-way array giving the linear
#' conversion equations between methods, \code{VarComp}, a two-way array
#' classified by methods and variance components and \code{data}, a copy of the
#' original \code{\link{Meth}} object supplied --- see the description under
#' \code{\link{BA.est}}.
#' 
#' A \code{MethComp} object has an attribute \code{Transform}, which is either
#' NULL, or a named list with elements \code{trans} and \code{inv}, both of
#' which are functions. The first is the transformation applied to measurements
#' before analysis; the results are all given on the transformed scale. The
#' second is the inverse transformation; this is only used when plotting the
#' resulting relationship between methods.
#' 
#' The methods \code{print}, \code{plot}, \code{lines} and \code{points} return
#' nothing.
#' @author Bendix Carstensen, Steno Diabetes Center, \email{bendix.carstensen@@regionh.dk }.
#' @seealso \code{\link{BA.est}} \code{\link{AltReg}} \code{\link{MCmcmc}}
#' @keywords design
#' @examples
#' 
#' data( ox )
#' BA.ox <- BA.est( ox, linked=TRUE )
#' print( BA.ox )
#' \dontrun{
#' AR.ox <- AltReg( ox, linked=TRUE  )
#' print( AR.ox )
#' plot( AR.ox ) }
#' 
#' @import stats
#' @import utils
#' @import graphics
#' @import grDevices
#' @export
MethComp <-
function( obj )
{
if( inherits( obj, "MethComp" ) )
  {
  Conv    <- obj$Conv
  VarComp <- obj$VarComp
  dfr     <- obj$data
  cls     <- class( obj )
  }
else
if( inherits( obj, "MCmcmc" ) )
  {
  dfr <- attr( obj, "data" )
  # The transformed data are stored in the MCmcmc object so transform
  # back --- well, IF they are transformed
  if( !is.null(attr(obj,"Transform")) ) dfr$y <- attr(obj,"Transform")$inv(dfr$y)
  Obj <- summary( obj )
  ca  <- Obj$conv.array
  dnam <- dimnames(ca)[c(2,1,3)]
  dnam[[3]] <- c( dnam[[3]], "int(t-f)","slope(t-f)","sd(t-f)" )
  # Store the array in a different layout
  # [This is crazy! the layout of the MCmcmc summary should be changed]
  Conv <- array( NA, dimnames=dnam, dim=sapply(dnam,length) )
  Nm <- dim(Conv)[1]
  for( i in 1:3 ) Conv[,,i] <- t(ca[,,i])
  for( i in 1:Nm ) for( j in 1:Nm ) Conv[i,j,4:6] <- y2DA( Conv[i,j,1:3] )
  VarComp <- Obj$VarComp[,-4,1]
  names(dimnames(VarComp)) <- c("Method","s.d.")
  cls <- c("MethComp","fromMCmcmc")
  }
else stop( "Input object (argument) must have class 'MethComp' or 'MCmcmc'.\n",
           "It has class ", class( obj ) )
res <- list( Conv = Conv,
          VarComp = VarComp,
             data = dfr )
class( res ) <- cls
attr( res, "Transform" ) <- attr( obj, "Transform" )
# Make sure that the RandomRaters attribute is always logical
RaRa <- attr( obj, "RandomRaters" )
attr( res, "RandomRaters" ) <- if( is.logical(RaRa) ) RaRa else FALSE
return( res )
}

################################################################################
## print method for MethComp
################################################################################
#' @export
print.MethComp <-
function( x, digits=3, ... )
{
if( !is.null( trans <- attr(x,"Transform") ) )
  cat( "\nNote: Response transformed by: ",
       paste( deparse( trans$trans ), collapse="" ), "\n\n" )
# Conversion table not relevant for random raters
if( !attr(x, "RandomRaters") )
  {
  cat("\n Conversion between methods:\n")
  if( inherits(x,"DA.reg") ) pcols <- c("alpha","beta","sd.pred","beta=1",
                                        "int(t-f)","slope(t-f)","sd(t-f)",
                                        "int(sd)","slope(sd)","sd=K")
  else
  if( inherits(x,"BA.est") ) pcols <- c("alpha","beta","sd.pred",
                                        "LoA-lo", "LoA-up")
  else pcols <- 1:(dim(x$Conv)[3])
  print( round( ftable( x$Conv[,,pcols] ), digits ) )
  }
# Variance component table not relevant for DA.reg where variances are not estimated
if( !is.null( x$VarComp ) )
  {
  cat("\n Variance components (sd):\n")
  print( round( x$VarComp, digits ) )
  }
}

################################################################################
## plot, lines and points for MethComp
################################################################################


#' Summarize conversion equations and prediction intervals between methods.
#' 
#' \code{plot.MethComp} plots the conversion function with prediction limits;
#' always using the original scale of measurements. It also sets the options
#' \code{"MethComp.wh.cmp"} indicating which two methods are plotted and
#' \code{"MethComp.pl.type"} indicating whether a plot of methods against each
#' other or a Bland-Altman type plot of differences versus averages. By default
#' the conversion lines are plotted.
#' 
#' \code{lines.MethComp} and \code{points.MethComp} adds conversion lines with
#' prediction limits and points to a plot.
#' 
#' @param x A \code{MethComp} object.
#' @param wh.comp Numeric or character of length 2. Which two methods should be
#' plotted.
#' @param pl.type Character. If "conv" it will be a plot of two methods against
#' each other, otherwise it will be a plot of the 1st minus the 2nd versus the
#' average; a Bland-Altman type plot.
#' @param dif.type Character. If "lin" (the default) a linear relationship
#' between methods is allowed. Otherwise a constant difference is assumed and
#' LoA can be indicated on the plot.
#' @param sd.type Should the estimated dependence of the SD (from
#' \code{\link{DA.reg}} be used when plotting prediction limits?
#' @param axlim The extent of the axes of the measurements.
#' @param diflim The extent of the axis of the differences.
#' @param points Logical. Should the points be included in the plot.
#' @param repl.conn Logical. Should replcate measurements be connected; this
#' assumes linked replicates.
#' @param col.conn Color of the lines connecting replicates.
#' @param lwd.conn Width of the connection lines.
#' @param grid Should there be a grid? If numerical, gridlines are drawn at
#' these locations.
#' @param N.grid Numeric. How many gridlines? If a vector of length>1, it will
#' be taken as the position of the gridlines.
#' @param col.grid Color of the gridlines.
#' @param col.lines Color of the conversion lines.
#' @param lwd Numerical vector of length 3. Width of the conversion line and
#' the prediction limits.
#' @param pch.points Plot character for points.
#' @param col.points Color of the points.
#' @param eqn Logical. Should the conversion equation be printed on the plot.
#' @param col.eqn Color of the conversion formula
#' @param font.eqn font for the conversion formula
#' @param digits The number of digits after the decimal point in the conversion
#' formulae.
#' @param mult Logical. Should ratios be plotted on a log-scale instead of
#' differences on a linear scale? See description of the argument for
#' \code{\link{BA.plot}}.
#' @param alpha 1 minus the confidence level for the prediction interval. If
#' not given, the prediction interval is constructed as plus/minus twice the
#' SD.
#' @param ... Further arguments.
#' @return \code{MethComp} returns a \code{MethComp} object, which is a list
#' with three elements, \code{Conv}, a three-way array giving the linear
#' conversion equations between methods, \code{VarComp}, a two-way array
#' classified by methods and variance components and \code{data}, a copy of the
#' original \code{\link{Meth}} object supplied --- see the description under
#' \code{\link{BA.est}}.
#' 
#' A \code{MethComp} object has an attribute \code{Transform}, which is either
#' NULL, or a named list with elements \code{trans} and \code{inv}, both of
#' which are functions. The first is the transformation applied to measurements
#' before analysis; the results are all given on the transformed scale. The
#' second is the inverse transformation; this is only used when plotting the
#' resulting relationship between methods.
#' 
#' The methods \code{print}, \code{plot}, \code{lines} and \code{points} return
#' nothing.
#' @author Bendix Carstensen, Steno Diabetes Center, \email{bendix.carstensen@@regionh.dk }.
#' @seealso \code{\link{BA.est}} \code{\link{AltReg}} \code{\link{MCmcmc}}
#' @keywords design
#' @examples
#' 
#' data( ox )
#' BA.ox <- BA.est( ox, linked=TRUE )
#' print( BA.ox )
#' \dontrun{
#' AR.ox <- AltReg( ox, linked=TRUE  )
#' print( AR.ox )
#' plot( AR.ox ) }
#' 
#' @export
plot.MethComp <-
function( x,
      wh.comp = 1:2,
      pl.type = "conv",
     dif.type = "lin",
      sd.type = "const",
        axlim = range(x$data$y,na.rm=TRUE),
       diflim = axlim-mean(axlim),
       points = FALSE,
    repl.conn = FALSE,
     col.conn = "gray",
     lwd.conn = 1,
         grid = TRUE,
       N.grid = 10,
     col.grid = grey(0.9),
          lwd = c(3,1,1),
    col.lines = "black",
   col.points = "black",
   pch.points = 16,
          eqn = is.null(attr(x,"Transform")),
      col.eqn = col.lines,
     font.eqn = 2,
       digits = 2,
         mult = FALSE,
        alpha = NULL,
          ... )
{
# Set the options
if( is.numeric(wh.comp) ) wh.comp <- levels( x$data$meth )[wh.comp]
 pl.type <- ifelse( substr(tolower( pl.type),1,1) == "c", "conv" , "BA" )
dif.type <- ifelse( substr(tolower(dif.type),1,1) == "c", "const", "lin" )
 sd.type <- ifelse( substr(tolower( sd.type),1,1) == "c", "const", "lin" )
options( MethComp.wh.comp = wh.comp,
         MethComp.pl.type = pl.type,
        MethComp.dif.type = dif.type,
         MethComp.sd.type = sd.type )
Mn <- wh.comp

# Check if linear SD is actually possible
if( sd.type == "lin" )
  if( !inherits(x,"DA.reg") )
    {
    sd.type <- "const"
    cat("Variable SD not possible, use DA.reg() or specify model=FALSE\n")
    }

if( pl.type == "conv" )
  # Conversion plot
  {
  plot( NA, xlim=axlim, ylim=axlim, type="n",
            xlab=Mn[2], ylab=Mn[1], ... )
  # Grid?
  if( is.logical( grid ) ) if( grid )
    grid <- if( length(N.grid)>1 ) N.grid else pretty( axlim, n=N.grid )
  abline( h=grid, v=grid, col=col.grid )
  }
else
  # Bland-Altman type plot
  {
  if( mult & any(diflim<=0) ) diflim <- c(0.5,2)
  if( mult & length(diflim)==1 ) diflim <- sort( c(diflim,1/diflim) )
  plot( NA, xlim=axlim, ylim=diflim, type="n", log=if(mult) "y" else "",
            xlab=paste( "(", Mn[1], "+",
                             Mn[2], ") / 2" ),
            ylab=paste( Mn[1], if(mult) "/" else "-", Mn[2] ), ... )
  # Grid?
  if( is.logical( grid ) )
    if( grid )
      {
       grid <- if( length(N.grid)>1 ) N.grid else pretty( axlim,
                                                         n=N.grid )
      if( mult )
      hgrid <- 1:20/10
      else
      hgrid <- pretty( axlim-mean(axlim),
                       n = if( length(N.grid)>1 ) length(N.grid)
                           else N.grid )
      abline( h=hgrid, v=grid, col=col.grid )
      }
  }
box()

## Construct the annotation formulae
if( eqn )
  {
  if( sd.type=="lin" )
    {
    # The conversion formula is multiplicative in the SD,
    # but additivity is what is needed:
    SL <- ( DA2y( x[["Conv"]][Mn[1],Mn[2],"int(t-f)"]+
                  x[["Conv"]][Mn[1],Mn[2],"int(sd)" ],
                  x[["Conv"]][Mn[1],Mn[2],"slope(t-f)"]+
                  x[["Conv"]][Mn[1],Mn[2],"slope(sd)" ] )
          - DA2y( x[["Conv"]][Mn[1],Mn[2],"int(t-f)"],
                  x[["Conv"]][Mn[1],Mn[2],"slope(t-f)"] ) )
    }
  A <- x[["Conv"]][Mn[1],Mn[2],  "alpha"]
  B <- x[["Conv"]][Mn[1],Mn[2],   "beta"]
  S <- x[["Conv"]][Mn[1],Mn[2],"sd.pred"]
  y.x <- paste( Mn[1], " = ",
                formatC( A, format="f", digits=digits ), if( B>0 ) "+",
     if( B!=1 ) formatC( B, format="f", digits=digits ),
                Mn[2], " (",
if( sd.type=="const" )        formatC( S , format="f", digits=digits ),
if( sd.type=="lin"   ) paste( formatC( SL["y1|2",  "int"], format="f", digits=digits ),
                              if( SL["y1|2","slope"]>0 ) "+",
                              formatC( SL["y1|2","slope"], format="f", digits=digits ),
                              Mn[2], sep="" ),
                ")", sep="" )
  A <- x[["Conv"]][Mn[2],Mn[1],  "alpha"]
  B <- x[["Conv"]][Mn[2],Mn[1],   "beta"]
  S <- x[["Conv"]][Mn[2],Mn[1],"sd.pred"]
  x.y <- paste( Mn[2], " = ",
                formatC( A, format="f", digits=digits ), if( B>0 ) "+",
     if( B!=1 ) formatC( B, format="f", digits=digits ),
                Mn[1], " (",
if( sd.type=="const" )        formatC( S , format="f", digits=digits ),
if( sd.type=="lin"   ) paste( formatC( SL["y2|1",  "int"], format="f", digits=digits ),
                              if(  SL["y2|1","slope"]>0 ) "+",
                              formatC( SL["y2|1","slope"], format="f", digits=digits ),
                              Mn[1], sep="" ),
                ")", sep="" )
  A <- x[["Conv"]][Mn[1],Mn[2],  "int(t-f)"]
  B <- x[["Conv"]][Mn[1],Mn[2],"slope(t-f)"]
  S <- x[["Conv"]][Mn[1],Mn[2],   "sd(t-f)"]
  if( sd.type=="lin" )
    {
    Sa <- x[["Conv"]][Mn[1],Mn[2],   "int(sd)"]
    Sb <- x[["Conv"]][Mn[1],Mn[2], "slope(sd)"]
    }
  D.A <- paste( Mn[1], "-", Mn[2], " = ",
                              formatC( A , format="f", digits=digits ), if( B>0 ) "+",
 if( B!=0 ) paste( if( B!=1 ) formatC( B , format="f", digits=digits ),
                              "(", Mn[1], "+", Mn[2], ")/2", sep="" ), " (",
if( sd.type=="const" )        formatC( S , format="f", digits=digits ),
if( sd.type=="lin"   ) paste( formatC( Sa, format="f", digits=digits ), if( Sb>0 ) "+",
                              formatC( Sb, format="f", digits=digits ), "Avg.", sep="" ),
                ")", sep="" )
# Heights and widths of the equations
  wul <- strwidth ( y.x, font=2 )
  hul <- strheight( y.x, font=2 )
  wlr <- strwidth ( x.y, font=2 )
  hlr <- strheight( x.y, font=2 )
  wDA <- strwidth ( D.A, font=2 )
  hDA <- strheight( D.A, font=2 )
  wxp <- 1.1
  hxp <- 2.0

  cn <- par("usr")
  if(mult) cn[3:4] <- 10^cn[3:4]

if( pl.type=="conv" )
  {
  if( is.numeric(grid) )
    {
    rect( cn[1],
          cn[4],
          cn[1] + wxp*wul,
          cn[4] - hxp*hul,
          border="white", col="white" )
    rect( cn[2],
          cn[3],
          cn[2] - wxp*wlr,
          cn[3] + hxp*hlr,
          border="white", col="white" )
    }
  text( cn[1] + wxp/2*wul,
        cn[4] - hxp/2*hul, y.x,
        font=font.eqn, col=col.eqn )
  text( cn[2] - wxp/2*wlr,
        cn[3] + hxp/2*hlr, x.y,
        font=font.eqn, col=col.eqn )
  }
if( pl.type=="BA" )
  {
  if( is.numeric(grid) )
    {
    rect( cn[2],
          cn[4],
          cn[2] -   wxp*max(wlr,wul),
          cn[4] - 2*hxp*max(hlr,hul),
          border="white", col="white" )
    rect( cn[2],
          cn[3],
          cn[2] - wxp*wDA,
          cn[3] + hxp*hDA,
          border="white", col="white" )
    }
  text( cn[2] - wxp/2*max(wlr,wul),
        cn[4] - hxp  *max(hlr,hul),
        paste( y.x, "\n", x.y ),
        font=font.eqn, col=col.eqn )
  text( cn[2] - wxp/2*wDA,
        cn[3] + hxp/2*hDA,
        D.A,
        font=font.eqn, col=col.eqn )
  }
}
if( eqn )
cat( "Relationships between methods:\n",
     D.A, "\n",
     y.x, "\n",
     x.y, "\n" )

              lines.MethComp( x,  col.lines = col.lines,
                                        lwd = lwd,
                                     digits = digits,
                                      alpha = alpha,
                                       mult = mult,
                                        ... )
if( points ) points.MethComp( x, col.points = col.points,
                                 pch.points = pch.points,
                                  repl.conn = repl.conn,
                                   col.conn = col.conn,
                                   lwd.conn = lwd.conn,
                                       mult = mult,
                                        ... )
box()
}

################################################################################
## lines.MethComp
################################################################################
#' @export
lines.MethComp <-
function( x,
      wh.comp = getOption("MethComp.wh.comp"),
      pl.type = getOption("MethComp.pl.type"),
     dif.type = getOption("MethComp.dif.type"),
      sd.type = getOption("MethComp.sd.type"),
    col.lines = "black",
          lwd = c(3,1,1),
       digits = 3,
         mult = FALSE,
        alpha = NULL,
          ... )
{
# Define the transformation
if( is.null( attr( x, "Transform" ) ) )
  trf <- itr <- function( x ) x
else
  {
  trf <- attr( x, "Transform" )$trans
  itr <- attr( x, "Transform" )$inv
  }

# The slope and the sd, used to plot the lines
 A <- x$Conv[wh.comp[1],wh.comp[2],  "alpha"]
 B <- x$Conv[wh.comp[1],wh.comp[2],   "beta"]
 S <- x$Conv[wh.comp[1],wh.comp[2],"sd.pred"]
# The same for the differences
if( "int(t-f)" %in% dimnames(x$Conv)[[3]] )
  { # Is the Conv out of DA.reg?
 a <- x$Conv[wh.comp[1],wh.comp[2],  "int(t-f)"]
 b <- x$Conv[wh.comp[1],wh.comp[2],"slope(t-f)"]
  }
else
  { # If not assume constant difference and constant SD
 a <- A
 b <- 0
  }
# And the same for the sd
if( "int(sd)" %in% dimnames(x$Conv)[[3]] )
  { # Is the Conv out of DA.reg?
Sa <- x$Conv[wh.comp[1],wh.comp[2],   "int(sd)"]
Sb <- x$Conv[wh.comp[1],wh.comp[2], "slope(sd)"]
  }
else
  {
Sa <- S # When slope b is 0, the SD if the diff is the pred.sd.
Sb <- 0
  }
# Define the method-1 points to use, making sure that the points span
# also the range of the BA-type plots:
axlim <- par("usr")[1:2]
# m1 is on the original scale, so is axlim;
# but A, B and S are for transformed measurements
# Expand well beyond the limits to accommodate the differnece-plot too
  m1 <- seq( axlim[1]-diff(axlim), axlim[2]+diff(axlim),, 500 )
trm1 <- trf( m1 )
  df <- nlevels(x$data$item)-1
# If alpha is not given, use 2, otherwise the t quantile
 qnt <- if( is.null(alpha) ) 2 else qt(1-alpha/2,df)
trm2 <- if( substr(sd.type,1,1) == "c" )
            cbind( A+B*trm1, S ) %*% rbind( c(1, 1, 1),
                                            c(0,-1, 1)*qnt )
        else cbind(1,trm1) %*%
             cbind( c(A,B),
                    # Limits coef for DA-reg converted to coefs
                    # for the limits of y1 versus y2
                    DA2y( a-qnt*Sa, b-qnt*Sb )["y1|2",1:2],
                    DA2y( a+qnt*Sa, b+qnt*Sb )["y1|2",1:2] )
  m2 <- itr( trm2 )

if( tolower(substr(pl.type,1,1)) == "c" )
     matlines( m1, m2,
               lwd=lwd, lty=1, col=col.lines )
else matlines( (m1+m2)/2, if(mult) m2/m1 else m2-m1,
               lwd=lwd, lty=1, col=col.lines )

# Transform status to be used when deciding if LoA should be written
no.tr <- is.null(attr(x,"Transform"))
is.log.tr <- FALSE
if( !no.tr ) is.log.tr <- max( abs( attr(x,"Transform")$trans(1:10)-log(1:10) ) ) < 1e-6

if( pl.type=="BA" &
    sd.type=="const" &
    dif.type=="const" &
    inherits(x,c("DA.reg","BA.est")) &
    ( no.tr | ( mult & is.log.tr ) ) )
  {
  LoA <- if(mult) (m2/m1)[length(m1),] else (m2-m1)[length(m1),]
  axis( side=4, at=LoA, labels=formatC(LoA,format="f",digits=digits),
        col=col.lines, col.axis=col.lines, las=1 )
  box()
  }
}

################################################################################
## points.MethComp
################################################################################
#' @export
points.MethComp <-
function( x,
      wh.comp = getOption("MethComp.wh.comp"),
      pl.type = getOption("MethComp.pl.type"),
   col.points = "black",
   pch.points = 16,
    repl.conn = FALSE,
     col.conn = "gray",
     lwd.conn = 1,
         mult = FALSE,
          ... )
{
if( is.numeric(wh.comp) ) wh.comp <- levels(x$data$meth)[wh.comp]
wide <- to.wide( x$data )

if( repl.conn) connect2mean( x$data, wh.comp = wh.comp,
                                     pl.type = pl.type,
                                    col.conn = col.conn,
                                    lwd.conn = lwd.conn )
if( pl.type!="BA" )
  {
  # Conversion plot
  points( wide[,wh.comp[2]], wide[,wh.comp[1]],
          col = col.points,
          pch = pch.points, ... )
  }
else
  {
  # Bland-Altman type plot
  points( (wide[,wh.comp[1]]+wide[,wh.comp[2]])/2,
          if(mult)
           wide[,wh.comp[1]]/wide[,wh.comp[2]]
          else
           wide[,wh.comp[1]]-wide[,wh.comp[2]],
           col = col.points,
           pch = pch.points, ... )
  }
}

################################################################################
## choose.trans
################################################################################

#' Functions to handle transformations of measurement results.
#' 
#' Choose a function and inverse based on a text string
#' 
#' @aliases choose.trans
#' @param tr A character string, or a list of two functions, they should be
#' each other's inverse. Names of the list are ignored.
#' @return \code{choose.trans} returns a named list with two elements "trans"
#' and "inv", both functions which are each other's inverse. This is intended
#' to be stored as an attribute \code{"Transform"} with the resulting object
#' and used in plotting and reporting. All results will be on the transformed
#' scale. If the \code{tr} argument to \code{choose.trans} is a character
#' constant, the appropriate named list of two functions will be generated.
#' Possibilities are: "exp", "log", "logit", "pctlogit" (transforms percentages
#' by the logit), "sqrt", "sq" (square), "cll" (complementary log-minus-log),
#' "ll" (log-minus-log).  If there is no match \code{NULL} is returned, which
#' will correspond to no transformation.
#' 
#' @author Bendix Carstensen, Steno Diabetes Center,
#' \url{https://bendixcarstensen.com/}.
#' @examples
#' 
#' choose.trans( "logit" )
#' 
#' @export choose.trans
choose.trans <-
function( tr )
# Function to allow a character argument to choose a transformation and the
# required inverse.
{
if( is.character(tr) )
  {
  ltr <- switch( tr,
                 log = list( trans = log,
                               inv = exp ),
                 exp = list( trans = exp,
                               inv = log ),
                sqrt = list( trans = sqrt,
                               inv = function(x) x^2 ),
                  sq = list( trans = function(x) x^2,
                               inv = sqrt ),
               logit = list( trans = function(p) log(p/(1-p)),
                               inv = function(x) 1/(1+exp(-x)) ),
            pctlogit = list( trans = function(p) log(p/(100-p)),
                               inv = function(x) 100/(1+exp(-x)) ),
                 cll = list( trans = function(p) log(-log(1-p)),
                               inv = function(x) 1-exp(-exp(x)) ),
                  ll = list( trans = function(p) log(-log(p)),
                               inv = function(x) exp(-exp(x)) ),
                  NULL )
  if( is.null(ltr) ) cat('Transformation "', paste("\b",tr,sep=""),
                         '\b" not known --- none applied.\n')
  }
else
if( is.list(tr) )
  {
  if( is.function(tr[[1]]) & is.function(tr[[2]]) )
    {
    ltr <- tr
    names( ltr ) <- c("trans","inv")
    }
  else stop( "Argument to 'choose.trans' must be character or\n",
             "a list of two functions: the transformtion and its inverse." )
  }
else ltr <- NULL
invisible( ltr )
}

################################################################################
## check.trans
################################################################################

#' Functions to handle transformations of measurement results.
#' 
#' Check whether two functions actually are each others inverse.
#' 
#' @param trans A list of two functions, each other's inverse.
#' @param y Vector of numerical values where the functions should be each
#' other's inverse.
#' @param trans.tol Numerical constant indication how precise the evaulation
#' should be.
#' 
#' @return \code{check.trans} returns nothing.
#' @author Bendix Carstensen, Steno Diabetes Center,
#' \url{https://bendixcarstensen.com/}.
#' 
#' @export check.trans
check.trans <-
function( trans, y, trans.tol=10e-6 )
{
if( any( abs( dif <- y - trans$inv(trans$trans(y)) ) > trans.tol ) )
  stop( "The transformation and its inverse seem not to agree:\n",
        "y - inv(trans(y)) has range ",
        paste( range(dif), collapse=" to " ),
        "\nyou may want to to change the current trans.tol=", trans.tol )
}
