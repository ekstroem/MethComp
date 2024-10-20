#' Bland-Altman plot of differences versus averages.
#' 
#' For two vectors of equal length representing measurements of the same
#' quantity by two different methods, the differences are plotted versus the
#' average. The limits of agreement (prediction limits for the differences) are
#' plotted, optionally a regression of differences of means is given too. Works
#' with \code{\link{Meth}} and \code{\link{MethComp}} objects too.
#' 
#' A plot of the relationship between the methods is produced; either a
#' Bland-Altman plot of the differences versus averages, or a 45 degree
#' rotation as a conversion between the methods. If \code{model=NULL} a simple
#' regression of averages on differences is made by calling \code{DA.reg}, and
#' the specified conversion plotted.
#' 
#' @param y1 Numerical vector of measurements by 1st method. Can also be a
#' \code{\link{Meth}} or a \code{\link{MethComp}} object, see details.
#' @param y2 Numerical vector of measurements by 2nd method. Must of same
#' length as \code{x}. Ignored if a \code{\link{Meth}} or a
#' \code{\link{MethComp}} objects is given for \code{y1}.
#' @param meth.names Label for the method names.
#' @param wh.comp Which methods should be compared. Either numerical or
#' character.
#' @param pl.type What type of plot should be made, \code{"BA"} for differences
#' versus averages, \code{"conv"} for method 1 versus method 2.
#' @param dif.type How should difference depend on the averages. \code{"const"}
#' or \code{"lin"}.
#' @param sd.type How should the standard deviation depend on the averages.
#' \code{"const"} or \code{"lin"}.
#' @param model Should a variance component model be used to compute the limits
#' of agreement? If \code{NULL} a simple analysis is made; other possibilities
#' are \code{"exch"} or \code{"linked"} for exchangeable or linked replicates.
#' @param eqax Should the axes be identical? If a Bland-Altman plot is drawn,
#' the axis for the differences will have the same extent as the axis for the
#' averages, but centered on 0 (see \code{diflim}).
#' @param axlim The limits of the axes.
#' @param diflim The limits of the difference axis.
#' @param grid Should a grid be drawn? If numeric it indicates the places where
#' the grid is drawn.
#' @param N.grid How many grid-lines should be drawn.
#' @param col.grid What should be the color of the grid?
#' @param points Logical. Should the observed points be drawn?
#' @param col.points What color should they have?
#' @param cex.points How large should they be?
#' @param pch.points What plot character for the points
#' @param lwd Numerical vector of 3, giving the width of the conversion line
#' (mean difference) and the limits of agreement.
#' @param col.lines What color should the lines have.
#' @param repl.conn Should replicate measurements be connected (within items)?
#' @param col.conn Color of connecting lines.
#' @param lwd.conn Width of connecting lines.
#' @param xlab x-axis label.
#' @param ylab y-axis label.
#' @param eqn Logical. Should the equations linking the methods be shown on the
#' plot? If a Bland-Altman plot is made, both the equations linking the methods
#' and the equation for the differences versus the averages are shown.
#' @param col.eqn Color for equations
#' @param font.eqn Font for equations
#' @param digits How many digits after the decimal point should be used when
#' showing the equations.
#' @param Transform Transformation applied to data prior to analysis. Plots are
#' made on the original scale after back-transformation.
#' @param mult Logical. If TRUE, ratios of measurement instead of differences
#' will be plotted in the Bland-Altman plot on a logarithmic axis, and limits
#' of agreement will be given on this scale?  This gives the same analysis as
#' using \code{Transform="log"}, but a different plot. Using another
#' transformation than the log is accommodated, but no LoA is shown on the
#' axis.
#' @param alpha 1 minus the confidence level. If \code{NULL} a multiplier of 2
#' is used for constructing prediction limits, otherwise a t-quantile with d.f.
#' equal th number of items minus 1.
#' @param ... Further parameters passed on to \code{\link{plot.MethComp}}
#' @return An object of class \code{\link{MethComp}} and either \code{DA.reg}
#' (if \code{model=NULL}) or \code{BA.est} (if \code{model} is character).
#' @author Bendix Carstensen \email{bendix.carstensen@@regionh.dk},
#' \url{https://BendixCarstensen.com}.
#' @seealso \code{\link{BA.est}}, \code{\link{DA.reg}}, \code{\link{MCmcmc}}.
#' @references JM Bland and DG Altman: Statistical methods for assessing
#' agreement between two methods of clinical measurement, Lancet, i, 1986, pp.
#' 307-310.
#' 
#' JM Bland and DG Altman. Measuring agreement in method comparison studies.
#' Statistical Methods in Medical Research, 8:136-160, 1999.
#' 
#' B Carstensen: Comparing methods of measurement: Extending the LoA by
#' regression. Stat Med. 2010 Feb 10;29(3):401-10.
#' @keywords models design
#' @examples
#' 
#' data( ox )
#' ox <- Meth( ox )
#' # The simplest possible Bland-Altman plot
#' BA.plot( ox )
#' 
#' ## With bells and whistles, comparing the naive and model
#' par( mfrow=c(2,2) )
#' BA.plot( ox, model=NULL, repl.conn=TRUE, col.lines="blue",
#'          axlim=c(0,100), diflim=c(-50,50), xaxs="i", yaxs="i",
#'          las=1, eqn=TRUE, dif.type="lin", pl.type="BA", sd.type="lin",
#'          grid=1:9*10, digits=3,font.eqn=1)
#' par(new=TRUE)
#' BA.plot( ox, model="linked", repl.conn=TRUE, col.lines="red",
#'          axlim=c(0,100), diflim=c(-50,50), xaxs="i", yaxs="i",
#'          las=1, eqn=FALSE, dif.type="lin", pl.type="BA", sd.type="lin",
#'         grid=1:0*10, digits=3)
#' BA.plot( ox, model=NULL, repl.conn=TRUE, col.lines="blue",
#'          axlim=c(0,100), diflim=c(-50,50), xaxs="i", yaxs="i",
#'          las=1, eqn=TRUE, dif.type="lin", pl.type="conv", sd.type="lin",
#'         grid=1:9*10, digits=3,font.eqn=1)
#' par(new=TRUE)
#' BA.plot( ox, model="linked", repl.conn=TRUE, col.lines="red",
#'          axlim=c(0,100), diflim=c(-50,50), xaxs="i", yaxs="i",
#'          las=1, eqn=FALSE, dif.type="lin", pl.type="conv", sd.type="lin",
#'          grid=1:9*10, digits=3)
#' # The same again, but now logit-transformed
#' BA.plot( ox, model=NULL, repl.conn=TRUE, col.lines="blue",
#'          axlim=c(0,100), diflim=c(-50,50), xaxs="i", yaxs="i",
#'          las=1, eqn=TRUE, dif.type="lin", pl.type="BA", sd.type="lin",
#'          grid=1:9*10, digits=3,font.eqn=1,Transform="pctlogit")
#' par(new=TRUE)
#' BA.plot( ox, model="linked", repl.conn=TRUE, col.lines="red",
#'          axlim=c(0,100), diflim=c(-50,50), xaxs="i", yaxs="i",
#'          las=1, eqn=FALSE, dif.type="lin", pl.type="BA", sd.type="lin",
#'          grid=1:0*10, digits=3,Transform="pctlogit")
#' BA.plot( ox, model=NULL, repl.conn=TRUE, col.lines="blue",
#'          axlim=c(0,100), diflim=c(-50,50), xaxs="i", yaxs="i",
#'          las=1, eqn=TRUE, dif.type="lin", pl.type="conv", sd.type="lin",
#'          grid=1:9*10, digits=3,font.eqn=1,Transform="pctlogit")
#' par(new=TRUE)
#' BA.plot( ox, model="linked", repl.conn=TRUE, col.lines="red",
#'          axlim=c(0,100), diflim=c(-50,50), xaxs="i", yaxs="i",
#'          las=1, eqn=FALSE, dif.type="lin", pl.type="conv", sd.type="lin",
#'          grid=1:9*10, digits=3,Transform="pctlogit")
#' 
#' @rdname plot
#' @export
BA.plot <-
function( y1, y2, meth.names = NULL,
                     wh.comp = 1:2,
                     pl.type = "BA",
                    dif.type = "const",
                     sd.type = "const",
                       model = if( inherits(y1,"Meth") & has.repl(y1) ) "exch"
                               else NULL,
                        eqax = FALSE,
                       axlim = if( is.data.frame(y1) ) range(y1$y) else range(c(y1,y2)),
                      diflim = NULL,
                        grid = TRUE,
                      N.grid = 10,
                    col.grid = grey(0.9),
                      points = TRUE,
                  col.points = "black",
                  cex.points = 1,
                  pch.points = 16,
                         lwd = c(3,1,1),
                   col.lines = "blue",
                   repl.conn = FALSE,
                    col.conn = "gray",
                    lwd.conn = 1,
                        xlab = NULL,
                        ylab = NULL,
                         eqn = FALSE,
                     col.eqn = col.lines,
                    font.eqn = 2,
                      digits = 2,
                   Transform = if( mult ) "log" else NULL,
                        mult = FALSE,
                       alpha = NULL,
                         ... )
{
# Allow sloppy definition of arguments
 pl.type <- ifelse( tolower( substr( pl.type,1,1) ) == "c", "conv" , "BA"  )
dif.type <- ifelse( tolower( substr(dif.type,1,1) ) == "c", "const", "lin" )
 sd.type <- ifelse( tolower( substr( sd.type,1,1) ) == "c", "const", "lin" )
 if( !is.null(model) )
   model <- ifelse( tolower( substr(   model,1,1) ) == "l", "linked", "exch" )

if( is.vector( y1 ) )
  # If we have a vector, check if it has the same length as the second argument
  if( length(y1)!=length(y2) )
    stop( "Arguments y1 and y2 must have same length, but",
          "length(y1)=",  length(y1),
        ", length(y2)=",  length(y2) )
  else
  {
  # And if they are of same length, make a Meth object out of it using
  # supplied names if givem
  tmp <- data.frame(y1,y2)
  if( is.character(meth.names) ) names(tmp) <- meth.names
  y1 <- Meth( tmp, y=1:2, print=FALSE )
  }

if( is.data.frame( y1 ) )
  {
  # If the dataframe is not a Meth object make it, Meth will take
  # care of the possible errors if the right columns are not there.
  if( !inherits(y1,"Meth") )
    y1 <- Meth( y1, print=FALSE )
  # Select the two methods to compare and subset the Meth object to
  # the two methods that we plot, and make sure wh.comp hold their names
  if( is.numeric(wh.comp) ) wh.comp <- levels(y1$meth)[wh.comp]
  obj <- Meth( y1[y1$meth %in% wh.comp,], print=FALSE )
  }

else stop("Wrong data structrue for y1 supplied: str(y1):", str(y1) )

# So we turn this into a MethComp object
if( is.null(model) )
M.obj <- DA.reg( obj, DA.slope = dif.type=="lin",
                      Transform = Transform )
else
M.obj <- BA.est( obj, linked = model=="linked",
                      Transform = Transform )

# Then we compute various parameters for the plotting

# axlim:
if( is.null(axlim) ) axlim <- range( obj$y )

# diflim:
if( is.null(diflim) ) diflim <- axlim - mean(axlim)

# And then we can use the default machinery to plot this:
plot.MethComp( M.obj,
             wh.comp = wh.comp,
             pl.type = pl.type,
            dif.type = dif.type,
             sd.type = sd.type,
               axlim = axlim,
              diflim = diflim,
              points = points,
                grid = grid,
              N.grid = N.grid,
            col.grid = col.grid,
                 lwd = lwd,
           col.lines = col.lines,
          col.points = col.points,
          pch.points = pch.points,
           repl.conn = repl.conn,
            col.conn = col.conn,
            lwd.conn = lwd.conn,
                 eqn = eqn,
             col.eqn = col.eqn,
            font.eqn = font.eqn,
              digits = digits,
               alpha = alpha,
                mult = mult,
                 ... )

attr( M.obj, "pl.type" ) <- c( pl.type =  pl.type,
                              dif.type = dif.type,
                               sd.type =  sd.type,
                                 model = model )
invisible( M.obj )
}

######################################################################
# Utility to connect points to means.

connect2mean <-
function( obj, wh.comp,
               pl.type = "conv",
              col.conn = "gray",
              lwd.conn = 1,
                   ... )
{
wob <- to.wide( obj )
              # The points
wob <- cbind( wob[,wh.comp[2]],
              wob[,wh.comp[1]],
              # - and the item averages
          ave(wob[,wh.comp[2]],wob[,"item"]),
          ave(wob[,wh.comp[1]],wob[,"item"]) )
# Convert to D-A coordinates if required
if( pl.type == "BA" )
  {
  ADmat <- rbind(c(0.5,-1),
                 c(0.5, 1))
  wob <- wob %*% rbind( cbind( ADmat, ADmat*0 ),
                        cbind( ADmat*0, ADmat ) )
  }
segments( wob[,1], wob[,2], wob[,3], wob[,4],
          col = col.conn, lwd=lwd.conn, ... )
}
