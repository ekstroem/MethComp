# The new method functions (pirs is already a method
# NOTE: "trace" is a debugging function from the base package
#       which will be masked by this one
trace <- function (obj, ...) UseMethod("trace")
post  <- function (obj, ...) UseMethod("post")




#' Functions to graphically assess the convergence of the MCMC-simulation in a
#' MCmcmc object
#' 
#' These functions display traces for the relevant subset of the parameters in a MCmcmc object.
#' 
#' A \code{lattice} plot is returned, which means that it must \code{print}ed
#' when these functions are called in a batch program or inside another
#' function or for-loop.
#' 
#' \code{trace} plots traces of the sampled chains, \code{post} plots posterior
#' densities of the parameters and \code{pairs} plots a scatter-plot matrix of
#' bivariate marginal posterior distributions.
#' 
#' @aliases check.MCmcmc trace.MCmcmc post.MCmcmc
#' @param obj A \code{MCmcmc} object.
#' @param what Character indicating what parameters to plot.  Possible values
#' are \code{"sd"} or \code{"var"} which gives plots for the variance
#' components (on the sd. scale), \code{"beta"} or \code{"slope"}, which gives
#' plots for slope parameters and \code{"alpha"} or \code{"int"}, which gives
#' plots for the intercept parameters.
#' @param scales Character vector of length two, with possible values "same" or
#' "free", indicating whether x- and y-axes of the plots should be constrained
#' to be the same across panels. For \code{pairs} only the first element is
#' used to decide whether all panles should have the same axes.
#' @param layout Character. If \code{"col"} parameters are displayed columnwise
#' by method, if \code{"row"} they are displayed row-wise.
#' @param aspect How should the panels be scaled. Default (\code{"fill"}) is to
#' make a panels take up as much place as possible.
#' @param \dots Further aruments passed on to the \code{\link[lattice]{Lattice}} package
#' @return A \code{\link[lattice]{Lattice}} plot.
#' @author Bendix Carstensen, Steno Diabetes Center, \email{bendix.carstensen@@regionh.dk },
#' \url{https://BendixCarstensen.com}.
#' @seealso \code{\link{MCmcmc}}, \code{\link{plot.MCmcmc}},
#' \code{\link{ox.MC}}, \code{\link{sbp.MC}}
#' @keywords models
#' @examples
#' 
#' # Load a provided MCmcmc object
#' data( ox.MC )
#' trace.MCmcmc( ox.MC, what="beta" )
#' pairs( ox.MC, what="sd" )
#' 
#' @export trace.MCmcmc
trace.MCmcmc <-
function( obj, what="sd",
       scales = c("same","free"),
       layout = "col",
       aspect = "fill",
          ... )
{
# require( coda )
# Expand scales so a single value is sufficient
scales <- c(scales,scales)
# Decide which function to invoke
res <-
if( tolower(what) %in% c("sd","var") )
  trace.sd( obj,
         scales = scales,
         layout = layout,
         aspect = aspect,
            ... )
else
if( tolower(what) %in% c("beta","slope") )
  trace.mean( obj,
           scales = scales,
           layout = layout,
           aspect = aspect,
         par.type = "beta",
              ... )
else
if( tolower(what) %in% c("alpha","int") )
  trace.mean( obj,
           scales = scales,
           layout = layout,
           aspect = aspect,
         par.type = "alpha",
              ... )
# print( res )
return( res )
}

find.vars <-
function( obj,
       layout = "col" )
{
# Find where all the variance estimates are
wh <- grep("sigma",Nam <- varnames(obj))
# Get the names of these
nam <- varnames(obj)[wh]
# Build up the subset indices by method
m.nam <- attributes(obj)$methods
Nm <- length( m.nam )
sb <- numeric(0)
for( i in m.nam ) sb <- c( sb, wh[grep(i,nam)] )
# Make the layout of the traceplot
if( is.character(layout) )
  {
  if( layout=="col" )
    {
    layout <- c( Nm, length(sb)/Nm )
    sb <- as.vector( matrix( sb, nrow=Nm, byrow=TRUE ) )
    }
  else
    {
    layout <- c( length(sb)/Nm, Nm )
    sb <- as.vector( t( matrix( sb, nrow=Nm, byrow=TRUE ) ) )
    }
  }
return( list( sb=sb, layout=layout ) )
}

find.mean <-
function( obj,
       layout = "col",
     par.type = "beta" )
{
# Build up the subset indices by method in the right order
m.nam <- attributes(obj)$methods
Nm <- length( m.nam )
sb <- numeric(0)
for( ir in 1:Nm ) for( ic in 1:Nm ) if( ir != ic )
{
sb <- c(sb, grep( paste(par.type,"\\[",m.nam[ir],".",m.nam[ic],sep=""),
                  varnames(obj) ) )
}
# Create a layout of panels
if( is.character(layout) )
  {
  if( layout=="col" ) layout <- c(Nm-1,Nm)
  else
    {
    layout <- c(Nm,Nm-1)
    sb <- as.vector( t( matrix( sb, nrow=Nm-1, byrow=FALSE ) ) )
    }
  }
return( list( sb=sb, layout=layout ) )
}


#' @export
trace.sd <-
function( obj,
       scales = c("free","free"),
       layout = "col",
       aspect = "fill",
          ... )
{
fv <- find.vars( obj, layout )
lattice::xyplot( subset( obj, fv$sb ),
                   scales = list(x=list(relation=scales[1]),
                                 y=list(relation=scales[2])),
                   layout = fv$layout,
                 as.table = TRUE,
                   aspect = aspect,
             par.settings = list(strip.background=list(col=gray(0.95))),
           par.strip.text = list(font=2),
                      ... )
}


#' @export
trace.mean <-
function( obj,
       scales = c("free","free"),
       layout = "col",
       aspect = "fill",
     par.type = "beta",
          ... )
{
fm <- find.mean( obj, layout=layout, par.type=par.type )
lattice::xyplot( subset.MCmcmc( obj, fm$sb ),
                   scales = list(x=list(relation=scales[1]),
                                 y=list(relation=scales[2])),
                   layout = fm$layout,
                   aspect = aspect,
                 as.table = TRUE,
             par.settings = list(strip.background=list(col=gray(0.95))),
           par.strip.text = list(font=2),
                      ...)
}

post <- function( obj, ... ) UseMethod("post")

#' @export
post.MCmcmc <-
function( obj, what="sd",
        check = TRUE,
       scales = "same",
       layout = "row",
          lwd = 2,
          col,
  plot.points = FALSE,
       aspect = "fill",
          ... )
{
# require( coda )
scales <- c(scales,scales)
# Decide on coloring
col <- if( check ) rainbow(length(obj)) # number of chains
       else "black"
# Decide which function to invoke
if( tolower(what) %in% c("sd","var","vc","sigma") )
res <- post.sd( obj,
              check = check,
             scales = scales,
             layout = layout,
                lwd = lwd,
                col = col,
        plot.points = plot.points,
             aspect = aspect,
                ... )

sel <- ( tolower(what) %in% c("alpha","int") ) +
       ( tolower(what) %in% c("beta","slope") )*2
if( sel > 0 )
res <- post.mean( obj,
                check = check,
               scales = scales,
               layout = layout,
                  lwd = lwd,
                  col = col,
          plot.points = plot.points,
               aspect = aspect,
             par.type = c("alpha","beta")[sel],
                  ... )
return( res )
}

#' @export
post.sd <-
function( obj,
        check = TRUE,
       scales = c("free","free"),
       layout = "row",
          lwd = 2,
          col,
  plot.points = FALSE,
       aspect = "fill",
          ... )
{
fv <- find.vars( obj )
obj <- subset.MCmcmc( obj, fv$sb )
if( !check ) obj <- coda::as.mcmc(as.matrix(obj))

lattice::densityplot( obj,
          layout = fv$layout,
             lwd = lwd,
             col = col,
     plot.points = plot.points,
          aspect = aspect,
  default.scales = list(x=list(relation=scales[1]),
                        y=list(relation=scales[2])),
    par.settings = list(strip.background=list(col=gray(0.95))),
  par.strip.text = list(font=2),
             ... )
}


#' @export
post.mean <-
function( obj,
        check = TRUE,
       scales = c("free","free"),
       layout = "row",
          lwd = 2,
          col,
  plot.points = FALSE,
       aspect = "fill",
     par.type = "beta",
          ... )
{
fm <- find.mean( obj, layout=layout, par.type=par.type )
obj <- subset.MCmcmc( obj, fm$sb )
# If we believe in convergence
if( !check ) obj <- coda::as.mcmc(as.matrix(obj))

lattice::densityplot( obj,
          layout = fm$layout,
             lwd = lwd,
             col = col,
     plot.points = plot.points,
          aspect = aspect,
  default.scales = list(x=list(relation=scales[1]),
                        y=list(relation=scales[2])),
    par.settings = list(strip.background=list(col=gray(0.95))),
  par.strip.text = list(font=2),
             ... )
}


#' Create a pairs plot for an MCmcmc object
#'
#' @param x An \code{MCmcmc} object.
#' @param what Character indicating what parameters to plot.  Possible values
#' are \code{"sd"} or \code{"var"} which gives plots for the variance
#' components (on the sd. scale), \code{"beta"} or \code{"slope"}, which gives
#' plots for slope parameters and \code{"alpha"} or \code{"int"}, which gives
#' plots for the intercept parameters.
#'
#' @param subset Character or numerical indicating the columns of the posterior
#' that should be plotted by \code{pairs}.
#' @param col Color of the lines points used for plotting of the posterior
#' densities.
#' @param pch Plot symbol for the points.
#' @param cex Plot character size for points in \code{pairs}.
#' @param scales Character vector of length two, with possible values "same" or
#' "free", indicating whether x- and y-axes of the plots should be constrained
#' to be the same across panels. For \code{pairs} only the first element is
#' used to decide whether all panles should have the same axes.
#' @param \dots Further aruments passed on to the \code{\link[lattice]{Lattice}}
#' function called: \code{trace} calls \code{\link[coda]{xyplot.mcmc}} from the
#' \code{coda} package, \code{post} calls \code{\link[coda]{densityplot.mcmc}} from the
#' \code{coda} package, % \code{acf} calls \code{\link{acfplot}}, \code{pairs}
#' calls \code{\link[graphics]{pairs}} from the \code{graphics} package.
#' @return A \code{\link[lattice]{Lattice}} plot.
#' @author Bendix Carstensen, Steno Diabetes Center, \email{bendix.carstensen@@regionh.dk },
#' \url{https://BendixCarstensen.com}.
#' @seealso \code{\link{MCmcmc}}, \code{\link{plot.MCmcmc}},
#' \code{\link{ox.MC}}, \code{\link{sbp.MC}}
#' @keywords models
#'
#' @rdname pairs
#' @export
pairs.MCmcmc <-
function( x, what = "sd",
           subset = NULL,
              col = NULL,
              pch = 16,
              cex = 0.2,
           scales = "free",
              ... )
{
# Select colunms from posterior based on what=
sbset <- NULL
if( any( what %in% c("sd","sigma") ) ) sbset <- c(sbset,"mi","ir","res")
if( any( what %in% c("all")        ) ) sbset <- c(sbset,"tot")
if( any( what %in% c("alpha")      ) ) sbset <- c(sbset,"alpha")
if( any( what %in% c("beta")       ) ) sbset <- c(sbset,"beta")
if( any( what %in% c("mn","mean")  ) ) sbset <- c(sbset,"alpha","beta")
# Select columns from posterior based on subset=
if( is.character(subset) )
  {
  sbset <- NULL
  for( i in 1:length(subset) )
     sbset <- c( sbset, grep( subset[i], colnames(x[[1]]) ) )
  }
if( is.numeric(subset) ) sbset <- subset
sobj <- subset.MCmcmc( x, subset=sbset )
sobj <- as.matrix( sobj )
sobj <- sobj[,order(colnames(sobj))]
if( !is.null(col) )
  {
  col <- if( length(col)==nrow(sobj) ) col
         else
         if( length(col)==coda::nchain(x) ) rep(col,each=coda::niter(x))
         else
         rep( col[1], nrow(sobj) )
  }
else col <- rep(rainbow(coda::nchain(x)),coda::niter(x))
if( toupper(scales)=="SAME" )
  {
  rg <- range( sobj )
  sobj <- rbind( sobj, rg[1], rg[2] )
  col <- c(col,rep("transparent",2))
  }

pairs( sobj, gap=0, pch=pch, cex=cex, col=col, ... )
invisible( varnames( sobj ) )
}
