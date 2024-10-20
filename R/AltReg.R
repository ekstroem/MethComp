ABconv <-
function( a , b, int.loc=0 )
{
# convert vectors of intercepts (at 0!) and slopes linking an arbitary
# true MU to the method to intercepts and slops for the relatioinships
# between methods.
if( length(a) != length(b) )
  stop( "a and b must have the same length" )
Nm <- length( a )
if( any(names(a)!=names(b)) )
  cat( "a and b have different names - names of b are used.\n" )
if( is.null(names(b)) ) names(b) <- 1:length(b)
names(a) <- names(b)
if( length(int.loc)<Nm ) int.loc <- rep(int.loc,Nm)[1:Nm]
dnam <- list( to=names(b), from=names(b) )
int <- slp <- array( NA, dim=sapply(dnam,length), dimnames=dnam )
for( i in 1:length(b) )
for( j in 1:length(b) )
{
slp[j,i] <- b[j]/b[i]
int[j,i] <- a[j]-a[i]*b[j]/b[i]+slp[j,i]*int.loc[i]
}
list( intercept=int, slope=slp, location=int.loc )
}



#' Estimate in a method comparison model with replicates
#' 
#' Estimates in the general model for method comparison studies with replicate
#' measurements by each method, allowing for a linear relationship between
#' methods, using the method of alternating regressions.
#' 
#' When fitting a model with both IxR and MxI interactions it may become very
#' unstable to have different variances of the MxI random effects for each
#' method, and hence the default option is to have a constant MxI variance
#' across methods. On the other hand it may be grossly inadequate to assume
#' these variances to be identical.
#' 
#' If only two methods are compared, it is not possible to separate different
#' variances of the MxI effect, and hence the \code{varMxI} is ignored in this
#' case.
#' 
#' The model fitted is formulated as: \deqn{y_{mir} = \alpha_m +
#' \beta_m(\mu_i+a_{ir} + c_{mi}) + }{y_mir = alpha_m +
#' beta_m*(mu_i+a_{ir}+c_mi) + e_mir}\deqn{ e_{mir}}{y_mir = alpha_m +
#' beta_m*(mu_i+a_{ir}+c_mi) + e_mir} and the relevant parameters to report are
#' the estimates sds of \eqn{a_{ir}}{a_{ir}} and \eqn{c_{mi}}{c_{mi}}
#' multiplied with the corresonidng \eqn{\beta_m}{beta_m}. Therefore, different
#' values of the variances for MxI and IxR are reported also when
#' \code{varMxI==FALSE}. Note that \code{varMxI==FALSE} is the default and that
#' this is the opposite of the default in \code{\link{BA.est}}.
#' 
#' @param data Data frame with the data in long format, (or a
#' \code{\link{Meth}} object) i.e. it must have columns \code{meth},
#' \code{item}, \code{repl} and \code{y}
#' @param linked Logical. Are the replicates linked across methods? If true, a
#' random \code{item} by \code{repl} is included in the model, otherwise not.
#' @param IxR Logical, alias for linked.
#' @param MxI Logical, should the method by item effect (matrix effect) be in
#' the model?
#' @param varMxI Logical, should the method by item effect have method-specific
#' variances. Ignored if only two methods are compared. See details.
#' @param eps Convergence criterion, the test is the max of the relative change
#' since last iteration in both mean and variance parameters.
#' @param maxiter Maximal number of iterations.
#' @param trace Should a trace of the iterations be printed? If \code{TRUE}
#' iteration number, convergence criterion and current estimates of means and
#' sds are printed.
#' @param sd.lim Estimated standard deviations below \code{sd.lim} are
#' disregarded in the evaluation of convergence. See details.
#' @param Transform A character string, or a list of two functions, each
#' other's inverse. The measurements are transformed by this before analysis.
#' Possibilities are: "exp", "log", "logit", "pctlogit" (transforms percentages
#' by the logit), "sqrt", "sq" (square), "cll" (complementary log-minus-log),
#' "ll" (log-minus-log). For further details see \code{\link{choose.trans}}.
#' @param trans.tol The tolerance used to check whether the supplied
#' transformation and its inverse combine to the identity.  Only used if
#' \code{Transform} is a list of two functions.
#' @return An object of class \code{c("MethComp","AltReg")}, which is a list
#' with three elements: \item{Conv}{A 3-way array with the 2 first dimensions
#' named "To:" and "From:", with methods as levels. The third dimension is
#' classifed by the linear parameters "alpha", "beta", and "sd".}
#' \item{VarComp}{A matrix with methods as rows and variance components as
#' columns. Entries are the estimated standard deviations.} \item{data}{The
#' original data used in the analysis, with untransformed measurements
#' (\code{y}s). This is needed for plotting purposes.} Moreover, if a
#' transformation was applied before analysis, an attribute "Transform" is
#' present; a list with two elements \code{trans} and \code{inv}, both of which
#' are functions, the first the transform, the last the inverse.
#' @author Bendix Carstensen, Steno Diabetes Center, \email{bendix.carstensen@@regionh.dk},
#' \url{https://BendixCarstensen.com}.
#' @seealso \code{\link{BA.est}}, \code{\link{DA.reg}}, \code{\link{Meth.sim}},
#' \code{\link{MethComp}}
#' @references B Carstensen: Comparing and predicting between several methods
#' of measurement. Biostatistics (2004), 5, 3, pp. 399--413.
#' @keywords models regression
#' @examples
#' 
#' data( ox )
#' ox <- Meth( ox )
#' \dontrun{
#' ox.AR <- AltReg( ox, linked=TRUE, trace=TRUE, Transform="pctlogit" )
#' str( ox.AR )
#' ox.AR
#' # plot the resulting conversion between methods
#' plot(ox.AR,pl.type="conv",axlim=c(20,100),points=TRUE,xaxs="i",yaxs="i",pch=16)
#' # - or the rotated plot
#' plot(ox.AR,pl.type="BA",axlim=c(20,100),points=TRUE,xaxs="i",yaxs="i",pch=16) }
#' 
#' @export
AltReg <-
function( data,           # Data frame in Meth format
        linked = FALSE,   # Are replicates linked across methods?
           IxR = linked,  # do.
           MxI = TRUE,    # Include matrix effect?
        varMxI = FALSE,   # Var of matrix effect varies with method?
           eps = 0.001,   # Convergence criterion
       maxiter = 50,      # Max no. iterations
         trace = FALSE,   # Should estimation trace be printed on screen?
        sd.lim = 0.01,    # Below this limit changes in sd estimates are ignored
                          # in the convergence criterion
     Transform = NULL,    # Transformation to be applied to data
     trans.tol = 1e-6
        )
{
# Only complete cases
dfr <- data[,c("meth","item","repl","y")]
dfr <- dfr[complete.cases(dfr),]

# Get the data as vectors in the current environment and make sure they are
# factors - otherwise we get problems calling lme
meth <- factor( dfr$meth )
item <- factor( dfr$item )
repl <- factor( dfr$repl )
y    <-         dfr$y

# Transform the response if required
Transform <- choose.trans( Transform )
if( !is.null(Transform) )
  {
  check.trans( Transform, y, trans.tol=trans.tol )
  dfr$y <- y <- Transform$trans( y )
  }

# Dimensions needed later
Nm <- nlevels( meth )
Mn <-  levels( meth )
Ni <- nlevels( item )
Nr <- nrow( dfr )/(Nm*Ni)

# Set the point for calculation of the intercept in the conv. crit.
GM <- mean( y )

# Vectors to store the residuals (well, BLUPS, posterior estimates...)
c.mi  <- rep( 0, length(y) )
a.ir  <- rep( 0, length(y) )

# Matrices to hold the results.
cf <- matrix ( 0, Nm, 3 )
colnames(cf) <- c("alpha","beta","sigma")
rownames(cf) <- Mn
cr <- matrix ( 0, Nm, Nm*2+3 )
colnames(cr) <- c( paste( "Intercept:", Mn[1] ), Mn[-1],
                   paste( "Slope:"    , Mn[1] ), Mn[-1],
                   "IxR", "MxI", "res" )
rownames(cr) <- Mn

# Initialise the "old" version of the coefficients to 0
cr.old <- cr

# Construct initial values for the zetas
if(  MxI &  IxR ) zeta.xpand <- fitted( lm( y ~ meth*item + item*repl ) )
if(  MxI & !IxR ) zeta.xpand <- fitted( lm( y ~ meth*item             ) )
if( !MxI &  IxR ) zeta.xpand <- fitted( lm( y ~             item*repl ) )
if( !MxI & !IxR ) zeta.xpand <- fitted( lm( y ~      item             ) )

# Set the criterion to a value larger than eps and initialize iteration counter
crit <- 2*eps
iter <- 0

# The actual iteration loop
while( crit > eps & iter < maxiter )
{
iter <- iter + 1

# First step, estimate alpha, beta, sigma using zeta.xpand
for( m in 1:nlevels(meth) )
   {
   sub <- ( meth == levels(meth)[m] )
   yy <- y[sub]
   xx <- zeta.xpand[sub]
   ii <- factor(item[sub])
   lm.x <- lme ( yy ~ xx, random = ~ 1 | ii )
   cf[m,c("alpha","beta")] <- summary(lm.x)$tTable[1:2,1]
   cf[m,"sigma"] <- unique( attr(lm.x$residuals,"std") )
   }

# Take the parameters and expand them to the units of the data
where.m <- as.integer( meth )
alpha.m <- cf[where.m,1]
 beta.m <- cf[where.m,2]
   wk.y <- ( y - alpha.m ) / beta.m

# Use the working units to estimate the mus and the variance components
VCE <- VC.est( data.frame( meth=meth, item=item, repl=repl, y=wk.y ),
               IxR = IxR,
               MxI = MxI,
            varMxI = varMxI,
              bias = FALSE,
             print = FALSE )

# Extract the variance components and put them on the correct scale
# - and correct the MxI using the correct d.f.
vcmp <- VCE$VarComp
if( IxR ) cr[,"IxR"] <- vcmp[,"IxR"]*cf[,2]
if( MxI ) cr[,"MxI"] <- vcmp[,"MxI"]*cf[,2]*Ni/(Ni-2)
          cr[,"res"] <- vcmp[,"res"]*cf[,2]

# Get the estimated random effects
if( IxR ) a.ir <- VCE$RanEff$IxR
if( MxI ) c.mi <- VCE$RanEff$MxI

# Get the estimated mu's
inam <- gsub("item","",names(VCE$Mu))
zeta.xpand <- VCE$Mu[match(item,inam)] + a.ir + c.mi

# Convert to unique mean parameters when assessing convergence
abc <- ABconv( cf[,1], cf[,2], int.loc=GM )
cr[,1:(2*Nm)] <- cbind( abc[[1]], abc[[2]] )

# Check convergence
conv <- abs( (cr - cr.old)/cr )
conv[,2*Nm+1:3] <- conv[,2*Nm+1:3]*(cr.old[,2*Nm+1:3]>sd.lim)
crit <- max( conv[!is.na(conv)] )
cr.old <- cr

# Print the current estimates if required
if( trace )
  {
  cat( "\niteration", iter, "criterion:", crit, "\n" )
  print(round(cbind(cf,cr),3))
  flush.console()
  }

# End of iteration loop
}

# Inform about convergence
if( iter == maxiter )
  cat( "\nAltReg reached maximum no. iterations ", iter,
       "\nLast convergence criterion was ", crit, " > target:", eps, "\n" )
else
  cat( "\nAltReg converged after ", iter, "iterations ",
       "\nLast convergence criterion was ", crit, "\n" )

# Convert to intercept 0
conv.new <- ABconv(cr[,1],cr[,1+Nm],int.loc=0)
cr[,   1:Nm] <- conv.new[[1]]
cr[,Nm+1:Nm] <- conv.new[[2]]
names( dimnames( cr ) ) <- c("To","From")

# Construct array to hold the conversion parameters
dnam <- list( "To:" = Mn,
            "From:" = Mn,
                      c("alpha","beta","sd.pred",
                        "int(t-f)","slope(t-f)","sd(t-f)") )
Conv <- array( NA, dim=sapply( dnam, length ), dimnames=dnam )
# Fill in the values realting methods
Conv[,,1] <- cr[1:Nm,1:Nm]
Conv[,,2] <- cr[1:Nm,1:Nm+Nm]
for( i in 1:Nm )
for( j in 1:Nm )
   {
   Conv[i,j,3] <- sqrt(sum(c(cr[i,c(if(i!=j)"MxI","res")]^2,
                (Conv[i,j,2]*cr[j,c(if(i!=j)"MxI","res")])^2)))
   Conv[i,j,4:6] <- y2DA( Conv[i,j,1:3] )
   }
# Then relating differences to averages

# Extract the estimated variance components
VarComp <- cr[,2*Nm+1:3]
names(dimnames(VarComp)) <- c("Method","  s.d.")

# Collect the results
res <- list( Conv = Conv,
          VarComp = VarComp,
             data = data )

class( res ) <- c("MethComp","AltReg")
attr( res, "Transform" ) <- Transform
attr( res, "RandomRaters" ) <- FALSE
invisible( res )
}
