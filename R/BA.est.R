#' Bias and variance components for a Bland-Altman plot.
#' 
#' A variance component model is fitted to method comparison data with
#' replicate measurements in each method by item stratum. The purpose is to
#' simplify the construction of a correct Bland-Altman-plot when replicate
#' measurements are available, and to give the REML-estimates of the relevant
#' variance components.
#' 
#' The model fitted is: \deqn{y=\alpha_m + \mu_i + c_{mi} + a_{ir} + e_{mir},
#' }{y=alpha_m + mu_i + c_mi + a_ir + e_ir, var(c_mi)=tau_m^2,
#' var(a_ir)=omega^2, var(e_mir)=sigma_m^2}\deqn{ \quad
#' \mathrm{var}(c_{mi})=\tau_m^2, }{y=alpha_m + mu_i + c_mi + a_ir + e_ir,
#' var(c_mi)=tau_m^2, var(a_ir)=omega^2, var(e_mir)=sigma_m^2}\deqn{ \quad
#' \mathrm{var}(a_{ir})=\omega^2, }{y=alpha_m + mu_i + c_mi + a_ir + e_ir,
#' var(c_mi)=tau_m^2, var(a_ir)=omega^2, var(e_mir)=sigma_m^2}\deqn{ \quad
#' \mathrm{var}(e_{mir})=\sigma_m^2, }{y=alpha_m + mu_i + c_mi + a_ir + e_ir,
#' var(c_mi)=tau_m^2, var(a_ir)=omega^2, var(e_mir)=sigma_m^2}\deqn{
#' y=alpha_m + mu_i + c_mi + a_ir + e_ir, var(c_mi)=tau_m^2,
#' var(a_ir)=omega^2, var(e_mir)=sigma_m^2} We can only fit separate variances
#' for the \eqn{\tau s}{tau's} if more than two methods are compared (i.e.
#' \code{nM} > 2), hence varMxI is ignored when \code{nM}==2.
#' 
#' The function \code{VC.est} is the workhorse; \code{BA.est} just calls it.
#' \code{VC.est} figures out which model to fit by \code{lme}, extracts results
#' and returns estimates. \code{VC.est} is also used as part of the fitting
#' algorithm in \code{\link{AltReg}}, where each iteration step requires fit of
#' this model. The function \code{VC.est} is actually just a wrapper for the
#' functions \code{VC.est.fixed} that handles the case with fixed methods
#' (usually 2 or three) i.e. the classical method comparison problem, and
#' \code{VC.est.random} that handles the situation where "methods" are merely a
#' random sample of raters from some population of raters; and therefore are
#' regarded as random.
#' 
#' @param data A \code{\link{Meth}} object representing method comparison data
#' with replicate measurements, i.e. with columns \code{meth}, \code{item},
#' \code{repl} and \code{y}.
#' @param linked Logical. Are replicates linked within item across methods?
#' @param IxR Logical. Should an item by repl interaction be included in the
#' model. This is needed when the replicates are linked within item across
#' methods, so it is just another name for the \code{linked} argument.  If
#' \code{linked=} is given, this is ignored.
#' @param MxI Logical. Should the method by item interaction (matrix effect) be
#' included in the model.
#' @param corMxI Logical. Should the method by item interaction allow
#' coorelated effects within item. Ignored if only two methods are compared.
#' @param varMxI Logical. Should the method by item interaction have a variance
#' that varies between methods. Ignored if only two methods are compared.
#' @param IxR.pr Logical. Should the item by repl interaction variation be
#' included in the prediction standard deviation?
#' @param bias Logical. Should a systematic bias between methods be estimated?
#' If \code{FALSE} no bias between methods are assumed, i.e.  \eqn{\alpha_m=0,
#' m=1,\ldots M}{alpha_m=0, m=1,...,M}.
#' @param alpha Numerical. Significance level. By default the value 2 is used
#' when computing prediction intervals, otherwise the
#' \eqn{1-\alpha/2}{1-alpha/2} t-quantile is used. The number of d.f. is taken
#' as the number of units minus the number of items minus the number of methods
#' minus 1 (\eqn{I-M-1}{I-M-1}).
#' @param Transform Transformation applied to data (\code{y}) before analysis.
#' See \code{\link{check.trans}} for possible values.
#' @param trans.tol Numerical. The tolerance used to check whether the supplied
#' transformation and its inverse combine to the identity.
#' @param random.raters Logical. Should methods/raters be considered as random.
#' Defaults to \code{FALSE} which corresponds to a fixed effect of
#' methods/raters.
#' @param lmecontrol A list of control parameters passed on to \code{lme}.
#' @param weightfunction Function to weigh variance components for random
#' raters. Defaults to \code{mean} but can also be \code{median}.
#' @return \code{BA.est} returns an object of class
#' \code{c("\link{MethComp}","BA.est")}, a list with four elements \code{Conv},
#' \code{VarComp}, \code{LoA}, \code{RepCoef}; \code{VC.est} returns
#' (invisibly!) a list with elements \code{Bias}, \code{VarComp}, \code{Mu},
#' \code{RanEff}.  These list components are: \item{Conv}{3-dimensional array
#' with dimensions "To", "From" and unnamed.  The first two dimensions have the
#' methods compared as levels, the last one
#' \code{c("alpha","beta","sd.pred","LoA: lower","upper")}.  It represents the
#' mean conversions between methods and the prediction standard deviation.
#' 
#' Where "To" and "From" take the same value the value of the "sd" component is
#' \eqn{\sqrt{2}}{sqrt(2)} times the residual variation for the method. If
#' \code{IxR.pr=TRUE} the variation between replicates are included too, i.e.
#' \eqn{\sqrt{2(\sigma_m^2+\omega^2)} } sqrt[2(sigma_m^2+omega^2)]. }
#' \item{VarComp}{A matrix of variance components (on the SD scale) with
#' methods as rows and variance components "IxR", "MxI" and "res" as columns.}
#' \item{LoA}{Four-column matrix with mean difference, lower and upper limit of
#' agreement and prediction SD. Each row in the matrix represents a pair of
#' methods.} \item{RepCoef}{Two-column matrix of repeatability SDs and
#' repeatability coefficients. The SDs are the standard deviation of the
#' difference between two measurements by the same method on the item under
#' identical circumstances; the repeatability coefficient the numerical extent
#' of the prediction interval for this difference, i.e.
#' \eqn{2\sqrt{2}}{2*sqrt(2)} times the sd.} \item{Mu}{Estimates of the
#' item-specific parameters.} \item{RanEff}{Estimates of the random effects
#' from the model (BLUPS).  This is a (possibly empty) list with possible
#' elements named \code{MxI} and \code{IxR} according to whether these random
#' effects are in the model.} The returned object has an attribute,
#' \code{Transform} with the transformation applied to data before analysis,
#' and its inverse --- see \code{\link{choose.trans}}.
#' @author Bendix Carstensen
#' @seealso \code{\link{BA.plot}}, \code{\link{perm.repl}}
#' @references Carstensen, Simpson & Gurrin: Statistical models for assessing
#' agreement in method comparison studies with replicate measurements, The
#' International Journal of Biostatistics: Vol. 4 : Iss. 1, Article 16.
#' \url{https://bepress.com}.
#' @keywords models design
#' @examples
#' 
#' data( ox )
#' ox <- Meth( ox )
#' summary( ox )
#' BA.est( ox )
#' BA.est( ox, linked=FALSE )
#' BA.est( ox, linked=TRUE, Transform="pctlogit" )
#' \dontrun{
#' data( sbp )
#' BA.est( sbp )
#' BA.est( sbp, linked=FALSE )
#' # Check what you get from VC.est
#' str( VC.est( sbp ) )}
#' 
#' @export
BA.est <-
function( data,
        linked = TRUE,  # Fit a model with replicate by item interaction
           IxR = has.repl(data), # Fit a model with replicate by item interaction
           MxI = has.repl(data), # To fit the model with a method by item interaction
        corMxI = FALSE, # Should method by item effects be correlated within items?
        varMxI = TRUE,  # Should method by item have method-specific variance?
        IxR.pr = FALSE, # Should the IxR varation be included with the prediction var?
          bias = TRUE,  # Should we estimate a bias between the methods?
         alpha = 0.05,
     Transform = NULL,
     trans.tol = 1e-6,
 random.raters = FALSE,
    lmecontrol = lmeControl(msMaxIter=300),
weightfunction = c("mean", "median")
        )
{
# Check that data has item, method and repl
rq.nam <- c("meth","item","repl","y")
if( sum( !is.na( wh <- match( rq.nam, names( data ) ) ) ) < 3 ) stop(
"\nThe supplied dataframe misses columns named ", rq.nam[is.na(wh)], ".\n" )
if( sum( !is.na( wh <- match( rq.nam, names( data ) ) ) ) == 3 ) stop(
"\nThe supplied dataframe misses the column named ", rq.nam[is.na(wh)], ".\n" )

WFUN <- match.fun(match.arg(weightfunction))

# Only complete cases
dfr <- Meth( data[,c("meth","item","repl","y")], print=FALSE )

# Exchangeability:
if( !missing(linked) ) IxR <- linked
# Should we use 2 or some t-quantile ( df = no. units minus no. param. )
cl.fact <- ifelse( missing(alpha),
                   2,
                   qt( 1-alpha/2,
                       nrow(data) - length(table(dfr$meth))
                                  - length(table(dfr$item)) - 1 ) )
# Transform the response if required
Transform <- choose.trans( Transform )
if( !is.null(Transform) )
  {
  check.trans( Transform, dfr$y, trans.tol=trans.tol )
  dfr$y <- Transform$trans( dfr$y )
  }
# Fit the relevant model
model.fit <- VC.est( data = dfr,
                      IxR = IxR,
                      MxI = MxI,
                   corMxI = corMxI,
                   varMxI = varMxI,
                     bias = bias,
            random.raters = random.raters,
               lmecontrol = lmecontrol )
Nm   <- length( model.fit$Bias )
Mnam <-  names( model.fit$Bias )

# Tease out the elements necessary to compute limits of agreement &c.
Bias <- model.fit$Bias
Vcmp <- model.fit$VarComp
omega <- Vcmp[,"IxR"]
tau   <- Vcmp[,"MxI"]
sigma <- Vcmp[,"res"]

# The limits of agreement
LoA <- matrix( NA, Nm*(Nm+1)/2, 4 )
colnames( LoA ) <- c("Mean","Lower","Upper", "SD")
rownames( LoA ) <- 1:nrow(LoA)
row.no <- 0
for( i in 1:Nm ) for( j in 1:i )
{
  row.no <- row.no + 1
  rownames( LoA )[row.no] <- paste( Mnam[i], "-", Mnam[j], " " )
  LoA[row.no,1] <- Bias[i] - Bias[j]
  pred.var <- sigma[i]^2 + sigma[j]^2
  if( i!=j & MxI    ) pred.var <- pred.var + tau[i]^2 + tau[j]^2
  if( i==j & IxR.pr ) pred.var <- pred.var + 2*omega[i]^2
  LoA[row.no,4] <- sqrt( pred.var )
  LoA[row.no,2] <- LoA[row.no,1] - cl.fact*LoA[row.no,4]
  LoA[row.no,3] <- LoA[row.no,1] + cl.fact*LoA[row.no,4]
}
diags <- cumsum(1:Nm)
RC <- cbind( LoA[diags,4], cl.fact*LoA[diags,4] )
colnames( RC ) <- c("SD","Coef.")
if( !missing(alpha) ) colnames( RC )[2] <- paste( "Coef.(alpha=", alpha, ")", sep="" )
rownames( RC ) <- Mnam

dnam <- list( "To:" = Mnam,
            "From:" = Mnam,
                      c("alpha","beta","sd.pred","beta=1",
                        "int(t-f)", "slope(t-f)", "sd(t-f)",
                        "int(sd)","slope(sd)","sd=K",
                        "LoA-lo", "LoA-up") )
Conv <- array( NA, dim=sapply( dnam, length ), dimnames=dnam )
Conv[,,"alpha"] <- outer( Bias, Bias, "-" )
Conv[,, "beta"] <- 1
# Derive the prediction errors;
# For the same method it is replications errors,
# plus variation betrween replicates if required
for( i in 1:Nm ) for( j in 1:Nm )
   {
   Conv[i,j,"sd.pred"] <- sqrt(sum(Vcmp[c(i,j),c(if(i!=j)          "MxI",
                                                 if(i==j & IxR.pr) "IxR",
                                                                   "res")]^2))
   Conv[i,j,c("LoA-lo","LoA-up")] <- Conv[i,j,1]+c(-1,1)*cl.fact*Conv[i,j,3]
   }
# Fill in columns corresponding to those from a DA.reg
Conv[,,    "beta=1"] <- 1
Conv[,,  "int(t-f)"] <- Conv[,,  "alpha"]
Conv[,,"slope(t-f)"] <- 0
Conv[,,   "sd(t-f)"] <-
Conv[,,   "int(sd)"] <- Conv[,,"sd.pred"]
Conv[,, "slope(sd)"] <- 0
Conv[,,      "sd=K"] <- 1

# Compute the LoA for the random raters situation
if (random.raters) {
  meanvarcomp <- apply(Vcmp**2, 2, WFUN)

  pred.var <- 2*(meanvarcomp["M"] +  meanvarcomp["MxI"] + meanvarcomp["res"])

  LoA <- matrix(0, 1, 4)
  colnames( LoA ) <- c("Mean","Lower","Upper", "SD")
  rownames( LoA ) <- "Rand. rater - rand. rater"

  LoA[1,4] <- sqrt( pred.var )
  LoA[1,2] <- -cl.fact*LoA[1,4]
  LoA[1,3] <-  cl.fact*LoA[1,4]
}
else {
  LoA <- LoA[-diags,,drop=FALSE]
}

# Return data on the original scale
res <- list( Conv = Conv,
          VarComp = Vcmp,
              LoA = LoA,
          RepCoef = RC,
             data = data )
class( res ) <- c("MethComp","BA.est")
attr( res, "Transform" ) <- Transform
attr( res, "RandomRaters" ) <- if( is.logical(random.raters) ) random.raters else FALSE
attr( res, "Repeatability" ) <- if( IxR.pr ) "Replication included"
                                else         "Replication excluded"
res
}

bias.BA.est <-
function( obj, ref=1, ... )
{
if( is.character( ref ) ) ref <- match(ref,dimnames(obj$Conv)[[1]])
if( is.na(ref) ) stop( "Wrong reference levels given, the methods are:\n  ",
                       paste( dimnames(obj$Conv)[[1]], collapse=", " ) )
if( inherits(obj,"BA.est") ) return( obj$Conv[,1,1]-obj$Conv[ref,1,1] )
else stop( "'bias' is only meaningful for objects of class BA.est" )
}
