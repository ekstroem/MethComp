#' Simulate a dataframe containing replicate measurements on the same items
#' using different methods.
#' 
#' Simulates a dataframe representing data from a method comparison study. It
#' is returned as a \code{\link{Meth}} object.
#' 
#' Data are simulated according to the following model for an observation
#' \eqn{y_{mir}}{y_mir}: \deqn{y_{mir} = \alpha_m + \beta_m(\mu_i+b_{ir} +
#' c_{mi}) + e_{mir}}{y_mir = alpha_m + beta_m*(mu_i+b_ir+c_mi) + e_mir} where
#' \eqn{b_{ir}}{b_ir} is a random \code{item} by \code{repl} interaction (with
#' standard deviation for method \eqn{m} the corresponding component of the
#' vector \eqn{\sigma_ir}{sigma_ir}), \eqn{c_{mi}}{c_mi} is a random
#' \code{meth} by \code{item} interaction (with standard deviation for method
#' \eqn{m} the corresponding component of the vector \eqn{\sigma_mi}{sigma_mi})
#' and \eqn{e_{mir}}{e_mir} is a residual error term (with standard deviation
#' for method \eqn{m}{m} the corresponding component of the vector
#' \eqn{\sigma_mir}{sigma_mir}).  The \eqn{\mu_i}{mu_i}'s are uniformly spaced
#' in a range specified by \code{mu.range}.
#' 
#' @param Ni The number of items (patient, animal, sample, unit etc.)
#' @param Nm The number of methods of measurement.
#' @param Nr The (maximal) number of replicate measurements for each
#' (item,method) pair.
#' @param nr The minimal number of replicate measurements for each
#' (item,method) pair. If \code{nr<Nr}, the number of replicates for each
#' (meth,item) pair is uniformly distributed on the points \code{nr:Nr},
#' otherwise \code{nr} is ignored. Different number of replicates is only
#' meaningful if replicates are not linked, hence \code{nr} is also ignored
#' when \code{sigma.ir>0}.
#' @param alpha A vector of method-specific intercepts for the linear equation
#' relating the "true" underlying item mean measurement to the mean measurement
#' on each method.
#' @param beta A vector of method-specific slopes for the linear equation
#' relating the "true" underlying item mean measurement to the mean measurement
#' on each method.
#' @param mu.range The range across items of the "true" mean measurement.  Item
#' means are uniformly spaced across the range.  If a vector length \code{Ni}
#' is given, the values of that vector will be used as "true" means.
#' @param sigma.mi A vector of method-specific standard deviations for a method
#' by item random effect.  Some or all components can be zero.
#' @param sigma.ir Method-specific standard deviations for the item by
#' replicate random effect.
#' @param sigma.mir A vector of method-specific residual standard deviations
#' for a method by item by replicate random effect (residual variation).  All
#' components must be greater than zero.
#' @param m.thin Fraction of the observations from each method to keep.
#' @param i.thin Fraction of the observations from each item to keep. If both
#' \code{m.thin} and \code{i.thin} are given the thinning is by their
#' componentwise product.
#' @return A \code{\link{Meth}} object, i.e. dataframe with columns
#' \code{meth}, \code{item}, \code{repl} and \code{y}, representing results
#' from a method comparison study.
#' @author Lyle Gurrin, University of Melbourne,
#' \url{https://mspgh.unimelb.edu.au/centres-institutes/centre-for-epidemiology-and-biostatistics}
#' 
#' Bendix Carstensen, Steno Diabetes Center, \url{https://BendixCarstensen.com}
#' @seealso \code{\link{summary.Meth}}, \code{\link{plot.Meth}},
#' \code{\link{MCmcmc}}
#' @keywords datagen manip
#' @examples
#' 
#'   Meth.sim( Ni=4, Nr=3 )
#'   xx <- Meth.sim( Nm=3, Nr=5, nr=2, alpha=1:3, beta=c(0.7,0.9,1.2), m.thin=0.7 )
#'   summary( xx )
#'   plot( xx )
#'   
#' @export
Meth.sim <-
function( Ni = 100,
          Nm = 2,
          Nr = 3,
          nr = Nr,
       alpha = rep(0,Nm),
        beta = rep(1,Nm),
    mu.range = c(0,100),
    sigma.mi = rep(5,Nm),
    sigma.ir = 2.5,
   sigma.mir = rep(5,Nm),
      m.thin = 1,
      i.thin = 1
        )
{
if( min(c(sigma.mi,sigma.ir,sigma.mir)) < 0)
    stop("\nVariance components must be greater than or equal to 0")

# Check number of parameters
parlength  <- c( length(alpha), length(beta), length(sigma.mi), length(sigma.mir) )
fishy <- parlength!=Nm
if( any( fishy ) )
    cat("\nThe parameter vector",
         c("alpha","beta","sigma.mi","sigma.mir")[fishy],
         "does not have length ", Nm, "(the number of methods specified)",
         "lengths are", parlength[fishy],
         "\nSubsetting / recycling will be applied." )

if( length(mu.range) != 2 &
    length(mu.range) != Ni )
    stop("\nmu.range must be a vector of length 2")

# First a complete grid of (meth,item)
meth <- rep( 1:Nm, Ni )
item <- rep( 1:Ni, each=Nm )
# Generate no. replicates for each combination in the grid, allowing varying
# numbers, but only if sigma.ir is 0 --- otherwise replicates are linked.
if( nr<Nr & sigma.ir==0 )
     reps <- sample( nr:Nr, length(meth), replace=TRUE )
else reps <-    rep(    Nr, length(meth) )
# Make a dataframe and expand it by indexing rows
dfr <- data.frame( meth=meth, item=item )[rep(1:length(meth),reps),]
# Use the standard function to generate replication numbers
# make.repl does not check for "y"
dfr <- make.repl( dfr )
# We need a copies of the repl in the current workspace later:
meth <- dfr$meth
item <- dfr$item
repl <- dfr$repl

# Generate proper uniformly DISTRIBUTED mu's
if( length(mu.range)==2 )
    mu <- runif( Ni, mu.range[1], mu.range[2] )
else
    mu <- mu.range
# Assign them to the long vector mu by indexing
mu <- mu[item]

# Use indexing by factor levels to expand the simulated variance components
e.ir <- rnorm( nlevels( IR <- interaction(item,repl) ), mean=0, sd=sigma.ir )
e.ir <- e.ir[as.integer(IR)]
e.mi <- rnorm( nlevels( MI <- interaction(meth,item) ), mean=0, sd=sigma.mi )
e.mi <- e.mi[as.integer(MI)]
e.mir <- rnorm( nrow(dfr), mean=0, sd=sigma.mir[meth] )
betavec <- beta[meth]
alphavec <- alpha[meth]

y <- alphavec + betavec*(mu + e.mi + e.ir) + e.mir

# Thin the dataframe at random
dfr <- data.frame( dfr, y=y )
m.thin <- rep( m.thin, Nm )[1:Nm]
m.thin <- m.thin[dfr$meth]
i.thin <- rep( i.thin, Ni )[1:Ni]
i.thin <- i.thin[dfr$item]
thin <- m.thin * i.thin
thin <- as.logical( rbinom( length(thin), 1, thin ) )
dfr <- dfr[thin,]
Meth( dfr, print=FALSE )
}
