#' Subset an MCmcmc object
#'
#' Subset an MCmcmc object
#'
#' @param x object to be subsetted.
#' @param subset Numerical, character or list giving the variables to keep.
#'                If numerical, the variables in the \code{MCmcmc} object with
#'                these numbers are selected. If character, each element of the
#'                character vector is "grep"ed against the variable names, and
#'                the matches are selected to the subset. If a list each element
#'                is used in turn, numerical and character elements can be mixed.
#' @param allow.repl Logical. Should duplicate columns be allowed in the result?
#' @param chains Numerical vector giving the number of the chains to keep.
#' @param ... further arguments to be passed to or from other methods.
#'
#' @rdname subset
#' @export
subset.MCmcmc <-
function( x, subset=NULL, allow.repl=FALSE, chains=NULL, ... ) {
# A small utility function
  char.2.num <-
  function( x, sbs ) {
  sb <- numeric(0)
  for( i in 1:length(sbs))
      sb <- c(sb, grep( sbs[i], varnames(x) ) )
  sb
  }
 
if( is.numeric(chains) )
  {
  chains <- (1:attributes(x)$mcmc.par$n.chains)[chains]
  att <- attributes( x )
  x <- x[chains]
  attributes( x ) <- att
  attributes( x )$mcmc.par$n.chains <- coda::nchain(x)
  attributes( x )$mcmc.par$dim <- dim(as.matrix(x))
  }

if( !is.null( subset ) )
{
# If a character vector is given in subset
if( is.character( subset ) )
  subset <- char.2.num( x, subset )

# A list of vectors (character or numeric can be mixed)
if( is.list( subset ) )
  {
  sb <- if( is.character( subset[[1]] ) )
            char.2.num( x, subset[[1]] )
       else subset[[1]]
  if( length( subset ) > 1 )
  for( i in 2:length( subset ) )
     {
     sx <- if( is.character( subset[[i]] ) )
               char.2.num( x, subset[[i]] )
          else subset[[i]]
     sb <- intersect( sx, sb )
     }
  subset <- sb
  }
if( !allow.repl ) subset <- unique( subset )
att <- attributes( x )
x <- x[,subset,drop=FALSE]
attributes( x ) <- att
}

return( x )
}

################################################################################
### mcmc.MCmcmc
################################################################################
mcmc <-
function( x, ... )
UseMethod("mcmc")

#' @export
mcmc.MCmcmc <-
function( x, ... )
{
# This function converts a MCmcmc object which inherits from
# mcmc.list to a MCmcmc object that inherits from mcmc by
# lumping together the chains
if( !inherits( x, "MCmcmc" ) ) stop( "The argument (here:",
                                       deparse( substitute( x ) ),
                                       ") must be a MCmcmc object." )
res <- coda::as.mcmc( as.matrix( x ) )
attributes( res ) <- attributes( x )
class( res ) <- c("MCmcmc","mcmc")
res
}
