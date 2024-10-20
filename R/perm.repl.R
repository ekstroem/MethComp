#' Manipulate the replicate numbering within (item,method)
#' 
#' Replicate numbers are generated within (item,method) in a dataframe
#' representing a method comparison study. The function assumes that
#' observations are in the correct order within each (item,method), i.e. if
#' replicate observations are non-exchangeable within method, linked
#' observations are assumed to be in the same order within each (item,method).
#' 
#' \code{make.repl} just adds replicate numbers in the order of the data.frame
#' rows.  \code{perm.repl} is designed to explore the effect of permuting the
#' replicates within (item,method). If replicates are truly exchangeable within
#' methods, the inference should be independent of this permutation.
#' 
#' @aliases perm.repl make.repl has.repl
#' @param data A \code{\link{Meth}} object or a data frame with columns
#' \code{meth}, \code{item} and \code{y}.
#' @return \code{make.repl} returns a dataframe with a column, \code{repl}
#' added or replaced, whereas \code{has.repl} returns a logical indicating
#' wheter a combination of (\code{meth},\code{item}) wioth more that one valid
#' \eqn{y}- value.
#' 
#' \code{perm.repl} returns a dataframe of class \code{\link{Meth}} where the
#' rows (i.e. replicates) are randomly permuted within
#' (\code{meth},\code{item}), and subsequently ordered by
#' (\code{meth},\code{item},\code{repl}).
#' @author Bendix Carstensen, Steno Diabetes Center,
#' \url{https://bendixcarstensen.com/}
#' @seealso \code{\link{perm.repl}}
#' @keywords manip datagen design
#' @examples
#' 
#'   data(ox)
#'   xx <- subset( ox, item<4 )[,-3]
#'   cbind( xx, make.repl(xx) )
#'   cbind( make.repl(xx), perm.repl(xx) )
#'   data( ox )
#'   xx <- subset( ox, item<4 )
#'   cbind( xx, perm.repl(xx) )
#'   # Replicates are linked in the oximetry dataset, so randomly permuting
#'   # them clearly inflates the limits of agreement:
#'   par( mfrow=c(1,2), mar=c(4,4,1,4) )
#'   BA.plot(           ox , ymax=30, digits=1 )
#'   BA.plot( perm.repl(ox), ymax=30, digits=1 )
#' 
#'   
#' @export
perm.repl <-
function( data )
{
# Check that data has item, method and repl
rq.nam <- c("meth","item","y")
if( sum( !is.na( wh <- match( rq.nam, names( data ) ) ) ) < 3 ) stop(
"\nPR: The supplied dataframe misses column(s) named ", rq.nam[is.na(wh)], ".\n" )

# Reorder data arbitrarily within (method,item)
random <- runif( nrow( data ) )
new.order <- order( data$meth, data$item, random )
data <- data[new.order,]

# Make the replicates in the new ordering
Meth( make.repl( data ), print=FALSE )
}

#' @export
make.repl <-
function( data )
{
# Check that data has item, method and repl
rq.nam <- c("meth","item")
if( sum( !is.na( wh <- match( rq.nam, names( data ) ) ) ) < 2 ) stop(
"\nThe supplied dataframe misses column(s) named ", rq.nam[is.na(wh)], ".\n" )

# Was it a Meth object?
was.Meth <- inherits( data, "Meth" )

# For this to work we must sort the dataframe first
ord <- order(data$meth,data$item)
data <- data[order(data$meth,data$item),]

# 0: (xx <- ) uniquely number all combinations of (method,item).
# 1: (tapply) find the smallest sequence number within each (method,item).
# 2: subtract this from the sequence number, to get 0,1,... within each (m,i).
# 3: add 1 to get new replicate numbers.
# 4: (as.vector) turn it into a vector.
# Note the use of factor(interaction...) to make sure that empty levels
#      of method or item don't screw up.
xx <- as.integer( factor( interaction( data$meth, data$item ) ) )
data$repl <- as.vector( 1:nrow(data) -
                        tapply( 1:nrow(data), xx, min )[xx] + 1 )
# Then reestablish the sequence:
data <- data[order(ord),]
if( was.Meth ) Meth( data, print=FALSE ) else data
}

#' @export
has.repl <-
function( data )
{
# This functions tests whether there actually are replicates
# in the dataframe x
if( !inherits( data, "data.frame" ) )
  stop( "argument must be a data frame (preferably a Meth object)")
if( !inherits( data, "Meth" ) ) data <- Meth( data, print=FALSE )
any( table( data$meth, data$item ) > 1 )
}
