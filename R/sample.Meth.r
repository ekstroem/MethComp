#' Sample Meth object with replacement
#'
#' Sample a \code{\link{Meth}} object with replacement.  If \code{how=="random"}, a random sample of the rows are
#' sampled, the existing values of \code{meth}, \code{item} and \code{y} are kept but new replicate numbers are 
#' generated.  If \code{how=="linked"}, a random sample of the linked observations (i.e.
#' observations with identical \code{item} and \code{repl} values) are
#' sampled with replacement and replicate numbers are kept. If
#' \code{how=="item"}, items are sampled with replacement, and their
#' observations are included the sampled numner of times.
#'
#' @param x A \code{Meth} object.
#' @param how Character. What sampling strategy should be used, one of
#' \code{"random"}, \code{"linked"} or \code{"item"}. Only the first letter is
#' significant. See details for explanation.
#' @param N How many observations should be sampled?
#' @returns A meth object 
#' @author Bendix Carstensen, \email{bendix.carstensen@@regionh.dk}
#' @keywords manip
#' @examples
#' data(fat)
#' # Different ways of selecting columns and generating replicate numbers
#' Sub1 <- Meth(fat,meth=2,item=1,repl=3,y=4,print=TRUE)
#' Sub2 <- Meth(fat,2,1,3,4,print=TRUE)
#' Sub3 <- Meth(fat,meth="Obs",item="Id",repl="Rep",y="Sub",print=TRUE)
#' summary( Sub3 )
#' plot( Sub3 )
#' 
#' # Use observation in different columns as methods
#' data( CardOutput )
#' head( CardOutput )
#' sv <- Meth( CardOutput, y=c("Svo2","Scvo2") )
#' # Note that replicates are generated if a non-unique item-id is used
#' sv <- Meth( CardOutput, y=c("Svo2","Scvo2"), item="Age" )
#' str( sv )
#' # A summary is not created if the the first argument (data=) is not used:
#' sv <- Meth( y=CardOutput[,c("Svo2","Scvo2")], item=CardOutput$VO2 )
#' summary(sv)
#' 
#' # Sample items
#' ssv <- sample.Meth( sv, how="item", N=8 )
#' 
#' # More than two methods
#' data( sbp )
#' plot( Meth( sbp ) )
#' # Creating non-unique replicate numbers per (meth,item) creates a warning:
#' data( hba1c )
#' hb1  <- with( hba1c,
#'               Meth( meth=dev, item=item, repl=d.ana-d.samp, y=y, print=TRUE ) )
#' hb2  <- with( subset(hba1c,type=="Cap"),
#'               Meth( meth=dev, item=item, repl=d.ana-d.samp, y=y, print=TRUE ) )
#'   
#' @export
sample.Meth <-
function( x,
        how = "random",
          N = if( how=="items" ) nlevels( x$item ) else nrow(x))
{
if( !inherits( x, "Meth" ) ) x <- Meth( x )

if( tolower(substr(how,1,1))=="r" )
  {
  Nr <- nrow( x )
  new.x <- x[sample(1:Nr,Nr,replace=T),]
  new.x <- Meth( make.repl( new.x ), print=FALSE )
  }
  
if( tolower(substr(how,1,1))=="l" )
  {
  Nm <- nlevels( x$meth )
  IxR <- interaction( x$item, x$repl )
  ir.id <- sample( levels( IxR ), N/Nm, replace=T )
  new.x <- x[NULL,]
  for( i in 1:length(ir.id) )
     {
     new.x <- rbind( new.x, x[IxR==ir.id[i],] )
     }
  new.x <- Meth( make.repl( new.x ), print=FALSE )
  }
  
if( tolower(substr(how,1,1))=="i" )
  {
  i.id <- sample( levels( x$item ), N, replace=T )
  new.x <- cbind(x,new.item=0)[NULL,]
  for( i in 1:length(i.id) )
     {
     new.x <- rbind( new.x,
                     cbind( x[x$item==i.id[i],],
                            new.item=i ) )
     }
  new.x$old.item <- new.x$item
  new.x$item     <- new.x$new.item
  new.x <- new.x[,-grep("new.item",names(new.x))]
  new.x <- Meth( make.repl( new.x ), print=FALSE )
  }

new.x
}
