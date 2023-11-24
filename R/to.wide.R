#' Functions to convert between long and wide representations of data
#'
#' These functions are merely wrappers for \code{\link{reshape}}.
#' Given the complicated syntax of \code{reshape} and the particularly simple
#' structure of this problem, the functions facilitate the conversion
#' enormously.
#'
#' If \code{data} represents method comparisons with exchangeable  replicates within method, the transformation to wide format does not necessarily make sense.
#'
#' @param data A \code{\link{Meth}} object.
#' @param warn Logical. Should a warning be printed when replicates are taken as items?
#'
#' @return A data frame with the reshaped data
#' @examples
#'
#' data( milk )
#' str( milk )
#' mw <- to.wide( milk )
#' str( mw )
#' ( mw <- subset( mw, as.integer(item) < 3 ) )
#' to.long( mw, 3:4 )
#' 
#' @importFrom stats reshape
#' @export to.wide
to.wide <- 
function (data, warn = TRUE) 
{
  
  data$meth <- factor(data$meth)  # Added this line to ensure encoding of the meth variable
  
  if (!inherits(data, "data.frame")) 
    stop("The argument must be a dataframe\n--- you supplied a ", 
         class(data))
  if (!inherits(data, "Meth")) 
    data <- Meth(data, print = FALSE)
  if (has.repl(data)) 
    data$id <- interaction(data$item, data$repl)
  else data$id <- data$item
  res <- reshape(data, direction = "wide", v.names = c("y", 
                                                       if ("mean.y" %in% names(data)) "mean.y"), timevar = "meth", 
                 idvar = "id")
  names(res) <- gsub("y\\.", "", names(res))
  attr(res, "reshapeWide")$varying <- gsub("y\\.", "", attr(res, 
                                                            "reshapeWide")$varying)
  class(res) <- "data.frame"
  onam <- c("item", "repl", levels(data$meth), if (length(grep("mean", 
                                                               names(data))) > 0) paste("mean", levels(data$meth), sep = "."))

  res <- res[, onam]
  res
}


#' Functions to convert between long and wide representations of data
#'
#' These functions are merely wrappers for \code{\link{reshape}}.
#' Given the complicated syntax of \code{reshape} and the particularly simple
#' structure of this problem, the functions facilitate the conversion
#' enormously.
#'
#' If \code{data} represents method comparisons with exchangeable  replicates within method, the transformation to wide format does not necessarily make sense.
#'
#' @param data A \code{\link{Meth}} object.
#' @param vars The variables representing measurements by different methods. Either a character vector of names, or a numerical vector with the number of the variables in the dataframe.
#'
#' @return A data frame with the reshaped data
#' @examples
#' 
#' data( milk )
#' str( milk )
#' mw <- to.wide( milk )
#' str( mw )
#' ( mw <- subset( mw, as.integer(item) < 3 ) )
#' to.long( mw, 3:4 )
#' 
#' @importFrom stats reshape
#' @export to.long
to.long <-
function( data, vars )
{
if( !is.data.frame(data) )
  stop( "The argument must be a dataframe\n--- you supplied a ", class(data) )
if(      missing( vars ) )
  stop( "You must supply names or numbers of the variables to be stacked" )
if( is.numeric  ( vars ) ) v.names <- names( data )[vars]
if( is.character( vars ) ) v.names <- vars
mn <- match( v.names, names( data ) )
if( any( is.na( mn ) ) ) stop( v.names[is.na( mn )], " is not in the dataframe" )
if( missing( vars ) ) stop("You must specify which variables contain measurements")
new <-
reshape( data, direction = "long",
                 varying = list(v.names),
                   times = v.names,
                 v.names = "y",
                 timevar = "meth",
                   idvar = "item" )
rownames( new )  <- NULL
new <- Meth( new, print=FALSE )
new
}
