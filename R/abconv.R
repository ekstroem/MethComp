#' Derive linear conversion coefficients from a set of indeterminate
#' coefficients
#' 
#' If a method comparison model is defined as \eqn{y_{mi} = \alpha_m + \beta_m
#' \mu_i, m=1,2} y_mi = alpha_m + beta_m*mu_i, m=1,2 the coefficients of the
#' linear conversion from method 1 to 2 are computed as: \eqn{\alpha_{2|1} =
#' -\alpha_2-\alpha_1\beta_2/\beta_1} alpha_(2|1) =
#' -alpha_2-alpha_1*beta_2/beta_1 \eqn{\beta_{2|1} =
#' \beta_2/\beta_1}{beta_(2|1) = beta_2/beta_1} Morover the the point where the
#' linear conversion function intersects the identity line is computed too..
#' The function is designed to work on numerical vectors of posterior samples
#' from BUGS output.
#' 
#' 
#' @param a1 Numerical vector of intercepts for first method.  Alternatively a
#' dataframe where the vectors are selected from.
#' @param b1 Numerical vector of slopes for first method. If \code{a1} is a
#' dataframe, \code{b1} is assumed to be a numerical vector of length 4
#' pointing to the columns of \code{a1} with the intercepts and slopes.
#' @param a2 Numerical vector of intercepts for second method.
#' @param b2 Numerical vector of slopes for second method.
#' @param col.names Names for the resulting three vectors.
#' @return A dataframe with three columns: intercept and slope for the
#' conversion from method 1 to method 2, and the value where the conversion is
#' the identity.
#' @author Bendix Carstensen, Steno Diabetes Center,
#' \url{https://BendixCarstensen.com}
#' @seealso \code{\link{BA.plot}}, \code{\link{MCmcmc}}
#' @references B Carstensen: Comparing and predicting between several methods
#' of measurement, Biostatistics, 5, pp 399-413, 2004
#' @keywords design
#' @examples
#' 
#' abconv( 0.3, 0.9, 0.8, 0.8 )
#'
#' @export 
abconv <-
function( a1, b1=1:4, a2=NULL, b2=NULL,
          col.names=c("alpha.2.1","beta.2.1","id.2.1") )
{
if( ( inherits( a1, "data.frame" ) |
      inherits( a1, "matrix" ) )
    & length( b1 )==4 )
  {
  cols <- a1
  wh <- b1
  a1 <- cols[,wh[1]]
  a2 <- cols[,wh[2]]
  b1 <- cols[,wh[3]]
  b2 <- cols[,wh[4]]
  }
a2.1 <- a2 - a1 * b2 / b1
b2.1 <- b2 / b1
id2.1 <- a2.1 / ( 1-b2.1 )
dfr <- data.frame( a2.1, b2.1, id2.1 )
names( dfr ) <- col.names
dfr
}

