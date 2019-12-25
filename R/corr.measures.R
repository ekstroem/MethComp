#' Classical association measures 
#'
#' A function that returns the values of some of the classical
#' association measures proposed in the literature
#'
#' @param x A vector of numeric values of length N
#' @param y A vector of numeric values of length N
#' @return A vector of four association measures
#'
#' @export
corr.measures <- function( x, y ) {
  compl <- complete.cases( data.frame( x, y ) )
  x <- x[compl]
  y <- y[compl]
  
  Vx <- var(x)
  Mx <- mean(x)
  Vy <- var(y)
  My <- mean(y)
  MD <- mean(x-y)
  VD <- var(x-y)
  Cxy <- cov(x,y)
  MSD <- MD^2 + VD
  corr <- cor( x, y )
  CCC <- 2*Cxy / (MSD+2*Cxy)
  AcC <- 2 / ( sqrt(Vx/Vy) + sqrt(Vy/Vx) + (Mx-My)^2/sqrt(Vx*Vy) )
  res <- c( corr, MSD, CCC, AcC )
  names( res ) <- c( "Corr", "MSD", "CCC", "Acc.C" )
  res
}

#' Function to identify the middle of a vector
#'
#' @param w A numeric vector of values
#' @param rm A value between 0 and 1 giving the percentage of extreme observations to remove
#' @return A logical vector of indices that a
#'
#' @export
middle <- function( w, rm=1/3 ) {
  qnt <- quantile( w, probs=0:1 + c(1,-1)*rm/2 )
  w > qnt[1] & w < qnt[2]
}

#' Function to identify the extremes of a vector
#'
#' @param w A numeric vector of values
#' @param rm A value between 0 and 1 giving the percentage of extreme observations to remove
#' @return A logical vector of indices that a
#'
#' @export 
ends <-
function( w, rm=1/3 ) {
    !middle( w, 1-rm )
}
