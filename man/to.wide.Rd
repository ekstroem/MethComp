\name{to.wide}
\alias{to.wide}
\alias{to.long}
\title{
  Functions to convert between long and wide representations of data.
  }
\description{
  These functions are merely wrappers for \code{\link{reshape}}.
  Given the complicated syntax of \code{reshape} and the particularly simple
  structure of this problem, the functions facilitate the conversion
  enormously.
  }
\usage{
  to.wide( data, warn=TRUE )
  to.long( data, vars )
  }
\arguments{
  \item{data}{A \code{\link{Meth}} object.}
  \item{warn}{Logical. Should a warning be printed when replicates are taken
              as items?}
  \item{vars}{The variables representing measurements by different methods.
              Either a character vector of names, or a numerical vector
              with the number of the variables in the dataframe.}
  }
\value{
  A dataframe.
  }
\details{
  If \code{data} represents method comparisons with exchangeable
  replicates within method, the transformation to wide format does not
  necessarily make sense. Also recognizes a
  }
\author{
  Bendix Carstensen, Steno Diabetes Center,
  \url{http://BendixCarstensen.com}
  }
\seealso{\code{\link{perm.repl}}}
\examples{
data( milk )
str( milk )
mw <- to.wide( milk )
str( mw )
( mw <- subset( mw, as.integer(item) < 3 ) )
to.long( mw, 3:4 )
}
\keyword{manip}
\keyword{datagen}
\keyword{design}