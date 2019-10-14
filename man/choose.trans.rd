\name{choose.trans}
\Rdversion{1.1}
\alias{choose.trans}
\alias{check.trans}
\title{Functions to handle transformations of measuremnt results.
}
\description{
  Choose a function and inverse based on a text string; check whether two
  functions actually are each others inverse.
  }
\usage{
  choose.trans( tr )
  check.trans( trans, y, trans.tol = 1e-05 )
  }
\arguments{
  \item{tr}{A character string, or a list of two functions, they
            should be each other's inverse. Names of the list are ignored.}
  \item{trans}{A list of two functions, each other's inverse.}
  \item{y}{Vector of numerical values where the functions should be each other's
           inverse.}
  \item{trans.tol}{Numerical constant indication how precise the evaulation
                   should be.}
  }
\value{\code{choose.trans} returns a named list with two elements "trans" and
                 "inv", both functions which
                 are each other's inverse. This is intended to be stored
                 as an attribute \code{"Transform"} with the resulting object
                 and used in plotting and reporting. All results will be on
                 the transformed scale. If the \code{tr} argument to
                 \code{choose.trans} is a character constant, the appropriate
                 named list of two functions will be generated.
                 Possibilities are: "exp", "log", "logit", "pctlogit"
                 (transforms percentages by the logit), "sqrt", "sq" (square),
                 "cll" (complementary log-minus-log), "ll" (log-minus-log).
  If there is no match \code{NULL} is returned, which will correspond to no
  transformation.
  
  \code{check.trans} returns nothing.
  }
\author{Bendix Carstensen, Steno Diabetes Center,
  \url{http://www.biostat.ku.dk/~bxc}.
  }
\examples{
choose.trans( "logit" )
}

