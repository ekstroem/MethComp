#' Measurement of oxygen saturation in blood
#' 
#' 61 children had their blood oxygen content measured at the Children's
#' Hospital in Melbourne, either with a chemical method analysing gases in the
#' blood (\code{CO}) or by a pulse oximeter measuring transcutaneously
#' (\code{pulse}). Replicates are linked between methods; i.e. replicate 1 for
#' each of the two methods are done at the same time. However, replicate
#' measurements were taken in quick succession so the pairs of measurements are
#' exchangeable within person.
#' 
#' 
#' @name ox
#' @docType data
#' @format A data frame with 354 observations on the following 4 variables.
#' \describe{ \item{\code{meth}}{Measurement methods, factor with levels
#' \code{CO}, \code{pulse}} \item{\code{item}}{Id for the child}
#' \item{\code{repl}}{Replicate of measurements. There were 3 measurements for
#' most children, 4 had only 2 replicates with each method, one only 1}
#' \item{\code{y}}{Oxygen saturation in percent.} }
#' @keywords datasets
#' @examples
#' 
#' data(ox)
#' str(ox)
#' ox <- Meth(ox)
#' with( ox, table(table(item)) )
#' summary( ox )
#' # The effect of basing LoA on means over replicates:
#' par( mfrow=c(1,2), mar=c(4,4,1,4) )
#' BA.plot(      ox , diflim=c(-20,20), axlim=c(20,100), repl.conn=TRUE )
#' # BA.plot( mean(ox), diflim=c(-20,20), axlim=c(20,100) )
#' 
NULL



#' Data from a rating experiment of recorgnizing point counts.
#' 
#' At the course "Statsitical Analysis of Method Comparison Studies" ai the
#' SISMEC conference in Ancona, on 28 September 2011, the participants on the
#' course were used as raters of ten pictures of points. Pitures were shown 3
#' times each to the participants, and they assessed the number of points in
#' each.
#' 
#' 
#' @name Ancona
#' @docType data
#' @format A data frame with 510 observations on the following 4 variables.
#' \describe{ \item{\code{rater}}{a factor with 17 levels}
#' \item{\code{item}}{a numeric vector indicating the pictures shown. The
#' value is the actual number of points.} \item{\code{repl}}{a numeric vector,
#' replicate number} \item{\code{score}}{a numeric vector, the number of
#' points in \code{item}} }
#' @source The course "Statsitical Analysis of Method Comparison Studies" ai
#' the SISMEC conference in Ancona, on 28 September 2011.
#' @keywords datasets
#' @examples
#' 
#' library( MethComp )
#' data( Ancona )
#' Anc <- Meth( Ancona, 1, 2, 3, 4 )
#' 
NULL


#' Measurements of Cardiac output.
#' 
#' Two different ways of measuring cardiac output and oxygen saturation in 15
#' critically ill persons.
#' 
#' 
#' @name CardOutput
#' @docType data
#' @format A data frame with 15 observations on the following 8 variables.
#' \describe{ \item{\code{Age}}{Patient age} \item{\code{Diag}}{Diagnosis, a
#' factor with levels \code{sepsis}, \code{cardiogenic}, \code{hypothermia}}
#' \item{\code{VO2}}{Oxygen consumption} \item{\code{Svo2}}{Mixed venous O2
#' saturation} \item{\code{Scvo2}}{Central venous oxygen saturation}
#' \item{\code{TCO}}{Thermodilution-derived cardiac output}
#' \item{\code{FCO}}{Fick-derived cardiac output.} \item{\code{Sex}}{Sex, a
#' factor with levels \code{F}, \code{M}} }
#' @source Avi A. Weinbroum, Philippe Biderman, Dror Soffer, Joseph M. Klausner
#' & Oded Szold:
#' 
#' Reliability of cardiac output calculation by the fick principle and central
#' venous oxygen saturation in emergency conditions.
#' 
#' Journal of Clinical Monitoring and Computing (2008) 22: 361-366
#' @keywords datasets
#' @examples
#' 
#' data(CardOutput)
#' 
NULL


#' Enzyme activity data
#' 
#' Three measurement of enzyme activity on 24 patients. The measurements is of
#' the enzymes sucrase and alkaline phosphatase. The interest is to compare the
#' 'homogenate' and 'pellet' methods.
#' 
#' 
#' @name Enzyme
#' @docType data
#' @format A data frame with 72 observations on the following 3 variables.
#' \describe{ \item{\code{meth}}{a factor with levels \code{SucHom}
#' \code{SucPel} \code{Alkphos}, representing three different measurements,
#' i.e.  homogenate and pellet values of sucrase, as well as homogenate values
#' of alkaline.} \item{\code{item}}{a numeric vector, the person ID for the 24
#' patients} \item{\code{y}}{a numeric vector, the measurements on the enzyme
#' activity.} }
#' @source R. L. Carter; Restricted Maximum Likelihood Estimation of Bias and
#' Reliability in the Comparison of Several Measuring Methods; Biometrics,
#' Dec., 1981, Vol. 37, No. 4, pp. 733-741.
#' @keywords datasets
#' @examples
#' 
#' data(Enzyme)
#' Enzyme <- Meth( Enzyme )
#' summary( Enzyme )
#' # plot( Enzyme )
#' 
NULL


#' Perception of points in a swarm
#' 
#' Five raters were asked to guess the number of points in a swarm for 10
#' different figures (which - unknown to the raters - were each repeated three
#' times).
#' 
#' The raters had approximately 10 seconds to judge each picture, and they
#' thought it were 30 different pictures. Before starting the experiment they
#' were shown 6 (unrelated) pictures and were told the number of points in each
#' of those pictures. The SAND column contains the picture id (which is also
#' the true number of points in the swarm).
#' 
#' @name rainman
#' @docType data
#' @format A data frame with 30 observations on the following 6 variables.
#' \describe{ \item{\code{SAND}}{The true number of points in the swarm. Each
#' picture is replicated thrice} \item{\code{ME}}{Ratings from judge 1}
#' \item{\code{TM}}{Ratings from judge 2} \item{\code{AJ}}{Ratings from judge
#' 3} \item{\code{BM}}{Ratings from judge 4} \item{\code{LO}}{Ratings from
#' judge 5} }
#' @source Collected by Claus Ekstrom.
#' @keywords datasets
#' @examples
#' 
#' library(MethComp)
#' data( rainman )
#' str( rainman )
#' RM <- Meth( rainman, item=1, y=2:6 )
#' head( RM )
#' BA.est( RM, linked=FALSE )
#' library(lme4)
#' mf <- lmer( y ~ meth + item + (1|MI),
#'                 data = transform( RM, MI=interaction(meth,item) ) )
#' summary( mf )
#' mr <- lmer( y ~ (1|meth) + (1|item) + (1|MI),
#'                 data = transform( RM, MI=interaction(meth,item) ) )
#' summary( mr )
#' 
#' #
#' # Point swarms were generated by the following program
#' #
#' \dontrun{
#' set.seed(2) # Original
#' npoints <- sample(4:30)*4
#' nplots <- 10
#' pdf(file="swarms.pdf", onefile=TRUE)
#' 
#' s1 <- sample(npoints[1:nplots])
#' print(s1)
#' for (i in 1:nplots) {
#'   n <- s1[i]
#'   set.seed(n)
#'   x <- runif(n)
#'   y <- runif(n)
#'   plot(x,y, xlim=c(-.15, 1.15), ylim=c(-.15, 1.15), pch=20, axes=F,
#'        xlab="", ylab="")
#' }
#' s1 <- sample(npoints[1:nplots])
#' print(s1)
#' for (i in 1:nplots) {
#'   n <- s1[i]
#'   set.seed(n)
#'   x <- runif(n)
#'   y <- runif(n)
#'   plot(y,x, xlim=c(-.15, 1.15), ylim=c(-.15, 1.15), pch=20, axes=F,
#'        xlab="", ylab="")
#' }
#' s1 <- sample(npoints[1:nplots])
#' print(s1)
#' for (i in 1:nplots) {
#'   n <- s1[i]
#'   set.seed(n)
#'   x <- runif(n)
#'   y <- runif(n)
#'   plot(-x,y, xlim=c(-1.15, .15), ylim=c(-.15, 1.15), pch=20, axes=F,
#'        xlab="", ylab="")
#' }
#' dev.off()
#' }
#' 
NULL


#' Measurement of fat content of human milk by two different methods.
#' 
#' Fat content of human milk determined by measurement of glycerol released by
#' enzymic hydrolysis of triglycerides (Trig) and measurement by the Standard
#' Gerber method (Gerber). Units are (g/100 ml).
#' 
#' 
#' @name milk
#' @docType data
#' @format A data frame with 90 observations on the following 3 variables.
#' \describe{ \item{\code{meth}}{a factor with levels \code{Gerber}
#' \code{Trig}} \item{\code{item}}{sample id} \item{\code{y}}{a numeric
#' vector} }
#' @source The dataset is adapted from table 3 in: JM Bland and DG Altman:
#' Measuring agreement in method comparison studies. Statistical Methods in
#' Medical Research, 8:136-160, 1999. See: Lucas A, Hudson GJ, Simpson P, Cole
#' TJ, Baker BA. An automated enzymic micromethod for the measurement of fat in
#' human milk. Journal of Dairy Research 1987; 54: 487-92.
#' @keywords datasets
#' @examples
#' 
#' data(milk)
#' str(milk)
#' milk <- Meth(milk)
#' plot(milk)
#' abline(0,1)
#' 
NULL


#' A MCmcmc object from the oximetry data.
#' 
#' This object is included for illustrative purposes. It is a result of using
#' \code{\link{MCmcmc}}, with \code{n.iter=20000}.
#' 
#' The data are the \code{\link{ox}} dataset, where measurements are linked
#' within replicate (=day of analysis).
#' 
#' @name ox.MC
#' @docType data
#' @format The format is a \code{\link{MCmcmc}} object.
#' @keywords datasets
#' @examples
#'  
#' data(ox.MC)
#' attr(ox.MC,"mcmc.par")
#' \dontrun{
#' print.MCmcmc(ox.MC)
#' trace.MCmcmc(ox.MC)
#' trace.MCmcmc(ox.MC,"beta")
#'  post.MCmcmc(ox.MC)
#'  post.MCmcmc(ox.MC,"beta") }
#' # A MCmcmc object also has class mcmc.list, so we can use the
#' # coda functions for covergence diagnostics:
#' \dontrun{ acfplot( subset.MCmcmc(ox.MC, subset="sigma")) }
#' 
NULL



#' Merits of two instruments designed to measure certain aspects of human lung
#' function (Vital Capacity)
#' 
#' Measurement on certain aspects of human lung capacity for 72 patients on 4
#' instrument-operative combination, i.e. two different instruments and two
#' different users, a skilled one and a new one.
#' 
#' 
#' @name VitCap
#' @docType data
#' @format A data frame with 288 observations on the following 5 variables.
#' \describe{ \item{\code{meth}}{a factor with levels \code{StNew},
#' \code{StSkil}, \code{ExpNew} and \code{ExpSkil}, representing the instrument
#' by user combinations. See below.} \item{\code{item}}{a numeric vector, the
#' person ID, i.e. the 72 patients} \item{\code{y}}{a numeric vector, the
#' measurements, i.e. vital capacity.} \item{\code{user}}{a factor with levels
#' \code{New} \code{Skil}, for the new user and the skilled user}
#' \item{\code{instrument}}{a factor with levels \code{Exp} and \code{St}, for
#' the experimental instrument and the standard one.} }
#' @source V. D. Barnett, Simultaneous Pairwise Linear Structural
#' Relationships, Biometrics, Mar. 1969, Vol. 25, No. 1, pp. 129-142.
#' @keywords datasets
#' @examples
#' 
#' data(VitCap)
#' Vcap <- Meth( VitCap )
#' str( Vcap )
#' plot( Vcap )
#' 
NULL



