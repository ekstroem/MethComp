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
#' @source Avi A. Weinbroum, Philippe Biderman, Dror Soffer, Joseph M. Klausner & Oded Szold:
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



#' Relative renal function by Scintigraphy
#' 
#' Measurements of the relative kidney function (=renal function) for 111
#' patients. The percentage of the total renal function present in the left
#' kidney is determined by one reference method, \code{DMSA} (static) and by
#' one of two dynamic methods, \code{DTPA} or \code{EC}.
#' 
#' 
#' @name scint
#' @docType data
#' @format A data frame with 222 observations on the following 5 variables:
#' \describe{ \item{\code{meth}}{Measurement method, a factor with levels
#' \code{DMSA}, \code{DTPA}, \code{EC}.} \item{\code{item}}{Patient
#' identification.} \item{\code{y}}{Percentage of total kidney function in the
#' left kidney.} \item{\code{age}}{Age of the patient.} \item{\code{sex}}{Sex
#' of the patient, a factor with levels \code{F}, \code{M}.} }
#' @source F. C. Domingues, G. Y. Fujikawa, H. Decker, G. Alonso, J. C.
#' Pereira, P. S. Duarte: Comparison of Relative Renal Function Measured with
#' Either 99mTc-DTPA or 99mTc-EC Dynamic Scintigraphies with that Measured with
#' 99mTc-DMSA Static Scintigraphy.  International Braz J Urol Vol. 32 (4):
#' 405-409, 2006
#' @keywords datasets
#' @examples
#' 
#'   data(scint)
#'   str(scint)
#'   # Make a Bland-Altman plot for each of the possible comparisons:
#'   par(mfrow=c(1,2),mgp=c(3,1,0)/1.6,mar=c(3,3,1,3))
#'   BA.plot(scint,comp.levels=c(1,2),ymax=15,digits=1,cex=2)
#'   BA.plot(scint,comp.levels=c(1,3),ymax=15,digits=1,cex=2)
#'   
NULL


#' Systolic blood pressure measured by three different methods.
#' 
#' For each subject (\code{item}) there are three replicate measurements by
#' three methods (two observers, J and R and the automatic machine, S). The
#' replicates are linked within (method,item).
#' 
#' 
#' @name sbp
#' @docType data
#' @format A data frame with 765 observations on the following 4 variables:
#' \describe{ \item{\code{meth}}{Methods, a factor with levels
#' \code{J}(observer 1), \code{R}(observer 2) and \code{S}(machine)}
#' \item{\code{item}}{Person id, numeric.} \item{\code{repl}}{Replicate
#' number, a numeric vector} \item{\code{y}}{Systolic blood pressure
#' masurement, a numeric vector} }
#' @seealso \code{\link{sbp.MC}}
#' @source The dataset is adapted from table 1 in: JM Bland and DG Altman:
#' Measuring agreement in method comparison studies. Statistical Methods in
#' Medical Research, 8:136-160, 1999. Originally supplied to Bland & Altman by
#' E. O'Brien, see: Altman DG, Bland JM. The analysis of blood pressure data.
#' In O'Brien E, O'Malley K eds. Blood pressure measurement. Amsterdam:
#' Elsevier, 1991: 287-314.
#' @keywords datasets
#' @examples
#' 
#' data(sbp)
#' par( mfrow=c(2,2), mar=c(4,4,1,4) )
#' BA.plot( sbp, comp=1:2 )
#' BA.plot( sbp, comp=2:3 )
#' BA.plot( sbp, comp=c(1,3) )
#' \dontrun{ BA.est( sbp, linked=TRUE ) }
#' 
NULL



#' Measurement of cardiac output by two different methods.
#' 
#' For each subject cardiac output is measured repeatedly (three to six times)
#' by impedance cardiography (IC) and radionuclide ventriculography (RV).
#' 
#' It is not entirely clear from the source whether the replicates are
#' exchangeable within (method,item) or whether they represent pairs of
#' measurements. From the description it looks as if replicates are linked
#' between methods, but in the paper they are treated as if they were not.
#' 
#' @name cardiac
#' @docType data
#' @format A data frame with 120 observations on the following 4 variables.
#' \describe{ \item{\code{meth}}{a factor with levels \code{IC} \code{RV}}
#' \item{\code{item}}{a numeric vector giving the item number.}
#' \item{\code{repl}}{a numeric vector with replicate number.}
#' \item{\code{y}}{the measuremnts of cardiac output.} }
#' @source The dataset is adapted from table 4 in: JM Bland and DG Altman:
#' Measuring agreement in method comparison studies. Statistical Methods in
#' Medical Research, 8:136-160, 1999. Originally supplied to Bland & Altman by
#' Dr LS Bowling, see: Bowling LS, Sageman WS, O'Connor SM, Cole R, Amundson
#' DE.  Lack of agreement between measurement of ejection fraction by impedance
#' cardiography versus radionuclide ventriculography. Critical Care Medicine
#' 1993; 21: 1523-27.
#' @keywords datasets
#' @examples
#' 
#' data(cardiac)
#' cardiac <- Meth(cardiac)
#' summary(cardiac)
#' # Visually check exchangeability
#' plot( cardiac )
#' plot( perm.repl( cardiac ) )
#' BA.est(cardiac)
#' # Run MCmcmc using BRugs for an insufficient amount of iterations
#' \dontrun{card.mi.ir <- MCmcmc( cardiac,
#'                                beta=FALSE, random=c("mi","ir"),
#'                                n.iter=100, trace=T )
#' print( card.mi.ir )}
#' 
NULL



#' Glucose measurements by different methods
#' 
#' 74 persons in 5 centres in Finland had blood glucose measured by 11
#' different methods, based on 4 different types of blood. Each person had
#' blood sampled at 0, 30, 60 and 120 min after a 75 g glucose load.
#' 
#' 
#' @name glucose
#' @docType data
#' @format A data frame with 1302 observations on the following 6 variables.
#' \describe{ \item{\code{meth}}{Method of measurement. A factor with 11
#' levels: \code{n.plas1} \code{n.plas2} \code{h.cap} \code{h.blood}
#' \code{h.plas} \code{h.serum} \code{m.plas} \code{m.serum} \code{o.cap}
#' \code{s.serum} \code{k.plas}.} \item{\code{type}}{Type of blood sample. A
#' factor with 4 levels: \code{blood} \code{plasma} \code{serum} \code{capil}}
#' \item{\code{item}}{Person id.} \item{\code{time}}{Time of blood sampling.
#' Minutes since glucose load.} \item{\code{cent}}{Center of sampling. Except
#' for the two first methods, \code{n.plas1} and \code{n.plas2}, samples were
#' analyzed at the centres too} \item{\code{y}}{Glucose measurement in
#' mmol/l.} }
#' @references B Carstensen, J Lindstrom, J Sundvall, K Borch-Johnsen1, J
#' Tuomilehto & the DPS Study Group: Measurement of Blood Glucose: Comparison
#' between different Types of Specimens. Annals of Clinical Biochemistry, to
#' appear.
#' @source The study was conducted at the National Public Health Institute in
#' Helsinki by Jaana Lindstrom.
#' @keywords datasets
#' @examples
#' 
#'   data( glucose )
#'   str( glucose )
#'   # Use only plasma and serum as methods and make a Bland-Altman plot
#'   gluc <- subset( glucose, type %in% c("plasma","serum") )
#'   gluc$meth <- gluc$type
#'   gluc$repl <- gluc$time
#'   BA.plot( gluc )
#'   
NULL


#' Peak Expiratory Flow Rate (PEFR) measurements with Wright peak flow and mini
#' Wright peak flow meter.
#' 
#' Measurement of PEFR with Wright peak flow and mini Wright peak flow meter on
#' 17 individuals.
#' 
#' 
#' @name PEFR
#' @docType data
#' @format A data frame with 68 observations on the following 3 variables.
#' \describe{ \item{\code{meth}}{a factor with levels \code{Wright} and
#' \code{Mini}, representing measurements by a Wright peak flow meter and a
#' mini Wright meter respectively, in random order.}
#' \item{\code{item}}{Numeric vector, the person ID.} \item{\code{y}}{Numeric
#' vector, the measurements, i.e. PEFR for the two measurements with a Wright
#' peak flow meter and a mini Wright meter respectively. The measurement unit
#' is l/min.} \item{\code{repl}}{Numeric vector, replicate number. Replicates
#' are exchangeable within item.} }
#' @source J. M. Bland and D. G. Altman (1986) Statistical Methods for
#' Assessing Agreement Between Two Methods of Clinical Measurement, Lancet.
#' 1986 Feb 8;1(8476):307-10.
#' @keywords datasets
#' @examples
#' 
#' data(PEFR)
#' PEFR <- Meth(PEFR)
#' summary(PEFR)
#' plot(PEFR)
#' plot(perm.repl(PEFR))
#' 
NULL


#' Measurements of subcutaneous and visceral fat
#' 
#' 43 persons had Subcutaneous and Visceral fat thickness measured at Steno
#' Diabetes Center in 2006 by two observers; all measurements were done three
#' times. The interest is to compare the measurements by the two observers.
#' Persons are items, observers are methods, the three replicates are
#' exchangeable within (person,observer)=(item,method)
#' 
#' 
#' @name fat
#' @docType data
#' @format A data frame with 258 observations on the following 6 variables.
#' \describe{ \item{\code{Id}}{Person id.} \item{\code{Obs}}{Observers, a
#' factor with levels \code{KL} and \code{SL}.} \item{\code{Rep}}{Replicate
#' --- exchangeable within person and observer.}
#' \item{\code{Sub}}{Subcutaneous fat measured in cm.}
#' \item{\code{Vic}}{Visceral fat measured in cm.} }
#' @keywords datasets
#' @examples
#' 
#' data(fat)
#' str(fat)
#' vic <- Meth( fat, meth=2, item=1, repl="Rep", y="Vic" )
#' str(vic)
#' BA.est( vic, linked=FALSE )
#' 
NULL


#' Measurements of HbA1c from Steno Diabetes Center
#' 
#' Three analysers (machines) for determination of HbA1c (glycosylated
#' haemoglobin) were tested on samples from 38 individuals. Each had drawn a
#' venous and capillary blood sample. These were analysed on five different
#' days.
#' 
#' In the terminology of method comparison studies, methods is the
#' cross-classification of \code{dev} and \code{type}, and replicate is
#' \code{d.ana}. It may be of interest to look at the effect of time between
#' \code{d.ana} and \code{d.samp}, i.e. the time between sampling and analysis.
#' 
#' @name hba1c
#' @docType data
#' @format A data frame with 835 observations on the following 6 variables.
#' \describe{ \item{\code{dev}}{Type of machine used.  A factor with levels
#' \code{BR.V2}, \code{BR.VC} and \code{Tosoh}.} \item{\code{type}}{Type of
#' blood analysed (capillary or venous).  A factor with levels \code{Cap}
#' \code{Ven}} \item{\code{item}}{Person-id. A numeric vector}
#' \item{\code{d.samp}}{Day of sampling.} \item{\code{d.ana}}{Day of
#' laboratory analysis.} \item{\code{y}}{The measured value of HbA1c.} }
#' @references These data were analysed as example in: Carstensen: Comparing
#' and predicting between several methods of measurement, Biostatistics 5, pp.
#' 399--413, 2004.
#' @source Bendix Carstensen, Steno Diabetes Center.
#' @keywords datasets
#' @examples
#' 
#' data(hba1c)
#' str(hba1c)
#' hb1  <- with( hba1c,
#'               Meth( meth = interaction(dev,type),
#'                     item = item,
#'                     repl = d.ana-d.samp,
#'                        y = y, print=TRUE ) )
#' 
NULL



#' A MCmcmc object from the hba1c data
#' 
#' This object is included for illustrative purposes. It is a result of a
#' 5-hour run using MCmcmc, with \code{n.iter=100000}.
#' 
#' The data are the venous measurements from the \code{\link{hba1c}} dataset,
#' using the day of analysis as replicate. Measurements are taken to be linked
#' within replicate (=day of analysis).
#' 
#' @name hba.MC
#' @docType data
#' @format The format is a \code{\link{MCmcmc}} object.
#' @keywords datasets
#' @examples
#' 
#' data(hba.MC)
#' attr(hba.MC,"mcmc.par")
#' # print.MCmcmc(hba.MC)
#' # One of the chains is really fishy (it's the first one)
#' # trace.MCmcmc(hba.MC)
#' # trace.MCmcmc(hba.MC,"beta")
#' # Try to have a look, excluding the first chain
#' # hba.MCsub <- subset.MCmcmc(hba.MC,chains=-1)
#' # trace.MCmcmc(hba.MCsub)
#' # trace.MCmcmc(hba.MCsub,"beta")
#' # A MCmcmc object also has class mcmc.list, so we can use the
#' # coda functions for covergence diagnostics:
#' # acfplot( subset.MCmcmc(hba.MC, subset="sigma"))
#' 
NULL


#' A MCmcmc object from the sbp data
#' 
#' This object is included for illustrative purposes. It is a result of using
#' \code{\link{MCmcmc}}, with \code{n.iter=100000} on the dataset
#' \code{\link{sbp}} from this package.
#' 
#' The basic data are measurements of systolic blood pressure from the
#' \code{\link{sbp}} dataset. Measurements are taken to be linked within
#' replicate.  The code used to generate the object was: \preformatted{
#' library(MethComp) data( sbp ) spb <- Meth( sbp ) sbp.MC <- MCmcmc( sbp,
#' linked=TRUE, n.iter=100000, program="JAGS" ) ) }
#' 
#' @name sbp.MC
#' @docType data
#' @format The format is a \code{\link{MCmcmc}} object.
#' @keywords datasets
#' @examples
#' 
#' data(sbp.MC)
#' # How was the data generated
#' attr(sbp.MC,"mcmc.par")
#' 
#' # Traceplots
#' trace.MCmcmc(sbp.MC)
#' trace.MCmcmc(sbp.MC,"beta")
#' 
#' # A MCmcmc object also has class mcmc.list, so we can use the
#' # standard coda functions for convergence diagnostics:
#' # acfplot( subset.MCmcmc(sbp.MC,subset="sigma") )
#' 
#' # Have a look at the correlation between the 9 variance parameters
#' pairs( sbp.MC )
#' 
#' # Have a look at whether the MxI variance components are the same between methods:
#' \dontrun{
#' pairs( sbp.MC, subset=c("mi"), eq=TRUE,
#'         panel=function(x,y,...)
#'               {
#'                abline(0,1)
#'                abline(v=median(x),h=median(y),col="gray")
#'                points(x,y,...)
#'               }
#'         ) }
#' 
NULL


#' Measurements of plasma volume measured by two different methods.
#' 
#' For each subject (\code{item}) the plasma volume is expressed as a
#' percentage of the expected value for normal individuals. Two alternative
#' sets of normal values are used, named Nadler and Hurley respectively.
#' 
#' 
#' @name plvol
#' @docType data
#' @format A data frame with 198 observations on the following 3 variables.
#' \describe{ \item{\code{meth}}{a factor with levels \code{Hurley} and
#' \code{Nadler}} \item{\code{item}}{a numeric vector} \item{\code{y}}{a
#' numeric vector} }
#' @source The datset is adapted from table 2 in: JM Bland and DG Altman:
#' Measuring agreement in method comparison studies. Statistical Methods in
#' Medical Research, 8:136-160, 1999. Originally supplied to Bland & Altman by
#' C Dore, see: Cotes PM, Dore CJ, Liu Yin JA, Lewis SM, Messinezy M, Pearson
#' TC, Reid C.  Determination of serum immunoreactive erythropoietin in the
#' investigation of erythrocytosis. New England Journal of Medicine 1986; 315:
#' 283-87.
#' @keywords datasets
#' @examples
#' 
#' data(plvol)
#' str(plvol)
#' plot( y[meth=="Nadler"]~y[meth=="Hurley"],data=plvol,
#'       xlab="Plasma volume (Hurley) (pct)",
#'       ylab="Plasma volume (Nadler) (pct)" )
#' abline(0,1)
#' par( mar=c(4,4,1,4) )
#' BA.plot(plvol)
#' 
NULL