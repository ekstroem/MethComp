#' Passing-Bablok regression
#' 
#' Implementation of the Passing-Bablok's procedure for assessing of the
#' equality of measurements by two different analytical methods.
#' 
#' This is an implementation of the original Passing-Bablok procedure of
#' fitting unbiased linear regression line to data in the method comparison
#' studies. It calcualtes the unbiased slope and intercept, along with their
#' confidence intervals. However, the tests for linearity is not yet fully
#' implemented.
#' 
#' It doesn't matter which results are assigned to "Method A" and "Method B",
#' however the "Method A" results will be plotted on the x-axis by the
#' \code{plot} method.
#' 
#' @aliases PBreg print.PBreg
#' @param x a \code{\link{Meth}} object, alternatively a numeric vector of
#' measurements by method A, or a data frame of exactly two columns, first
#' column with measurements by method A, second column with measurements by
#' method B.
#' @param y a numeric vector of measurements by method B - must be of the same
#' length as \code{x}. If not provided, \code{x} must be the \code{\link{Meth}}
#' object or a data frame of exactly 2 columns.
#' @param conf.level confidence level for calculation of confidence boundaries
#' - 0.05 is the default.
#' @param wh.meth Which of the methods from the \code{Meth} object are used in
#' the regression.
#' @return \code{PBreg} returns an object of class \code{"PBreg"}, for which
#' the \code{print}, \code{predict} and \code{plot} methods are defined.
#' 
#' An object of class \code{"PBreg"} is a list composed of the following
#' elements:
#' 
#' \item{coefficients}{a matrix of 3 columns and 2 rows, containing the
#' estimates of the intercept and slope, along with their confidence
#' boundaries.} \item{residuals}{defined as in the \code{"lm"} class, as the
#' response minus the fitted value.} \item{fitted.values}{the fitted values.}
#' \item{model}{the model data frame used.} \item{n}{a vector of two values:
#' the number of observations read, and the number of observations used.}
#' \item{S}{A vector of all slope estimates.} \item{I}{A vector of all
#' intercept estimates.} \item{adj}{A vector of fit parameters, where \emph{Ss}
#' is the number of estimated slopes (\code{length(S)}), \emph{K} is the offset
#' for slopes <(-1), \emph{M1} and \emph{M2} are the locations of confidence
#' boundaries in \code{S}, and \emph{l} and \emph{L} are the numbers of points
#' above and below the fitted line, used in cusum calculation.} \item{cusum}{A
#' vector of cumulative sums of residuals sorted by the D-rank.} \item{Di}{A
#' vector of D-ranks.}
#' @note Please note that this method can become very computationally intensive
#' for larger numbers of observations. One can expect a reasonable computation
#' times for datasets with fewer than 100 observations.
#' @author Michal J. Figurski \email{mfigrs@@gmail.com}
#' @seealso \code{\link{plot.PBreg}, \link{predict.PBreg}, \link{Deming}}.
#' @references Passing, H. and Bablok, W. (1983), A New Biometrical Procedure
#' for Testing the Equality of Measurements from Two Different Analytical
#' Methods. \emph{Journal of Clinical Chemistry and Clinical Biochemistry}, Vol
#' 21, 709--720
#' @examples
#' 
#'   ## Model data frame generation
#'   a <- data.frame(x=seq(1, 30)+rnorm(mean=0, sd=1, n=30),
#'                   y=seq(1, 30)*rnorm(mean=1, sd=0.4, n=30))
#' 
#'   ## Call to PBreg
#'   x <- PBreg(a)
#'   print(x)
#' 
#'   par(mfrow=c(2,2))
#'   plot(x, s=1:4)
#' 
#'   ## A real data example
#'   data(milk)
#'   milk <- Meth(milk)
#'   summary(milk)
#'   PBmilk <- PBreg(milk)
#'   par(mfrow=c(2,2))
#'   plot(PBmilk, s=1:4)
#' 
#' @export
PBreg <- function(x, y=NULL, conf.level=0.05, wh.meth=1:2) {
    meths  = c("Method A","Method B")
    if (is.null(y)) {
        if (inherits(x, "Meth"))  {
            meths    = c(levels(x$meth)[wh.meth])
            a        = to.wide(x)[,meths]
            names(a) = c("x","y")
            }
        else {
            a        = x
            meths    = colnames(a)
            names(a) = c("x","y")
            }
        }
    else    a        = data.frame(x=x, y=y)
    nread = nrow(a)
    a     = a[complete.cases(a$x,a$y),]
    n     = nrow(a)
    ch    = choose(n,2)
    nn    = combn(n,2)
    S     = I = NULL
    for (i1 in 1:ch)
        {
        data  = a[nn[,i1],]
        slope = (data$y[2]-data$y[1])/(data$x[2]-data$x[1])
        int   = data$y[2] - slope * data$x[2]
        S     = c(S, slope)
        I     = c(I, int)
        }
    S    = S[order(S)]
    I    = I[order(S)]
    ch   = length(S)
    K    = length(S[S<(-1)])
    if ((ch %% 2)==0)  slo = (S[(ch)/2 + K] + S[(ch)/2 + K + 1])/2
    else               slo = S[(ch+1)/2 + K]
    M1   = round((ch-qnorm(1-conf.level/2) * sqrt((n * (n-1) * (2*n+5))/18))/2,0)
    M2   = ch-M1+1
    CIs  = c(S[M1+K], S[M2+K])
    int  = median(a$y - slo*a$x, na.rm=T)
    CIi  = c(median(a$y - CIs[2]*a$x, na.rm=T), median(a$y - CIs[1]*a$x, na.rm=T))
    fit  = slo*a$x+int
    res  = a$y - fit
    l    = length(res[res>0])
    L    = length(res[res<0])
    ri   = ifelse(res>0, sqrt(L/l), ifelse(res<0, -sqrt(l/L),0))
    Di   = (a$y + 1/slo*a$x - int)/sqrt(1+1/(slo^2))
    csum = cumsum(ri[order(Di)])
    m    = matrix(c(int, CIi, slo, CIs), nrow=2, byrow=T)
    rownames(m) = c("Intercept","Slope")
    colnames(m) = c("Estimate", "2.5%CI", "97.5%CI")
    invisible(structure(list(coefficients = m,
                                residuals = res,
                            fitted.values = fit,
                                    model = a,
                                        n = c(nread=nread,
                                              nused=n),
                                        S = S,
                                        I = I,
                                      adj = c(Ss=ch,
                                               K=K,
                                              M1=M1,
                                              M2=M2,
                                               l=l,
                                               L=L),
                                    cusum = csum,
                                       Di = Di,
                                    meths = meths ),
                        class="PBreg"))
}



#' Predict results from PBreg object
#'
#' A predict method for the \code{"PBreg"} class object, that is a result of Passing-Bablok regression.
#'
#' @param object an object of class \code{"PBreg"}.
#' @param newdata an optional vector of new values of \code{x} to make predictions for. If omitted, the fitted values will be used.
#' @param interval type of interval calculation - either \code{confidence} or \code{none}. The former is the default.
#' @param level String. The type of interval to compute. Either "tolerance" or "confidence" (the default).
#' @param ... Not used
#'
#' @returns If \code{interval} is \code{"confidence"} this function returns a data frame with three columns: "fit", "lwr" and "upr" - similarly to \code{predict.lm}.
#'
#' If \code{interval} is \code{"none"} a vector of predicted values is returned.
#'
#' @author Michal J. Figurski \email{mfigrs@gmail.com}
#'
#' @examples
#' ## Model data frame generation
#' a <- data.frame(x=seq(1, 30)+rnorm(mean=0, sd=1, n=30),
#'                 y=seq(1, 30)*rnorm(mean=1, sd=0.4, n=30))
#'
#' ## Call to PBreg
#' x <- PBreg(a)
#' print(x)
#' predict(x, interval="none")
#'
#' ## Or the same using "Meth" object
#' a <- Meth(a, y=1:2)
#' x <- PBreg(a)
#' print(x)
#' predict(x)
#'
#' @rdname predict
#' @export
predict.PBreg <- function(object, newdata = object$model$x, interval="confidence", level=0.95,...) {
    S    = object$S
    ch   = object$adj["Ss"]
    x    = newdata
    N    = length(x)
    n    = nrow(object$model)
    K    = object$adj["K"]
    M1   = round((ch-qnorm(1-(1-level)/2) * sqrt((n * (n-1) * (2*n+5))/18))/2,0)
    M2   = ch-M1+1
    S    = S[(M1+K):(M2+K)]
    I    = NULL
    for (i1 in 1:length(S)) {
        I    = c(I, median(object$model$y - S[i1] * object$model$x))
    }

    y.ci = data.frame(fit=rep(NA,N), lwr=rep(NA, N), upr=rep(NA, N))
    for (i1 in 1:N) {
        y    = x[i1] * S + I
        y.ci[i1,] = quantile(y, c(0.5,0,1), na.rm=T)
    }

    if (interval=="confidence") { return(y.ci) }
    else { return(as.vector(y.ci$fit)) }
}

#' @export
print.PBreg <- function(x,...) {
    cat("\nPassing-Bablok linear regression of", x$meths[2], "on", x$meths[1], "\n\n")
    cat(paste("Observations read: ", x$n[1], ", used: ", x$n[2],"\n", sep=""))
    cat(paste("Slopes calculated: ", x$adj[1], ", offset: ", x$adj[2],"\n\n",sep=""))
    print(x$coefficients)
    cat("\nUnadjusted summary of slopes:\n")
    print(summary(x$S))
    cat("\nSummary of residuals:\n")
    print(summary(x$residuals))
    cat("\nTest for linearity:")
    # !!! Not working very well !!!
    cat(if(any((x$cusum<1.36*sqrt(x$adj[5]+x$adj[6]))==FALSE)) " (failed)\n" else " (passed)\n")
    cat("Linearity test not fully implemented in this version.\n\n")
}


#' Passing-Bablok regression - plot method
#' 
#' A plot method for the \code{"PBreg"} class object, that is a result of
#' Passing-Bablok regression.
#' 
#' @param x an object of class \code{"PBreg"}
#' @param pch Which plotting character should be used for the points.
#' @param bg Background colour for the plotting character.
#' @param xlim Limits for the x-axis.
#' @param ylim Limits for the y-axis.
#' @param xlab Label on the x-axis.
#' @param ylab Label on the y-axis.
#' @param subtype a numeric value or vector, that selects the desired plot
#' subtype.  Subtype \bold{1} is an x-y plot of raw data with regression line
#' and confidence boundaries for the fit as a shaded area.  This is the
#' default.  Subtype \bold{2} is a ranked residuals plot.  Subtype \bold{3} is
#' the "Cusum" plot useful for assessing linearity of the fit. Plot subtypes 1
#' through 3 are standard plots from the 1983 paper by Passing and Bablok - see
#' the reference.  Plot subtype \bold{4} is a histogram (with overlaid density
#' line) of the individual slopes.  The range of this plot is limited to 5 x
#' IQR for better visibility.
#' @param colors A list of 6 elements allowing customization of colors of
#' various plot elements. For plot subtype 1: "CI" is the color of the shaded
#' confidence interval area; and "fit" is the color of fit line. For plot
#' subtypes 2 & 3: "ref" is the color of the horizontal reference line. For
#' plot subtype 4: "bars" is the bar background color, "dens" is the color of
#' the density line, and "ref2" is a vector of two colors for lines indicating
#' the median and confidence limits.
#' @param ... other parameters as in \code{"plot"}, some of which are
#' pre-defined for improved appearance. This affects only the subtype 1 plot.
#' @author Michal J. Figurski \email{mfigrs@@gmail.com}
#' @seealso \code{\link{PBreg}, \link{Deming}}.
#' @references Passing, H. and Bablok, W. (1983), A New Biometrical Procedure
#' for Testing the Equality of Measurements from Two Different Analytical
#' Methods. \emph{Journal of Clinical Chemistry and Clinical Biochemistry},
#' \bold{Vol 21}, 709--720
#' @examples
#' 
#'   ## Model data frame generation
#'   a <- data.frame(x=seq(1, 30)+rnorm(mean=0, sd=1, n=30),
#'                   y=seq(1, 30)*rnorm(mean=1, sd=0.4, n=30))
#' 
#'   ## Call to PBreg
#'   x <- PBreg(a)
#'   print(x)
#'   par(mfrow=c(2,2))
#'   plot(x, s=1:4)
#' 
#'   ## Or the same using "Meth" object
#'   a <- Meth(a, y=1:2)
#'   x <- PBreg(a)
#'   print(x)
#'   par(mfrow=c(2,2))
#'   plot(x, s=1:4)
#' 
#' @rdname plot
#' @export
plot.PBreg <- function(x, pch=21, bg="#2200aa33", xlim=c(0, max(x$model)), ylim=c(0, max(x$model)),
    xlab=x$meths[1], ylab=x$meths[2], subtype=1, colors = list(CI="#ccaaff50", fit="blue",
    ref="#99999955", bars="gray", dens="#8866aaa0", ref2=c("#1222bb99","#bb221299") ), ...)
    {
    ints    = c(x$coefficients[3],x$coefficients[1],x$coefficients[5])
    slos    = c(x$coefficients[4],x$coefficients[2],x$coefficients[6])
    if (any(subtype==1)) {
        m       = max(x$model, na.rm=T)
        xs      = seq(-0.1*m, 1.1*m, length=70)
        cis     = cbind(x=xs, predict(x, newdata=xs, interval="confidence"))
        plot(x$model, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, type="n", ...)
        px = c(-.1*m, cis$x, 1.1*m, 1.1*m, rev(cis$x), -.1*m)
        py = c(-.1*m, cis$upr, 1.1*m, 1.1*m, rev(cis$lwr), -.1*m)
        polygon(px,py,col=colors[["CI"]], border=NA)
        abline(0,1, lwd=0.5, lty=2)
        points(x$model, pch=pch, bg=bg)
        abline(ints[2], slos[2], lwd=2, col=colors[["fit"]])

        text(m*0.10,m*0.94, "Intercept =", adj=c(1,0), cex=0.8)
        text(m*0.12,m*0.94, paste(formatC(ints[2], digits=4, format="g"), " [", formatC(ints[1], digits=4, format="g"),
            " : ", formatC(ints[3], digits=4, format="g"), "]", sep=""), adj=c(0,0), cex=0.8)
        text(m*0.10,m*0.90, "Slope =", adj=c(1,0), cex=0.8)
        text(m*0.12,m*0.90, paste(formatC(slos[2], digits=4, format="g"), " [", formatC(slos[1], digits=4, format="g"),
            " : ", formatC(slos[3], digits=4, format="g"), "]", sep="")  , adj=c(0,0), cex=0.8)
    }
    if (any(subtype==2)) {
        ranked = x$residuals[order(x$Di)]
        ylim   = c(-max(abs(ranked)),max(abs(ranked)))
        plot(ranked, ylab="Residuals", ylim=ylim, pch=pch, bg=bg)
        abline(0,0, col=colors[["ref"]], lwd=1.5)
    }
    if (any(subtype==3)) {
        ylim   = c(-max(abs(x$cusum)),max(abs(x$cusum)))
        plot(x$cusum, ylab="Cusum", type="l", ylim=ylim, lwd=2)
        abline(0,0, col=colors[["ref"]], lwd=1.5)
    }
    if (any(subtype==4)) {
        S = x$S[x$S> slos[2]-2.5*IQR(x$S, na.rm=T) & x$S< slos[2]+2.5*IQR(x$S, na.rm=T)]
        h = hist(S, xlab="Individual slopes (range: 5 x IQR)", col=colors[["bars"]], main="",...)
        d = density(S, na.rm=T)
        d$y = d$y*(max(h$counts)/max(d$y))
        lines(d, lwd=2, col=colors[["dens"]])
        abline(v=slos, col=colors[["ref2"]], lwd=c(0.5,1.5), lty=c(2,1))
    }
}

