\name{fitRTDE}
\alias{fitRTDE}
\alias{summary.fitRTDE}
\alias{print.fitRTDE}
\alias{plot.fitRTDE}

\title{Fitting a Tail Dependence model with a Robust Estimator}
\description{
  Fit a Tail Dependence model with a Robust Estimator.
}
\usage{
fitRTDE(obs, nbpoint, alpha, omega, method="MDPDE", fix.arg=list(rho=-1),
    boundary.method="log", control=list())


\method{print}{fitRTDE}(x, \dots)
\method{summary}{fitRTDE}(object, \dots)
\method{plot}{fitRTDE}(x, which=1:2, main, \dots)

}
\arguments{
    \item{obs}{bivariate numeric dataset.}
    \item{nbpoint}{a numeric for the number of largest points to be selected.}
    \item{alpha}{a numeric for the power divergence parameter.}
    \item{omega}{a numeric for omega, see section Details.}

    \item{method}{a character string equals to \code{"MDPDE"}.}
    \item{fix.arg}{a named list of fixed arguments:
        either \eqn{rho} only e.g. \code{list(rho=-1)}
        or \eqn{rho, delta} e.g. \code{list(rho=-1, delta=0)}.}
    \item{boundary.method}{a character string: either "log"
        or "simple", see section Details.}
    \item{control}{A list of control paremeters. See section Details.}


  \item{x, object}{an \R object inheriting from \code{"fitRTDE"}.}
  \item{\dots}{arguments to be passed to subsequent methods.}
  \item{which}{an integer (1 or 2) to specify whether to plot eta or
  delta, respectively.}
  \item{main}{a main title for the plot.}

}
\details{
    The function \code{fitRTDE} fits an extended Pareto distribution 
    (\eqn{\eta,\tau} are fitted while \eqn{\rho} is fixed)
    on the relative excess of \eqn{Z_\omega} (see \code{\link{zvalueRTDE}})
    using a robust estimator based on the minimum distance power 
    divergence criterion (see \code{\link{MDPD}}).
    The boundary enforcement on \eqn{\eta,\tau} is either done
    by the bounded BFGS algorithm (see \code{\link{optim}} with 
    \code{method="L-BFGS-B"}) or by the bounded Nelder-Mead
    algorithm (see \code{\link{constrOptim}} with
    \code{method="Nelder-Mead"}) .


}
\value{
  \code{fitRTDE} returns an object of class \code{"fitRTDE"}
  having the following components:
    \describe{
    \item{\code{n}}{rownumber of \code{data}.}
    \item{\code{n0}}{rownumber of \code{contamin}.}
    \item{\code{alpha}}{a vector of \code{alpha} parameters.}
    \item{\code{omega}}{a vector of \code{omega} parameters.}
    \item{\code{m}}{a vector of \code{nbpoint}.}
    \item{\code{rho}}{a numeric for \code{rho}.}
    \item{\code{eta}}{estimate of \eqn{eta}.}
    \item{\code{delta}}{estimate of \eqn{delta}.}
    \item{\code{Ztilde}}{see \code{\link{zvalueRTDE}}.}
}



}

\references{
 C. Dutang, Y. Goegebeur, A. Guillou (2014),
  \emph{Robust and bias-corrected estimation of the coefficient of tail dependence},
  Volume 57, Insurance: Mathematics and Economics

This work was supported by a research grant (VKR023480) from VILLUM FONDEN and an international project for scientific cooperation (PICS-6416).


}
\author{
Christophe Dutang
}
\examples{

#####
# (1) simulation 

omega <- 1/2
m <- 48
n <- 100
obs <- cbind(rupareto(n), rupareto(n)) + rupareto(n)

#function of m
system.time(
x <- fitRTDE(obs, nbpoint=m:(n-m), 0, 1/2)
)
x
summary(x)
plot(x, which=1)
plot(x, which=2)


}
\keyword{distribution}
