\name{EPD}
\alias{EPD}
\alias{dEPD}
\alias{pEPD}
\alias{qEPD}
\alias{rEPD}

\title{The Extended Pareto Distribution}
\description{
  Density function, distribution function, quantile function, random generation.
}
\usage{
dEPD(x, eta, delta, rho, tau, log = FALSE)
pEPD(q, eta, delta, rho, tau, lower.tail=TRUE, log.p = FALSE)
qEPD(p, eta, delta, rho, tau, lower.tail=TRUE, log.p = FALSE,
    control=list())
rEPD(n, eta, delta, rho, tau)    
}
\arguments{
    \item{x, q}{vector of quantiles.}
    \item{p}{vector of probabilities.}
    \item{n}{number of observations. If \code{length(n) > 1}, the length
    is taken to be the number required.}

    \item{eta}{first shape parameter.}
    \item{delta}{nuisance parameter.}
    \item{rho, tau}{second shape parameter.}
    
    \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
    \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}, otherwise, \eqn{P[X > x]}.}

    \item{control}{A list of control paremeters. See section Details.}
}
\details{
    The extended Pareto distribution is defined by the following density
    \deqn{
        f(x) = \frac{1}{\eta} x^{-1/\eta-1}[1+\delta(1-x^{-\tau})]^{-1/\eta-1}[1+\delta(1-(1-\tau)x^{-\tau})]
    }{
        f(x) = 1/\eta x^{-1/\eta-1}[1+\delta(1-x^{-\tau})]^{-1/\eta-1}[1+\delta(1-(1-\tau)x^{-\tau})]
    }
    for all \eqn{x>1} when parametrized by \eqn{\tau}.
    However, a typical parametrization is obtained by
    setting \eqn{\tau=-\rho/\eta}, i.e.
    \deqn{
        f(x) = \frac{1}{\eta} x^{-1/\eta-1}[1+\delta(1-x^{\rho/\eta})]^{-1/\eta-1}[1+\delta(1-(1+\rho/\eta)x^{\rho/\eta})]
    }{
        f(x) = 1/\eta x^{-1/\eta-1}[1+\delta(1-x^{\rho/\eta})]^{-1/\eta-1}[1+\delta(1-(1+\rho/\eta)x^{\rho/\eta})]
    }
    for all \eqn{x>1} when parametrized by \eqn{\rho}.


  The \code{control} argument is a list that can supply any of the
  following components:
  \describe{
    \item{\code{upperbound}}{The upperbound used in the \code{optimize} function
        when computing numerical quantiles, default to \code{1e6}.}
    \item{\code{tol}}{the desired accuracy used in the \code{optimize} function
        when computing numerical quantiles, default to \code{1e-9}.}
    }
}
\value{
  \code{dEPD} gives the density,
  \code{pEPD} gives the distribution function,
  \code{qEPD} gives the quantile function, and
  \code{rEPD} generates random deviates.

  The length of the result is determined by \code{n} for
  \code{rEPD}, and is the maximum of the lengths of the
  numerical parameters for the other functions.  
  
  The numerical parameters other than \code{n} are recycled to the
  length of the result.  Only the first elements of the logical
  parameters are used.
}

\references{
J. Beirlant, E. Joossens, J. Segers (2009),
  \emph{Second-order refined peaks-over-threshold modelling for heavy-tailed distributions}, 
  Journal of Statistical Planning and Inference,
Volume 139, Issue 8, Pages 2800-2815.

 C. Dutang, Y. Goegebeur, A. Guillou (2014),
  \emph{Robust and bias-corrected estimation of the coefficient of tail dependence},
  Insurance: Mathematics and Economics

This work was supported by a research grant (VKR023480) from VILLUM FONDEN and an international project for scientific cooperation (PICS-6416).



}
\author{
Christophe Dutang
}
\examples{

#####
# (1) density function
x <- seq(0, 5, length=24)

cbind(x, dEPD(x, 1/2, 1/4, -1))

#####
# (2) distribution function

cbind(x, pEPD(x, 1/2, 1/4, -1, lower=FALSE))




		
}
\keyword{distribution}
