\name{MDPD}
\alias{MDPD}

\title{The Minimum Distance Power Divergence statistics}
\description{
  Computes the power divergence statistics then used a minimization problem.
}
\usage{
MDPD(theta, densfun, obs, alpha, ..., control=list())

}
\arguments{
    \item{theta}{the parameter of the distribution given as a vector.}
    \item{densfun}{a function computing the theoretical density function.}
    \item{obs}{a numeric vector of observations}
    \item{alpha}{a numeric for the power divergence parameter.}
    \item{\dots}{further arguments to be passed to the density function.}
    \item{control}{A list of control paremeters. See section Details.}

}
\details{
    The Power Divergence for a density function \eqn{f} and
    observations \eqn{X_1,...,X_n} is defined as
    \deqn{
        \Delta(f,\alpha) = \int_{R} f^{1+\alpha}(x)dx-\left ( 1+\frac{1}{\alpha} \right )
        \frac{1}{n} \sum_{i=1}^n f^\alpha(X_i)
    }{
        \Delta(f,\alpha) = integral( f^{1+\alpha}(x), dx, {R})-( 1+1/\alpha  ) \sum_{i=1}^n f^\alpha(X_i)/n
        }
    for \eqn{\alpha> 0}
    \deqn{
        \Delta(f,0) = -\frac{1}{n}\sum_{i=1}^n \log f(X_i)
    }{
        \Delta(f,0) = -\sum_{i=1}^n \log f(X_i)/n
        }
    for \eqn{\alpha = 0}.
    
    The \code{control} argument is a list that can supply any of the
  following components:
  \describe{
    \item{\code{eps}}{a small positive floating-point number used when
        \code{integrate} stalled, default to \code{1e-3}.}
    \item{\code{tol}}{the desired accuracy used in the \code{integrate} function
        when computing the power divergence, default to \code{1e-3}.}
    \item{\code{lower}}{the lower bound of the domain of the density function, 
        default to 1.}
    \item{\code{upper}}{the lower bound of the domain of the density function, 
        default to infinity.}
    }
}
\value{
  \code{MDPD} returns the power divergence against the density function \code{densfun}
  as a numeric.
}

\references{
Basu, A., Harris, I.R., Hjort, N.L., Jones, M.C., (1998).
\emph{Robust and efficient estimation by minimizing a density power divergence}, 
Biometrika, 85, 549-559.


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
# (1) small example

omega <- 1/2
m <- 10
n <- 100
obs <- cbind(rupareto(n), rupareto(n)) + rupareto(n)

#unit Pareto transform
z <- zvalueRTDE(obs, omega, nbpoint=m, output="relexcess")

MDPD(c(1/2, 1/4), dEPD, z$Z, alpha=0, rho=-1)




		
}
\keyword{distribution}
