\name{FGM}
\alias{FGM}
\alias{dFGM}
\alias{pFGM}
\alias{qFGM}
\alias{rFGM}

\title{The Eyraud Farlie Gumbel Morgenstern Distribution}
\description{
  Density function, distribution function, quantile function, random generation.
}
\usage{
dFGM(u, v, alpha, log = FALSE)
pFGM(u, v, alpha, lower.tail=TRUE, log.p = FALSE)
qFGM(p, alpha, lower.tail=TRUE, log.p = FALSE)
rFGM(n, alpha)
}
\arguments{
    \item{u, v}{vector of quantiles.}
    \item{p}{vector of probabilities.}
    \item{n}{number of observations. If \code{length(n) > 1}, the length
    is taken to be the number required.}

    \item{alpha}{shape parameter.}

    \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
    \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}, otherwise, \eqn{P[X > x]}.}

}
\details{
    The FGM is defined by the following distribution function
    \deqn{
        C(u,v) = u*v*(1+\alpha*(1-u)*(1-v))
    }{
        C(u,v) = u*v*(1+\alpha*(1-u)*(1-v))
    }
    for all \eqn{u,v} in [0,1] and \eqn{\alpha} in [0,1].
    When \code{lower.tail=FALSE}, \code{pFGM} returns the survival copula 
    \eqn{P(U > u, V > v)}.

}
\value{
  \code{dFGM} gives the density,
  \code{pFGM} gives the distribution function,
  \code{qFGM} gives the quantile function, and
  \code{rFGM} generates random deviates.

  The length of the result is determined by \code{n} for
  \code{rFGM}, and is the maximum of the lengths of the
  numerical parameters for the other functions.  
  
  The numerical parameters other than \code{n} are recycled to the
  length of the result.  Only the first elements of the logical
  parameters are used.
}

\references{
 Nelsen, R. (2006),
  \emph{An Introduction to Copula, Second Edition}, Springer.

}
\author{
Christophe Dutang
}
\examples{

#####
# (1) density function
u <- v <- seq(0, 1, length=25)

cbind(u, v, dFGM(u, v, 1/2))
cbind(u, v, outer(u, v, dFGM, alpha=1/2))


#####
# (2) distribution function

cbind(u, v, pFGM(u, v, 1/2))
cbind(u, v, outer(u, v, pFGM, alpha=1/2))




		
}
\keyword{distribution}
