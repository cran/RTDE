\name{Frank}
\alias{Frank}
\alias{dfrank}
\alias{pfrank}
\alias{qfrank}
\alias{rfrank}

\title{The Eyraud Farlie Gumbel Morgenstern Distribution}
\description{
  Density function, distribution function, quantile function, random generation.
}
\usage{
dfrank(u, v, alpha, log = FALSE)
pfrank(u, v, alpha, lower.tail=TRUE, log.p = FALSE)
qfrank(p, alpha, lower.tail=TRUE, log.p = FALSE)
rfrank(n, alpha)
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
    The Frank is defined by the following distribution function
    \deqn{
        C(u,v) = - \frac{1}{\alpha} \log\left[1-\frac{(1-e^{-\alpha u})(1-e^{-\alpha v}) }{ 1-e^{-\alpha}}\right],
    }{
        C(u,v) = - 1/\alpha *log(
        1-(1-exp(-\alpha u))*(1-exp(-\alpha v)) /( 1-exp(-\alpha))
        ),
    }
    for all \eqn{u,v} in [0,1].
    When \code{lower.tail=FALSE}, \code{pfrank} returns the survival copula 
    \eqn{P(U > u, V > v)}.

}
\value{
  \code{dfrank} gives the density,
  \code{pfrank} gives the distribution function,
  \code{qfrank} gives the quantile function, and
  \code{rfrank} generates random deviates.

  The length of the result is determined by \code{n} for
  \code{rfrank}, and is the maximum of the lengths of the
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

cbind(u, v, dfrank(u, v, 1/2))
cbind(u, v, outer(u, v, dfrank, alpha=1/2))


#####
# (2) distribution function

cbind(u, v, pfrank(u, v, 1/2))
cbind(u, v, outer(u, v, pfrank, alpha=1/2))




		
}
\keyword{distribution}
