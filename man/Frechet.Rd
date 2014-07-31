\name{Frechet}
\alias{Frechet}
\alias{dfrechet}
\alias{pfrechet}
\alias{qfrechet}
\alias{rfrechet}
\alias{dufrechet}
\alias{pufrechet}
\alias{qufrechet}
\alias{rufrechet}

\title{The Frechet Distribution}
\description{
  Density function, distribution function, quantile function, random generation.
}
\usage{
dfrechet(x, shape, xmin, log = FALSE)
pfrechet(q, shape, xmin, lower.tail=TRUE, log.p = FALSE)
qfrechet(p, shape, xmin, lower.tail=TRUE, log.p = FALSE)
rfrechet(n, shape, xmin)

dufrechet(x, log = FALSE)
pufrechet(q, lower.tail=TRUE, log.p = FALSE)
qufrechet(p, lower.tail=TRUE, log.p = FALSE)
rufrechet(n)

}
\arguments{
    \item{x, q}{vector of quantiles.}
    \item{p}{vector of probabilities.}
    \item{n}{number of observations. If \code{length(n) > 1}, the length
    is taken to be the number required.}

    \item{shape}{shape parameter.}
    \item{xmin}{lower bound parameter.}

    \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
    \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}, otherwise, \eqn{P[X > x]}.}

}
\details{
    The Frechet distribution is defined by the following density
    \deqn{
        f(x) = shape * (x - xmin)^{(-shape-1)} * exp(-(x - xmin)^{(-shape)})
    }{
        f(x) = shape * (x - xmin)^{(-shape-1)} * exp(-(x - xmin)^{(-shape)})
    }
    for all \eqn{x>xmin}.
    The unit Frechet distribution corresponds to \code{xmin=0} and 
    \code{shape=1}.
}
\value{
  \code{dfrechet, dufrechet} give the density,
  \code{pfrechet, pufrechet} give the distribution function,
  \code{qfrechet, qufrechet} give the quantile function, and
  \code{rfrechet, rufrechet} generate random deviates.

  The length of the result is determined by \code{n} for
  \code{rfrechet, rufrechet}, and is the maximum of the lengths of the
  numerical parameters for the other functions.  
  
  The numerical parameters other than \code{n} are recycled to the
  length of the result.  Only the first elements of the logical
  parameters are used.
}

\references{
 Kotz, S. and Nadarajah, S. (2000),
  \emph{Extreme Value Distributions: Theory and Applications}, 
  Imperial College Press.

 Beirlant, J., Goegebeur, Y., Teugels, J., Segers (2004),
  \emph{Statistics of Extremes: Theory and Applications},
  John Wiley and Sons.


}
\author{
Christophe Dutang
}
\examples{

#####
# (1) density function
x <- seq(0, 5, length=24)

cbind(x, dfrechet(x, 1/2, 1/4))

#####
# (2) distribution function

cbind(x, pfrechet(x, 1/2, 1/4))




		
}
\keyword{distribution}
