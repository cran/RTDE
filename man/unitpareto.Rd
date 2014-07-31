\name{upareto}
\alias{upareto}
\alias{dupareto}
\alias{pupareto}
\alias{qupareto}
\alias{rupareto}

\title{The unit Pareto Distribution}
\description{
  Density function, distribution function, quantile function, random generation.
}
\usage{
dupareto(x, log = FALSE)
pupareto(q, lower.tail=TRUE, log.p = FALSE)
qupareto(p, lower.tail=TRUE, log.p = FALSE)
rupareto(n)
}
\arguments{
    \item{x, q}{vector of quantiles.}
    \item{p}{vector of probabilities.}
    \item{n}{number of observations. If \code{length(n) > 1}, the length
    is taken to be the number required.}

    \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
    \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}, otherwise, \eqn{P[X > x]}.}

}
\details{
    The extended Pareto distribution is defined by the following density
    and distribution function
    \deqn{
        f(x) = \frac{1}{x^2}, F(x) = 1-\frac{1}{x},
    }{
        f(x) = 1/x^2, F(x) = 1-1/x,
    }
    for all \eqn{x>0}.


}
\value{
  \code{dupareto} gives the density,
  \code{pupareto} gives the distribution function,
  \code{qupareto} gives the quantile function, and
  \code{rupareto} generates random deviates.

  The length of the result is determined by \code{n} for
  \code{rupareto}, and is the maximum of the lengths of the
  numerical parameters for the other functions.  
  
  The numerical parameters other than \code{n} are recycled to the
  length of the result.  Only the first elements of the logical
  parameters are used.
}

\references{
 Johnson, N.L., Kotz, S. and Balakrishnan, N. (2000),
  \emph{Continuous Univariate Distributions, Volume 1, Second Edition}, 
  John Wiley and Sons.

}
\author{
Christophe Dutang
}
\examples{

#####
# (1) density function
x <- seq(0, 5, length=24)

cbind(x, dupareto(x))

#####
# (2) distribution function

cbind(x, pupareto(x))




		
}
\keyword{distribution}
