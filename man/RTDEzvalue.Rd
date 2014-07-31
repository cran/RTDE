\name{zvalueRTDE}
\alias{zvalueRTDE}
\alias{summary.zvalueRTDE}
\alias{print.zvalueRTDE}
\alias{relexcess}
\alias{relexcess.default}
\alias{relexcess.zvalueRTDE}

\title{The Z-value random variable}
\description{
  Compute the Z-value variable from a bivariate dataset.
}
\usage{
zvalueRTDE(obs, omega, nbpoint, output=c("orig", "relexcess"), 
    marg=c("upareto", "ufrechet", "uunif"))

\method{print}{zvalueRTDE}(x, \dots)
\method{summary}{zvalueRTDE}(object, \dots)


relexcess(x, nbpoint, ...)
\method{relexcess}{default}(x, nbpoint, \dots)
\method{relexcess}{zvalueRTDE}(x, nbpoint, \dots)
}
\arguments{
    \item{obs}{bivariate numeric dataset.}
    \item{omega}{a numeric for omega, see Details.}
    \item{nbpoint}{a numeric for the number of largest points to be selected.}
    \item{output}{a character string for the output: 
        either \code{"orig"} for original value 
        or \code{"relexcess"} for relative excess.}
    \item{marg}{a character string for the empirical margin transformation:
        either \code{"upareto"} for unit Pareto, 
        \code{"ufrechet"} for unit Frechet or 
        \code{"uunif"} for unit uniform margin.}

  \item{x, object}{an \R object inheriting from \code{"zvalueRTDE"}.}
  \item{\dots}{arguments to be passed to subsequent methods.}

}
\details{
    Given a bivariate dataset \eqn{(X_i, Y_i)_i} of \eqn{n} points,
    two variables are defined:
    (1) for \code{output="orig"}, the \eqn{\tilde Z_{\omega,i}} variable
    \deqn{\tilde Z_{\omega,i} = \min \left(
        f\left(\frac{R_i^X}{n+1}\right),
        \frac{\omega}{1-\omega} f\left(\frac{R_i^Y}{n+1}\right) \right)
    }{ \min (
        f(R_i^X/(n+1)),
        \omega/(1-\omega) f(R_i^Y/(n+1)) )
    }
    where \eqn{f(x)} is the margin transformation and \eqn{i=1,...,n};
    (2) for \code{output="relexcess"}, the \eqn{Z_{j}} variable
    \deqn{
        \frac{\widetilde Z_{\omega,n-m+j,n}}{\widetilde Z_{\omega,n-m,n}}
    }{
        \widetilde Z_{\omega,n-m+j,n}/\widetilde Z_{\omega,n-m,n}
    }
    where \eqn{m} equals \code{nbpoint}, \eqn{j=1,\dots, m},
    and \eqn{\widetilde Z_{\omega,1,n},...,
     \widetilde Z_{\omega,n,n}} are the order statistics of 
     \eqn{\widetilde Z_{\omega,1},...,\widetilde Z_{\omega,n}}.
    The margin transformation is 
    \deqn{
    f(x) = \frac{1}{1-x}, f(x) = \frac{1}{-\log(x)}, f(x) = x,
    }{f(x) = 1/(1-x), f(x) = -1/log(x), f(x) = x,
    }
    respectively for unit Pareto (\code{marg="upareto"}),
    unit Frechet (\code{marg="ufrechet"}) and unit uniform margin
    (\code{marg="uunif"}).


}
\value{
  \code{zvalueRTDE} computes the Z-variable and
  returns an object of class \code{"zvalueRTDE"}
  having the following components \code{type} (either
  \code{"orig"} or \code{"relexcess"}), \code{omega},
  \code{Ztilde} or \code{Z}, \code{n}, possibly \code{m}.
  
  \code{relexcess} computes the relative excesses
  from a Z-variable and returns an object of class \code{"zvalueRTDE"}
  of type \code{"relexcess"}.

}

\references{
 C. Dutang, Y. Goegebeur, A. Guillou (2014),
  \emph{Robust and bias-corrected estimation of the coefficient of tail dependence},
  Volume 57, Insurance: Mathematics and Economics

This work was supported by a research grant (VKR023480) from VILLUM FONDEN and an international project for scientific cooperation (PICS-6416).
}
\seealso{
See \code{\link{fitRTDE}} for the fitting process and
\code{\link{dataRTDE}} for the data-handling process.

}
\author{
Christophe Dutang
}
\examples{

#####
# (1) example

omega <- 1/2
m <- 10
n <- 100
obs <- cbind(rupareto(n), rupareto(n)) + rupareto(n)

#unit Pareto transform
zvalueRTDE(obs, omega, output="orig")

relexcess(zvalueRTDE(obs, omega, output="orig"), m)
zvalueRTDE(obs, omega, nbpoint=m, output="relexcess")

		
}
\keyword{distribution}
