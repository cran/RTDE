\name{RTDE}
\alias{RTDE}
\alias{summary.RTDE}
\alias{print.RTDE}
\alias{plot.RTDE}

\alias{prob}
\alias{prob.default}
\alias{prob.RTDE}


\title{Data object used for a Tail Dependence model}
\description{
  Data object used for a Tail Dependence model.
}
\usage{
RTDE(obs=NULL, simu=list(), contamin=list(),
    nbpoint, alpha, omega, method="MDPDE", fix.arg=list(rho=-1),
    boundary.method="log", core=1, keepdata, control=list())


\method{print}{RTDE}(x, \dots)
\method{summary}{RTDE}(object, \dots)
\method{plot}{RTDE}(x, which=1:3, FUN=mean, main, \dots)

prob(object, q, \dots)
\method{prob}{default}(object, q, \dots)
\method{prob}{RTDE}(object, q, \dots)


}
\arguments{
    \item{obs}{bivariate numeric dataset.}

    \item{simu}{a names list with components:
        \code{"nb"}, \code{"marg"}, \code{"cop"}, \code{"replicate"}.
        When needed, \code{"cop.par"} must be provided,
        see \code{\link{dataRTDE}}.}

    \item{contamin}{a names list with components:
        \code{"eps"}, \code{"method"}, \code{"marg"}, \code{"cop"}.
        When needed, \code{"cop.par"} must be provided,
        see \code{\link{dataRTDE}}.}

    \item{nbpoint}{a numeric for the number of largest points to be selected.}
    \item{alpha}{a numeric for the power divergence parameter.}
    \item{omega}{a numeric for omega, see section Details.}

    \item{method}{a character string equals to \code{"MDPDE"}.}
    \item{fix.arg}{a named list of fixed arguments:
        either \eqn{rho} only e.g. \code{list(rho=-1)}
        or \eqn{rho, delta} e.g. \code{list(rho=-1, delta=0)}.}
    \item{boundary.method}{a character string: either "log"
        or "simple", see section Details.}

    \item{core}{a numeric for the number of core to be used,
        only relevant for simulated data.}
    \item{keepdata}{a logical whether to return or not the dataset.}
    \item{control}{A list of control paremeters for \code{\link{fitRTDE}}.}

  \item{x, object}{an \R object inheriting from \code{"RTDE"}.}
  \item{\dots}{arguments to be passed to subsequent methods.}
  \item{which}{an integer to specify what to plot: 1 eta, 2 delta, 
  3 probability estimates.}
  \item{FUN}{the function to be applied, default to \code{\link{mean}}.}
  \item{main}{a main title for the plot.}
  \item{q}{vector of quantiles.}

}
\details{
    The function \code{RTDE} handles (empirical or simulated) data 
    (cf. \code{\link{dataRTDE}})
    and then fits a bivariate tail model using a \code{method} criterion
        (cf. \code{\link{fitRTDE}} and \code{\link{MDPD}}) based
        on an extended Pareto distribution approximation (\code{\link{EPD}}).
    Typical distributions for simulated data and/or contaminations are
    \itemize{
    \item{Marginal}{Unit Pareto \code{\link{upareto}}, Frechet \code{\link{Frechet}}.}
    \item{Copula}{Frank \code{\link{Frank}}, FGM \code{\link{FGM}}.}
    }
    For a good introduction, please refer to references.
}
\value{
  \code{RTDE} returns an object of class \code{"RTDE"}
  having the following components:
    \describe{
    \item{\code{obs.type}}{see \code{\link{dataRTDE}}.}
    \item{\code{data}}{see \code{\link{dataRTDE}}.}
    \item{\code{fit}}{see \code{\link{fitRTDE}}.}
    \item{\code{simu}}{see \code{\link{dataRTDE}}.}
    \item{\code{contamin}}{see \code{\link{dataRTDE}}.}
    \item{\code{setting}}{a list summarizing the computation.}
}
}

\seealso{

See \code{\link{fitRTDE}} for the fitting process and
\code{\link{dataRTDE}} for the data-handling process.

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

n <- 100
x <- RTDE(simu=list(nb=n, marg="ufrechet", cop="indep", replicate=1),
	nbpoint=10:11, alpha=0, omega=1/2)
x	
summary(x)


}
\keyword{distribution}
