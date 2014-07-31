\name{dataRTDE}
\alias{dataRTDE}
\alias{summary.dataRTDE}
\alias{print.dataRTDE}
\alias{plot.dataRTDE}

\title{Data object used for a Tail Dependence model}
\description{
  Data object used for a Tail Dependence model.
}
\usage{
dataRTDE(obs, simu.nb, simu.marg=c("ufrechet", "upareto"), 
    simu.cop=c("indep", "FGM", "Frank"), simu.cop.par=NULL,
    contamin.eps=NULL, contamin.method=c("NA","max+","+"),
    contamin.marg=c("ufrechet", "upareto"),
    contamin.cop=c("indep", "FGM", "Frank"),
    contamin.cop.par=NULL, control=list())


\method{print}{dataRTDE}(x, \dots)
\method{summary}{dataRTDE}(object, \dots)
\method{plot}{dataRTDE}(x, which=1:2, \dots)

}
\arguments{
    \item{obs}{bivariate numeric dataset.}

    \item{simu.nb}{a numeric for the sample size of simulated data.}
    \item{simu.marg}{a character string for the marginal distribution: 
        either \code{"ufrechet"} (default) or \code{"upareto"}.}
    \item{simu.cop}{a character string ofr the copula: 
        either \code{"indep"} (default), \code{"FGM"} or \code{"Frank"}.}
    \item{simu.cop.par}{a numeric for the copula parameter, default to \code{NULL}.}

    \item{contamin.eps}{a numeric for the percentage (of \code{simu.nb}) of contaminated data.}
    \item{contamin.method}{a character string for the contamination method:
        either \code{"NA"} (default), \code{"max+"} or \code{"+"}.}
    \item{contamin.marg}{a character string for the marginal distribution:
        either \code{"ufrechet"} (default) or \code{"upareto"}.}
    \item{contamin.cop}{a character string ofr the copula:
        either \code{"indep"} (default), \code{"FGM"} or \code{"Frank"}.}
    \item{contamin.cop.par}{a numeric for the copula parameter, default to \code{NULL}.}

    \item{control}{A list of control paremeters. Unused.}

  \item{x, object}{an \R object inheriting from \code{"dataRTDE"}.}
  \item{\dots}{arguments to be passed to subsequent methods.}
  \item{which}{an integer (1 or 2) to specify whether to plot in 
  original scale or unit-Pareto scale, respectively.}

}
\details{
    The function \code{dataRTDE} handles empirical or simulated data and may 
    add a contamination. 
    \describe{
    \item{Empirical data}{When \code{obs} is provided, \code{dataRTDE} just wraps the 
        two-column matrix \eqn{(X_i, Y_i)_i}.}
    \item{Simulated data}{When \code{simu.XXX} are provided, 
        \code{dataRTDE} simulates random vectors \eqn{(X_i, Y_i)_i}
        from the copula \code{simu.cop} with parameter \code{simu.cop.par} and 
        marginal \code{simu.marg}.}
    }
    Note that end-user must choose between empirical data (\code{obs} is provided) and simulated
    data (\code{simu.XXX} are provided). Not both can be provided.
    In addition to data handling \eqn{(X_i, Y_i)_i}, 
    a contamination can be processed by adding new simulated points \eqn{(\tilde X_i, \tilde Y_i)_i}
    when \code{contamin.method != "NA"}.
    Those points \eqn{(\tilde X_i, \tilde Y_i)_i} are simulated from the copula 
    \code{contamin.cop} with parameter \code{contamin.cop.par} and marginal \code{contamin.cop.par}.
    If \code{contamin.method != "+"}, the points \eqn{(\tilde X_i, \tilde Y_i)_i} are the contaminations,
    while if \code{contamin.method != "max+"} the contaminations are obtained by adding the
    component-wise maximum of the data: \eqn{(\tilde X_i + X_{n,n}, \tilde Y_i)_i + Y_{n,n}},
    where \eqn{X_{n,n}=max(X_1,...,X_n)}, idem for \eqn{Y_{n,n}}.

}
\value{
  \code{dataRTDE} returns an object of class \code{"dataRTDE"}
  having the following components:
    \describe{
    \item{\code{n}}{rownumber of \code{data}.}
    \item{\code{n0}}{rownumber of \code{contamin}.}
    \item{\code{data}}{original or simulated data.}
    \item{\code{contamin}}{contaminated data.}
}



}

\references{
 C. Dutang, Y. Goegebeur, A. Guillou (2014),
  \emph{Robust and bias-corrected estimation of the coefficient of tail dependence},
  Volume 57, Insurance: Mathematics and Economics

This work was supported by a research grant (VKR023480) from VILLUM FONDEN and an international project for scientific cooperation (PICS-6416).
}
\seealso{
See \code{\link{fitRTDE}} for the fitting process and
\code{\link{zvalueRTDE}} for the z-value computation.

}

\author{
Christophe Dutang
}
\examples{

#####
# (1) simulation

n <- 100
x <- dataRTDE(simu.nb=n, simu.marg="ufrechet", simu.cop="indep")
print(x)
summary(x)
plot(x, xlab="x", ylab="y")

#####
# (2) part of the workers' compensation dataset

x1 <- c(
  21.798086,  22.640528,  22.572010,  24.789710,  25.876764,  28.033613,
  22.525887,  12.004031,  12.713178,  13.596610,  14.811727,  12.774073,
  20.245789,  24.242468,  50.216515,  56.099793,  58.109747,  67.807105,
  73.852437,  84.208474,  83.604216,  19.507341,  20.810822,  23.838122,
  24.212193,  25.367578,  35.401344,  37.580989,  12.428727,  13.492474,
  23.471988,  24.101833,  24.766193,  26.078216)

x2 <- c(
 0.538707, 0.439184, 1.059775, 0.560013, 1.004997, 1.097314, 0.609833, 0.270222,
 0.229566, 0.596850, 0.196539, 0.134248, 0.489312, 0.418218, 0.769208, 0.649707,
 0.503919, 0.675466, 0.545745, 1.562266, 0.931762, 0.291125, 0.499927, 0.151084,
 0.141910, 0.300373, 0.119761, 0.141300, 0.377662, 0.169574, 0.243585, 0.061215,
 0.055272, 0.312816, 0.160196, 0.623029, 0.280707, 0.174422, 0.176666, 0.153907,
 0.605122, 0.664457, 0.348918, 0.370878)

obs <- dataRTDE(cbind(x1, x2))
obs
summary(obs)

plot(obs)

}
\keyword{distribution}
