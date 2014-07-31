\name{qqparetoplot}
\alias{qqparetoplot}

\title{The QQ Pareto plot}
\description{
  Plot the quantile-quantile Pareto plot
}
\usage{
qqparetoplot(x, ..., highlight=c("red","cross"))

}
\arguments{
    \item{x}{data vector.}
    \item{highlight}{character string used in \code{points}
        to plot outliers.}

    \item{\dots}{further arguments for \code{plot.default}.}
}
\details{
\code{qqparetoplot} plots the quantile-quantile Pareto plot
and may highlight some points having name \code{"new"}.

}
\value{
Invisible list with component \code{x} for the x-coordinates
and \code{y} for the y-coordinates.
}

\author{
Christophe Dutang
}
\examples{

#####
# (1) small examples

set.seed(1234)
x <- rupareto(100)
qqparetoplot(x)

x <- rexp(100)
qqparetoplot(x)

		
}
\keyword{distribution}
