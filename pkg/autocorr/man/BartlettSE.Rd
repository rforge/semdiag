\name{BartlettSE}
\alias{BartlettSE}

\title{
Standard error estimates based on Bartlett's formula
}
\description{
BartlettSE is a wrapper of Bartlett_Shumway. This function returns standard error estimates of autocorrelations up to lagmax.
}
\usage{
BartlettSE(x, L, lagmax)
}
\arguments{
  \item{x}{
a numeric vector of time series data with no missing values
}
  \item{L}{
an upper limit for the summed terms. Default is 30.
}
  \item{lagmax}{
maximum lag at which to calculate autocorrelations.
}
}
\details{
Compute standard error estimates using Bartlett's formula.
}
\value{
a vector of standard error estimates of autocorrelations up to lagmax.
}
\references{
Bartlett, M. S. (1955). An introduction to stochastic processes. Cambridge: Cambridge University Press. (p.289)

Shumway, R. H., & Stoffer, D. S. (2006). Time series analysis and its applications with r examples (2nd ed.). New York: Springer.  (p.519)
}
\author{
Zijun Ke <keziyun@mail.sysu.edu.cn> and Zhiyong Zhang <zhiyongzhang@nd.edu>
}

\seealso{
\code{\link{Bartlett_Shumway}}
}
