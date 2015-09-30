\name{Bartlett_Shumway}
\alias{Bartlett_Shumway}

\title{
Standard error estimates based on Bartlett's formula
}
\description{
The function Bartlett_Shumway computes standard error estimates of autocorrelations based on Bartlett's formula. This function is not intended to use separately from the function BartlettSE. 
}
\usage{
Bartlett_Shumway(x, rho, L, T)
}

\arguments{
  \item{x}{
lag of the target autocorrelation.
}
  \item{rho}{
vector of estimated autocorrelations. The first element should be the autocorrelation at lag 0, that is, 1.
}
  \item{L}{
upper limit for the summed terms to calculate standard error estimates of autocorrelations. Default is 30.
}
  \item{T}{
time series length or number of time points.
}
}

\value{
This function returns a standard error estimate of autocorrelation at lag x.
}
\references{
Bartlett, M. S. (1955). An introduction to stochastic processes. Cambridge: Cambridge University Press. (p.289)

Shumway, R. H., & Stoffer, D. S. (2006). Time series analysis and its applications with r examples (2nd ed.). New York: Springer.  (p.519)
}
\author{
Zijun Ke <keziyun@mail.sysu.edu.cn> and Zhiyong Zhang <zhiyongzhang@nd.edu>
}

\seealso{
\code{\link{BartlettSE}}
}

