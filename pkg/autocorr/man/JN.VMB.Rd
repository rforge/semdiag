\name{JN.VMB}
\alias{JN.VMB}

\title{
The acceleration for the vectorized moving block bootstrap.
}
\description{
This function computes the acceleration constants for the vectorized moving block bootstrap.
}
\usage{
JN.VMB(data, lagmax, l)
}

\arguments{
  \item{data}{
a vector of time series data.
}
  \item{lagmax}{
maximum lag at which to calculate autocorrelations.
}
  \item{l}{
block size of the vectorized moving block bootstrap
}
}
\details{
Computation of this jackknife estimate of acceleration involves computing autocorrelations based on pairs of observations from samples with l consecutive observations left out at a time.

In the presence of missing values, estimates of autocorrelations are computed from complete cases, which may not be valid.
}
\value{
a vector of the acceleration constants for autocorrelations up to lagmax.
}
\references{
Kunsch, H. (1989). The jackknife and the bootstrap for general stationary observations. The Annals of Statistics, 17(3), 1217-1241.
}
\author{
Zijun Ke <keziyun@mail.sysu.edu.cn> and Zhiyong Zhang <zhiyongzhang@nd.edu>
}

\seealso{
pair.VMB,corr.VMB,VMB
}
