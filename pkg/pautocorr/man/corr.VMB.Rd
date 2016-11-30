\name{corr.VMB}
\alias{corr.VMB}

\title{
Estimating autocorrelations for vectorized moving block bootstrap samples.
}
\description{
This function computes autocorrelations for vectorized moving block bootstrap samples when the position indices for pairs of observations are given. This function is not intended to use separately from functions pair.VMB and VMB.
}
\usage{
corr.VMB(pair.id.M, data)
}
\arguments{
  \item{pair.id.M}{
a list of position indices of pairs of observations to calculate autocorrelations. An object returned by the function pair.VMB. 
}
  \item{data}{
a numeric vector of time series data.
}
}
\details{In the presence of missing values, estimates of autocorrelations are computed from complete cases, which may not be valid.
}
\value{
a vector of estimated autocorrelations.
}
\references{
Kunsch, H. (1989). The jackknife and the bootstrap for general stationary observations. The Annals of Statistics, 17(3), 1217-1241.

Zhang, G., & Browne, M. W. (2010). Bootstrap standard error estimates in dynamic factor analysis. Multivariate Behavioral Research, 45(453-482).
}
\author{
Zijun Ke <keziyun@mail.sysu.edu.cn> and Zhiyong Zhang <zhiyongzhang@nd.edu>
}

\seealso{
pair.VMB for getting the position indices for pairs of observations; VMB for a wrapper function; JN.VMB for computing the acceleration constant for the vectorized moving block bootstrap.
}
