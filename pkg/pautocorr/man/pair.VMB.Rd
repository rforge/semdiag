\name{pair.VMB}
\alias{pair.VMB}

\title{
Forming pairs of observations for the vectorized moving block bootstrap.
}
\description{
This function pair.VMB returns position indices of pairs of observations for the vectorized moving block bootstrap.
}
\usage{
pair.VMB(id.M, lagmax, n.t)
}

\arguments{
  \item{id.M}{
a matrix giving position indices for observations in each resampled block. Columns represent different blocks.
}
  \item{lagmax}{
maximum lag at which to calculate autocorrelations.
}
  \item{n.t}{
time series length or the number of time points.
}
}
\details{
For autocorrelation at lag h, pair.VMB returns indices in id.M as well as indices for observations h lag apart. If some indices exceed the time series length, then the corresponding pairs of observations are removed.
}
\value{
a list of indices of pairs of observations for computing autocorrelations up to lagmax, each of which is a matrix of indices for autocorrelation at a lag smaller than or equal to lagmax.
}
\references{
Kunsch, H. (1989). The jackknife and the bootstrap for general stationary observations. The Annals of Statistics, 17(3), 1217-1241.

Zhang, G., & Browne, M. W. (2010). Bootstrap standard error estimates in dynamic factor analysis. Multivariate Behavioral Research, 45(453-482).
}
\author{
Zijun Ke <keziyun@mail.sysu.edu.cn> and Zhiyong Zhang <zhiyongzhang@nd.edu>
}

\seealso{
corr.VMB,JN.VMB,VMB
}
