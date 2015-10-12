\name{VMB}
\alias{VMB}

\title{
The vectorized moving block bootstrap
}
\description{
This function VMB performs the vectorized moving block bootstrap to carry out tests on autocorrelations.
}
\usage{
VMB(acf.est, ahat, data, l, a1, a2, boot, lagmax)
}

\arguments{
  \item{acf.est}{
a vector of sample autocorrelation estimates up to lagmax.
}
  \item{ahat}{
a vector of estimated acceleration constants for autocorrelations up to lagmax. An object returned by JN.VMB
}
  \item{data}{
a vector of time series data with no missing values.
}
  \item{l}{
block size for the vectorized moving block bootstrap.
}
  \item{a1}{
a scaler or a numeric vector of percentage(s) for the lower limit(s) of confidence intervals.
}
  \item{a2}{
a scaler or a numeric vector of percentage(s) for the lower limit(s) of confidence intervals.
}
  \item{boot}{
number of bootstrap replications.
}
  \item{lagmax}{
maximum lag at which to calculate autocorrelations.
}
}
\details{
The vectorized moving block bootstrap constructs blocks by splitting the original time series data of length n.t into n.t-l+1 blocks of length l. These n.t-l+1 blocks are then resampled with replacement. Observations in the selected blocks and observations h lags later are paired and are used to compute the estimate for the autocorrelation at lag h.
}
\value{
a list containing two components:
se:	standard error estimates for autocorrelations.
CI:	a list of estimated confidence intervals for autocorrelations. Contain two components:
	per:		percentile intervals
	BCa:		bias-corrected and accelerated intervals.
}
\references{
Kunsch, H. (1989). The jackknife and the bootstrap for general stationary observations. The Annals of Statistics, 17(3), 1217-1241.
Zhang, G., & Browne, M. W. (2010). Bootstrap standard error estimates in dynamic factor analysis. Multivariate Behavioral Research, 45(453-482).
}
\author{
Zijun Ke <keziyun@mail.sysu.edu.cn> and Zhiyong Zhang <zhiyongzhang@nd.edu>
}

\seealso{
JN.VMB,pair.VMB,corr.VMB
}
