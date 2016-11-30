\name{Surrogate}
\alias{Surrogate}

\title{
The surrogate data method
}
\description{
This function Surrogate carries out the surrogate data method for testing autocorrelations. 
}
\usage{
Surrogate(ahat, data, a1, a2, boot, lagmax)
}
\arguments{
  \item{ahat}{
a vector of estimated acceleration constants for autocorrelations up to lagmax. An Object returned by JN.NB
}
  \item{data}{
a vector of time series data.
}
  \item{a1}{
a scaler or a numeric vector of percentage(s) for the lower limit(s) of confidence intervals.
}
  \item{a2}{
a scaler or a numeric vector of percentage(s) for the upper limit(s) of confidence intervals.
}
  \item{boot}{
number of surrogate data sets
}
  \item{lagmax}{
maximum lag at which to calculate autocorrelations
}
}
\details{
In the presence of missing values, estimates of autocorrelations are computed from complete cases, which may not be valid.
}
\value{
a list containing two components:
se	standard error estimates for autocorrelations under the null.
CI	a list of estimated confidence intervals for autocorrelations under the null. Contain two components:
	per	percentile intervals
	BCa	bias-corrected and accelerated intervals.

}
\references{
Ke, Z. & Zhang, Z. (2015). Testing Autocorrelations: Asymptotic Methods versus Resampling Techniques

Theiler, J., Eubank, S., Longtin, A., Galdrikan, B., & Farmer, J. D. (1992). Testing for nonlinearity in time series: The method of surrogate data. Physica D, 58, 77-94.
}
\author{
Zijun Ke <keziyun@mail.sysu.edu.cn> and Zhiyong Zhang <zhiyongzhang@nd.edu>
}

\seealso{
JN.NB
}

