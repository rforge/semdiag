\name{JN.NB}
\alias{JN.NB}

\title{
The acceleration constant for the surrogate data method.
}
\description{
This function JN.NB computes the acceleration constant for the surrogate data method by sequentially deleting one observation from the sample data points.
}
\usage{
JN.NB(data, lagmax)
}

\arguments{
  \item{data}{
a numeric vector of time series data.
}
  \item{lagmax}{
maximum lag at which to compute the acceleration constants for different autocorrelations.
}
}
\details{
This function iteratively deletes one data point at a time from the original sample and computes autocorrelations at each iteration. The acceleration constant is obtained by computing a quantity similar to the skewness of autocorrelation estimates at different iterations.

In the presence of missing values, estimates of autocorrelations are computed from complete cases, which may not be valid.
}
\value{
a vector of accelerations for autocorrelations up to lagmax.
}
\references{
Efron, B., & Tibshirani, R. J. (1994). An introduction to the bootstrap. Chapman and Hall/CRC.
}
\author{
Zijun Ke <keziyun@mail.sysu.edu.cn> and Zhiyong Zhang <zhiyongzhang@nd.edu>
}

\seealso{
Surrogate
}
