\name{Surrogate}
\alias{Surrogate}
\alias{JN.NB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{The surrogate data method}

\description{
Function Surrogate carries out the surrogate data method for testing (partial) autocorrelations. 
Function JN.NB computes the acceleration constant for the surrogate data method by sequentially deleting one observation from the sample data points.
}
\usage{
Surrogate(acf.est, ahat, x, a1, a2, boot, lagmax)
JN.NB(x, lagmax)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{acf.est}{a vector of sample autocorrelation estimates up to lagmax.}
  \item{ahat}{a vector of estimated acceleration constants for autocorrelations up to lagmax. An object returned by JN.NB.}
  \item{x}{a vector of time series data.}
  \item{a1,a2}{the percentages for the lower and upper limits of confidence intervals.}
  \item{boot}{number of surrogate data sets}
  \item{lagmax}{maximum lag at which to calculate (partial) autocorrelations}
}
\details{
Function JN.NB iteratively deletes one data point at a time from the original sample and computes (partial) autocorrelation estimates at each iteration. The acceleration constant is obtained by computing a quantity similar to the skewness of (partial) autocorrelation estimates at different iterations.

In the presence of missing values, estimates of (partial) autocorrelations are computed from complete cases, which may not be valid.
}
\value{
The function Surrogate returns a list with two components: acf and pacf.

For acf:

se: standard error estimates for autocorrelations.

CI: a list of estimated confidence intervals for autocorrelations. Contain two elements: per and BCa

For acf:

se: standard error estimates for partial autocorrelations.

CI: a list of estimated confidence intervals for partial autocorrelations. Contain two elements: per and BCa
	
Function JN.NB returns a vector of accelerations for autocorrelations up to lagmax.

}

\references{
Theiler, J., Eubank, S., Longtin, A., Galdrikan, B., & Farmer, J. D. (1992). Testing for nonlinearity in time series: The method of surrogate data. Physica D, 58, 77-94.
Efron, B., & Tibshirani, R. J. (1994). An introduction to the bootstrap. Chapman and Hall/CRC.
}

\author{Zijun Ke <keziyun@mail.sysu.edu.cn> and Zhiyong Zhang <zhiyongzhang@nd.edu>}
