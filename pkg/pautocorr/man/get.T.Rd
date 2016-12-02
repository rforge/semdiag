\name{get.T}
\alias{get.T}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
find time series length with missing data
}
\description{
 count the number of available cases and compute the time series length
}
\usage{
	get.T(x,lagmax)
}
\arguments{
  \item{x}{a univariate series}
  \item{lagmax}{maximum lag at which to calculate autocorrelations.}
}
\details{The returned T for lag l equals the number of available cases (pairs of observations with no missing values) plus l. }
\value{a vector of size lagmax listing the time series lengthes for different lags}
\references{}
\author{Zijun Ke <keziyun@mail.sysu.edu.cn> and Zhiyong Zhang <zhiyongzhang@nd.edu>}
