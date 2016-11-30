\name{Perc.CI}
\alias{Perc.CI}

\title{
Percentile intervals.
}
\description{
This function constructs percentile intervals.
}
\usage{
Perc.CI(est.b, a1, a2)
}

\arguments{
  \item{est.b}{
a matrix of bootstrap sample estimates for multiple variables. Each column represents one variable.
}
  \item{a1}{
a scaler or a numeric vector of percentage(s) for the lower limit(s) of confidence intervals.
}
  \item{a2}{
a scaler or a numeric vector of percentage(s) for the upper limit(s) of confidence intervals.
}
}

\value{
a list with components of a size the same as that of a1. Each component records estimated confidence intervals with a confidence level specified by each element in a1. The first column in each component reports the lower limits of the estimated confidence intervals, whereas the second reports the upper limits.
}
\references{
Efron, B., & Tibshirani, R. J. (1994). An introduction to the bootstrap. Chapman and Hall/CRC.
}
\author{
Zijun Ke <keziyun@mail.sysu.edu.cn> and Zhiyong Zhang <zhiyongzhang@nd.edu>
}

\seealso{
BCa.CI
}
