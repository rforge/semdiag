\name{BCa.CI}
\alias{BCa.CI}

\title{
Bias-corrected and accelerated confidence intervals.
}
\description{
This function BCa.CI constructs bias-corrected and accelerated confidence intervals.
}
\usage{
BCa.CI(est.b, est, B, ahat, a1, a2)
}

\arguments{
  \item{est.b}{
a matrix of bootstrap sample estimates for multiple variables. Each column represents one variable.
}
  \item{est}{
the original sample estimate.
}
  \item{B}{
number of bootstrap replications.
}
  \item{ahat}{
estimated acceleration constant.
}
  \item{a1}{
a scaler or a numeric vector of percentage(s) for the lower limit of the confidence intervals.
}
  \item{a2}{
a scaler or a numeric vector of percentage(s) for the upper limit of the confidence intervals.
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
Perc.CI
}
