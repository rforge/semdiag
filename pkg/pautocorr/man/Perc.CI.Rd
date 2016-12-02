\name{Perc.CI}
\alias{Perc.CI}
\alias{BCa.CI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Percentile and Bias-corrected and accelerated (BCa) intervals
}
\description{
Function Perc.CI constructs percentile intervals.
Function BCa.CI constructs BCa intervals.
}
\usage{
Perc.CI(est.b, a1, a2)
BCa.CI(est.b, est, B, ahat, a1, a2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{est.b}{a matrix of bootstrap sample estimates for multiple variables. Each column represents one variable.}
  \item{a1,a2}{the percentages for the lower and upper limits of confidence intervals.}
  \item{est}{the original sample estimates.}
  \item{B}{number of replications.}
  \item{ahat}{estimated acceleration constant.}
}
\details{
In the presence of missing values, estimates of autocorrelations are computed based on available cases, which may not be valid.
}
\value{a matrix of the obtained confidence intervals for (partial) autocorrelations.}

\references{Efron, B., & Tibshirani, R. J. (1994). An introduction to the bootstrap. Chapman and Hall/CRC.}

\author{Zijun Ke <keziyun@mail.sysu.edu.cn> and Zhiyong Zhang <zhiyongzhang@nd.edu>}
