\name{RFRAdiag.summary}
\alias{RFRAdiag.summary}
\title{The RFRAdiag.summary function}
\description{A function that summarizes the results from the "R-FRA" method.}
\usage{
RFRAdiag.summary(d, alpha = 0.025, digits = 2)
}
\arguments{
  \item{d}{Distance from semdiag function.}
  \item{alpha}{Alpha level is 0.025 by default.}
  \item{digits}{Digits of results to be kept.}
}

\value{
An object which contains IDs of identified outliers and leverage observations.
}
\references{
Yuan, K.-H., & Zhang, Z. (2012). Structural equation modeling diagnostics using R package semdiag and EQS. Structural Equation Modeling: An Interdisciplinary Journal, 19(4), 683-702.
}
\author{
Xin Tong and Zhiyong Zhang
}

\keyword{R-FRA method}
