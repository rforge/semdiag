\name{rsem.se}
\alias{rsem.se}
\title{
Calculate robust standard errors
}
\description{
Calculate robust standard errors
}
\usage{
rsem.se(object, gamma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{object}{
Output from lavaan analysis, such as growth, factor, sem functions.
}
  \item{gamma}{
Robust covariance matrix for saturated mean and covariances
}
}

\references{
Ke-Hai Yuan and Zhiyong Zhang (2011) Robust Structural Equation Modeling with Missing Data and Auxiliary Variables
}
\author{
Ke-Hai Yuan and Zhiyong Zhang
}

\examples{
x<-array(1:6, c(2,3))
rsem.vec(x)
}

