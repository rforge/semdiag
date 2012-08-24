\name{rsem.fit}
\alias{rsem.fit}
\title{
Calculate robust test statistics
}
\description{
Calculate robust test statistics
}
\usage{
rsem.fit(object, gamma, musig)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{object}{
Output from lavaan analysis, such as growth, factor, sem functions.
}
  \item{gamma}{
Robust covariance matrix for saturated mean and covariances
}

  \item{musig}{
Robust saturated mean and covariances
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

