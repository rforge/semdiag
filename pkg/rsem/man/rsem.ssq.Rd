\name{rsem.ssq}
\alias{rsem.ssq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the squared sum of a matrix
}
\description{
Calculate the squared sum of a matrix
}
\usage{
rsem.ssq(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A matrix
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
rsem.ssq(x)
}

