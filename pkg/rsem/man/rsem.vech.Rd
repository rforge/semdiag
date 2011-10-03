\name{rsem.vech}
\alias{rsem.vech}
\title{
Stacking lower triange of a matrix to a vector
}
\description{
Stacking lower triange of a matrix to a vector
}
\usage{
rsem.vech(x)
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
x<-array(1:9, c(3,3))
rsem.vec(x)
}

