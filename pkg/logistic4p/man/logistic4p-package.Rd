\name{logistic4p-package}
\alias{logistic4p-package}
\docType{package}
\title{
\packageTitle{logistic4p}
}
\description{
\packageDescription{logistic4p}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{logistic4p}
\packageIndices{logistic4p}

}
\author{
\packageAuthor{logistic4p}

Maintainer: \packageMaintainer{logistic4p}
}
\references{Liu, H. and Zhang, Z. (2016) Logistic Regression with Misclassification in Dependent Variables: Method and Software.(In preparation.)

}

\keyword{Logistic regression, misclassification, binary outcome }

\examples{
data(nlsy)
x=nlsy[, -1]
y=nlsy[,1]
mod=logistic4p(x, y, model='fn')
}
