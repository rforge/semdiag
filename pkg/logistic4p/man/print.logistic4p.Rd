\name{print.logistic4p}
\alias{print.logistic4p}

\title{Printing Outputs of Logistic Regression with Misclassification Parameters

}
\description{
This is an function to print the inherit outputs of. logistic4p
}
\usage{
\method{print}{logistic4p}(x, ...)
}

\arguments{
  \item{x}{
An object of class 'logistic4p'.
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}

\author{
Haiyan Liu and Zhiyong Zhang
}
\examples{
data(nlsy)
y=nlsy[,1]
x=nlsy[,-1]

mod=logistic4p(x,y)
print.logistic4p(mod)
}

