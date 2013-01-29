\name{summary.alpha}
\alias{summary}
\alias{summary.alpha}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print alpha and its confidence interval.
}
\description{
Print alpha and its confidence interval.}
\usage{
\method{summary}{alpha}(object, prob = 0.05,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Results from the function \code{\link{cronbach}}.
}
  \item{prob}{
Alpha level for confidence interval.
}
  \item{...}{Option can be passed to the summary function.}
}
\author{
Zhiyong Zhang and Ke-Hai Yuan
}
\references{
Zhang, Z. & Yuan, K.-H. (2013). Robust Cronbach's alpha.
}
\examples{
data(y)
alpha<-cronbach(y)
summary(alpha)
}
