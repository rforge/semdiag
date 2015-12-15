\name{mdout}
\alias{mdout}
\title{Identify multivariate outlying observations in a dataset}
\description{
Identify multivariate outlying observations based on squared Mahalanobis distances.
}
\usage{
mdout(data,alpha)
}
\arguments{
  \item{data}{A matrix which contains data to be detected.}
  \item{alpha}{The Alpha level for the test.}
}

\value{
 An object which contains IDs of the identified multivariate outlying observations.
}

\author{Xin Tong and Zhiyong Zhang}


\examples{
\dontrun{
data(N300)
outlying <- mdout(N300,alpha=0.025)
outlying
}
}

\keyword{multivariate outlying observation detection}