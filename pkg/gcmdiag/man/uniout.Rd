\name{uniout}
\alias{uniout}
\title{Identify univariate outlying observations for a variable}
\description{
Identify univariate outlying observations (i.e., univariate outliers) for a variable
}
\usage{
uniout(data)
}
\arguments{
  \item{data}{A vector which contains data to be detected.}
}

\value{
An object which contains IDs of the identified univariate outlying observations.
}

\author{Xin Tong and Zhiyong Zhang}

\examples{
\dontrun{
data(N300)
outlier <- uniout(N300[,1])
outlier
}
}

\keyword{univariate outlying observation detection}