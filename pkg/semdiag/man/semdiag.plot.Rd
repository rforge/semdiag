\name{semdiag.plot}
\alias{semdiag.plot}
\alias{semdiag.summary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot the diagnostics graphs
}
\description{
Plot the diagnostics graphs
}
\usage{
semdiag.plot(d, alpha=.01, label=0, cex=1)

semdiag.summary(d, alpha=.01, digits=2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{
Distance from semdiag function
}
  \item{label}{
Label automatically if 0. 1, label manually
}
\item{cex}{size of the labels}
  \item{alpha}{Critical value, default 0.01}
  \item{digits}{Digits of results to be kept}
}
\value{
Print outliers and leverage cases. 
}
