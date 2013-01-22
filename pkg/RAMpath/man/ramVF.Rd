\name{RAMVF}
\alias{RAMVF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate a vector field plot based on the bivariate lcsm
}
\description{
Generate a vector field plot based on the bivariate lcsm
}
\usage{
RAMVF(RAMout, ylim, xlim, ninterval=10, scale=.1, length=.25, ...)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{RAMout}{
Output from the RAMBLCS function
}
  \item{ylim}{
Range of y data, for example, c(0,80) from 0 to 80
}
  \item{xlim}{
Range of x data, for example, c(0,80) from 0 to 80
}
  \item{ninterval}{
Number of intervals for plotting. The default is 10.
}
  \item{scale}{
Time interval to calculate vector fields.
}
  \item{length}{
The length of arrows to plot
}
  \item{\dots}{
Options for plot and arrows function.
}
}
\examples{
data(ex3)
test.blcs<-RAMBLCS(ex3, 1:6, 7:12, RAM.out=TRUE)
RAMVF(test.blcs, c(0,80),c(0,80), length=.05, xlab='X', ylab='Y',scale=.5, ninterval=9)
}