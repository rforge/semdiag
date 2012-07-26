\name{rsem.emmusig}
\alias{rsem.emmusig}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Robust mean and covariance matrix using Huber-type weight
}
\description{
Robust mean and covariance matrix using Huber-type weight.
}
\usage{
rsem.emmusig(xpattern, varphi=.1, max.it=1000, st='i')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xpattern}{
Missing data pattern output from \code{rsem.pattern}. 
}
  \item{varphi}{
  Proportion of data to be down-weighted. Default is 0.1.
}
  \item{max.it}{Maximum number of iterations for EM. Default is 1000}
  \item{st}{Starting values for EM algorithm. The default is 0 for mean and I for covariance. Alternative, the starting values can be estimated according to MCD.}
}
\details{
Estimate mean and covariance matrix using the expectation robust (ER) algorithm.
}
\value{

  \item{err}{Error code. 0: good. 1: maximum iterations are exceeded.}
  \item{mu}{Mean vector}
  \item{sigma}{Covariance matrix}

}
\references{
Ke-Hai Yuan and Zhiyong Zhang (2011) Robust Structural Equation Modeling with Missing Data and Auxiliary Variables
}
\author{
Ke-Hai Yuan and Zhiyong Zhang
}

\seealso{
\code{\link{rsem.emmusig}}
}
\examples{
#dset<-read.table('MardiaMV25.dat.txt', na.string='-99')  
#dset<-data.matrix(dset)                                  
#n<-dim(dset)[1]
#p<-dim(dset)[2]
#miss_pattern<-rsem.pattern(n,p,dset)
#misinfo<-miss_pattern$misinfo
#V_forana<-c(1,2,4,5)
#em_results<-rsem.emmusig(dset,misinfo)
#em_results
}
