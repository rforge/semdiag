\name{rsem.Ascov}
\alias{rsem.Ascov}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sandwich-type covariance matrix 
}
\description{
Returns the sandwich type covariance matrix. This function is not intended to use seperately from the \code{rsem.emmusig} function.
}
\usage{
rsem.Ascov(xpattern, musig, varphi=.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xpattern}{
Missing data pattern output from \code{rsem.pattern}. 
}
  \item{musig}{
Robust mean and covariance matrix from \code{rsem.emmusig}
}

  \item{varphi}{
  Proportion of data to be down-weighted. Default is 0.1.
}

}
\details{
Data should be a matrix. To change a data frame to a matrix, using data.matrix(x).
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{Abeta}{A matrix}
  \item{Bbeta}{B matrix}
  \item{Gamma}{Sandwich type covariance matrix}
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
#hmu1<-em_results$mu                           
#hsigma1<-em_results$sigma
#rsem.Ascov(x, hmu1, hsigma1)
}

