\name{rsem.pattern}
\alias{rsem.pattern}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Obtaining missing data patterns
}
\description{
This function obtains the missing data patterns and the number of cases in each patterns. It also tells the number of observed variables and their indices for each pattern.
}
\usage{
rsem.pattern(x, n, p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A matrix as data
}
  \item{n}{
Sample size (not required)
}
  \item{p}{
Number of variables (not required)
}
}
\details{
The missing data pattern matrix has 2+p columns. The first column is the number cases in that pattern. The second column is the number of observed variables. The last p columns are a permutation of 1:p where the first part includes the indices for the observed variables and the second part includes the indices for the unobserved variables. 
}
\value{
  \item{x}{Data ordered according to missing data pattern}
  \item{comp2 }{Missing data pattern matrix}
}
\references{
Ke-Hai Yuan and Zhiyong Zhang (2011) Robust Structural Equation Modeling with Missing Data and Auxiliary Variables
}
\author{
Ke-Hai Yuan and Zhiyong Zhang
}

\examples{
#dset<-read.table('MardiaMV25.dat.txt', na.string='-99')  
#dset<-data.matrix(dset)                                  
#n<-dim(dset)[1]
#p<-dim(dset)[2]
#miss_pattern<-rsem.pattern(n,p,dset)
#miss_pattern
}
