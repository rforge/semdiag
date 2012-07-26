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
rsem.pattern(x, print=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A matrix as data
}
  \item{print}{
Whether to print the missing data pattern. The default is TRUE.
}
}
\details{
The missing data pattern matrix has 2+p columns. The first column is the number cases in that pattern. The second column is the number of observed variables. The last p columns are a matrix with 1 denoting observed data and 0 denoting missing data. 
}
\value{
  \item{x}{Data ordered according to missing data pattern}
  \item{misinfo}{Missing data pattern matrix}
  \item{mispat}{Missing data pattern in better readable form.}
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
