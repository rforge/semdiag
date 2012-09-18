\name{rsem}
\alias{rsem}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
The main function for robust SEM analysis
}
\description{
This is the function to carry out all analysis.
}
\usage{
rsem(dset, select, EQSmodel, moment=TRUE, varphi=.1, st='i', max.it=1000, eqsdata='data.txt', eqsweight='weight.txt', EQSpgm="C:/Progra~1/EQS61/WINEQS.EXE", serial="1234")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dset}{
A data matrix or a data frame
}
  \item{select}{
Variables to be seleted for SEM analysis. If omitted, all variables in the data set will be used.
}
  \item{moment}{With mean structure. For covariance only, set moment=FALSE.}
  \item{EQSmodel}{
The input file  for EQS. If omitted, only the first-stage analysis will be conducted.
}
  \item{varphi}{
  Proportion of data to be down-weighted. Default is 0.1.
}
  \item{max.it}{Maximum number of iterations for EM. Default is 1000}
  \item{st}{Starting values for EM algorithm. The default is 0 for mean and I for covariance. Alternative, the starting values can be estimated according to MCD.}
  \item{eqsdata}{Data file name used in EQS}
  \item{eqsweight}{File name for weight matrix}
  \item{EQSpgm}{The path to the installed EQS program}
  \item{serial}{The serial no of EQS}
}
\details{
This function will run the robust analysis and output results.
}
\value{
%%  ~Describe the value returned
If \code{EQSmodel} is not supplied
  \item{sem}{Information for SEM analysis including estimated means, covariance matrix and their sandwich type covariance matrix in the order of mean first and then covariance matrix.}
  \item{misinfo}{Information related to missing data pattern}
  \item{em}{Results from expectation robust algorithm}
  \item{ascov}{Covariance matrix}
If \code{EQSmodel} is supplied,   
  \item{sem}{Information for SEM analysis including estimated means, covariance matrix and their sandwich type covariance matrix according to the requirement of EQS.}
In addition, the following model parameters are from EQS  
  \item{fit.stat}{Fit indices and associated p-values}
  \item{para}{Parameter estimates}
  \item{eqs}{All information from REQS}  
}
\references{
Ke-Hai Yuan and Zhiyong Zhang (2011) Robust Structural Equation Modeling with Missing Data and Auxiliary Variables
}
\author{
Ke-Hai Yuan and Zhiyong Zhang
}

\seealso{
\code{\link{rsem.pattern}}, \code{\link{rsem.emmusig}}, \code{\link{rsem.Ascov}} 
}
\examples{
\dontrun{
## an example
 ## to use eqs, first load the package semdiag
 library(semdiag)
 data(mardiamv25)
 analysis<-rsem(mardiamv25, c(1,2,4,5), 'eqsinput.eqs')
 }
}
