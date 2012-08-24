\name{rsem.print}
\alias{rsem.print}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Orgnize the output for Lavaan with roubust s.e. and test statistics
}
\description{
Orgnize the output for Lavaan with roubust s.e. and test statistics. Modified from the print function of Lavaan.
}
\usage{
rsem.print(object, robust.se, robust.fit, estimates=TRUE, fit.measures=FALSE, standardized=FALSE, rsquare=FALSE, std.nox=FALSE, modindices=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Output from lavaan analysis, such as growth, factor, sem functions.
}
  \item{robust.se}{
Robust standard error from the function rsem.se
}
  \item{robust.fit}{Robust fit statistics from the function rsem.fit}
  \item{estimates}{
Show parameter estimates
}
  \item{fit.measures}{
  Show fit statistics of lavaan (no need for it)
}
  \item{standardized}{standardized coefficients}
  \item{rsquare}{R square for dependent variables.}
  \item{std.nox}{to add}
  \item{modindices}{Modification indeces}
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
##\dontrun{
## an example
 data(mardiamv25)
 names(mardiamv25)<-paste('V', 1:5, sep='')
 
 fa.model<-'f1 =~ V1 + V2
f2 =~ V4 + V5 
f1 ~ 1
f2 ~ 1
V1 ~0*1
V2 ~0*1
V4 ~0*1
V5 ~0*1'

pat<-rsem.pattern(mardiamv25)

phi<-0.1
musig<-rsem.emmusig(pat, varphi=phi)

res.lavaan<-sem(fa.model, sample.cov=musig$sigma, sample.mean=musig$mu, sample.nobs=88,mimic='EQS')

ascov<-rsem.Ascov(pat, musig, varphi=phi)

robust.se<-rsem.se(res.lavaan, ascov$Gamma)

robust.fit <- rsem.fit(res.lavaan, ascov$Gamma, musig)

rsem.print(res.lavaan, robust.se, robust.fit)
## }
}
