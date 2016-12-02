\name{BartlettSE}
\alias{BartlettSE}
\alias{Bartlett_Shumway}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Standard error estimates for autocorrelations based on Bartlett's formula
}
\description{
Function BartlettSE is a wrapper of Bartlett_Shumway. This function returns standard error estimates of autocorrelations up to lagmax.
}
\usage{
BartlettSE(x, L, lagmax)
Bartlett_Shumway(x, rho, L, T)
}

\arguments{
  \item{x}{a numeric vector of time series data.}
  \item{L}{an upper limit for the summed terms. Default is 30.}
  \item{lagmax}{maximum lag at which to calculate autocorrelations.}
  \item{T}{time series length or number of time points.}
}
\details{
Compute standard error estimates for autocorrelations using Bartlett's formula.
In the presence of missing values, estimates of autocorrelations are computed using available cases, which may not be valid. 
}

\value{a vector of standard error estimates of autocorrelations up to lagmax.}

\references{
Bartlett, M. S. (1955). An introduction to stochastic processes. Cambridge: Cambridge University Press. (p.289)
Shumway, R. H., & Stoffer, D. S. (2006). Time series analysis and its applications with r examples (2nd ed.). New York: Springer.  (p.519)
}
\author{Zijun Ke <keziyun@mail.sysu.edu.cn> and Zhiyong Zhang <zhiyongzhang@nd.edu>}
\note{
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
