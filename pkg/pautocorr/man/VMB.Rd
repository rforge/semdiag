\name{VMB}
\alias{VMB}
\alias{JN.VMB}
\alias{pair.VMB}
\alias{corr.VMB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{The vectorized moving block bootstrap}

\description{
The function VMB tests (partial) autocorrelations using the vectorized moving block (VMB) bootstrap.
Function JN.VMB computes the acceleration constants for the VMB bootstrap.
Function pair.VMB returns position indices of pairs of observations for the VMB.
Function corr.VMB computes autocorrelations for the VMB bootstrap. This function is not intended to use separately from functions pair.VMB and VMB.
}

\usage{
VMB(acf.est, ahat, x, l, a1, a2, B, lagmax)
JN.VMB(x, lagmax, l)
corr.VMB(pair.id.M, x)
pair.VMB(id.M, lagmax, n.t)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{acf.est}{a vector of sample autocorrelation estimates up to lagmax.}
  \item{ahat}{a vector of estimated acceleration constants for autocorrelations up to lagmax. An object returned by JN.VMB.}
  \item{x}{a vector of time series data.}
  \item{l}{block size for the VMB bootstrap.}
  \item{a1,a2}{the percentages for the lower and upper limits of confidence intervals.}
  \item{B}{number of bootstrap replications.}
  \item{lagmax}{maximum lag at which to calculate autocorrelations.}
  \item{pair.id.M}{a list of position indices of pairs of observations to calculate autocorrelations. An object returned by the function pair.VMB. }
  \item{id.M}{a matrix giving position indices for observations in each resampled block. Columns represent different blocks.}
  \item{n.t}{time series length or the number of time points.}
}

\details{
The VMB bootstrap constructs blocks by splitting the original time series data of length n.t into n.t-l+1 blocks of length l. These n.t-l+1 blocks are then resampled with replacement. Observations in the selected blocks and observations h lags later are paired and are used to compute the estimate for the autocorrelation at lag h. Partial autocorrelations are estimated using the estimated autocorrelations through the Dubin-Levinson algorithm.

In the presence of missing values, estimates of autocorrelations are computed based on available cases, which may not be valid.

The jackknife estimate of acceleration for the VMB bootstrap involves computing autocorrelations based on pairs of observations from samples with l consecutive observations left out at a time. The Dubin-Levinson algorithm is used to computed partial autocorrelations using the estimated autocorrelations.

For autocorrelation at lag h, pair.VMB returns indices in id.M as well as indices of observations h lag apart from id.M. If indices exceed the time series length, those pairs of indices are removed.

}
\value{
The function VMB returns a list with two components:
	\item{acf}{
		\item{se}{standard error estimates for autocorrelations.}
		\item{CI}{a list of estimated confidence intervals for autocorrelations. Contain two elements:
			\item{per}{percentile intervals}
			\item{BCa}{bias-corrected and accelerated intervals.}
		}
	}
	\item{pacf}{
		\item{se}{standard error estimates for partial autocorrelations.}
		\item{CI}{a list of estimated confidence intervals for partial autocorrelations. Contain two elements:
			\item{per}{percentile intervals}
			\item{BCa}{bias-corrected and accelerated intervals.}
		}
	}
	

Function JN.VMB returns a list with two elements:
	\item{acf}{a vector of the acceleration constants for autocorrelations up to lagmax.}
	\item{pacf}{a vector of the acceleration constants for autocorrelations up to lagmax.}

Function corr.VMB returns a vector of estimated autocorrelations.

Function pair.VMB returns a list of indices of pairs of observations for computing autocorrelations up to lagmax, each of which is a matrix of indices for autocorrelation at a lag smaller than or equal to lagmax.

}
\references{
Künsch, H. (1989). The jackknife and the bootstrap for general stationary observations. The Annals of Statistics, 17(3), 1217-1241.
Zhang, G., & Browne, M. W. (2010). Bootstrap standard error estimates in dynamic factor analysis. Multivariate Behavioral Research, 45(453-482).
}
\author{
Zijun Ke <keziyun@mail.sysu.edu.cn> and Zhiyong Zhang <zhiyongzhang@nd.edu>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
JN.VMB,pair.VMB,corr.VMB
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
