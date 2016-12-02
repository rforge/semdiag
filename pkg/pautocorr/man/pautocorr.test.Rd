\name{pautocorr.test}
\alias{pautocorr.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Test the ACF and the PACF using both asymptotic and resampling methods
}
\description{
This is the function to carry out tests on the autocorrelation function (ACF) and the partial autocorrelation function (PACF).
}
\usage{
pautocorr.test(x,lagmax=3,method='all',alpha = 0.05,L=30,B=2000, digits=3,
		print = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a numeric vector of time series data.}
  \item{lagmax}{maximum lag at which to calculate autocorrelations.}
  \item{method}{character string giving the method of tests of (partial) autocorrelations. One of 'all' (the default),'asymptotic','surrogate' or 'vmb'. 'all': to conduct all tests; 'surrogate' to conduct tests using the surrogate data method; 'asymptotic' to conduct tests using the conventional asymptotic test; 'bartlett' to conduct tests based on Bartlett's formula; 'vmb' to conduct tests using the vectorized moving block bootstrap.}
  \item{alpha}{the significance level. Default is 0.05.}
  \item{L}{an upper limit for the summed terms in Bartlett's formula. Default is 30.}
  \item{B}{a number of resampling replications for the surrogate data method or the vectorized moving block bootstrap. }
  \item{digits}{integer indicating the number of decimal places.}
  \item{print}{logical. If TRUE (the default) numerical results are printed.}
}
\details{
The pautocorr.test function conduct tests on (partial) autocorrelations using both asymptotic and resampling methods: the surrogate data method (surrogate), the 1/T approximation method (asymptotic), the Bartlett's formula method (bartlett), and the vectorized moving block bootstrap (vmb). 

Function plot can be used to visualize results (only when method = 'all'). See plot.pautocorr.

The surrogate data method simulates the sampling distribution of (partial) autocorrelation under the null by shuffling the order of the time series data. The conventional 1/T approximation method is motivated by the fact that under the null, the time series data reduced to uncorrelated data and uses the standard error estimator of correlations (sqrt(1/N)) as that of (partial) autocorrelations. The Bartlett's formula method computes standard error estimates of autocorrelations according to Bartlett's formula. It is currently not available for partial autocorrelations. The vectorized moving block bootstrap method simulates the sampling distribution of (partial) autocorrelations under the alternative by implementing an improved version of the moving block bootstrap which tackles the "bad joints" problem in the conventional simple moving block bootstrap. 

In the presence of missing values, estimates of (partial) autocorrelations are computed based on available cases, which may not be valid.
}

\value{

A list of the following elements is returned invisibly: 
\item{acf}{
	\item{Asymptotic}{If method = 'asymptotic' or 'all', a matrix listing the estimated autocorrelations, standard error, z statistics and significance results is returned}
	\item{Bartlett}{If method = 'barlett' or 'all', a matrix listing the estimated autocorrelations, confidence intervals and significance results is returned}
	\item{Surrogate}{If method = 'surrogate' or 'all', a matrix listing the estimated autocorrelations, confidence intervals obtained using the percentile method and the BCa methods and significance results based on both types of intervals is returned}
	\item{VMB}{If method = 'vmb' or 'all', a matrix listing the estimated autocorrelations, confidence intervals obtained using the percentile method and the BCa methods and significance results based on both types of intervals is returned}
}
\item{pacf}{
	\item{Asymptotic}{If method = 'asymptotic' or 'all', a matrix listing the estimated partial autocorrelations, standard error, z statistics and significance results is returned}
	\item{Surrogate}{If method = 'surrogate' or 'all', a matrix listing the estimated partial autocorrelations, confidence intervals obtained using the percentile method and the BCa methods and significance results based on both types of intervals is returned}
	\item{VMB}{If method = 'vmb' or 'all', a matrix listing the estimated partial autocorrelations, confidence intervals obtained using the percentile method and the BCa methods and significance results based on both types of intervals is returned}
}
}
\references{

Bartlett, M. S. (1955). An introduction to stochastic processes. Cambridge: Cambridge University Press.
Efron, B., & Tibshirani, R. J. (1994). An introduction to the bootstrap. Chapman and Hall/CRC.
Shumway, R. H., & Stoffer, D. S. (2006). Time series analysis and its applications with r examples (2nd ed.). New York: Springer.
Theiler, J., Eubank, S., Longtin, A., Galdrikan, B., & Farmer, J. D. (1992). Testing for nonlinearity in time series: The method of surrogate data. Physica D, 58, 77-94.
K¨¹nsch, H. (1989). The jackknife and the bootstrap for general stationary observations. The Annals of Statistics, 17(3), 1217-1241.

}
\author{
Zijun Ke <keziyun@mail.sysu.edu.cn> and Zhiyong Zhang <zhiyongzhang@nd.edu>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
	\code{\link{http://127.0.0.1:31515/library/stats/html/acf.html }}
	plot.pautocorr
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
da = data(daily.moral)[,'daily.moral']
ts.data = ts(data = da,start = 1,end = 8,frequency = 7)
d.adj = da - decompose(ts.data)$trend
res = pautocorr.test(na.omit(d.adj),lagmax=15,print = TRUE)
plot(res)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
