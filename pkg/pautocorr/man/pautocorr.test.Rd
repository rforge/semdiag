\name{pautocorr.test}
\alias{pautocorr.test}

\title{
The main function for the six tests of autocorrelations and partial autocorrelations
}
\description{
This is the function to carry out the six tests of autocorrelations and partial autocorrelations.
}
\usage{
pautocorr.test(x, lagmax=3, method='all', alpha = 0.05, L=30, B=1000, 
digits=3, print = FALSE)
}

\arguments{
  \item{x}{
a numeric vector of time series data.
}
  \item{lagmax}{
maximum lag at which to calculate autocorrelations.
}
  \item{method}{
character string giving the method of tests of aucorrelations. method = 'surrogate' will perform the surrogate data method (the default); method = 'asymptotic' will perform the conventional asymptotic test on autocorrelations; method='bartlett' will perform a test based on Bartlett's formula; method = 'vmb' will perform tests using the vectorized moving block bootstrap; method='all' will conduct all types of analysis.
}
  \item{alpha}{
the significance level. Default is 0.05.
}
  \item{L}{
an upper limit for the summed terms in Bartlett's formula. Default is 30.
}
  \item{B}{
a number of resampling replications for the surrogate data method or the vectorized moving block bootstrap. 
}
  \item{digits}{
integer indicating the number of decimal places.
}
\item{print}{
whether to print the output.
}
}
\details{
The pautocorr.test function will perform tests on autocorrelations using four different methods: the surrogate data method (surrogate), the 1/T approximation method (asymptotic), the Bartlett's formula method (bartlett), or the vectorized moving block bootstrap (vmb).

The surrogate data method simulates the sampling distribution of autocorrelation under the null by shuflying the order of the time series data. The conventional 1/T approximation method is motivated by the fact that under the null, the time series data reduced to uncorrelated data and uses the standard error estimator of correlations (sqrt(1/N)) as that of autocorrelations. The Bartlett's formula method computes standard error estimates of aucorrelations according to Bartlett's formula. The vectorized moving block bootstrap method simulates the sampling distribution of autocorrelations under the alternative by implementing an improved version of the moving block bootstrap which tackles the "bad joints" problem in the conventional simple moving block bootstrap. 
}
\value{
The function pautocorr.test returns a table in which rows correspond to autocorrelations at various lags; and columns report sample estimates of autocorrelations, standard error estimates and z statistics (if the asymptotic 1/T approximation method is selected) or the upper and lower limits of the constructed confidence intervals, and whether the test is significant given the chosen significance level.}
\references{
Bartlett, M. S. (1955). An introduction to stochastic processes. Cambridge: Cambridge University Press.
Shumway, R. H., & Stoffer, D. S. (2006). Time series analysis and its applications with r examples (2nd ed.). New York: Springer.
Theiler, J., Eubank, S., Longtin, A., Galdrikan, B., & Farmer, J. D. (1992). Testing for nonlinearity in time series: The method of surrogate data. Physica D, 58, 77-94.
Kunsch, H. (1989). The jackknife and the bootstrap for general stationary observations. The Annals of Statistics, 17(3), 1217-1241.

}
\author{
Zijun Ke <keziyun@mail.sysu.edu.cn> and Zhiyong Zhang <zhiyongzhang@nd.edu>
}

