\name{plot.pautocorr}
\alias{plot.pautocorr}
\alias{plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot the results of pautocorr.test
}
\description{
This function visualizes the results of function pautocorr.test.
}
\usage{
\method{plot}{pautocorr}(x,alpha=0.05,
	layout.v = c(2,2),
	legendpos = list(A='bottomright',B ='topleft'),...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{x}{
		a list of results on (partial) autocorrelation testing returned by pautocorr.test. Plots are generated only when method = 'all'.
	}
	\item{alpha}{
		the significance level. Default is 0.05.
	}
	\item{layout.v}{
		A vector of the form c(nr, nc). Subsequent figures will be drawn in an nr-by-nc array on the device by rows (mfrow). 
	} 
	\item{legendpos}{
		a list of x and y co-ordinates to be used to position the legend. They can be specified by keyword or in any way which is accepted by xy.coords. See Examples.
	}
	\item{...}{Options passed to plot function}
}
\value{
A graph with two rows of ACF and PACF plots is generated. The first row visualizes the results of the asymptotic method and the surrogate data method. The second row visualizes the results of the Bartlett's formula method and the vectorized moving block bootstrap.

Note that plots will be generated only when method is set to 'all' in pautocorr.test.
}

\author{
Zijun Ke <keziyun@mail.sysu.edu.cn> and Zhiyong Zhang <zhiyongzhang@nd.edu>
}
