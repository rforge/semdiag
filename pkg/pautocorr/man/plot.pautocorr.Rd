\name{plot.pautocorr}
\alias{plot.pautocorr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plot the results of pautocorr.test
}
\description{
This function visualizes the results of function pautocorr.test.
}
\usage{
plot.pautocorr(x,alpha=0.05,
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
}
\details{
}
\value{
A graph with two rows of ACF and PACF plots is generated. The first row visualizes the results of the asymptotic method and the surrogate data method. The second row visualizes the results of the Bartlett's formula method and the vectorized moving block bootstrap.

Note that plots will be generated only when method is set to 'all' in pautocorr.test.
}
\references{
}
\author{
Zijun Ke <keziyun@mail.sysu.edu.cn> and Zhiyong Zhang <zhiyongzhang@nd.edu>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\code{\link{http://127.0.0.1:31515/library/stats/html/acf.html }}
	pautocorr.test
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
da = data(daily.moral)[,'daily.moral']
ts.data = ts(data = da,start = 1,end = 8,frequency = 7)
d.adj = da - decompose(ts.data)$trend
res = pautocorr.test(na.omit(d.adj),lagmax = 15)
plot(x,legendpos = list(A=c(9,-0.35),B ='topleft'))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
