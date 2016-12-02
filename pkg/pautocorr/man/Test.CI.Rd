\name{Test.CI}
\alias{Test.CI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Performing hypothesis testing using confidence intervals}
\description{
This function Test.CI returns the results of hypothesis testing based on confidence intervals.
}
\usage{Test.CI(CI, test.value)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{CI}{
a vector or a matrix of the lower and upper limit(s) of confidence interval(s) for multiple variables. Rows represent different variables.
}
  \item{test.value}{
a numeric vector with size equal to the number of rows in CI. Significance is determined by examining whether confidence intervals in CI cover the values in test.value.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a vector of 0 or 1. "1" indicates significant results whereas "0" indicates non-significant results.
}
\references{
%% ~put references to the literature/web site here ~
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
