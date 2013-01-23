\name{plot.alpha}
\alias{plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Alpha related plot
}
\description{
Generate plot related to alpha. Three plot can be generated. (1) The weight plot will plot the weight associated with alpha calculation. (2) Profile plot will plot the cases with smallest weights and the average value. (3) If diag=TRUE, the program will try different downweight rate for determining a better rate.}
\usage{
\method{plot}{alpha}(res, weight = TRUE, profile = 5, diag = FALSE, length = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res}{
Results from the function \code{\link{cronbach}}.
}
  \item{weight}{
Whether to plot the weights.
}
  \item{profile}{
Number of cases used on the profile plot. At most 10.}
  \item{diag}{
Whether to conduct the diagnostics analysis for downweight rate.}
  \item{length}{
The number of rate tried between 0 and varphi.
}
}
\author{
Zhiyong Zhang and Ke-Hai Yuan
}
\references{
Zhang, Z. & Yuan, K.-H. (2013). Robust Cronbach's alpha.
}
\examples{
data(y)
alpha<-cronbach(y)
plot(alpha)
}