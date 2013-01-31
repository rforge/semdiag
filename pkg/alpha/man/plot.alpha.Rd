\name{plot.alpha}
\alias{plot.alpha}
\alias{plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Alpha related plot
}
\description{
Generate plot related to alpha. Three plot can be generated. (1) The weight plot will plot the weight associated with alpha calculation. (2) Profile plot will plot the cases with smallest weights and the average value. (3) If diag=TRUE, the program will try different downweight rate for determining a better rate.}
\usage{
\method{plot}{alpha}(x, type="weight", profile=5, interval=0.01,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Results from the function \code{\link{cronbach}}.
}
  \item{type}{
Three types of plots can be generated. \code{type="weight"} generates the plot of weight of each case.
\code{type="profile"} generates a profile plot for the smallest weight. 
\code{type="weight"} generates the diagnostics plot for \code{varphi} with an interval of .01. 
}
  \item{profile}{
Number of cases used on the profile plot. At most 10.}
  \item{interval}{
The interval used in the diagnostic plot. The default is 0.01.}
\item{...}{Options can be passed to the \code{\link{plot}} function.}
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
