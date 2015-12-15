\name{gcmdiag}
\alias{gcmdiag}
\title{The gcmdiag function}
\description{
A function to identify outlying observations in growth curve modeling using six different methods.
}
\usage{
gcmdiag(data, method, lgcm, alpha = 0.025)
}

\arguments{
  \item{data}{Data frame or data matrix.}
  \item{method}{Select a method from c('UD','MD-SMD','MST','MD-IGA', 'NR-FRA','R-FRA'), which can be used to identify outlying observations. Using method="UD" leads to a univariate detection method. "MD-SMD" represents multivariate detection based on robust squared Mahalanobis distances. "MST" represents the mean shift testing method. "MD-IGA" allows to use the multivariate detection based on individual-level growth analysis. "NR-FRA" is the non-robust model-based latent factor and residual analysis, while "R-FRA" is the robust model-based latent factor and residual analysis.}
  \item{lgcm}{Specify a linear growth curve model for "R-FRA" method.}
  \item{alpha}{The Alpha level for the tests. It is 0.025 by default.}
}

\details{
Note that only the methods of "NR-FRA" and "R-FRA" need us to specify a linear growth curve model.
For example, for a linear growth curve model with 4 measurement occasions, the model can be specified in the following way:
lgcm<-specifyModel()
	b0 -> y1, NA, 1
	b0 -> y2, NA, 1
	b0 -> y3, NA, 1
	b0 -> y4, NA, 1
	b1 -> y1, NA, 0
	b1 -> y2, NA, 1
	b1 -> y3, NA, 2
 	b1 -> y4, NA, 3
 	b0 <-> b0, sb0, NA
 	b1 <-> b1, sb1, NA
 	b0 <-> b1, sb01, NA
 	y1 <-> y1, s1, NA
 	y2 <-> y2, s2, NA
 	y3 <-> y3, s3, NA
	y4 <-> y4, s4, NA

}
\value{
  \item{OutlyingObs}{IDs of identified outlying observations from methods "UD", "MD-SMD", or "MST".}
  \item{Outliers}{IDs of identified outliers from methods "MD-IGA", "NR-FRA", or "R-FRA".}
  \item{LeverageObs}{IDs of identified leverage observations from methods "MD-IGA", "NR-FRA", or "R-FRA".}
}
\references{
Tong,X. and Zhang, Z. (2015). Outlying observation diagnostics in growth curve modeling.}
\author{Xin Tong and Zhiyong Zhang}

\examples{
\dontrun{
data(N300)
outlying <- gcmdiag(data=N300,method="MD-SMD") 
outlying
outlying$OutlyingObs

lgcm<-specifyModel()
	b0 -> y1, NA, 1
	b0 -> y2, NA, 1
	b0 -> y3, NA, 1
	b0 -> y4, NA, 1
	b1 -> y1, NA, 0
	b1 -> y2, NA, 1
	b1 -> y3, NA, 2
 	b1 -> y4, NA, 3
 	b0 <-> b0, sb0, NA
 	b1 <-> b1, sb1, NA
 	b0 <-> b1, sb01, NA
 	y1 <-> y1, s1, NA
 	y2 <-> y2, s2, NA
 	y3 <-> y3, s3, NA
	y4 <-> y4, s4, NA

outlying <- gcmdiag(data=N300,method="R-FRA",lgcm) 
outlying
}
}

\keyword{Outlying observation detection}