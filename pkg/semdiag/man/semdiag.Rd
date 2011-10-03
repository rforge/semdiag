\name{semdiag}
\alias{semdiag}
\title{
The \code{semdiag} function
}
\description{
A function to calculate distance and parameter estimates for SEM diagnostics
}
\usage{
semdiag(x,  EQSmodel, varphi=0.1, EQSdata='data.txt', D='E', delete=integer(0), max_it=1000, EQSprog='C:/Progra~1/EQS61/WINEQS',serial=1234, ram.path, software='EQS')}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Data frame or data matrix
}
\item{ram.path}{Ram path for R sem package}
\item{software}{Software to be used. \code{sem} for R sem package and \code{EQS} for the use of EQS}
  \item{varphi}{
Percentage of data to be down-weighted
}
  \item{EQSmodel}{
EQS input file name
}
  \item{EQSdata}{
Data file name used in EQS input file
}
  \item{D}{
How to treat prediction error. E: errors; F: factors
}
  \item{delete}{
A vector of data to be deleted. For example c(99,100) delete the 99th and 100th cases.
}
  \item{max_it}{
The maximum number of iterations
}
  \item{EQSprog}{
The path to where EQS program is installed.
}
  \item{serial}{
Serial no. for EQS
}
}

\value{
%%list(d_f = d_f, d_r = d_r$Md_r[, 1:2], mu = d_r$mu, p = p, 
%%        q = q, res = list(nml = nml.res, tsr = tsr.res, dr = dr.res), 
%%        eqs = list(nml = ml.temp, tsr = model0, dr = d_r$eqs), 
%%        x = x)
\item{d_f}{Distance for f}
\item{d_r}{Distance for r}
\item{mu}{Mean}
\item{p}{Number of observed variables}
\item{q}{Number of factors used in calculating f and r}
\item{res}{Model fit and paramter estimates. nml: normal ML; tsr: two-stagte robust; dr: direct robust}
\item{eqs}{Full EQS output for the above three models}
\item{x}{Data}
%% ...
}
\author{
Zhiyong Zhang and Ke-Hai Yuan

Maintainer: Zhiyong Zhang <zhiyongzhang@nd.edu>
}
\references{
Yuan, K.-H. and Zhang, Z. (2011). Structural Equation Modeling Diagnostics Using R \code{semdiag} Package and EQS

Yuan, K.-H., and Hayashi, K. (2010). Fitting data to model: Structural equation modeling diagnosis using two scatter plots. Psychological Methods, 15, 335--351.
}

\examples{
\dontrun{
## Examples based on EQS
library(semdiag)

## Example 1. Normally distributed data
data(N100)

## Not run
## The EQS input file is semplot.eqs
## Model 1: treating prediction errors as factors
N100out.1<-semdiag(N100, 'semplot.eqs', D='F')

## Diagnostics plot
semdiag.plot(N100out.1)

## Summary output 
semdiag.summary(N100out.1)

## Model 0: treating prediction errors the same as measurement errors
N100out.0<-semdiag(N100, 'semplot.eqs')

## Diagnostics plot
semdiag.plot(N100out.0)

## Summary output 
semdiag.summary(N100out.0)

## Example 2. Contaminated data
data(N85)

## The EQS input file is semplot.eqs
## Model 1: treating prediction errors as factors
N85out.1<-semdiag(N85, 'semplot.eqs', D='F')

## Diagnostics plot
semdiag.plot(N85out.1)

## Summary output 
semdiag.summary(N85out.1)

## Model 0: treating prediction errors the same as measurement errors
N85out.0<-semdiag(N85, 'semplot.eqs', D='E')

## Diagnostics plot
semdiag.plot(N85out.0)

## Summary output 
semdiag.summary(N85out.0)

## Case profile plot
semdiag.cpp(N85out.0, cases=c(86, 90, 98:100))

## Delete the 99th and 100th observations
N85out.1.del<-semdiag(N85, 'semplot.eqs', D='F', delete=c(99,100))

## Examples based on the sem package
library(sem)
data(N100)

## path diagram for the model
sem1<-specify.model()
f1 -> y1, NA, 1
f1 -> y2, l1, NA
f1 -> y3, l2, NA
f2 -> y4, NA, 1
f2 -> y5, l3, NA
f2 -> y6, l4, NA
f3 -> y7, NA, 1
f3 -> y8, l5, NA
f3 -> y9, l6, NA
f1 -> f2, g1, NA
f1 -> f3, g2, NA
f2 -> f3, g3, NA
y1 <-> y1, e1, NA
y2 <-> y2, e2, NA
y3 <-> y3, e3, NA
y4 <-> y4, e4, NA
y5 <-> y5, e5, NA
y6 <-> y6, e6, NA
y7 <-> y7, e7, NA
y8 <-> y8, e8, NA
y9 <-> y9, e9, NA
f1 <-> f1, e10, NA
f2 <-> f2, e11, NA
f3 <-> f3, e12, NA

## Model 1: treating prediction errors as factors
N100out.1<-semdiag(N100, ram.path=sem1, software='sem', D='F')

## Diagnostics plot
semdiag.plot(N100out.1)

## Summary output 
semdiag.summary(N100out.1)

## Model 0: treating prediction errors the same as measurement errors
N100out.0<-semdiag(N100, ram.path=sem1, software='sem')

## Diagnostics plot
semdiag.plot(N100out.0)

## Summary output 
semdiag.summary(N100out.0)

## Example 2. Contaminated data
data(N85)

## Model 1: treating prediction errors as factors
N85out.1<-semdiag(N85, ram.path=sem1, software='sem', D='F')

## Diagnostics plot
semdiag.plot(N85out.1)

## Summary output 
semdiag.summary(N85out.1)

## Model 0: treating prediction errors the same as measurement errors
N85out.0<-semdiag(N85, ram.path=sem1, software='sem', D='E')

## Diagnostics plot
semdiag.plot(N85out.0)

## Summary output 
semdiag.summary(N85out.0)

## Case profile plot
semdiag.cpp(N85out.0, cases=c(86, 90, 98:100))

## Delete the 99th and 100th observations
N85out.1.del<-semdiag(N85, ram.path=sem1, software='sem', D='F', delete=c(99,100))
}

}


