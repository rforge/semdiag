\name{semdiag.robfit}
\alias{semdiag.robfit}
\alias{semdiag.robfit1}
\alias{semdiag.robfit.r}
\alias{semdiag.robfit1.r}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Robust method for calculating d_r
}
\description{
Robust method for calculating d_r
}
\usage{
semdiag.robfit(lisrel0, x, q, varphi, EQSmodel, EQSdata, max_it=1000, EQSprog='C:/Progra~1/EQS61/WINEQS',serial=1234)

semdiag.robfit1(lisrel0, x, q, varphi, EQSmodel, EQSdata, max_it=1000, EQSprog='C:/Progra~1/EQS61/WINEQS',serial=1234)

semdiag.robfit.r(lisrel0, x, q, varphi, ram.path, max_it=1000)

semdiag.robfit1.r(lisrel0, x, q, varphi, ram.path, max_it=1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lisrel0}{
Lisrel notation matrices
}
  \item{x}{
Data frame or data matrix
}
  \item{varphi}{
Percentage of data to be down-weighted
}
  \item{EQSmodel}{
EQS input file name
}
  \item{EQSdata}{
Data file name used in EQS input file
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
\item{q}{number of factors to be counted}
\item{ram.path}{Ram path for the sem package}
}

\value{
Distance d_r
}
