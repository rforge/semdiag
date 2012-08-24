\name{ramFit}
\alias{ramFit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fit a model using lavaan based on ram input
}
\description{
Fit a model using lavaan based on ram input}
\usage{
ramFit(ramModel, data, type=c('ram','lavaan'), digits = 3, zero.print = "0", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ramModel}{
An ram model
}
  \item{data}{
data
}
\item{type}{
  ram: specify a ram model; lavaan: specify a lavaan model
}
  \item{digits}{
Digits for print
}
  \item{zero.print}{
Format of zeros
}
  \item{\dots}{
Options for lavaan
}
}
