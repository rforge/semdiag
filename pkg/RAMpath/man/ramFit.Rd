\name{RAMFit}
\alias{RAMFit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Fit a model using lavaan based on RAM input
}
\description{
Fit a model using lavaan based on RAM input}
\usage{
RAMFit(RAMModel, data, type=c('RAM','lavaan'), digits = 3, zero.print = "0", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{RAMModel}{
An RAM model
}
  \item{data}{
data
}
\item{type}{
  RAM: specify a RAM model; lavaan: specify a lavaan model
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

\value{
\item{A and Ase}{A matrix and its standard error}
\item{S and Sse}{S matrix and its standard error}
\item{lavaan}{Original lavaan output}
\item{fit}{Model fit statistics and indices}
}

\examples{
## Example 1. A path model
data(ex1)
m1<-'
manifest=3
label=age,hvlt,ept
arrow(2,1)=?
arrow(3,1)=?
arrow(3,2)=?
sling(1,1)=?
sling(2,2)=?
sling(3,3)=?
'

## Fit the model
res1<-RAMFit(m1, ex1)

## More output from Lavaan
summary(res1$lavaan, fit=TRUE)

## Effects and variance decomposition
bridge<-RAMPathBridge(res1)
summary(bridge)
summary(bridge, type='bridge')

## plot the path diagRAM
plot(bridge, 'ex1')

## plot the effects from age to ept
plot(bridge, 'ex1effect', 'age','ept')

## plot the bridges for ept
plot(bridge, 'ex1bridge', 'ept','hvlt', type='bridge')

## summarize
summary(bridge)
summary(bridge, type='bridge')


## Example 2: An SEM model (MIMIC model)
data(ex2)
## Using lavaan directly for model estimation and specification
mimic<-'
R =~ ws1 + ls1 + lt1
R ~ edu + gender
'

mimic.res<-sem(mimic, data=ex2)

mimic.RAM<-lavaan2RAM(mimic.res)

## plot the path diagRAM
bridge<-RAMPathBridge(mimic.RAM, allbridge=FALSE, indirect=FALSE)
plot(bridge, 'mimic')

}