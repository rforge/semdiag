\name{logistic4p}
\alias{logistic4p}

\title{
Logistic Regressions with Misclassification Correction
}
\description{
logistic4p is used to fit logistic regressions with correction of the misclassifications in the binary dependent variable. It is specified by 
}
\usage{
logistic4p(x, y, initial, model = c("lg", "fp.fn", "fp", "fn", "equal"),
max.iter = 1000, epsilon = 1e-06, detail = FALSE)
}

\arguments{
  \item{x, y}{
x is a data frame or data matrix containing the predictor variables and y is the vector of outcomes. The number of rows in x must be the same as the length of y.
}
  
  \item{initial}{
starting values for the parameters in the model(FP,FN misclassification parameters and those in the linear predictor); 
if not specified, the default initials are 0 for the misclassification parameters and 
estimates obtained from the logistic regression for the parameters in the linear predictor. 
}
  \item{model}{ a character string specifying the model to be used in the analysis. 

Currently available options are "lg" (logistic regression), "fp.fn" (logistic regression with both FP and FN parameters), 
"fp" (logistic regression with the FP parameter), 
"fn" (logistic regression with the FN parameter), "equal" (logistic regression with FN=FN).

If it is not specified, the default one ('lg') will be used. 
             }
  \item{max.iter}{
a positive integer giving the maximal number of iterations; if it is reached, the algorithm will stop.
  }
  \item{epsilon}{
a positive convergence tolerance epsilon; the iterations converge when max(|par-par_{old}|)<epsilon.
}
  \item{detail}{
logical indicating if the itermediate output should be printed after each iteration.
}
}
\details{

This package implements the logistic regressions with misclassification corrections. There are five different models which can be specified by 'model'.

In the specification, x is a matrix of data frame of 
predictors fitted to the model; y is a numeric vector taking either 0 or 1.
 
The 'initial' is the vector of starting values for both misclassification and regression coefficients parameters in the model. It is suggested to provide 'initial', 
however if not, the default one will be used. 


For the background to warning messages about 'fitted probabilities numerically 0 or 1 occurred', when the fitted probabilities of 
some individuals are either 0 or 1.

The package cannot handle missing data problems currently. If there are missing values in either x or y, there will be warning message. 

}



\value{
logistic4p returns a list of values inheriting from "logistic4p". 

\item{estimates}{ a named matrix of estimates including parameter estimates, standard errors, z-scores, and p-values.}
\item{n.iter }{ an integer giving the number of iteration used}
\item{d}{ the actual max absolute difference of the parameters of the last two iterations, d=max(|par.final-par_{old}|).}
\item{loglike}{ loglikelihood evaluated at the parameter estimates.}
\item{AIC}{ Akaike Information Criterion.}
\item{BIC}{ Bayesian Information Criterion.}
\item{converged}{ logical indicating whether the current procedure converged or not.}
}

\references{Liu, H. and Zhang, Z. (2016) Logistic Regression with Misclassification in Dependent Variables: Method and Software.(In preparation.)

}
\author{
Haiyan Liu and Zhiyong Zhang
}


\examples{
\dontrun{ 
data(nlsy)
y=nlsy[, 1]
x=nlsy[,-1]

mod1=logistic4p(x,y)
mod1
mod1$estimates

mod2=logistic4p(x,y, model='fp.fn')

mod3=logistic4p(x,y, model='fn')
}
}
