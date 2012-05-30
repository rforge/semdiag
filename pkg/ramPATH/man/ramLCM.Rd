\name{ramLCM}
\alias{ramLCM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Conduct growth curve analysis
}
\description{
Conduct growth curve analysis}
\usage{
ramLCM(data, outcome, model = c("all", "no", "linear", "quadratic", "latent"), basis = 0:(length(outcome) - 1), predictor, equal.var = TRUE, digits = 3, ram.out = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Data
}
  \item{outcome}{
Outcome variable indices
}
  \item{model}{
Models to fit
}
  \item{basis}{
Basis coefficients
}
  \item{predictor}{
Covariates as predictors
}
  \item{equal.var}{
Set residual variances to be equal
}
  \item{digits}{
Print digits
}
  \item{ram.out}{
Print ram matrices
}
  \item{\dots}{
lavaan options
}
}
