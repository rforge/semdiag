\name{semdiag.mdist.f}
\alias{semdiag.mdist.f}
\alias{semdiag.mdist.f1}
\alias{semdiag.mdist.f.r}
\alias{semdiag.mdist.f1.r}
\alias{semdiag.mdist.r}
\alias{semdiag.mdist.r1}
\alias{semdiag.mdist.r.r}
\alias{semdiag.mdist.r1.r}
\title{
Caculate the M-distance for factors and residuals
}
\description{
Function to calculate M-distance for factors and residuals
}
\usage{
semdiag.mdist.f(x, mu, lisrel)

semdiag.mdist.f1(x, mu, lisrel)

semdiag.mdist.f.r(x, mu, lisrel)

semdiag.mdist.f1.r(x, mu, lisrel)

semdiag.mdist.r(x, mu, lisrel)

semdiag.mdist.r1(x, mu, lisrel)

semdiag.mdist.r.r(x, mu, lisrel)

semdiag.mdist.r1.r(x, mu, lisrel)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Data}
  \item{mu}{Mean}
  \item{lisrel}{Lisrel notation matrices}
}

\value{
\code{semdiag.mdist.f}: M-distance for factors based on Bartlett-factor score using EQS

\code{semdiag.mdist.f1}: M-distance for factors based on Bartlett-factor score based on all latent factors (regarding D as factors) using EQS

\code{semdiag.mdist.f.r}: M-distance for factors based on Bartlett-factor score using R \code{sem} package

\code{semdiag.mdist.f1.r}: M-distance for factors based on Bartlett-factor score based on all latent factors (regarding D as factors) using R \code{sem} package

\code{semdiag.mdist.r}: M-distance for residuals using EQS

\code{semdiag.mdist.r1}: M-distance for residuals based on all latent factors (regarding D as factors) using EQS

\code{semdiag.mdist.r.r}: M-distance for residuals using R \code{sem} package

\code{semdiag.mdist.r1.r}: M-distance for residuals based on all latent factors (regarding D as factors) using R \code{sem} package
}
