\name{rate}
\alias{rate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Rate an package using scores 1-5
}
\description{
Rate an package using scores 1-5
}
\usage{
rate(package="base", download=NULL, like=NULL, rating=NULL, comment=NULL, meta=TRUE, lib.loc = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{package}{
The name of the package to comment on. The default is R base.
}
\item{download}{To indicate one has downloaded the named package}
\item{like}{Vote to like a package}
\item{rating}{Rating of 1, 2, 3, 4, 5 from Bad, OK, Good, Very Good, Excellent}
\item{comment}{Comment on the package.}
  \item{lib.loc}{
a character vector describing the location of R library trees to search through, or NULL. The default value of NULL corresponds to checking the loaded namespace, then all libraries currently known in .libPaths().
}
\item{meta}{Whether to provide meta information of the R package.}
}
\example{
rate(package="rsem", download=1, rating=5,
     comment="I found the package was useful for analyzing my non-normal missing data.")
}