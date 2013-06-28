\name{rate}
\alias{rate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Rate an package using scores 1-5
}
\description{
Rate an package using scores 1-5. See \code{\link{pkginfo}} on how to set global options on package information.
}
\usage{
rate(rating=5, package, email, name, meta=TRUE, lib.loc = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{rating}{Rating of 1, 2, 3, 4, 5 from Bad, OK, Good, Very Good, Excellent}
\item{package}{
The name of the package to comment on. The default is R base.
}
\item{email}{Email address of the rater}
\item{name}{Name of the rater}
  \item{lib.loc}{
a character vector describing the location of R library trees to search through, or NULL. The default value of NULL corresponds to checking the loaded namespace, then all libraries currently known in .libPaths().
}
\item{meta}{Whether to provide meta information of the R package.}
}
