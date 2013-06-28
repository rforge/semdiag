\name{like}
\alias{like}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Like a package
}
\description{
Like a package. See \code{\link{pkginfo}} on how to set global options on package information.
}
\usage{
like(package, email, name, meta=TRUE, lib.loc = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
