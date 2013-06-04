\name{viewreply}
\alias{viewreply}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
View replies to a question
}
\description{
View replies to a question. See \code{\link{setRstats}} on how to set global options on package information.
}
\usage{
viewreply(id, package, lib.loc = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{id}{The id number of the question to answer. You can find the id number by \code{\link{view}}}
  \item{package}{
The name of the package to comment on. The default is R base.
}
  \item{lib.loc}{
a character vector describing the location of R library trees to search through, or NULL. The default value of NULL corresponds to checking the loaded namespace, then all libraries currently known in .libPaths().
}
}

