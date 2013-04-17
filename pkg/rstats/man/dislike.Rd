\name{dislike}
\alias{dislike}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Vote to dislike a package
}
\description{
Vote to dislike a package
}
\usage{
dislike(package="base",lib.loc = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{package}{
The name of the package to comment on. The default is R base.
}
  \item{lib.loc}{
a character vector describing the location of R library trees to search through, or NULL. The default value of NULL corresponds to checking the loaded namespace, then all libraries currently known in .libPaths().
}
}
