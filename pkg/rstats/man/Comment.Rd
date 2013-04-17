\name{Comment}
\alias{Comment}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Add a comment to a package
}
\description{
Make a comment on a package using less than 1000 characters.
}
\usage{
Comment(package="base",comment=NULL,lib.loc = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{package}{
The name of the package to comment on. The default is R base.
}
\item{comment}{Comment on the package.}
  \item{lib.loc}{
a character vector describing the location of R library trees to search through, or NULL. The default value of NULL corresponds to checking the loaded namespace, then all libraries currently known in .libPaths().
}
}
