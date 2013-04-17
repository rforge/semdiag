\name{view}
\alias{view}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
View the usage information about an package
}
\description{
View the usage information about an package
}
\usage{
view(package="base",lib.loc = NULL)
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
\details{
The usage information includes number of downloads, number of likes, number of dislikes, and the average rating.
}
