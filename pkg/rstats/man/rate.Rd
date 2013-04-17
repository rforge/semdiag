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
rate(package="base",rating=5,lib.loc = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{package}{
The name of the package to comment on. The default is R base.
}
  \item{lib.loc}{
a character vector describing the location of R library trees to search through, or NULL. The default value of NULL corresponds to checking the loaded namespace, then all libraries currently known in .libPaths().
}
  \item{rating}{
Rating of 1, 2, 3, 4, 5 from Bad, OK, Good, Very Good, Excellent
}
}
