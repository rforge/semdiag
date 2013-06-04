\name{ask}
\alias{ask}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Ask a question about a package
}
\description{
Ask a question about a package. See \code{\link{setRinfo}} on how to set global options on package information.
}
\usage{
ask(comment=NULL, package,  email, name, cc, meta=TRUE, lib.loc = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{comment}{Question to ask.}
\item{package}{
The name of the package to comment on. The default is R base.
}
\item{email}{Email address. You will receive email if someone answers your question.}
\item{name}{Your name}
\item{cc}{cc the question, for example, to r-help@r-project.org.}
  \item{lib.loc}{
a character vector describing the location of R library trees to search through, or NULL. The default value of NULL corresponds to checking the loaded namespace, then all libraries currently known in .libPaths().
}
\item{meta}{Whether to provide meta information of the R package.}
}
