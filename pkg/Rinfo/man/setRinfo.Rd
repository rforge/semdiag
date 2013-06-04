\name{setRinfo}
\alias{setRinfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Set the global parameter for the package
}
\description{
Set the global parameter for the package to save time. See \code{\link{options}} for more information. The information is saved in 
\code{options()$rinfo}. Once set, you don't need to provide package name information in other functions anymore.
}
\usage{
setRinfo(package='base', email='', name='',cc='')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{package}{
The name of the package to comment on. The default is R base.
}
  \item{email}{Your email.}
  \item{name}{Your name.}
  \item{cc}{cc the question, for example, to r-help@r-project.org.}
}
