\name{Test.CI}
\alias{Test.CI}

\title{
Performing hypothesis testing using confidence intervals.
}
\description{
This function Test.CI returns the results of hypothesis testing based on confidence intervals.
}
\usage{
Test.CI(CI, test.value)
}

\arguments{
  \item{CI}{
a vector or a matrix of the lower and upper limit(s) of confidence interval(s) for multiple variables. Rows represent different variables.
}
  \item{test.value}{
a numeric vector with size equal to the number of rows in CI. Significance is determined by examining whether confidence intervals in CI cover the values in test.value.
}
}

\value{
a vector of 0 or 1. "1" indicates significant results whereas "0" indicates non-significant results.
}

\author{
Zijun Ke <keziyun@mail.sysu.edu.cn> and Zhiyong Zhang <zhiyongzhang@nd.edu>
}
