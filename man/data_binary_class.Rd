% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{data_binary_class}
\alias{data_binary_class}
\title{An artificial data set for logistic regression}
\format{
\subsection{\code{data_binary_class}}{

A data frame with 10000 rows and 7 columns:
\describe{
\item{X1-X6}{explanatory variables generated from multivariate normal distribution}
\item{y}{response variable}
}
}
}
\usage{
data_binary_class
}
\description{
A data.frame with binary response and explanatory variables generated from multivariate normal distribution
\eqn{N(\boldsymbol{0}, \boldsymbol{\Sigma})}, where\eqn{\boldsymbol{\Sigma}_{ij}=0.5^{I(i\neq j)},
i,j=1,\dots,7.} The probability of the class label being 1 for a point \eqn{\boldsymbol{x}} is
\eqn{h(\boldsymbol{x}, \boldsymbol{\beta}) = 1/(1 + \exp(−\boldsymbol{x}^{T}\boldsymbol{\beta}))},
where \eqn{\boldsymbol{\beta}} is a 7 \eqn{\times} 1 vector of 0.5.
}
\keyword{datasets}
