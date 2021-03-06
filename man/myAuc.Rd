% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/myAuc.R
\name{myAuc}
\alias{myAuc}
\title{another implementation of the AUC in analogy to the implementation in glmnet}
\usage{
myAuc(y, prob)
}
\arguments{
\item{y}{a factor with levels 0 and 1}

\item{prob}{the predicted class probabilities}
}
\value{
the AUC
}
\description{
another implementation of the AUC in analogy to the implementation in glmnet
}
\examples{
myAuc(y=factor(c(rep(0, 5), rep(1, 5))), prob=sin(1:10)^2)
}

