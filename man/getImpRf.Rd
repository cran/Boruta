\name{getImpRf}
\alias{getImpRf}
\alias{getImpRfGini}
\alias{getImpRfRaw}
\alias{getImpRfZ}
\title{randomForest importance adapters}
\usage{
  getImpRfZ(x, y, ...)

  getImpRfRaw(x, y, ...)

  getImpRfGini(x, y, ...)
}
\arguments{
  \item{x}{data frame of predictors including shadows.}

  \item{y}{response vector.}

  \item{...}{parameters passed to the underlying
  \code{\link{randomForest}} call; they are relayed from
  \code{...} of \code{\link{Boruta}}.}
}
\description{
  Those function is intended to be given to a \code{getImp}
  argument of \code{\link{Boruta}} function to be called by
  the Boruta algorithm as an importance source.
  \code{getImpRfZ} generates default, normalized
  permutation importance, \code{getImpRfRaw} raw
  permutation importance, finally \code{getImpRfGini}
  generates Gini index importance.
}

