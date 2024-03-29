% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trandapters.R
\name{decohereTransdapter}
\alias{decohereTransdapter}
\title{Decohere transdapter}
\usage{
decohereTransdapter(adapter = getImpRfZ)
}
\arguments{
\item{adapter}{importance adapter to transform.}
}
\value{
transformed importance adapter which can be fed into \code{getImp} argument of the \code{\link{Boruta}} function.
}
\description{
Applies the decoherence transformation to the input, destroying all multivariate interactions.
It will trash the Boruta result, only apply if you know what are you doing!
Works only for categorical decision.
}
\examples{
set.seed(777)
# SRX data only contains multivariate interactions
data(srx)
# Decoherence transform removes them all,
# leaving no confirmed features
Boruta(Y~.,data=srx,getImp=decohereTransdapter())
}
