\name{plotImpHistory}
\alias{plotImpHistory}
\title{Plot Boruta object as importance history}
\usage{
  plotImpHistory(x,
    colCode = c("green", "yellow", "red", "blue"),
    showRounds = TRUE, col = NULL, type = "l", lty = 1,
    pch = 0, xlab = "Classifier run", ylab = "Importance",
    ...)
}
\arguments{
  \item{x}{an object of a class Boruta.}

  \item{colCode}{a vector containing colour codes for
  attribute decisions, respectively Confirmed, Tentative,
  Rejected and shadow.}

  \item{showRounds}{if set, to \code{TRUE}, gray lines
  separating round will be drawn.}

  \item{col}{standard \code{col} attribute, passed to
  \code{\link{matplot}}. If given, suppresses effects of
  \code{colCode}.}

  \item{type}{Plot type that will be passed to
  \code{\link{matplot}}.}

  \item{lty}{Line type that will be passed to
  \code{\link{matplot}}.}

  \item{pch}{Point mark type that will be passed to
  \code{\link{matplot}}.}

  \item{xlab}{X axis label that will be passed to
  \code{\link{matplot}}.}

  \item{ylab}{Y axis label that will be passed to
  \code{\link{matplot}}.}

  \item{...}{additional graphical parameter that will be
  passed to \code{\link{matplot}}.}
}
\value{
  Invisible copy of \code{x}.
}
\description{
  alternative plot method for Boruta objects, showing
  matplot of attribute importances over run.
}
\examples{
\dontrun{
library(mlbench); data(Sonar);
#Takes some time, so be patient
Boruta(Class~.,data=Sonar,doTrace=2)->Bor.son;
print(Bor.son);
plotImpHistory(Bor.son);
}
}
\author{
  Miron B. Kursa
}

