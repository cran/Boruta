\name{TentativeRoughFix}
\alias{TentativeRoughFix}
\title{Rough fix of Tentative attributes}
\usage{
  TentativeRoughFix(x, averageOver = "finalRound")
}
\arguments{
  \item{x}{an object of a class Boruta.}

  \item{averageOver}{Either number of last importance
  source runs to average over, \code{'finalRound'} for
  averaging over last (final) round or}
}
\value{
  A Boruta class object with modified \code{finalDecision}
  element. Such object has few additional elements:
  \item{originalDecision}{Original \code{finalDecision}.}
  \item{averageOver}{Copy of \code{averageOver} parameter.}
}
\description{
  In some circumstances (too short Boruta run, unfortunate
  mixing of shadow attributes, tricky dataset\ldots),
  Boruta can leave some attributes Tentative.
  TentativeRoughFix performs a simplified, weaker test for
  judging such attributes.
}
\details{
  Function claims as Confirmed those attributes that have
  median importance higher than the median importance of
  maximal shadow attribute, and the rest as Rejected.
  Depending of the user choice, medians for the test are
  count over last round, all rounds or N last importance
  source runs.
}
\note{
  This function should be used only when strict decision is
  highly desired, because this test is much weaker than
  Boruta and can lower the confidence of the final result.
}
\author{
  Miron B. Kursa
}

