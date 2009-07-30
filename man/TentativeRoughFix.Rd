
\name{TentativeRoughFix}
\alias{TentativeRoughFix}
 
\title{Rough test for tentative attributes}
\description{
	In some circumstances (too short \code{Boruta} run, unfortunate mixing of shadow attributes, tricky dataset\ldots), \code{Boruta}
	can leave some attributes \code{Tentative}. \code{TentativeRoughFix} performs a simplified, weaker test for judging such
	attributes. 
}
\usage{
TentativeRoughFix(x,averageOver='finalRound')
}

\arguments{
 \item{x}{an object of class \code{Boruta}.}
 \item{averageOver}{Either number of last \code{randomForest} runs to average over, 'finalRound' for averaging
 		over last (final) round or 'allRounds' for averaging over whole \code{Boruta} run.}
}

\value{
  A \code{Boruta} class object with modified \code{finalDecision} element. Such object has few additional elements:
  \item{originalDecision}{Original \code{finalDecision}.}
  \item{averageOver}{Copy of \code{averageOver} parameter.}
}


\details{
	Function claims as \code{Confirmed} those attributes that have median ZScore higher than the median ZScore of 
	maximal	shadow attribute (\code{maxRand}), and the rest as \code{Rejected}. Depending of the user choice, medians for the test
	are count over last round, all rounds or N last \code{randomForest} runs.
}

\note{
This function should be used only when strict decision is highly desired, because this test is
much weaker than \code{Boruta} and can lower the confidence of the final result. 
}

\author{Miron B. Kursa}



