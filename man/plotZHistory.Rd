\name{plotZHistory}
\alias{plotZHistory}
 
\title{Plotting history of ZScores}
\description{
  \code{plotZHistory} visualises ZScoreHistory in some more straightforward way than plot.Boruta. It plots each attributes' (including randomised 
  meta-attributes, \code{randMax}, \code{randMean}, \code{randMin}) ZScore as a function of Random Forest run it was obtained in. The colour of 
  each line corresponds to a Boruta decision; see \code{colCode} argument note.  
}

\usage{
plotZHistory(x,colCode=c('green','yellow','red','blue'),showRounds=TRUE,
		col=NULL,type="l",lty=1,pch=0,
		xlab='Random Forest run',ylab='Z-Scores',...)
}

\arguments{
 \item{x}{an object of class \code{Boruta}.}
 \item{colCode}{a vector containing colour codes for attribute decisions, respectively \code{Confirmed}, \code{Tentative},
 				\code{Rejected}, \code{Random}.}
 \item{showRounds}{if set, to \code{TRUE}, gray lines separating round will be drawn.}
 \item{col}{standard \code{col} attribute. If given, suppresses \code{colCode}.}  
 \item{type,lty,pch,xlab,ylab,...}{additional graphical parameters that will be passed to \code{matplot}.}
}

\value{
  None.
}

\examples{
\dontrun{
library(mlbench); data(Sonar);
#Takes some time, so be patient
Boruta(Class~.,data=Sonar,doTrace=2)->Bor.son;
print(Bor.son);
plotZHistory(Bor.son);
}
}

\author{Miron B. Kursa}

\keyword{hplot}
