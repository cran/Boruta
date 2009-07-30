\name{Boruta plot}
\alias{plot.Boruta}
 
\title{Plotting boxplots of ZScores}
\description{
  \code{plot.Boruta} visualises ZScoreHistory. Each attribute (including randomised meta-attributes, \code{randMax},
  \code{randMean}, \code{randMin}) is represented as a boxplot. Its colour corresponds to a Boruta decision; see \code{colCode}
  argument note. Boxplots are ordered due to their median ZScore.  
}

\usage{
\method{plot}{Boruta}(x,colCode=c('green','yellow','red','blue'),sort=TRUE,
		whichRand=c(TRUE,TRUE,TRUE),col=NULL,
		xlab='Attributes',ylab='Z-Score',...)
}

\arguments{
 \item{x}{an object of class \code{Boruta}.}
 \item{colCode}{a vector containing colour codes for attribute decisions, respectively \code{Confirmed}, \code{Tentative},
 				\code{Rejected}, \code{Random}.}
 \item{sort}{controls weather boxplots should be ordered, or left in original order.}
 \item{whichRand}{a vector controlling which randomised meta-attributes should be drawn; respectively 
 				\code{randMax}, \code{randMean}, \code{randMin}.}
 \item{col}{standard \code{col} attribute. If given, suppresses \code{colCode}.}  
 \item{xlab,ylab,...}{additional graphical parameters that will be passed to \code{boxplot}.}
}

\value{
  None.
}

\note{
If \code{col} is given and \code{sort} is \code{TRUE}, the \code{col} will be permuted, so that its order corresponds to 
attribute order in \code{ZScoreHistory}. 
}

\examples{
\dontrun{
library(mlbench); data(HouseVotes84);
na.omit(HouseVotes84)->hvo;
#Takes some time, so be patient
Boruta(Class~.,data=hvo,doTrace=2)->Bor.hvo;
print(Bor.hvo);
plot(Bor.hvo);

}
}

\author{Miron B. Kursa}

\keyword{hplot}
