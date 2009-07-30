\name{Get formula}
\alias{getConfirmedFormula}
\alias{getNonRejectedFormula}
 
\title{Converting Boruta result into a formula}
\description{
	This two functions convert \code{Boruta} result into a convenient formula object, that may be easily passed to other \code{R} functions 
	without additional parsing. \code{getConfirmedFormula} includes \code{Confirmed} attributes in formula only, \code{getNonRejectedFormula} includes 
	both \code{Confirmed} and \code{Tentative}. 
}
\usage{
getConfirmedFormula(x)
getNonRejectedFormula(x)
}

\arguments{
 \item{x}{an object of class \code{Boruta}.}
}

\value{
  A formula corresponding to \code{Boruta$finalDecision}.
}


\note{
Those functions work only if Boruta has been run using formula object as a model definition. They return error otherwise.
}

\examples{
\dontrun{
library(mlbench); data(Ozone);
na.omit(Ozone)->ozo;
#Takes some time, so be patient
Boruta(V4~.,data=ozo,doTrace=2)->Bor.ozo;
cat('Random forest run on all attributes:\n');
print(randomForest(V4~.,data=ozo));
cat('Random forest run only on confirmed attributes:\n');
print(randomForest(getConfirmedFormula(Bor.ozo),data=ozo));
}
}

\author{Miron B. Kursa}



