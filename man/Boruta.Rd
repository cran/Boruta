\name{Boruta}
\alias{Boruta}
\alias{Boruta.formula}
\alias{Boruta.default}
\alias{print.Boruta}

\title{Important attribute search using Boruta algorithm}

\description{
  \code{Boruta} is an algorithm of finding important attributes in information systems by 
  iterative learning of the randomForest classifier.
} 
\usage{
\method{Boruta}{formula}(formula,data=.GlobalEnv,...)
\method{Boruta}{default}(x,y,confidence=0.999,maxRuns=100,light=TRUE,doTrace=0,getImp=getImpRf,...)
\method{print}{Boruta}(x,...)
}
\arguments{
  \item{x, formula}{data frame of predictors or a formula describing model to be analysed.}
  \item{data}{data frame containing model variables. Global environment is default.}
  \item{y}{response vector; factor for classification, numeric vector for regression.}
  \item{confidence}{confidence level. Default value should be used. Lower value may reduce computation time of test runs.}
  \item{maxRuns}{maximal number of randomForest runs in the final round. You may increase it to resolve attributes left \code{Tentative}. }
  \item{doTrace}{0 means no tracing, 1 means printing a "." sign after each randomForest run, 
  					2 means same as 1, plus consecutive reporting of test results.}
  \item{getImp}{Function used to obtain attribute importance. The default is \code{getImpRf}, which runs randomForest and gathers Z-scores of mean decrease accuracy measure.}
  \item{light}{if set to \code{TRUE}, Boruta runs in standard, light mode. If set to \code{FALSE},
  Boruta runs in more restrictive, force mode.}
  \item{...}{additional parameters that will be passed to \code{getImp} function.}
}


\value{
  An object of class \code{Boruta}, which is a list with the
  following components:
  \item{finalDecision}{a factor of three value: \code{Confirmed}, \code{Rejected} or \code{Tentative},
  					   containing final result of feature selection.}
  \item{ZScoreHistory}{a data frame of ZScores of attributes gathered in each randomForest run. 
   Beside predictors' ZScores contains maximal, mean and minimal ZScore of shadow attributes in each run. Rejected
   attributes have \code{-Inf} ZScore assumed.}
  \item{timeTaken}{time taken by the computation.}
  \item{impSource}{string describing the source of importance, equal to a comment attribute of the \code{getImp} argument.}
  \item{call}{the original call of the \code{Boruta} function.}
}

\details{
\code{Boruta} iteratively compares ZScores of attributes with ZScores of shadow attributes, created by
  shuffling original ones. Attributes that have significantly worst importance than shadow ones
  are being consecutively dropped. On the other hand, attributes that are significantly better than
  shadows are admitted to be \code{Confirmed}.
  If algorithm is run in default light mode, unimportant attributes are being dropped along with their random shadows,
  while in the force mode all shadow attributes are preserved during the whole Boruta run.
  Algorithm stops when only \code{Confirmed} attributes are left, or when it reaches \code{maxRuns} randomForest runs
  in the last round. If the second scenario occurs, some attributes may be left without a decision. They are
  claimed \code{Tentative}. You may try to extend \code{maxRuns} or lower \code{confidence} to clarify them, but in
  some cases their ZScores do fluctuate too much for Boruta to converge. 
  Instead, you can use \code{\link{TentativeRoughFix}} function, which will perform other, weaker test to make a final
  decision, or simply treat them as undecided in further analysis.
  }
  
\note{
 Boruta was originally based on a Z-score of \code{randomForest}'s mean decrease of accuracy score. 
 While the current version of the package allows one to use arbitrary importance source using the \code{getImp} argument, the documentation and function/element naming still follow the original implementation because of the compatibility issues.
}

\examples{
set.seed(777);
#Add some nonsense attributes to iris dataset by shuffling original attributes
iris.extended<-data.frame(iris,apply(iris[,-5],2,sample));
names(iris.extended)[6:9]<-paste("Nonsense",1:4,sep="");
#Run Boruta on this data
Boruta(Species~.,data=iris.extended,doTrace=2)->Boruta.iris.extended
#Nonsense attributes should be rejected
print(Boruta.iris.extended);

\dontrun{
#Boruta using rFerns' importance (rFerns package must be installed!)
#Definition of ferns' importance adapter
getImpFerns<-function(x,y,...){
 f<-rFerns(x,y,saveForest=FALSE,importance=TRUE,...);
 f$importance[,1]
}
#Those are optional
attr(getImpFerns,"toLoad")<-"rFerns";
comment(getImpFerns)<-"rFerns importance"
#Running altered Boruta on the Iris data
Boruta(Species~.,data=iris.extended,getImp=getImpFerns)->Boruta.ferns.irisE
print(Boruta.ferns.irisE);
}

\dontrun{
#Boruta on the Ozone data from mlbench 
library(mlbench); data(Ozone);
na.omit(Ozone)->ozo;
#Takes some time, so be patient
Boruta(V4~.,data=ozo,doTrace=2)->Bor.ozo;
cat('Random forest run on all attributes:\n');
print(randomForest(V4~.,data=ozo));
cat('Random forest run only on confirmed attributes:\n');
print(randomForest(getConfirmedFormula(Bor.ozo),data=ozo));
}

\dontrun{
#Boruta on the HouseVotes84 data from mlbench 
library(mlbench); data(HouseVotes84);
na.omit(HouseVotes84)->hvo;
#Takes some time, so be patient
Boruta(Class~.,data=hvo,doTrace=2)->Bor.hvo;
print(Bor.hvo);
plot(Bor.hvo);
}

\dontrun{
#Boruta on the Sonar data from mlbench 
library(mlbench); data(Sonar);
#Takes some time, so be patient
Boruta(Class~.,data=Sonar,doTrace=2)->Bor.son;
print(Bor.son);
#Shows important bands
plot(Bor.son,sort=FALSE);
}

}

\references{
	Miron B. Kursa, Witold R. Rudnicki (2010). Feature Selection with the Boruta Package.
        \emph{Journal of Statistical Software, 36(11)}, p. 1-13.
        URL: \url{http://www.jstatsoft.org/v36/i11/}
}

\author{Miron B. Kursa, based on the idea & original code by Witold R. Rudnicki.}

\keyword{tree}


