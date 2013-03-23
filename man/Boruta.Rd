\name{Boruta}
\alias{Boruta}
\alias{Boruta.default}
\alias{Boruta.formula}
\title{Important attribute search using Boruta algorithm}
\usage{
  Boruta(x, ...)

  \method{Boruta}{default} (x, y, confidence = 0.999,
    maxRuns = 100, light = TRUE, doTrace = 0,
    getImp = getImpRf, ...)

  \method{Boruta}{formula} (formula, data = .GlobalEnv,
    ...)
}
\arguments{
  \item{x}{data frame of predictors.}

  \item{y}{response vector; factor for classification,
  numeric vector for regression.}

  \item{getImp}{function used to obtain attribute
  importance. The default is getImpRf, which runs random
  forest from \code{randomForest} package and gathers
  Z-scores of mean decrease accuracy measure.}

  \item{confidence}{confidence level. Default value should
  be used. Lower value may reduce computation time of test
  runs.}

  \item{maxRuns}{maximal number of randomForest runs in the
  final round. You may increase it to resolve attributes
  left Tentative.}

  \item{doTrace}{verbosity level. 0 means no tracing, 1
  means printing a "." sign after each importance source
  run, 2 means same as 1, plus consecutive reporting of
  test results.}

  \item{light}{if set to \code{TRUE}, Boruta runs in
  standard mode, in which attributes claimed Rejected are
  removed with their shadows; if set to \code{FALSE},
  Boruta runs in `forced' mode, in which all shadows are
  present during the whole run.}

  \item{...}{additional parameters passed to getImp.}

  \item{formula}{alternatively, formula describing model to
  be analysed.}

  \item{data}{in which to interpret formula.}
}
\value{
  An object of class \code{Boruta}, which is a list with
  the following components: \item{finalDecision}{a factor
  of three value: \code{Confirmed}, \code{Rejected} or
  \code{Tentative}, containing final result of feature
  selection.} \item{ImpHistory}{a data frame of importances
  of attributes gathered in each importance source run.
  Beside predictors' importances contains maximal, mean and
  minimal importance of shadow attributes in each run.
  Rejected attributes have \code{-Inf} importance assumed.}
  \item{timeTaken}{time taken by the computation.}
  \item{impSource}{string describing the source of
  importance, equal to a comment attribute of the
  \code{getImp} argument.} \item{call}{the original call of
  the \code{Boruta} function.}
}
\description{
  Boruta is an all-relevant feature selection wrapper
  algorithm. It finds relevant features by comparing
  original attributes' importance with importance
  achievable at random, estimated using their permuted
  copies.
}
\details{
  Boruta iteratively compares importances of attributes
  with importances of shadow attributes, created by
  shuffling original ones. Attributes that have
  significantly worst importance than shadow ones are being
  consecutively dropped. On the other hand, attributes that
  are significantly better than shadows are admitted to be
  Confirmed. If algorithm is run in default light mode,
  unimportant attributes are being dropped along with their
  random shadows, while in the forced mode all shadow
  attributes are preserved during the whole Boruta run.
  Algorithm stops when only Confirmed attributes are left,
  or when it reaches \code{maxRuns} importance source runs
  in the last round. If the second scenario occurs, some
  attributes may be left without a decision. They are
  claimed Tentative. You may try to extend \code{maxRuns}
  or lower \code{confidence} to clarify them, but in some
  cases their importances do fluctuate too much for Boruta
  to converge. Instead, you can use
  \code{\link{TentativeRoughFix}} function, which will
  perform other, weaker test to make a final decision, or
  simply treat them as undecided in further analysis.
}
\note{
  Version 2.0.0 changes some name conventions and thus may
  be incompatible with scripts written for 1.x.x version
  and old Boruta objects. Solutions of most problems of
  this kind should boil down to change of
  \code{ZScoreHistory} to \code{ImpHistory} in script
  source or Boruta object structure.

  In normal use, \code{light} should be set to \code{TRUE};
  force mode is experimental and has not been well tested
  yet.
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
\dontrun{
#Boruta on the Ozone data from mlbench
library(mlbench); data(Ozone);
na.omit(Ozone)->ozo;
#Takes some time, so be patient
Boruta(V4~.,data=ozo,doTrace=2)->Bor.ozo;
cat('Random forest run on all attributes:\\n');
print(randomForest(V4~.,data=ozo));
cat('Random forest run only on confirmed attributes:\\n');
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
\author{
  Miron B. Kursa, based on the idea & original code by
  Witold R. Rudnicki.
}
\references{
  Miron B. Kursa, Witold R. Rudnicki (2010). Feature
  Selection with the Boruta Package. \emph{Journal of
  Statistical Software, 36(11)}, p. 1-13. URL:
  \url{http://www.jstatsoft.org/v36/i11/}
}

