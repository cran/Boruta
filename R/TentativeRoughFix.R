# Function that estimates rough decision for Tentative attributes
# left after Boruta run.
# Author: Miron B. Kursa
###############################################################################


##' @name TentativeRoughFix
##' @title Rough fix of Tentative attributes
##' @description In some circumstances (too short Boruta run,
##' unfortunate mixing of shadow attributes, tricky dataset\ldots), Boruta
##' can leave some attributes Tentative. \code{TentativeRoughFix} performs a
##' simplified, weaker test for judging such
##' attributes.
##' @param x an object of a class Boruta.
##' @param averageOver Either number of last importance source runs to
##' average over or Inf for averaging over the whole Boruta run.
##' @return A Boruta class object with modified \code{finalDecision} element.
##' Such object has few additional elements:
##' \item{originalDecision}{Original \code{finalDecision}.}
##' \item{averageOver}{Copy of \code{averageOver} parameter.}
##' @details Function claims as Confirmed those attributes that
##' have median importance higher than the median importance of
##' maximal shadow attribute, and the rest as Rejected.
##' Depending of the user choice, medians for the test
##' are count over last round, all rounds or N last
##' importance source runs.
##' @note This function should be used only when strict decision is
##' highly desired, because this test is much weaker than Boruta
##' and can lower the confidence of the final result.
##' @note \code{x} has to be made with \code{holdHistory} set to
##' \code{TRUE} for this code to run.
##' @author Miron B. Kursa
##' @export
TentativeRoughFix<-function(x,averageOver=Inf){
 if(class(x)!='Boruta')
  stop('This function needs Boruta object as an argument.');
 if(is.null(x$ImpHistory))
  stop('Importance history was not stored during the Boruta run.');
 if(!is.numeric(averageOver))
  stop('averageOver should be a numeric vector.');
 if(length(averageOver)!=1)
  stop('averageOver should be a one-element vector.');
 if(averageOver<1)
  stop('averageOver should be positive.');

 tentIdx<-which(x$finalDecision=='Tentative');
 if(length(tentIdx)==0){
  warning('There are no Tentative attributes! Returning original object.');
  return(x);
 }

 nRuns<-dim(x$ImpHistory)[1];


 if(averageOver>nRuns)
  averageOver<-nRuns;

 impHistorySubset<-x$ImpHistory[(nRuns-averageOver+1):nRuns,];
 medianTentImp<-sapply(impHistorySubset[,tentIdx],median);
 medianShaMaxImp<-median(impHistorySubset[,'shadowMax']);
 medianTentImp>medianShaMaxImp->toOrdain;

 ans<-x;
 ans$roughfixed<-TRUE;
 ans$averageOver<-averageOver;
 ans$originalDecision<-x$finalDecision;
 ans$finalDecision[tentIdx[toOrdain]]<-'Confirmed';
 ans$finalDecision[tentIdx[!toOrdain]]<-'Rejected';

 return(ans);
}
