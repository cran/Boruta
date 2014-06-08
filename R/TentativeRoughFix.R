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
##' average over, \code{'finalRound'} for averaging over last (final) round or
## \code{'allRounds'} for averaging over whole Boruta run.
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
##' @author Miron B. Kursa
##' @export
TentativeRoughFix<-function(x,averageOver='finalRound'){
 if(class(x)!='Boruta')
  stop('This function needs Boruta object as an argument.');

 tentIdx<-which(x$finalDecision=='Tentative');
 if(length(tentIdx)==0){
  warning('There are no Tentative attributes! Returning original object.');
  return(x);
 }

 nRuns<-dim(x$ImpHistory)[1];
 if(!is.numeric(averageOver)){
  averageOver<-if(averageOver=='finalRound'){
   nRuns-3*x$roundRuns
  }else{
   if(averageOver=='allRounds') nRuns else 0
  }
 }

 if(averageOver<1)
  stop('Bad averageOver argument!');
 if(averageOver>nRuns)
  stop('averageOver exceeds number of runs!');

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
