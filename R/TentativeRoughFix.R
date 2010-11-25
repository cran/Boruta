# Function that estimates rough decision for Tentative attributes
# left after Boruta run.
# Author: Miron B. Kursa
###############################################################################

##TentativeRoughFix provides an approximate test to resolve Tentatives 
TentativeRoughFix<-function(x,averageOver='finalRound'){
	if(class(x)!='Boruta') stop('This function needs Boruta object as an argument.');
	
	tentIdx<-which(x$finalDecision=='Tentative');
	if(length(tentIdx)==0) {
		warning('There are no Tentative attributes! Returning original object.');
		return(x);
	}
	
	nRuns<-dim(x$ZScoreHistory)[1];
	if(!is.numeric(averageOver)){
		if(averageOver=='finalRound') {averageOver<-nRuns-3*x$roundRuns} else {
		if(averageOver=='allRounds') {averageOver<-nRuns} else {
			averageOver<-1;	
		}}	
	}
	
	if(averageOver<1) stop('Bad averageOver argument!');
	if(averageOver>nRuns) stop('averageOver exceeds number of runs!');
	
	sapply(x$ZScoreHistory[(nRuns-averageOver+1):nRuns,tentIdx],median)>median(x$ZScore[(nRuns-averageOver+1):nRuns,'randMax'])->dd;
	ans<-x;
	ans$roughfixed=TRUE;
	ans$averageOver=averageOver;
	ans$originalDecision<-x$finalDecision;
	ans$finalDecision[tentIdx[dd]]<-'Confirmed';
	ans$finalDecision[tentIdx[!dd]]<-'Rejected';
	
	return(ans);
}
