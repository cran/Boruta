# Two functions converting Boruta result into a convenient formula,
# for instance for testing or passing to classification algorithms.
# Author: Miron B. Kursa
###############################################################################

getConfirmedFormula<-function(x){
	if(class(x)!='Boruta') stop('This function needs Boruta object as an argument.');
	if(is.null(x$call[["formula"]])) stop('The model for this Boruta run was not a formula.');
	deparse(x$call[["formula"]][[2]])->dec;
	preds<-paste(names(x$finalDecision)[x$finalDecision=='Confirmed'],collapse="+");
	return(as.formula(sprintf('%s~%s',dec,preds)));
}

getNonRejectedFormula<-function(x){
	if(class(x)!='Boruta') stop('This function needs Boruta object as an argument.');
	if(is.null(x$call[["formula"]])) stop('The model for this Boruta run was not a formula.');
	deparse(x$call[["formula"]][[2]])->dec;
	preds<-paste(names(x$finalDecision)[x$finalDecision!='Rejected'],collapse="+");
	return(as.formula(sprintf('%s~%s',dec,preds)));	
}
