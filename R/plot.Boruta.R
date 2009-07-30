# Plot method for Boruta
# Author: Miron B. Kursa
###############################################################################

##plot.Boruta draws ZScores obtained during process as boxplots and final decision as their colours.
plot.Boruta<-function(x,colCode=c('green','yellow','red','blue'),sort=TRUE,whichRand=c(TRUE,TRUE,TRUE),
		col=NULL,xlab='Attributes',ylab='Z-Score',...){
	#Checking arguments
	if(class(x)!='Boruta') stop('This function needs Boruta object as an argument.')
	if(is.null(col) & length(colCode)!=4) stop('colCode should have 4 elements.')
	#Removal of -Infs
	as.list(x$ZScoreHistory)->lz;lz<-lapply(lz,function(x) x[is.finite(x)]);
	#Selection of randomised meta-attributes
	numRand<-sum(whichRand);
	lz[c(rep(TRUE,length(x$finalDecision)),whichRand)]->lz;
	#Generating col
	if(is.null(col)){
		rep(colCode[4],length(x$finalDecision)+numRand)->cc;
		cc[c(x$finalDecision=='Confirmed',rep(FALSE,numRand))]<-colCode[1];
		cc[c(x$finalDecision=='Tentative',rep(FALSE,numRand))]<-colCode[2];
		cc[c(x$finalDecision=='Rejected',rep(FALSE,numRand))]<-colCode[3];
		col=cc;
	}
	#Ordering boxes due to attribute median ZScore
	if(sort) {ii<-order(sapply(lz,median));lz[ii]->lz;col<-col[ii]}
	#Final plotting
	boxplot(lz,xlab=xlab,ylab=ylab,col=col,...);
}
