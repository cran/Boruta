# Plot functions for Boruta.
# Author: Miron B. Kursa
###############################################################################

##generateCol is internaly used by plot.Boruta and plotZHistory
generateCol<-function(x,colCode,col,numRand){
	#Checking arguments
	if(is.null(col) & length(colCode)!=4) stop('colCode should have 4 elements.');
	#Generating col
	if(is.null(col)){
		rep(colCode[4],length(x$finalDecision)+numRand)->cc;
		cc[c(x$finalDecision=='Confirmed',rep(FALSE,numRand))]<-colCode[1];
		cc[c(x$finalDecision=='Tentative',rep(FALSE,numRand))]<-colCode[2];
		cc[c(x$finalDecision=='Rejected',rep(FALSE,numRand))]<-colCode[3];
		col=cc;
	}
	return(col);	
}

##plot.Boruta draws ZScores obtained during process as boxplots and final decision as their colours.
plot.Boruta<-function(x,colCode=c('green','yellow','red','blue'),sort=TRUE,whichRand=c(TRUE,TRUE,TRUE),
		col=NULL,xlab='Attributes',ylab='Importance',...){
	#Checking arguments
	if(class(x)!='Boruta') stop('This function needs Boruta object as an argument.');
	#Removal of -Infs and conversion to a list
        lz<-lapply(1:ncol(x$ZScoreHistory),function(i) x$ZScoreHistory[is.finite(x$ZScoreHistory[,i]),i]);
        colnames(x$ZScoreHistory)->names(lz);
	#Selection of randomised meta-attributes
	numRand<-sum(whichRand);
	lz[c(rep(TRUE,length(x$finalDecision)),whichRand)]->lz;
	#Generating color vector
	col<-generateCol(x,colCode,col,numRand);
	#Ordering boxes due to attribute median ZScore
	if(sort) {ii<-order(sapply(lz,median));lz[ii]->lz;col<-col[ii]}
	#Final plotting
	boxplot(lz,xlab=xlab,ylab=ylab,col=col,...);
}

##plotZHistory draws ZScores obtained during process as lines and final decision as their colours.
plotZHistory<-function(x,colCode=c('green','yellow','red','blue'),showRounds=TRUE,col=NULL,type="l",lty=1,pch=0,
		xlab='Random Forest run',ylab='Importance',...){
	#Checking arguments
	if(class(x)!='Boruta') stop('This function needs Boruta object as an argument.');
	col<-generateCol(x,colCode,col,3)
	#Final plotting
	matplot(0:(nrow(x$ZScoreHistory)-1),x$ZScoreHistory,xlab=xlab,ylab=ylab,col=col,type=type,lty=lty,pch=pch,...);
	if(showRounds) abline(v=(0:3)*x$roundRuns,col="gray");
}
