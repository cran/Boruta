# Core of Boruta.
# Author: Miron B. Kursa, based on the idea & original code by Witold R. Rudnicki
###############################################################################

##Boruta method
Boruta<-function(x,...){
	UseMethod("Boruta");
}

##Boruta.default implements actual Boruta algorithm
Boruta.default<-function(x,y,confidence=0.999,maxRuns=100,light=TRUE,doTrace=0,...){
	timeStart<-Sys.time();
	require(randomForest);
	cl<-match.call();
	cl[[1]]<-as.name('Boruta');
	##Performing some checks on x & y
	if(length(grep('^rand',names(x)))>0) 
		stop('Attributes with names starting from "rand" are reserved for internal use. Please rename them.');
	if(any(c(is.na(x),is.na(y))))
		stop('NAs in input are prohibited. Please remove them.');
	if(maxRuns<11)
		stop('maxRuns must be greater than 10.')
	
	##rfCaller expands the information system with newly built random attributes.
	rfCaller<-function(roundLevel){
		#Depending on wheather we use light or force version of Boruta, we remove Rejected attributes
		if(light) xrand<-x[,decReg!="Rejected",drop=F] else xrand<-x;
		#Thre must be at least 5 random attributes.
		while(dim(xrand)[2]<5) xrand<-cbind(xrand,xrand);
		#Now, we permute values in each attribute
		nRandA<-dim(xrand)[2];
		#Sampling in a way that saves factor columns
		data.frame(lapply(xrand,sample))->xrand;
		names(xrand)<-paste('rand',1:nRandA,sep="");
		if(doTrace>0) cat('.');
		#Calling randomForest. "..." can be used by the user to pass rf attributes (feg. ntrees)
		randomForest(cbind(x[,decReg!="Rejected"],xrand),y,importance=TRUE,keep.forest=FALSE,...)->rf;
		impRaw<-importance(rf,1,scale=TRUE)[,1]; 
		#Importance must have Rejected attributes put on place and filled with -Infs
		imp<-rep(-Inf,nAtt+nRandA);names(imp)<-c(attNames,names(xrand));
		impRaw->imp[c(decReg!="Rejected",rep(TRUE,nRandA))];
		randI<-rev(sort(imp[(nAtt+1):length(imp)]));imp[1:nAtt]->imp;
		#Update hits register	
 		randThre<-randI[roundLevel];
		imp>randThre->hits;
		hitReg[hits]<<-hitReg[hits]+1;
		#Update ZHistory
		imp<-c(imp,randMax=max(randI),randMean=mean(randI),randMin=min(randI));
		ZHistory<<-c(ZHistory,list(imp));
		return(NULL);
	}
	
	##doTests checks whether number of hits is significant.
	doTests<-function(runs,doAcceptances){
		#If attribute is significantly more frequent better than randMax, its claimed Confirmed (in final round only)
		toAccept<-hitReg>qbinom(confidence,runs,0.5,lower.tail=TRUE);
		(doAcceptances & decReg=="Tentative" & toAccept)->toAccept;
		#If attribute is significantly more frequent worse than randMax, its claimed Rejected (=Random). 
		#In initial round, criterion for being random is lowered, in order to compensate fluctuations. 
		#Thus we don't judge if attribute is confirmed till the final round.
		toReject<-hitReg<qbinom(confidence,runs,0.5,lower.tail=FALSE);
		(decReg=="Tentative" & toReject)->toReject;
		#Updating decReg
		decReg[toAccept]<<-"Confirmed";
		"Rejected"->>decReg[toReject];
		#Trace the result
		nAcc<-sum(toAccept);
		nRej<-sum(toReject);
		if(doTrace==2 & nAcc>0) cat('\n',nAcc,' attributes confirmed after this test: ',attNames[toAccept],'\n') 
		if(doTrace==2 & nRej>0) cat('\n',nRej,' attributes rejected after this test: ',attNames[toReject],'\n') 
	}

	##Creating some useful constants
	nAtt<-dim(x)[2];nObjects<-dim(x)[1]; 
	attNames<-names(x);confLevels<-c("Tentative","Confirmed","Rejected");
	#Calculating number of runs in a round (How much runs we need, so that probability of worst 
	#	important attribute (infinitesimally better than randMax) having 0 hits is smaller than 1-confidence)
	roundRuns<-ceiling(-log(1-confidence,2));
	
	##Initiating registers
	decReg<-factor(rep("Tentative",nAtt),levels=confLevels);
	hitReg<-rep(0,nAtt);names(hitReg)<-attNames; 
	ZHistory<-list();
	
	##Main loop
	#Initial rounds
	roundLevels<-c(5,3,2);
	for(round in 1:3) if(any(decReg!="Rejected")){
	 if(doTrace>0) cat(sprintf('Initial round %d: ',round));
	 replicate(roundRuns,rfCaller(roundLevels[round]));
	 doTests(roundRuns,FALSE);
	 hitReg<-0*hitReg;
	 if(doTrace>0) cat('\n');
	}
	
	#Final round
	if(doTrace>0) cat('Final round: ');
	runInFinalRound<-0;
	while(any(decReg=="Tentative") & runInFinalRound<maxRuns){
		rfCaller(1); runInFinalRound+1->runInFinalRound;
		if(runInFinalRound>=roundRuns) doTests(runInFinalRound,TRUE);
	}
	
	##Building result
	if(doTrace>0) cat('\n');
        ZHistory<-do.call(rbind,ZHistory);
	#c(attNames,"randMax","randMean","randMin")->names(ZHistory);
	names(decReg)<-attNames;
	ans<-list(finalDecision=decReg,ZScoreHistory=ZHistory,
			confidence=confidence,maxRuns=maxRuns,light=light,roundRuns=roundRuns,
			timeTaken=Sys.time()-timeStart,roughfixed=FALSE,call=cl);
	"Boruta"->class(ans);
	return(ans);
}

##Boruta.formula implements formulae interpretation
Boruta.formula<-function(formula,data=.GlobalEnv,...){
	##Grab and interpret the formula
	terms.formula(formula,data=data)->t;
	x<-eval(attr(t,"variables"),data);
	apply(attr(t,"factors"),1,sum)>0->sel;
	nam<-rownames(attr(t,"factors"))[sel];
	data.frame(x[sel])->df;names(df)<-nam;
	x[[attr(t,"response")]]->dec;
	##Run Boruta
	ans<-Boruta.default(df,dec,...);
	ans$call<-match.call();
	ans$call[[1]]<-as.name('Boruta'); 
	formula->ans$call[["formula"]];
	return(ans);
}

##print.Boruta prints the Boruta object in convenient, shortened form
print.Boruta<-function(x,...){
	if(class(x)!='Boruta') stop("This is NOT a Boruta object!")
	cat(paste('Boruta performed ',dim(x$ZScoreHistory)[1],' randomForest runs in ',format(x$timeTaken),'.\n',sep=''));
	if(x$roughfixed) cat(paste('Tentatives roughfixed over ',x$averageOver,' last randomForest runs.\n',sep=''));
	if(sum(x$finalDecision=='Confirmed')==0){
		cat('        No attributes has been deemed important\n')} else {
		writeLines(strwrap(paste(sum(x$finalDecision=='Confirmed'),' attributes confirmed important: ',
		 paste(sep='',collapse=' ',names(x$finalDecision[x$finalDecision=='Confirmed']))),indent=8));
	}
	if(sum(x$finalDecision=='Rejected')==0){
		cat('        No attributes has been deemed unimportant\n')} else {
		writeLines(strwrap(paste(sum(x$finalDecision=='Rejected'),' attributes confirmed unimportant: ',
		 paste(sep='',collapse=' ',names(x$finalDecision[x$finalDecision=='Rejected']))),indent=8));
	}
	if(sum(x$finalDecision=='Tentative')!=0){
		writeLines(strwrap(paste(sum(x$finalDecision=='Tentative'),' tentative attributes left: ',
		 paste(sep='',collapse=' ',names(x$finalDecision[x$finalDecision=='Tentative']))),indent=8));
	}		
}

##attStats shows a data frame containing decision for attributes and some stats on their ZScores
attStats<-function(x){
	if(class(x)!='Boruta') stop('This function needs Boruta object as an argument.');
	lz<-lapply(1:ncol(x$ZScoreHistory),function(i) x$ZScoreHistory[is.finite(x$ZScoreHistory[,i]),i]);
        colnames(x$ZScoreHistory)->names(lz);
	mr<-lz$randMax;lz[1:(length(lz)-3)]->lz;
	t(sapply(lz,function(x) c(mean(x),median(x),min(x),max(x),sum(mr[1:length(x)]<x)/length(mr))))->st;
	st<-data.frame(st,x$finalDecision);
	names(st)<-c("meanZ","medianZ","minZ","maxZ","normHits","decision");
	return(st);
}


