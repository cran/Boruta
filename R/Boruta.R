# Core of Boruta.
# Author: Miron B. Kursa, based on the idea & original code by Witold R. Rudnicki
###############################################################################

##' @name Boruta
##' @rdname Boruta
##' @export Boruta
Boruta<-function(x,...){
        UseMethod("Boruta");
}

##Adapter of RandomForest importance
getImpRf<-function(x,y,...){
        randomForest(x,y,importance=TRUE,keep.forest=FALSE,...)->rf;
        importance(rf,1,scale=TRUE)[,1];
}

attr(getImpRf,"toLoad")<-'randomForest';
comment(getImpRf)<-'randomForest Z-score';

##' @name Boruta.default
##' @rdname Boruta
##' @title Important attribute search using Boruta algorithm
##' @method Boruta default
##' @description Boruta is an all-relevant feature selection wrapper algorithm.
##' It finds relevant features by comparing original attributes'
##' importance with importance achievable at random, estimated
##' using their permuted copies.
##' @param x data frame of predictors.
##' @param y response vector; factor for classification, numeric vector for regression.
##' @param getImp function used to obtain attribute importance. The default is getImpRf, which runs random forest from \code{randomForest} package and gathers Z-scores of mean decrease accuracy measure.
##' @param confidence confidence level. Default value should be used. Lower value may reduce computation time of test runs.
##' @param maxRuns maximal number of randomForest runs in the final round. You may increase it to resolve attributes left Tentative.
##' @param doTrace verbosity level. 0 means no tracing, 1 means printing a "." sign after each importance source run,
##' 2 means same as 1, plus consecutive reporting of test results.
##' @param light if set to \code{TRUE}, Boruta runs in standard mode, in which attributes claimed Rejected are removed with their shadows;
##' if set to \code{FALSE}, Boruta runs in `forced' mode, in which all shadows are present during the whole run.
##' @param ... additional parameters passed to getImp.
##' @return An object of class \code{Boruta}, which is a list with the following components:
##' \item{finalDecision}{a factor of three value: \code{Confirmed}, \code{Rejected} or \code{Tentative},
##' containing final result of feature selection.}
##' \item{ImpHistory}{a data frame of importances of attributes gathered in each importance source run.
##' Beside predictors' importances contains maximal, mean and minimal importance of shadow attributes in each run.
##' Rejected attributes have \code{-Inf} importance assumed.}
##' \item{timeTaken}{time taken by the computation.}
##' \item{impSource}{string describing the source of importance, equal to a comment attribute of the \code{getImp} argument.}
##' \item{call}{the original call of the \code{Boruta} function.}
##' @details Boruta iteratively compares importances of attributes with importances of shadow attributes, created by
##' shuffling original ones. Attributes that have significantly worst importance than shadow ones
##' are being consecutively dropped. On the other hand, attributes that are significantly better than
##' shadows are admitted to be Confirmed.
##' If algorithm is run in default light mode, unimportant attributes are being dropped along with their random shadows,
##' while in the forced mode all shadow attributes are preserved during the whole Boruta run.
##' Algorithm stops when only Confirmed attributes are left, or when it reaches \code{maxRuns} importance source runs
##' in the last round. If the second scenario occurs, some attributes may be left without a decision. They are
##' claimed Tentative. You may try to extend \code{maxRuns} or lower \code{confidence} to clarify them, but in
##' some cases their importances do fluctuate too much for Boruta to converge.
##' Instead, you can use \code{\link{TentativeRoughFix}} function, which will perform other, weaker test to make a final
##' decision, or simply treat them as undecided in further analysis.
##' @note Version 2.0.0 changes some name conventions and thus may be incompatible with scripts written for 1.x.x version and
##' old Boruta objects. Solutions of most problems of this kind should boil down to change of \code{ZScoreHistory} to \code{ImpHistory}
##' in script source or Boruta object structure.
##'
##' In normal use, \code{light} should be set to \code{TRUE}; force mode is experimental and has not been well tested yet.
##' @references Miron B. Kursa, Witold R. Rudnicki (2010). Feature Selection with the Boruta Package.
##' \emph{Journal of Statistical Software, 36(11)}, p. 1-13.
##' URL: \url{http://www.jstatsoft.org/v36/i11/}
##' @author Miron B. Kursa, based on the idea & original code by Witold R. Rudnicki.
##' @S3method Boruta default
##' @examples
##' set.seed(777);
##' #Add some nonsense attributes to iris dataset by shuffling original attributes
##' iris.extended<-data.frame(iris,apply(iris[,-5],2,sample));
##' names(iris.extended)[6:9]<-paste("Nonsense",1:4,sep="");
##' #Run Boruta on this data
##' Boruta(Species~.,data=iris.extended,doTrace=2)->Boruta.iris.extended
##' #Nonsense attributes should be rejected
##' print(Boruta.iris.extended);
##' #Boruta using rFerns' importance (rFerns package must be installed!)
##' #Definition of ferns' importance adapter
##' getImpFerns<-function(x,y,...){
##' f<-rFerns(x,y,saveForest=FALSE,importance=TRUE,...);
##' f$importance[,1]
##' }
##' #Those are optional
##' attr(getImpFerns,"toLoad")<-"rFerns";
##' comment(getImpFerns)<-"rFerns importance"
##' #Running altered Boruta on the Iris data
##' Boruta(Species~.,data=iris.extended,getImp=getImpFerns)->Boruta.ferns.irisE
##' print(Boruta.ferns.irisE);
##' \dontrun{
##' #Boruta on the Ozone data from mlbench
##' library(mlbench); data(Ozone);
##' na.omit(Ozone)->ozo;
##' #Takes some time, so be patient
##' Boruta(V4~.,data=ozo,doTrace=2)->Bor.ozo;
##' cat('Random forest run on all attributes:\n');
##' print(randomForest(V4~.,data=ozo));
##' cat('Random forest run only on confirmed attributes:\n');
##' print(randomForest(getConfirmedFormula(Bor.ozo),data=ozo));
##' }
##' \dontrun{
##' #Boruta on the HouseVotes84 data from mlbench
##' library(mlbench); data(HouseVotes84);
##' na.omit(HouseVotes84)->hvo;
##' #Takes some time, so be patient
##' Boruta(Class~.,data=hvo,doTrace=2)->Bor.hvo;
##' print(Bor.hvo);
##' plot(Bor.hvo);
##' }
##' \dontrun{
##' #Boruta on the Sonar data from mlbench
##' library(mlbench); data(Sonar);
##' #Takes some time, so be patient
##' Boruta(Class~.,data=Sonar,doTrace=2)->Bor.son;
##' print(Bor.son);
##' #Shows important bands
##' plot(Bor.son,sort=FALSE);
##' }
Boruta.default<-function(x,y,confidence=0.999,maxRuns=100,light=TRUE,doTrace=0,getImp=getImpRf,...){
        if(!is.null(attr(getImp,"toLoad"))){
                #Load packages required by getImp
                if(!all(sapply(attr(getImp,"toLoad"),require,character.only=TRUE))){
                        stop('Unable to load all packages required by given getImp.');
                }
        }

        #Timer starts... now!
        timeStart<-Sys.time();

        #Extract the call to store in output
        cl<-match.call();
        cl[[1]]<-as.name('Boruta');

        ##Some checks on x & y
        if(length(grep('^shadow',names(x)))>0)
                stop('Attributes with names starting from "shadow" are reserved for internal use. Please rename them.');
        if(any(c(is.na(x),is.na(y))))
                stop('NAs in input are prohibited. Please remove them.');
        if(maxRuns<11)
                stop('maxRuns must be greater than 10.')

        ##rfCaller expands the information system with newly built random attributes, calculates importance
        ## and updates hits register and ZHistory
        rfCaller<-function(roundLevel){
                #xSha is going to be a data frame with shadow attributes; time to init it.
                # Depending on wheather we use light or forced version of Boruta, we remove Rejected attributes
                xSha<-if(light) x[,decReg!="Rejected",drop=F] else x;
                while(dim(xSha)[2]<5) xSha<-cbind(xSha,xSha); #There must be at least 5 random attributes.

                #Now, we permute values in each attribute
                nSha<-ncol(xSha);
                data.frame(lapply(xSha,sample))->xSha;
                names(xSha)<-paste('shadow',1:nSha,sep="");

                #Notifying user of our progress
                if(doTrace>0) cat('.');

                #Calling importance source; "..." can be used by the user to pass rf attributes (for instance ntree)
                impRaw<-getImp(cbind(x[,decReg!="Rejected"],xSha),y,...);

                #Importance must have Rejected attributes put on place and filled with -Infs
                imp<-rep(-Inf,nAtt+nSha);names(imp)<-c(attNames,names(xSha));
                impRaw->imp[c(decReg!="Rejected",rep(TRUE,nSha))];
                shaI<-rev(sort(imp[(nAtt+1):length(imp)]));imp[1:nAtt]->imp;

                #Now we increment hit register for features that got better importance than roundLevel-ith best shadow
                imp>shaI[roundLevel]->hits;
                hitReg[hits]<<-hitReg[hits]+1;

                #And update ZHistory with scores obtained in this iteration
                imp<-c(imp,shadowMax=max(shaI),shadowMean=mean(shaI),shadowMin=min(shaI));
                ZHistory<<-c(ZHistory,list(imp));

                return(NULL);
        }

        ##doTests checks whether number of hits is significant
        doTests<-function(runs,doAcceptances){
                #If attribute is significantly more frequent better than shadowMax, its claimed Confirmed (in final round only)
                toAccept<-hitReg>qbinom(confidence,runs,0.5,lower.tail=TRUE);
                (doAcceptances & decReg=="Tentative" & toAccept)->toAccept;

                #If attribute is significantly more frequent worse than shadowMax, its claimed Rejected (=irrelevant)
                #In initial round, criterion for being random is lowered, in order to compensate fluctuations
                #Thus we don't judge if attribute is confirmed till the final round
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
        nAtt<-ncol(x); nrow(x)->nObjects;
        attNames<-names(x); c("Tentative","Confirmed","Rejected")->confLevels;

        #Calculating number of runs in a round (How much runs we need, so that probability of worst
        # important attribute (infinitesimally better than shadowMax) having 0 hits is smaller than 1-confidence)
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
        names(decReg)<-attNames;
        ans<-list(finalDecision=decReg,ImpHistory=ZHistory,
                        confidence=confidence,maxRuns=maxRuns,light=light,roundRuns=roundRuns,
                        timeTaken=Sys.time()-timeStart,roughfixed=FALSE,call=cl,impSource=comment(getImp));

        "Boruta"->class(ans);
        return(ans);
}

##' @name Boruta.formula
##' @rdname Boruta
##' @method Boruta formula
##' @param formula alternatively, formula describing model to be analysed.
##' @param data in which to interpret formula.
##' @S3method Boruta formula
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

##' @name print.Boruta
##' @method print Boruta
##' @title Print Boruta object
##' @description print method for Boruta objects.
##' @param x an object of a class Boruta.
##' @param ... additional arguments passed to \code{\link{print}}.
##' @return Invisible copy of \code{x}.
##' @author Miron B. Kursa
##' @S3method print Boruta

#TODO: Print should react about alternative importance!
print.Boruta<-function(x,...){
        if(class(x)!='Boruta') stop("This is NOT a Boruta object!")
        cat(paste('Boruta performed ',dim(x$ImpHistory)[1],' randomForest runs in ',format(x$timeTaken),'.\n',sep=''));
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
        invisible(x)
}


##' @name attStats
##' @title Extract attribute statistics
##' @description attStats shows a summary of a Boruta run in an attribute-centred way. It produces a data frame containing some importance
##' stats as well as the number of hits that attribute scored and the decision it was given.
##' @param x an object of a class Boruta, from which attribute stats should be extracted.

##' @return A data frame containing, for each attribute that was originally in information system,
##' mean, median, maximal and minimal importance, number of hits normalised to number of importance
##' source runs performed and the decision copied from \code{finalDecision}.
##' @note  When using a Boruta object generated by a \code{\link{TentativeRoughFix}}, the resulting data frame will consist a rough fixed decision.
##' @author Miron B. Kursa
##' @export attStats

##' @examples
##' \dontrun{
##' library(mlbench); data(Sonar);
##' #Takes some time, so be patient
##' Boruta(Class~.,data=Sonar,doTrace=2)->Bor.son;
##' print(Bor.son);
##' stats<-attStats(Bor.son);
##' print(stats);
##' plot(normHits~meanZ,col=stats$decision,data=stats);
##' }
attStats<-function(x){
        if(class(x)!='Boruta') stop('This function needs Boruta object as an argument.');
        lz<-lapply(1:ncol(x$ImpHistory),function(i) x$ImpHistory[is.finite(x$ImpHistory[,i]),i]);
        colnames(x$ImpHistory)->names(lz);
        mr<-lz$shadowMax; lz[1:(length(lz)-3)]->lz;
        t(sapply(lz,function(x) c(mean(x),median(x),min(x),max(x),sum(mr[1:length(x)]<x)/length(mr))))->st;
        st<-data.frame(st,x$finalDecision);
        names(st)<-c("meanZ","medianZ","minZ","maxZ","normHits","decision");
        return(st);
}

##' @name getSelectedAttributes
##' @title Extract names of the selected attributes
##' @param x an object of a class Boruta, from which relevant attributes names should be extracted.
##' @param withTentative if set to \code{TRUE}, Tentative attributes will be also returned.
##' @return A character vector with names of the relevant attributes.
##' @author Miron B. Kursa
##' @export getSelectedAttributes
##' @examples
##' data(iris);
##' #Takes some time, so be patient
##' Boruta(Species~.,data=iris,doTrace=2)->Bor.iris;
##' print(Bor.iris);
##' print(getSelectedAttributes(Bor.iris));
getSelectedAttributes<-function(x,withTentative=FALSE){
        if(class(x)!='Boruta') stop('This function needs Boruta object as an argument.');
        names(x$finalDecision)[
                x$finalDecision%in%(if(withTentative) "Confirmed" else c("Confirmed","Tentative"))
        ]
}

