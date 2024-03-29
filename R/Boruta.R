# Core of Boruta

#' @export
#' @rdname Boruta
Boruta<-function(x,...)
 UseMethod("Boruta")

#' Feature selection with the Boruta algorithm
#'
#' Boruta is an all relevant feature selection wrapper algorithm, capable of working with any classification method that output variable importance measure (VIM); by default, Boruta uses Random Forest.
#' The method performs a top-down search for relevant features by comparing original attributes' importance with importance achievable at random, estimated using their permuted copies, and progressively eliminating irrelevant features to stabilise that test.
#' @rdname Boruta
#' @method Boruta default
#' @param x data frame of predictors.
#' @param y response vector; factor for classification, numeric vector for regression, \code{Surv} object for survival (supports depends on importance adapter capabilities).
#' @param getImp function used to obtain attribute importance.
#' The default is getImpRfZ, which runs random forest from the \code{ranger} package and gathers Z-scores of mean decrease accuracy measure.
#' It should return a numeric vector of a size identical to the number of columns of its first argument, containing importance measure of respective attributes.
#' Any order-preserving transformation of this measure will yield the same result.
#' It is assumed that more important attributes get higher importance. +-Inf are accepted, NaNs and NAs are treated as 0s, with a warning.
#' @param pValue confidence level. Default value should be used.
#' @param mcAdj if set to \code{TRUE}, a multiple comparisons adjustment using the Bonferroni method will be applied. Default value should be used; older (1.x and 2.x) versions of Boruta were effectively using \code{FALSE}.
#' @param maxRuns maximal number of importance source runs.
#' You may increase it to resolve attributes left Tentative.
#' @param holdHistory if set to \code{TRUE}, the full history of importance is stored and returned as the \code{ImpHistory} element of the result.
#' Can be used to decrease a memory footprint of Boruta in case this side data is not used, especially when the number of attributes is huge; yet it disables plotting of such made \code{Boruta} objects and the use of the \code{\link{TentativeRoughFix}} function.
#' @param doTrace verbosity level. 0 means no tracing, 1 means reporting decision about each attribute as soon as it is justified, 2 means the same as 1, plus reporting each importance source run, 3 means the same as 2, plus reporting of hits assigned to yet undecided attributes.
#' @param ... additional parameters passed to \code{getImp}.
#' @return An object of class \code{Boruta}, which is a list with the following components:
#' \item{finalDecision}{a factor of three value: \code{Confirmed}, \code{Rejected} or \code{Tentative}, containing final result of feature selection.}
#' \item{ImpHistory}{a data frame of importances of attributes gathered in each importance source run.
#' Beside predictors' importances, it contains maximal, mean and minimal importance of shadow attributes in each run.
#' Rejected attributes get \code{-Inf} importance.
#' Set to \code{NULL} if \code{holdHistory} was given \code{FALSE}.}
#' \item{timeTaken}{time taken by the computation.}
#' \item{impSource}{string describing the source of importance, equal to a comment attribute of the \code{getImp} argument.}
#' \item{call}{the original call of the \code{Boruta} function.}
#' @details Boruta iteratively compares importances of attributes with importances of shadow attributes, created by shuffling original ones.
#' Attributes that have significantly worst importance than shadow ones are being consecutively dropped.
#' On the other hand, attributes that are significantly better than shadows are admitted to be Confirmed.
#' Shadows are re-created in each iteration.
#' Algorithm stops when only Confirmed attributes are left, or when it reaches \code{maxRuns} importance source runs.
#' If the second scenario occurs, some attributes may be left without a decision.
#' They are claimed Tentative.
#' You may try to extend \code{maxRuns} or lower \code{pValue} to clarify them, but in some cases their importances do fluctuate too much for Boruta to converge.
#' Instead, you can use \code{\link{TentativeRoughFix}} function, which will perform other, weaker test to make a final decision, or simply treat them as undecided in further analysis.
#' @references Miron B. Kursa, Witold R. Rudnicki (2010). Feature Selection with the Boruta Package.
#' \emph{Journal of Statistical Software, 36(11)}, p. 1-13.
#' URL: \doi{10.18637/jss.v036.i11}
#' @export
#' @examples
#' set.seed(777)
#'
#' #Boruta on the "small redundant XOR" problem; read ?srx for details
#' data(srx)
#' Boruta(Y~.,data=srx)->Boruta.srx
#'
#' #Results summary
#' print(Boruta.srx)
#'
#' #Result plot
#' plot(Boruta.srx)
#'
#' #Attribute statistics
#' attStats(Boruta.srx)
#'
#' #Using alternative importance source, rFerns
#' Boruta(Y~.,data=srx,getImp=getImpFerns)->Boruta.srx.ferns
#' print(Boruta.srx.ferns)
#' 
#' #Verbose
#' Boruta(Y~.,data=srx,doTrace=2)->Boruta.srx
#'
#' \dontrun{
#' #Boruta on the iris problem extended with artificial irrelevant features
#' #Generate said features
#' iris.extended<-data.frame(iris,apply(iris[,-5],2,sample))
#' names(iris.extended)[6:9]<-paste("Nonsense",1:4,sep="")
#' #Run Boruta on this data
#' Boruta(Species~.,data=iris.extended,doTrace=2)->Boruta.iris.extended
#' #Nonsense attributes should be rejected
#' print(Boruta.iris.extended)
#' }
#'
#' \dontrun{
#' #Boruta on the HouseVotes84 data from mlbench
#' library(mlbench); data(HouseVotes84)
#' na.omit(HouseVotes84)->hvo
#' #Takes some time, so be patient
#' Boruta(Class~.,data=hvo,doTrace=2)->Bor.hvo
#' print(Bor.hvo)
#' plot(Bor.hvo)
#' plotImpHistory(Bor.hvo)
#' }
#' \dontrun{
#' #Boruta on the Ozone data from mlbench
#' library(mlbench); data(Ozone)
#' library(randomForest)
#' na.omit(Ozone)->ozo
#' Boruta(V4~.,data=ozo,doTrace=2)->Bor.ozo
#' cat('Random forest run on all attributes:\n')
#' print(randomForest(V4~.,data=ozo))
#' cat('Random forest run only on confirmed attributes:\n')
#' print(randomForest(ozo[,getSelectedAttributes(Bor.ozo)],ozo$V4))
#' }
#' \dontrun{
#' #Boruta on the Sonar data from mlbench
#' library(mlbench); data(Sonar)
#' #Takes some time, so be patient
#' Boruta(Class~.,data=Sonar,doTrace=2)->Bor.son
#' print(Bor.son)
#' #Shows important bands
#' plot(Bor.son,sort=FALSE)
#' }
Boruta.default<-function(x,y,pValue=0.01,mcAdj=TRUE,maxRuns=100,doTrace=0,holdHistory=TRUE,getImp=getImpRfZ,...){
 #Timer starts... now!
 timeStart<-Sys.time()

 #Extract the call to store in output
 cl<-match.call()
 cl[[1]]<-as.name('Boruta')

 #Convert x into a data.frame
 if(!is.data.frame(x))
  x<-data.frame(x)

 ##Some checks on x & y
 if(length(grep('^shadow',names(x)))>0)
  stop('Attributes with names starting from "shadow" are reserved for internal use. Please rename them.')
 if(maxRuns<11)
  stop('maxRuns must be greater than 10.')

 ##Expands the information system with newly built random attributes and calculates importance
 addShadowsAndGetImp<-function(decReg,runs){
  #xSha is going to be a data frame with shadow attributes; time to init it.
  xSha<-x[,decReg!="Rejected",drop=F]
  while(dim(xSha)[2]<5) xSha<-cbind(xSha,xSha); #There must be at least 5 random attributes.

  #Now, we permute values in each attribute
  nSha<-ncol(xSha)
  data.frame(lapply(xSha,sample))->xSha
  names(xSha)<-paste('shadow',1:nSha,sep="")

  #Notifying user of our progress
  if(doTrace>1)
   message(sprintf(' %s. run of importance source...',runs))

  #Calling importance source; "..." can be used by the user to pass rf attributes (for instance ntree)
  impRaw<-getImp(cbind(x[,decReg!="Rejected"],xSha),y,...)
  if(!is.numeric(impRaw))
   stop("getImp result is not a numeric vector. Please check the given getImp function.")
  if(length(impRaw)!=sum(decReg!="Rejected")+ncol(xSha))
   stop("getImp result has a wrong length. Please check the given getImp function.")
  if(any(is.na(impRaw)|is.nan(impRaw))){
   impRaw[is.na(impRaw)|is.nan(impRaw)]<-0
   warning("getImp result contains NA(s) or NaN(s); replacing with 0(s), yet this is suspicious.")
  }

  #Importance must have Rejected attributes put on place and filled with -Infs
  imp<-rep(-Inf,nAtt+nSha);names(imp)<-c(attNames,names(xSha))
  impRaw->imp[c(decReg!="Rejected",rep(TRUE,nSha))]
  shaImp<-imp[(nAtt+1):length(imp)];imp[1:nAtt]->imp

  return(list(imp=imp,shaImp=shaImp))
 }

 ##Assigns hits
 assignHits<-function(hitReg,curImp){
  curImp$imp>max(curImp$shaImp)->hits
  if(doTrace>2){
   uncMask<-decReg=="Tentative"
   intHits<-sum(hits[uncMask])
   if(intHits>0)
    message(sprintf("Assigned hit to %s attribute%s out of %s undecided.",sum(hits[uncMask]),if(intHits==1) "" else "s",sum(uncMask)))
   else
    message("None of undecided attributes scored a hit.")
  }
  hitReg[hits]<-hitReg[hits]+1
  return(hitReg)
 }

 ##Checks whether number of hits is significant
 doTests<-function(decReg,hitReg,runs){
  pAdjMethod<-ifelse(mcAdj[1],'bonferroni','none')
  #If attribute is significantly more frequent better than shadowMax, its claimed Confirmed
  toAccept<-stats::p.adjust(stats::pbinom(hitReg-1,runs,0.5,lower.tail=FALSE),method=pAdjMethod)<pValue
  (decReg=="Tentative" & toAccept)->toAccept

  #If attribute is significantly more frequent worse than shadowMax, its claimed Rejected (=irrelevant)
  toReject<-stats::p.adjust(stats::pbinom(hitReg,runs,0.5,lower.tail=TRUE),method=pAdjMethod)<pValue
  (decReg=="Tentative" & toReject)->toReject

  #Update decReg
  decReg[toAccept]<-"Confirmed";"Rejected"->decReg[toReject]

  #Report progress
  if(doTrace>0){
   nAcc<-sum(toAccept)
   nRej<-sum(toReject)
   nLeft<-sum(decReg=="Tentative")
   if(nAcc+nRej>0)
    message(sprintf("After %s iterations, +%s: ",runs,format(difftime(Sys.time(),timeStart),digits=2)))
   if(nAcc>0)
    message(sprintf(" confirmed %s attribute%s: %s",
     nAcc,ifelse(nAcc==1,'','s'),.attListPrettyPrint(attNames[toAccept])))
   if(nRej>0)
    message(sprintf(" rejected %s attribute%s: %s",
     nRej,ifelse(nRej==1,'','s'),.attListPrettyPrint(attNames[toReject])))
   if(nAcc+nRej>0)
    if(nLeft>0){
     message(sprintf(" still have %s attribute%s left.\n",
      nLeft,ifelse(nLeft==1,'','s')))
    }else{
     if(nAcc+nRej>0) message(" no more attributes left.\n")
    }
  }
  return(decReg)
 }

 ##Creating some useful constants
 nAtt<-ncol(x); nrow(x)->nObjects
 attNames<-names(x); c("Tentative","Confirmed","Rejected")->confLevels

 ##Initiate state
 decReg<-factor(rep("Tentative",nAtt),levels=confLevels)
 hitReg<-rep(0,nAtt);names(hitReg)<-attNames
 impHistory<-list()
 runs<-0

 ##Main loop

 while(any(decReg=="Tentative") && (runs+1->runs)<maxRuns){
  curImp<-addShadowsAndGetImp(decReg,runs)
  hitReg<-assignHits(hitReg,curImp)
  decReg<-doTests(decReg,hitReg,runs)

  #If needed, update impHistory with scores obtained in this iteration
  if(holdHistory){
   imp<-c(curImp$imp,
    shadowMax=max(curImp$shaImp),
    shadowMean=mean(curImp$shaImp),
    shadowMin=min(curImp$shaImp))
   impHistory<-c(impHistory,list(imp))
  }
 }

 ##Building result
 impHistory<-do.call(rbind,impHistory)
 names(decReg)<-attNames
 ans<-list(finalDecision=decReg,ImpHistory=impHistory,
   pValue=pValue,maxRuns=maxRuns,light=TRUE,mcAdj=mcAdj,
   timeTaken=Sys.time()-timeStart,roughfixed=FALSE,call=cl,
   impSource=comment(getImp))

 "Boruta"->class(ans)
 return(ans)
}

.attListPrettyPrint<-function(x,limit=5){
 x<-sort(x)
 if(length(x)<limit+1)
  return(sprintf("%s;",paste(x,collapse=", ")))
 sprintf("%s and %s more;",paste(utils::head(x,limit),collapse=", "),length(x)-limit)
}

#' @rdname Boruta
#' @method Boruta formula
#' @param formula alternatively, formula describing model to be analysed.
#' @param data in which to interpret formula.
#' @export
Boruta.formula<-function(formula,data,...){
 make_df(formula,data,parent.frame())->d

 ##Run Boruta
 ans<-Boruta.default(d$X,d$Y,...)
 ans$call<-match.call()
 ans$call[[1]]<-as.name('Boruta')
 formula->ans$call[["formula"]]
 return(ans)
}

#' Print Boruta object
#'
#' Print method for the Boruta objects.
#' @method print Boruta
#' @param x an object of a class Boruta.
#' @param ... additional arguments passed to \code{\link{print}}.
#' @return Invisible copy of \code{x}.
#' @export
print.Boruta<-function(x,...){
 stopifnot(inherits(x,'Boruta'))
 cat(paste('Boruta performed ',dim(x$ImpHistory)[1],' iterations in ',format(x$timeTaken),'.\n',sep=''))
 if(x$roughfixed) cat(paste('Tentatives roughfixed over the last ',x$averageOver,' iterations.\n',sep=''))
 if(sum(x$finalDecision=='Confirmed')==0){
  cat(' No attributes deemed important.\n')} else {
  writeLines(strwrap(paste(sum(x$finalDecision=='Confirmed'),' attributes confirmed important: ',
   .attListPrettyPrint(names(x$finalDecision[x$finalDecision=='Confirmed']))),indent=1))
 }
 if(sum(x$finalDecision=='Rejected')==0){
  cat(' No attributes deemed unimportant.\n')} else {
  writeLines(strwrap(paste(sum(x$finalDecision=='Rejected'),' attributes confirmed unimportant: ',
   .attListPrettyPrint(names(x$finalDecision[x$finalDecision=='Rejected']))),indent=1))
 }
 if(sum(x$finalDecision=='Tentative')!=0){
  writeLines(strwrap(paste(sum(x$finalDecision=='Tentative'),' tentative attributes left: ',
   .attListPrettyPrint(names(x$finalDecision[x$finalDecision=='Tentative']))),indent=1))
 }
 invisible(x)
}

#' Small redundant XOR data
#'
#' A synthetic data set with 32 rows corresponding to all combinations of values of five logical features, A, B, N1, N2 and N3.
#' The decision Y is equal to A xor B, hence N1--N3 are irrelevant attributes.
#' The set also contains 3 additional features, A or B (AoB), A and B (AnB) and not A (nA), which provide a redundant, but still relevant way to reconstruct Y.
#'
#' This is set is an easy way to demonstrate the difference between all relevant feature selection methods, which should select all features except N1--N3, and minimal optimal ones, which will probably ignore most of them.
#' @format A data frame with 8 predictors, 4 relevant: A, B, AoB, AnB and nA, as well as 3 irrelevant N1, N2 and N3, and decision attribute Y.
#' @source \url{https://blog.mbq.me/relevance-and-redundancy/}
#' @usage data(srx)
"srx"

