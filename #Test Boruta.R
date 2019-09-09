# Boruta test
set.seed(777)
iris.extended<-data.frame(iris,apply(iris[,-5],2,sample))
names(iris.extended)[6:9]<-paste("Nonsense",1:4,sep="")

# formula version
# Boruta(Species~.,data=iris.extended,doTrace=2)->Boruta.iris.extended
# Boruta.iris.extended

#Run Boruta on this data
Boruta.iris.extended <- Boruta(y = iris.extended$Species, 
                               x = iris.extended[,-which(colnames(iris.extended) == "Species")], 
                               doTrace=2)
#Nonsense attributes should be rejected, except 1
print(Boruta.iris.extended)
Boruta.iris.extended2 <- ResumeBoruta(checkpoint = Boruta.iris.extended, y = iris.extended$Species, x = iris.extended[,-which(colnames(iris.extended) == "Species")], maxAdditionalRuns=1000, doTrace=2)
#Last nonsense attribute shoudl be confirmed important.
print(Boruta.iris.extended)
print(Boruta.iris.extended2)

# Same but using H2O randomForest
# > head(x)
# Sepal.Length Sepal.Width Petal.Length Petal.Width Nonsense1 Nonsense2 Nonsense3 Nonsense4
# 1          5.1         3.5          1.4         0.2       5.1       3.4       4.5       1.4
# 2          4.9         3.0          1.4         0.2       5.1       2.3       1.3       2.3
# 3          4.7         3.2          1.3         0.2       5.5       3.3       4.8       0.2
# 4          4.6         3.1          1.5         0.2       5.8       2.6       6.1       2.0
# 5          5.0         3.6          1.4         0.2       4.8       2.4       4.4       1.0
# 6          5.4         3.9          1.7         0.4       4.9       3.1       1.5       0.4

library(h2o)
h2o.init()
Boruta.iris.extended_h2o <- Boruta(y = iris.extended[, "Species"], 
                                   x = iris.extended[, setdiff(colnames(iris.extended), "Species")],
                                   getImp = getImpH2O,
                                   h2o_model_fn = h2o::h2o.randomForest,
                                   importance_type = "relative_importance",
                                   model_id = "Boruta_h2o_test",
                                   # ntrees = 500,
                                   nfolds = 5,
                                   doTrace=2)
print(Boruta.iris.extended_h2o)
plotImpHistory(Boruta.iris.extended_h2o)

Boruta.iris.extended_h2o$hits
colSums(Boruta.iris.extended_h2o$ImpHistory[,1:8] > Boruta.iris.extended_h2o$ImpHistory[, "shadowMax"])

Boruta.iris.extended_h2o_2 <- ResumeBoruta(checkpoint = Boruta.iris.extended_h2o,
                                          y = iris.extended[, "Species"], 
                                          x = iris.extended[, setdiff(colnames(iris.extended), "Species")],
                                          getImp = getImpH2O,
                                          h2o_model_fn = h2o::h2o.randomForest,
                                          importance_type = "relative_importance",
                                          model_id = "Boruta_h2o_test",
                                          #ntrees = 500,
                                          nfolds = 5,
                                          maxAdditionalRuns = 100,
                                          doTrace=2)

library(h2o)
h2o.init()
iris.hex <- as.h2o(iris.extended, destination_frame = "iris_extended")
x <- h2o.assign(iris.hex[, setdiff(colnames(iris.hex), "Species")], "x")
y <- h2o.assign(iris.hex[, "Species"], "y")

# h2o.ls() # x, y, iris.hex
# 
# fn <- function(x = x, y = y) {
#   print(h2o.ls())
#   x_new <- h2o.assign(x, "x_new")
#   print(h2o.ls())
#   # x <- h2o.assign(x, "x") # fails with Destination key must differ from input frame x 
#   x <- h2o.assign(x, "x_Boruta")
#   print(h2o.ls())
#   return(TRUE)
# }
# fn(x = x, y = y)
# h2o.ls() # x_Boruta, x_new, x, y, iris_extended

Boruta.iris.extended_h2o <- Boruta(y = y, 
                                   x = x,
                                   getImp = getImpH2O,
                                   h2o_model_fn = h2o::h2o.randomForest,
                                   importance_type = "relative_importance",
                                   model_id = "Boruta_h2o_test",
                                   ntrees = 500,
                                   nfolds = 5,
                                   doTrace=2)
print(Boruta.iris.extended_h2o)
plotImpHistory(Boruta.iris.extended_h2o)

# test speed for h2o shuffle
library(h2o)
library(rbenchmark)
h2o.init()
iris.hex <- as.h2o(iris.extended, destination_frame = "iris_extended")


dat1 <- h2o.assign(iris.hex, "dat1")
dat2 <- h2o.assign(iris.hex, "dat2")
dat3 <- h2o.assign(iris.hex, "dat3")

tmp1 <- h2o.shuffle_column(dat = dat1, col_idx = 1)
tmp2 <- h2o.shuffle_column2(dat = dat2, col_idx = 1, modify_inline = FALSE)
h2o.shuffle_column2(dat = dat3, col_idx = 1, modify_inline = TRUE)

for(i in seq_len(ncol(x))) 
  h2o.shuffle_column2(dat = dat2, col_idx = i, modify_inline = TRUE)

tail(dat1)
tail(dat2)

res <- h2o.assign(dat2, "res")
head(res)

# test replications elapsed relative user.self sys.self user.child sys.child
# 1    A           10 127.376    1.954   110.904    1.341          0         0
# 2    B           10  85.070    1.305    71.308    0.928          0         0
# 3    B           10  65.203    1.000    53.221    0.893          0         0

# removing more copy code
# test replications elapsed relative user.self sys.self user.child sys.child
# 1    A           10 126.967    2.617   111.067    1.211          0         0
# 2    B           10  65.966    1.360    53.583    0.865          0         0
# 3    C           10  48.512    1.000    38.097    0.723          0         0

benchmark(
  A = for(i in seq_len(ncol(x))) { dat1 <- h2o.shuffle_column(dat = dat1, col_idx = i) },
  B = for(i in seq_len(ncol(x))) { dat2 <- h2o.shuffle_column2(dat = dat2, col_idx = i, modify_inline = FALSE) },
  C = for(i in seq_len(ncol(x))) { h2o.shuffle_column2(dat = dat3, col_idx = i, modify_inline = TRUE) },
  replications = 10
)



# h2o.gbm, h2o.randomForest, h2o.deeplearning, h2o.xgboost
# h2o.glm needs multinomial family
# h2o.naiveBayes does not do varimp
# h2o.gbm, h2o.deeplearning do not do well at distinguishing features here
# xgboost only really uses Petal.Length and Petal.Width
model.hex <- h2o.randomForest(x = setdiff(colnames(iris.extended), "Species"),
                     y = "Species",
                     training_frame = iris.hex,
                     nfolds = 5,
                     fold_assignment = "Random",
                     # family = "multinomial",
                     keep_cross_validation_models = TRUE)
h2o.varimp(model.hex)
model_cv1.hex <- h2o.getModel(model.hex@model$cross_validation_models[[1]]$name)
h2o.varimp(model_cv1.hex)

# For naiveBayes:
model.hex@model$pcond
# this is mean and sd for numeric features, probability of predictor given response for factor features
# search pcond in https://github.com/h2oai/h2o-3/blob/master/h2o-algos/src/main/java/hex/naivebayes/NaiveBayes.java
# for factor features, could use this to rank and remove them, but cannot handle numerics

pcond <- model.hex@model$pcond
tmp <- lapply(pcond, function(x) as.numeric(x$mean))
# tmp <- lapply(tmp, log)
tmp <- lapply(tmp, mean)
names(tmp) <- sapply(pcond, function(x) names(x)[[1]])
unlist(tmp)
colMeans(iris.extended[, -which(colnames(iris.extended) == "Species")])

x <- iris.extended[, -which(colnames(iris.extended) == "Species")]
y <- iris.extended$Species

doTrace=2
pValue=0.01
mcAdj=TRUE
maxRuns=100
holdHistory=TRUE
getImp=getImpRfZ

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
if(any(c(is.na(x),is.na(y))))
  stop('Cannot process NAs in input. Please remove them.')
if(maxRuns<11)
  stop('maxRuns must be greater than 10.')


##Creating some useful constants
nAtt<-ncol(x); nrow(x)->nObjects
attNames<-names(x); c("Tentative","Confirmed","Rejected")->confLevels

##Initiate state
decReg<-factor(rep("Tentative",nAtt),levels=confLevels)
hitReg<-rep(0,nAtt);names(hitReg)<-attNames
impHistory<-list()
runs<-0

runs+1->runs

# curImp<-addShadowsAndGetImp(decReg,runs,x,y,getImp,doTrace,...)
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
nAtt<-ncol(x)
attNames<-names(x)
imp<-rep(-Inf,nAtt+nSha);names(imp)<-c(attNames,names(xSha))
impRaw->imp[c(decReg!="Rejected",rep(TRUE,nSha))]
shaImp<-imp[(nAtt+1):length(imp)];imp[1:nAtt]->imp

# return(list(imp=imp,shaImp=shaImp))
curImp <- list(imp=imp,shaImp=shaImp)

# hitReg<-assignHits(hitReg,curImp,doTrace)
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
# return(hitReg)

# decReg<-doTests(decReg,hitReg,runs,mcAdj,pValue,doTrace,timeStart)
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
  names(hitReg)->attNames
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


#If needed, update impHistory with scores obtained in this iteration
if(holdHistory){
  imp<-c(curImp$imp,
         shadowMax=max(curImp$shaImp),
         shadowMean=mean(curImp$shaImp),
         shadowMin=min(curImp$shaImp))
  impHistory<-c(impHistory,list(imp))
}
