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

#Run Boruta on this data
Boruta.iris.extended <- Boruta(y = iris.extended$Species, 
                               x = iris.extended[,-which(colnames(iris.extended) == "Species")], 
                               getImp = getImpCV,
                               getUnderlyingImp = getImpRfZ,
                               nfolds = 5,
                               doTrace=2)

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
# h2o.init(enable_assertions = FALSE)
h2o.init()
Boruta.iris.extended_h2o <- Boruta(y = iris.extended[, "Species"], 
                                   x = iris.extended[, setdiff(colnames(iris.extended), "Species")],
                                   getImp = getImpH2O,
                                   h2o_model_fn = h2o::h2o.randomForest,
                                   importance_type = "relative_importance",
                                   # model_id = "Boruta_h2o_test",
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
                                          # model_id = "Boruta_h2o_test",
                                          #ntrees = 500,
                                          nfolds = 5,
                                          maxAdditionalRuns = 100,
                                          doTrace=2)

library(h2o)
h2o.init(max_mem_size = "12G")
# h2o.init(ip = "h2o_planetexpressship1", startH2O = FALSE)

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
                                   # model_id = "Boruta_h2o_test",
                                   ntrees = 100,
                                   nfolds = 5,
                                   maxRuns = 11,
                                   doTrace=2)
print(Boruta.iris.extended_h2o)
plotImpHistory(Boruta.iris.extended_h2o)

stopifnot(h2o.all(x == iris.hex[, setdiff(colnames(iris.hex), "Species")]),
          h2o.all(y == iris.hex[, "Species"]))

Boruta.iris.extended_h2o_2 <- ResumeBoruta(checkpoint = Boruta.iris.extended_h2o,
                                           y = y, 
                                           x = x,
                                           getImp = getImpH2O,
                                           h2o_model_fn = h2o::h2o.randomForest,
                                           importance_type = "relative_importance",
                                           # model_id = "Boruta_h2o_test",
                                           ntrees = 100,
                                           nfolds = 5,
                                           doTrace=2,
                                           maxAdditionalRuns = 100)


#Run Boruta on this data
Boruta.iris.extended <- Boruta(y = y, 
                               x = x, 
                               h2o_model_fn = h2o::h2o.randomForest,
                               importance_type = "relative_importance",
                               getImp = getImpCV,
                               getUnderlyingImp = getImpH2O,
                               nfolds = 5,
                               doTrace=2)

# run on larger data set
# library(dplyr)
h2o.init(max_mem_size = "12G")
# h2o.init(ip = "h2o_planetexpressship1", startH2O = FALSE)
h2o.removeAll()
library(rbenchmark)
batting.hex <- as.h2o(Lahman::Batting, destination_frame = "batting")
h2o.impute(batting.hex)
batting.hex <- h2o.merge(x = batting.hex,
                         y = h2o.impute(batting.hex, method = "mode", column = "lgID", by = "teamID"),
                         by = "teamID")
batting.hex <- batting.hex[, -which(colnames(batting.hex) %in% c("lgID", "playerID"))]



x <- h2o.assign(batting.hex[, -which(colnames(batting.hex) == "R")], "x")
y <- h2o.assign(batting.hex[, "R"], "y")

# single replication: 
# test replications   elapsed relative user.self sys.self user.child sys.child
# 1                  h2o            1 12829.305   25.016   601.085   34.465          0         0
# 2            h2o_folds            1 11609.409   22.637   191.062   22.027          0         0
# 3    h2o_outside_folds            1  2358.982    4.600   199.986    7.744          0         0
# 4               ranger            1  1901.693    3.708  7238.204   14.722          0         0
# 5 ranger_outside_folds            1   512.849    1.000  1941.673    3.356          0         0

# 10 replications (and eliminating the copy for CV)
# test replications   elapsed relative user.self sys.self user.child sys.child
# 1    h2o_outside_folds           10 23996.821    4.554  1624.236   75.754      0.866     0.237
# 2               ranger           10 19146.209    3.634 68858.546  275.979      0.000     0.000
# 3 ranger_outside_folds           10  5268.847    1.000 19495.427   51.472      0.917     0.206


benchmark(
  # h2o = Boruta(x = x, 
  #              y = y, 
  #              getImp = getImpH2O,
  #              h2o_model_fn = h2o::h2o.randomForest,
  #              importance_type = "relative_importance",
  #              ntrees = 100,
  #              doTrace=2),
  # 
  # h2o_folds = Boruta(x = x, 
  #                    y = y, 
  #                    h2o_model_fn = h2o::h2o.randomForest,
  #                    importance_type = "relative_importance",
  #                    ntrees = 100,
  #                    getImp = getImpH2O,
  #                    nfolds = 5,
  #                    doTrace=2),
  
  h2o_outside_folds = Boruta(x = x, 
                             y = y, 
                             getImp = getImpCV,
                             nfolds = 5,
                             getUnderlyingImp = getImpH2O,
                             h2o_model_fn = h2o::h2o.randomForest,
                             importance_type = "relative_importance",
                             ntrees = 100,
                             doTrace=2),
  
  ranger = Boruta(x = as.data.frame(x), 
                  y = as.vector(y), 
                  getImp = getImpRfZ,
                  num.trees = 100,
                  doTrace=2),
  
  ranger_outside_folds = Boruta(x = as.data.frame(x), 
                                y = as.vector(y), 
                                getImp = getImpCV,
                                getUnderlyingImp = getImpRfZ,
                                num.trees = 100,
                                nfolds = 5,
                                doTrace=2),
  replications = 10
)


h2o.ls()
colnames(xSha)
colnames(h2o.getFrame("Boruta_xSha"))
colnames(h2o.getFrame("RTMP_sid_8254_7"))
dim(xSha)
tail(xSha)



print(Boruta.iris.extended)
plotImpHistory(Boruta.iris.extended)

stopifnot(h2o.all(x == batting.hex[, -which(colnames(batting.hex) == "R")]),
          h2o.all(y == batting.hex[, "R"]))

Boruta.iris.extended2 <- Boruta(y = batting.hex[, "R"], 
                               x = batting.hex[, -which(colnames(batting.hex) == "R")], 
                               h2o_model_fn = h2o::h2o.randomForest,
                               importance_type = "relative_importance",
                               getImp = getImpCV,
                               getUnderlyingImp = getImpH2O,
                               nfolds = 5,
                               doTrace=2)

print(Boruta.iris.extended2)
plotImpHistory(Boruta.iris.extended2)

stopifnot(h2o.all(x == batting.hex[, -which(colnames(batting.hex) == "R")]),
          h2o.all(y == batting.hex[, "R"]))

# iris_hf <- as.h2o(iris)
# my_sample <- function(x, size, replace = FALSE, prob = NULL) {
#   i <- runif(size)
#   x[i]
# }
# 
# summary(apply(iris_hf, 2, sample.int, size = nrow(iris_hf)))


# test bugs for h2o shuffle
h2o.shuffle_column <- function(dat, col) {
  # shuffled_dat <- h2o.assign(dat, "shuffled_dat")
  # dat <- h2o.assign(dat, "shuffle_column_dat")
  
  cols <- colnames(dat)
  col_subset <- h2o.cbind(dat[, col, drop = FALSE], h2o.runif(dat))
  
  # col_subset <- h2o.assign(col_subset, "shuffle_column_cbind")
  col_subset <- h2o.arrange(col_subset, "rnd")
  # col_subset <- h2o.assign(col_subset, "shuffle_column_tmp")
  
  # shuffled_dat <- h2o.cbind(dat[, cols_to_keep], col_subset[, col, drop = FALSE])
  # shuffled_dat <- shuffled_dat[, cols, drop = FALSE]
  dat[, col] <-  col_subset[, col, drop = FALSE]
  
  # out <- h2o.assign(dat, "shuffled_dat")
  # h2o.rm(col_subset)
  # h2o.rm("shuffle_column_cbind")
  # h2o.rm("shuffle_column_dat")
  
  
  return(dat)
}

h2o.shuffle_column <- function(dat, col, copy_and_remove = FALSE) { 
  h2o.no_progress()
  dat[, col] <- as.h2o(sample(as.vector(dat[, col]), size = nrow(dat)), destination_frame = "shuffle_column_tmp")
  h2o.show_progress()
  
  if(copy_and_remove) {
    out <- h2o.assign(dat, "shuffled_dat")
    h2o.rm("shuffle_column_tmp")
    return(out)
  }
  
  return(dat)
}


createCounter <- function(value) { function(i) { value <<- value+i; return(value)} }


h2o.removeAll()
x_orig <- h2o.assign(h2o.createFrame(rows = 1e05, cols = 100), "x_orig")
x <- h2o.assign(x_orig, "x")

tmp_fn <- function(x, copy_and_remove = FALSE) {
  for(col in colnames(x)) {
    cat(sprintf("%s ", col))
    x <- h2o.shuffle_column(x, col, copy_and_remove)
  }
  cat("\n\n")
}

benchmark(
  A = tmp_fn(x_orig, copy_and_remove = FALSE),
  B = tmp_fn(x_orig, copy_and_remove = TRUE), # about 50% slower
  replications = 10
)

counter <- createCounter(0)
tmp <- replicate(5, {
  # x_internal <- h2o.assign(x, "x_internal")
  cat(sprintf("%d\n", counter(1)))
  for(col in colnames(x)) {
    cat(sprintf("%s ", col))
    x <- h2o.shuffle_column(x, col)
  }
  cat("\n\n")
})


# test speed for h2o shuffle
library(h2o)
library(rbenchmark)
h2o.init()
iris.hex <- as.h2o(iris.extended, destination_frame = "iris_extended")


dat1 <- h2o.assign(iris.hex, "dat1")
dat2 <- h2o.assign(iris.hex, "dat2")
dat3 <- h2o.assign(iris.hex, "dat3")
dat4 <- h2o.assign(iris.hex, "dat4")
dat5 <- h2o.assign(iris.hex, "dat5")

# or a larger data set
dat1 <- h2o.assign(h2o.createFrame(), "dat1")
dat2 <- h2o.assign(h2o.createFrame(), "dat2")
dat3 <- h2o.assign(h2o.createFrame(), "dat3")
dat4 <- h2o.assign(h2o.createFrame(), "dat4")
dat5 <- h2o.assign(h2o.createFrame(), "dat5")



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

# test replications elapsed relative user.self sys.self user.child sys.child
# 1    A           10 130.974    4.034   112.086    1.371          0         0
# 2    B           10  68.344    2.105    54.493    0.813          0         0
# 3    C           10  51.374    1.582    38.945    0.740          0         0
# 4    D           10  32.467    1.000    23.487    0.497          0         0

# test replications elapsed relative user.self sys.self user.child sys.child
# 1    A           10 145.628    7.185   125.576    1.335          0         0
# 2    B           10  76.607    3.780    60.417    0.914          0         0
# 3    C           10  56.456    2.786    42.995    0.676          0         0
# 4    D           10  48.482    2.392    35.261    0.661          0         0
# 5    E           10  20.267    1.000    10.275    0.468          0         0

library(data.table)
options("h2o.use.data.table"=TRUE)

benchmark(
  A = for(i in seq_len(ncol(iris.hex))) { dat1 <- h2o.shuffle_column(dat = dat1, col_idx = i) },
  B = for(i in seq_len(ncol(iris.hex))) { dat2 <- h2o.shuffle_column2(dat = dat2, col_idx = i, modify_inline = FALSE) },
  C = for(i in seq_len(ncol(iris.hex))) { h2o.shuffle_column2(dat = dat3, col_idx = i, modify_inline = TRUE) },
  D = for(i in seq_len(ncol(iris.hex))) { dat4 <- h2o.shuffle_column3(dat = dat4, col_idx = i) },
  E = for(i in seq_len(ncol(iris.hex))) { dat5 <- h2o.shuffle_column4(dat = dat5, col_idx = i) },
  replications = 10
)


# test ranger alternatives
library(caret)
library(dplyr)

set.seed(777)
iris.extended<-data.frame(iris,apply(iris[,-5],2,sample))
names(iris.extended)[6:9]<-paste("Nonsense",1:4,sep="")


y = iris.extended$Species
x = iris.extended[,-which(colnames(iris.extended) == "Species")]
x$shadow.Boruta.decision<-y

# x100
# test replications elapsed relative user.self sys.self user.child sys.child
# 1    A          100   2.170    1.322     4.469    0.602          0         0
# 2    B          100   1.641    1.000     3.033    0.502          0         0
# 3    C          100   1.811    1.104     4.054    0.477          0         0

x <- as.data.frame(h2o::h2o.createFrame(cols = 30, missing_fraction = 0))
x$shadow.Boruta.decision <- rnorm(nrow(x))

# x10
# test replications elapsed relative user.self sys.self user.child sys.child
# 1    A           10 104.969    1.698   401.995    1.404      0.002     0.011
# 2    B           10  61.820    1.000   236.864    1.019      0.000     0.000
# 3    C           10  70.239    1.136   268.612    1.011      0.000     0.000

# holdoutRF only slightly more than ranger for permutation, but gives two
# test replications elapsed relative user.self sys.self user.child sys.child
# 1    A           10 114.356    1.899   408.144    1.419          0         0
# 2    B           10  60.225    1.000   232.318    1.117          0         0
# 3    C           10  69.566    1.155   264.356    1.017          0         0
# 4    D           10 133.189    2.212   511.136    1.536          0         0

benchmark(
  A = ranger::ranger(data=x,dependent.variable.name="shadow.Boruta.decision",  
                     num.trees=500,importance="permutation",
                     scale.permutation.importance=FALSE, # need oob.error to calculate permutation importance
                     write.forest=FALSE)$variable.importance,
  
  B = ranger::ranger(data=x,dependent.variable.name="shadow.Boruta.decision", 
                     num.trees=500,importance="impurity",
                     scale.permutation.importance=FALSE,
                     write.forest=FALSE,
                     oob.error = FALSE)$variable.importance,
  
  C = ranger::ranger(data=x,dependent.variable.name="shadow.Boruta.decision", 
                     num.trees=500,importance="impurity_corrected",
                     scale.permutation.importance=FALSE,
                      write.forest=FALSE,
                     oob.error = FALSE)$variable.importance,
  
  D = ranger::holdoutRF(data=x,dependent.variable.name="shadow.Boruta.decision", 
                    num.trees=500, # importance is always set to permutation
                    scale.permutation.importance=FALSE,
                    write.forest=FALSE),
replications = 10)

tmp <- ranger::holdoutRF(data=x,dependent.variable.name="shadow.Boruta.decision", 
                  num.trees=500, # importance is always set to permutation
                  scale.permutation.importance=FALSE,
                  write.forest=FALSE)


             

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
