### R code from vignette source 'transdapters.Rnw'

###################################################
### code chunk number 1: noop
###################################################
library(Boruta)

noopTransdapter<-function(adapter=getImpRfZ){
 adapter
}


###################################################
### code chunk number 2: noopuse
###################################################
set.seed(17)
Boruta(Species~.,iris,getImp=noopTransdapter())


###################################################
### code chunk number 3: srximp
###################################################
set.seed(17)
data(srx)

srx_na<-srx
# Randomly punch 25 holes in the SRX data
holes<-25
holes<-cbind(
 sample(nrow(srx),holes,replace=TRUE),
 sample(ncol(srx),holes,replace=TRUE)
)
srx_na[holes]<-NA
# Use impute transdapter to mitigate them with internal imputation
Boruta(Y~.,srx_na,getImp=imputeTransdapter(getImpRfZ))


###################################################
### code chunk number 4: srxdeco
###################################################
set.seed(17)
Boruta(Y~.,srx,getImp=decohereTransdapter())


