context("Importance source tests")

#Regular test

data(srx)
X<-srx[,-ncol(srx)]
Y<-srx$Y

impSources<-c(
 "getImpExtraGini","getImpExtraRaw","getImpExtraZ","getImpFerns",
 "getImpLegacyRfGini","getImpLegacyRfRaw","getImpLegacyRfZ",
 "getImpRfGini","getImpRfRaw","getImpRfZ","getImpXgboost")

for(e in impSources)
 test_that(sprintf("Importance source %s works",e),{
  set.seed(777)
  #Run Boruta on this data
  Boruta(X,Y,getImp=get(e))->ans
  
  if(e!="getImpXgboost"){
   #Nonsense attributes should be rejected
   expect_equal(
    sort(getSelectedAttributes(ans)),
    sort(c('A','B','AnB','AoB','nA'))
   )
  }else{
   #Xgboost is minimal optimal
   expect_equal(
    sort(getSelectedAttributes(ans)),
    sort(c('AnB','AoB'))
   )
  }
 })

test_that("Invalid source is caught",{
 expect_error(Boruta(Species~.,data=iris,getImp=function(...) 1:10),"getImp result has a wrong length")
 expect_error(Boruta(Species~.,data=iris,getImp=function(...) "x"),"getImp result is not a numeric vector")
 expect_warning(Boruta(Species~.,data=iris,getImp=function(x,...) c(0,rep(NA,ncol(x)-1))),"getImp result contains NA")
})


#Censored data test

X<-iris[,2:5]
X$N1<-runif(150)
X$N2<-runif(150)
Yt<-iris[,1]
Ys<-Yt<=6.9
Yt[Yt>6.9]<-6.9

#Mock Surv object
Y<-cbind(time=Yt,status=Ys)
class(Y)<-c("Surv")
attr(Y,"type")<-"right"

test_that("Ranger sources work on censored",{
 set.seed(777)
 expect_error(Boruta(X,Y,getImp=getImpRfGini),"Ranger cannot produce Gini")
 expect_equal(sort(getSelectedAttributes(Boruta(X,Y))),sort(names(iris)[-1]))
 expect_equal(sort(getSelectedAttributes(Boruta(X,Y,getImp=getImpRfRaw))),sort(names(iris)[-1]))
})

