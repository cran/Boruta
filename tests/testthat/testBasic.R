context("Basic tests")

test_that("Selection works on an extended iris",{
 set.seed(777)
 #Add some nonsense attributes to iris dataset by shuffling original attributes
 iris.extended<-data.frame(iris,apply(iris[,-5],2,sample))
 names(iris.extended)[6:9]<-paste("Nonsense",1:4,sep="")
 #Run Boruta on this data
 expect_message(Boruta(Species~.,data=iris.extended,doTrace=10)->Boruta.iris.extended)
 #Nonsense attributes should be rejected
 expect_equal(
  getSelectedAttributes(Boruta.iris.extended),
  names(iris)[-5]
 )

 attStats(Boruta.iris.extended)->a
 expect_equal(sort(rownames(a)),sort(names(iris.extended)[-5]))
 expect_equal(names(a),c("meanImp","medianImp","minImp","maxImp","normHits","decision"))

 rownames(a)[a$decision!="Rejected"]->conf_tent
 
 getSelectedAttributes(Boruta.iris.extended,withTentative=TRUE)->a
 expect_equal(sort(a),sort(conf_tent))

 expect_output(print(Boruta.iris.extended)," 4 attributes confirmed important:")

 tempfile()->canvf
 on.exit(unlink(canvf))
 pdf(canvf)
 on.exit(dev.off())
 expect_silent(plot(Boruta.iris.extended))
 expect_silent(plotImpHistory(Boruta.iris.extended))
})

test_that("No-Boruta input is rejected",{
 expect_error(getSelectedAttributes(iris))
 expect_error(attStats(iris))
 expect_error(TentativeRoughFix(iris))
 expect_error(plot.Boruta(iris))
 expect_error(print.Boruta(iris))
 expect_error(plotImpHistory(iris))
 expect_error(getConfirmedFormula(iris))
 expect_error(getNonRejectedFormula(iris))
})

#Mocked importance provider that always make Rel* features confirmed, Irr* ones rejected and others tentative
getImpMock<-function(x,y,...) 
 sapply(names(x),function(n) 
  if(grepl("^Rel",n)) 7 
  else if(grepl("^Irr",n)) -7 
  else if(grepl("^shadow",n)) 0 
  else rnorm(1)
 )
MockX<-data.frame(Rel1=1:5,Rel2=1:5,Rel3=1:5,Irr1=1:5,Irr2=1:5,Irr3=1:5,Irr4=1:5,Tent1=1:5,Tent2=1:5)
MockY<-1:5

test_that("Misc errors are caught",{
 set.seed(777)
 expect_silent(Boruta(MockX,MockY,getImp=getImpMock)->B)
 expect_error(plot.Boruta(B,colCode=c("green")))
 expect_error(plotImpHistory(B,colCode=c("green")))

 X<-iris[,1:4]
 names(X)[2]<-"shadow.mystery"
 expect_error(Boruta(X,iris$Species),"Attributes with names ")

 X<-iris[,1:4]
 X[20,3]<-NA
 expect_error(Boruta(X,iris$Species),"Cannot process NAs in input")

 expect_error(Boruta(iris[,-5],iris$Species,maxRuns=10),"maxRuns must be greater ")
})

test_that("Models without history are handled correctly",{
 set.seed(777)
 Boruta(MockX,MockY,holdHistory=FALSE,getImp=getImpMock)->B
 expect_null(B$ImpHistory)
 expect_output(print(B)," 3 attributes confirmed important")
 expect_error(plot.Boruta(B))
 expect_error(plotImpHistory(B))
 expect_error(attStats(B))
 expect_error(TentativeRoughFix(B))
})

test_that("TentativeRoughFix works",{
 set.seed(777)
 Boruta(MockX,MockY,getImp=getImpMock)->B
 expect_error(TentativeRoughFix(B,averageOver="a"))
 expect_error(TentativeRoughFix(B,averageOver=-11))
 expect_error(TentativeRoughFix(B,averageOver=1:10))
 TentativeRoughFix(B)->Bf
 expect_equal(getSelectedAttributes(Bf),getSelectedAttributes(Bf,withTentative=TRUE))
 expect_output(print(Bf),"Tentatives roughfixed")

 set.seed(777)
 Boruta(MockX[,1:7],MockY,getImp=getImpMock)->B
 expect_warning(TentativeRoughFix(B))
})

test_that("Edge cases are printed",{
 expect_message(Boruta(MockX[,1:7],MockY,getImp=getImpMock,doTrace=20),"no more attributes")
 Boruta(MockX[,1:3],MockY,getImp=getImpMock,doTrace=20)->Bc
 expect_output(print(Bc),"No attributes deemed unimportant")
 Boruta(MockX[,4:7],MockY,getImp=getImpMock,doTrace=20)->Br
 expect_output(print(Br),"No attributes deemed important")
})

test_that("Formulae are built",{
 set.seed(777)
 expect_silent(Boruta(MockX,MockY,getImp=getImpMock)->B)
 expect_error(getConfirmedFormula(B))
 expect_error(getNonRejectedFormula(B))

 MockX$Y<-MockY
 expect_silent(Boruta(Y~.,data=MockX,getImp=getImpMock)->B)
 expect_equal(deparse(getConfirmedFormula(B)),"Y ~ Rel1 + Rel2 + Rel3")
 expect_equal(deparse(getNonRejectedFormula(B)),"Y ~ Rel1 + Rel2 + Rel3 + Tent1 + Tent2")
})

test_that("Matrices are coerced",{
 expect_silent(Boruta(as.matrix(MockX),MockY,getImp=getImpMock)->B)
 expect_equal(getSelectedAttributes(B),c("Rel1","Rel2","Rel3"))
 expect_equal(getSelectedAttributes(B,TRUE),c("Rel1","Rel2","Rel3","Tent1","Tent2"))
})



