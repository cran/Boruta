context("Basic tests")

test_that("Selection works on an extended iris",{
 set.seed(777)
 #Add some nonsense attributes to iris dataset by shuffling original attributes
 iris.extended<-data.frame(iris,apply(iris[,-5],2,sample))
 names(iris.extended)[6:9]<-paste("Nonsense",1:4,sep="")
 #Run Boruta on this data
 Boruta(Species~.,data=iris.extended)->Boruta.iris.extended
 #Nonsense attributes should be rejected
 expect_equal(
  getSelectedAttributes(Boruta.iris.extended),
  names(iris)[-5]
 )
})
