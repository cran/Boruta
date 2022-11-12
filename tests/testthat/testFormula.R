context("Formula parsing")

test_that("Formula is parsed correctly in df",{
 test<-data.frame(two=2L,one=1L,three=3L)
 seven<-7L
 five<-5L
 expect_identical(make_df(one~two,test),list(X=test[,"two",drop=FALSE],Y=1L))
 expect_identical(make_df(one~.-three,test),list(X=test[,"two",drop=FALSE],Y=1L))
 expect_identical(make_df(one~.-three-two+two,test),list(X=test[,"two",drop=FALSE],Y=1L))
 expect_identical(make_df(one~two+.-three,test),list(X=test[,"two",drop=FALSE],Y=1L))
 expect_identical(make_df(seven~two+.-three-one,test),list(X=test[,"two",drop=FALSE],Y=7L))
 expect_identical(
  make_df(seven~.+two+.-three-one+five,test),
  list(X=cbind(test[,"two",drop=FALSE],five=5L),Y=7L))
 expect_identical(make_df(seven~five),list(X=data.frame(five=5L),Y=7L))
 expect_identical(make_df(one~I(2),test),list(X=data.frame(I.2.=I(2)),Y=1L))
 expect_error(make_df(one~.,data=1:10),"data must be a data.frame")
 expect_error(make_df(one~.-four,test),"Cannot omit four which is not in data")
 expect_error(make_df(one~.-I(3L),test),"Cannot omit something that is not a feature name")
 expect_error(make_df(one~two*three,test),"Invalid operator `*`")
 expect_error(make_df(one~.+two*three,test),"Invalid sub-expression")
 expect_error(make_df(seven~.),"Cannot use `.` without data")
})
