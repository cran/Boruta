context("Importance transdapter tests")

test_that("Imputation transdapter removes NAs",{
 mock<-data.frame(
  a=c(NA,NA,NA,NA),
  b=factor('a',NA,'b','b'),
  c=1:4,
  d=c(1,2,NA,4)
 )
 imputeTransdapter(function(x,y,z,...){
  expect_equal(z,17)
  expect_true(!any(is.na(x)))
  expect_true(!any(is.na(y)))
 })(mock[,1:3],mock$d,17)
})

test_that("Decoherence transdapter works",{
 set.seed(1)
 mock<-data.frame(
  a=1:6,
  b=6:1,
  y=factor(c('a','a','a','b','b','b'))
 )
 decohereTransdapter(function(x,y,z,...){
  expect_equal(z,17)
  expect_equal(sort(x$a[y=='a']),1:3)
  expect_equal(sort(x$b[y=='b']),1:3)
 })(mock[,1:2],mock$y,17)
})

test_that("Conditional transdapter works",{
 set.seed(1)
 mock<-data.frame(
  a=1:6,
  b=6:1,
  y=factor(c('a','a','a','b','b','b'))
 )
 s_a<-runif(2)
 s_b<-runif(2)
 was_a<-FALSE
 was_b<-FALSE
 conditionalTransdapter(function(x,y,z,...){
  if(all(y=='a')){
   was_a<<-TRUE
   expect_equal(sort(x$a),1:3)
   expect_equal(sort(x$b),4:6)
   s_a
  }else if(all(y=='b')){
   was_b<<-TRUE
   expect_equal(sort(x$b),1:3)
   expect_equal(sort(x$a),4:6)
   s_b
  }else stop("Wrong y")
 },groups=mock$y)(mock[,1:2],mock$y,17)->S
 expect_equal(S,(s_a+s_b)/2)
 expect_true(was_a)
 expect_true(was_b)
})
