library(Boruta)

data(iris)
set.seed(17)

Boruta(Species~.,data=iris,threads=2)->A

stopifnot(identical(getSelectedAttributes(A),names(iris)[-5]))
