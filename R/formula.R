make_df<-function(formula,data,enc){
 if(missing(enc)) enc<-parent.frame()

 if(missing(data)){
  env<-enc
  have<-c()
 }else{
  if(!is.data.frame(data)) stop("data must be a data.frame")
  env<-data
  have<-names(data)
 }
 to_delete<-c()
 to_add<-c()
 new_features<-list()
 has_dot<-FALSE

 f<-stats::as.formula(formula)

 Ye<-f[[2]]
 Y<-eval(Ye,env,enc)
 Yn<-deparse(Ye)
 if(is.symbol(Ye)&&(Yn%in%have)) to_delete<-Yn

 f[[3]]->f
 while(TRUE){
  if(length(f)==3){
   oper<-deparse(f[[1]])
   element<-f[[3]]
   if(length(element)!=1)
    if(!identical(element[[1]],quote(I)))
     stop("Invalid sub-expression ",deparse(element))
  }else{
   #This is the last element
   oper<-'+'
   element<-f
  }

  if(oper=='-'){
   if(!is.symbol(element))
    stop(sprintf("Cannot omit something that is not a feature name (%s)",deparse(element)))
   element<-deparse(element)
   if(!(element%in%have))
    stop(sprintf("Cannot omit %s which is not in data",element))
   to_delete<-c(to_delete,element)
  }else if(oper=='+'){
   if(is.symbol(element)){
    deparse(element)->en
    if(en=='.'){
     if(missing(data)) stop("Cannot use `.` without data")
     has_dot<-TRUE
    }else{
     if(en%in%have){
      to_add<-c(to_add,en)
     }else{
      eval(element,env,enc)->val
      new_features<-c(new_features,stats::setNames(list(val),en))
     }
    }
   }else{
    eval(element,env,enc)->val
    new_features<-c(new_features,stats::setNames(list(val),deparse(element)))
   }
  }else
   stop(sprintf("Invalid operator `%s`; only `+` & `-` allowed",oper))
  if(length(f)<3) break
  f<-f[[2]]
 }

 if(has_dot){
  to_delete<-setdiff(to_delete,to_add)
  X<-data
  if(length(to_delete)>0)
   X<-X[,setdiff(names(X),to_delete),drop=FALSE]
  if(length(new_features)>0)
   X<-data.frame(X,new_features)
 }else{
  if(!missing(data)){
   X<-data[,setdiff(to_add,to_delete),drop=FALSE]
   if(length(new_features)>0)
    X<-data.frame(X,new_features)
  }else if(length(new_features)>0){
    X<-data.frame(new_features)
  } #else can't happen because of formula syntax properties; Y~ is invalid
 }
 list(X=X,Y=Y)
}
