# Importance sources

#' randomForest importance adapters
#'
#' Those function is intended to be given to a \code{getImp} argument of \code{\link{Boruta}} function to be called by the Boruta algorithm as an importance source.
#' \code{getImpLegacyRfZ} generates default, normalized permutation importance, \code{getImpLegacyRfRaw} raw permutation importance, finally \code{getImpLegacyRfGini} generates Gini index importance, all using \code{\link[randomForest]{randomForest}} as a Random Forest algorithm implementation.
#' @name getImpLegacyRf
#' @rdname getImpLegacyRf
#' @aliases getImpLegacyRfZ getImpLegacyRfGini getLegacyImpRfRaw
#' @note The \code{getImpLegacyRfZ} function was a default importance source in Boruta versions prior to 5.0; since then \code{\link{ranger}} Random Forest implementation is used instead of \code{\link[randomForest]{randomForest}}, for speed, memory conservation and an ability to utilise multithreading.
#' Both importance sources should generally lead to the same results, yet there are differences.
#'
#' Most notably, ranger by default treats factor attributes as ordered (and works very slow if instructed otherwise with \code{respect.unordered.factors=TRUE}); on the other hand it lifts 32 levels limit specific to \code{\link[randomForest]{randomForest}}.
#' To this end, Boruta decision for factor attributes may be different.
#'
#' Random Forest methods has two main parameters, number of attributes tried at each split and the number of trees in the forest; first one is called \code{mtry} in both implementations, but the second \code{ntree} in \code{\link[randomForest]{randomForest}} and \code{num.trees} in \code{\link{ranger}}.
#' To this end, to maintain compatibility, \code{getImpRf*} functions still accept \code{ntree} parameter relaying it into \code{num.trees}.
#' Still, both parameters take the same defaults in both implementations (square root of the number all all attributes and 500 respectively).
#'
#' Moreover, \code{\link{ranger}} brings some addition capabilities to Boruta, like analysis of survival problems or sticky variables which are always considered on splits.
#'
#' Finally, the results for the same PRNG seed will be different.
#' @param x data frame of predictors including shadows.
#' @param y response vector.
#' @param ... parameters passed to the underlying \code{\link[randomForest]{randomForest}} call; they are relayed from \code{...} of \code{\link{Boruta}}.
#' @examples
#' set.seed(777)
#' #Add some nonsense attributes to iris dataset by shuffling original attributes
#' iris.extended<-data.frame(iris,apply(iris[,-5],2,sample))
#' names(iris.extended)[6:9]<-paste("Nonsense",1:4,sep="")
#' #Run Boruta on this data
#' Boruta(Species~.,getImp=getImpLegacyRfZ,
#'  data=iris.extended,doTrace=2)->Boruta.iris.extended
#' #Nonsense attributes should be rejected
#' print(Boruta.iris.extended)
#' @export
getImpLegacyRfZ<-function(x,y,...){
  randomForest::randomForest(x,y,
                             importance=TRUE,keep.forest=FALSE,...)->rf
  randomForest::importance(rf,1,scale=TRUE)[,1]
}
comment(getImpLegacyRfZ)<-'randomForest normalized permutation importance'

#' @rdname getImpLegacyRf
#' @export
getImpLegacyRfRaw<-function(x,y,...){
  randomForest::randomForest(x,y,
                             importance=TRUE,keep.forest=FALSE,...)->rf
  randomForest::importance(rf,1,scale=FALSE)[,1]
}
comment(getImpLegacyRfRaw)<-'randomForest raw permutation importance'

#' @rdname getImpLegacyRf
#' @export
getImpLegacyRfGini<-function(x,y,...){
  randomForest::randomForest(x,y,
                             keep.forest=FALSE,...)->rf
  randomForest::importance(rf,2,scale=FALSE)[,1]
}
comment(getImpLegacyRfGini)<-'randomForest Gini index importance'


#' ranger Random Forest importance adapters
#'
#' Those function is intended to be given to a \code{getImp} argument of \code{\link{Boruta}} function to be called by the Boruta algorithm as an importance source.
#' \code{getImpRfZ} generates default, normalized permutation importance, \code{getImpRfRaw} raw permutation importance, finally \code{getImpRfGini} generates Gini index importance.
#' @name getImpRf
#' @rdname getImpRf
#' @aliases getImpRfZ getImpRfGini getImpRfRaw
#' @param x data frame of predictors including shadows.
#' @param y response vector.
#' @param ntree  Number of trees in the forest; copied into \code{\link{ranger}}'s native num.trees, put to retain transparent compatibility with randomForest.
#' @param num.trees  Number of trees in the forest, as according to \code{\link{ranger}}'s nomenclature. If not given, set to \code{ntree} value. If both are given, \code{num.trees} takes precedence.
#' @param ... parameters passed to the underlying \code{\link{ranger}} call; they are relayed from \code{...} of \code{\link{Boruta}}.
#' @note Prior to Boruta 5.0, \code{getImpLegacyRfZ} function was a default importance source in Boruta; see \link{getImpLegacyRf} for more details.
#' @import ranger
#' @export
getImpRfZ<-function(x,y,ntree=500,num.trees=ntree,...){
  if(inherits(y,"Surv")){
    x$shadow.Boruta.time<-y[,"time"]
    x$shadow.Boruta.status<-y[,"status"]
    return(ranger::ranger(data=x,
                          dependent.variable.name="shadow.Boruta.time",
                          status.variable.name="shadow.Boruta.status",
                          num.trees=num.trees,importance="permutation",
                          scale.permutation.importance=TRUE,
                          write.forest=FALSE,...)$variable.importance)
  }
  #Abusing the fact that Boruta disallows attributes with names
  # starting from "shadow"
  x$shadow.Boruta.decision<-y
  ranger::ranger(data=x,dependent.variable.name="shadow.Boruta.decision",
                 num.trees=num.trees,importance="permutation",
                 scale.permutation.importance=TRUE,
                 write.forest=FALSE,...)$variable.importance
}
comment(getImpRfZ)<-'ranger normalized permutation importance'

#' @rdname getImpRf
#' @export
getImpRfGini<-function(x,y,ntree=500,num.trees=ntree,...){
  if(inherits(y,"Surv"))
    stop("Ranger cannot produce Gini importance for survival problems.")
  x$shadow.Boruta.decision<-y
  ranger::ranger(data=x,dependent.variable.name="shadow.Boruta.decision",
                 num.trees=num.trees,importance="impurity",
                 scale.permutation.importance=FALSE,
                 write.forest=FALSE,...)$variable.importance
}
comment(getImpRfGini)<-'ranger Gini index importance'

#' @rdname getImpRf
#' @export
getImpRfRaw<-function(x,y,ntree=500,num.trees=ntree,...){
  if(inherits(y,"Surv")){
    x$shadow.Boruta.time<-y[,"time"]
    x$shadow.Boruta.status<-y[,"status"]
    return(ranger::ranger(data=x,
                          dependent.variable.name="shadow.Boruta.time",
                          status.variable.name="shadow.Boruta.status",
                          num.trees=num.trees,importance="permutation",
                          write.forest=FALSE,...)$variable.importance)
  }
  x$shadow.Boruta.decision<-y
  ranger::ranger(data=x,dependent.variable.name="shadow.Boruta.decision",
                 num.trees=num.trees,importance="permutation",
                 scale.permutation.importance=FALSE,
                 write.forest=FALSE,...)$variable.importance
}
comment(getImpRfRaw)<-'ranger raw permutation importance'

#' ranger Extra-trees importance adapters
#'
#' Those function is intended to be given to a \code{getImp} argument of \code{\link{Boruta}} function to be called by the Boruta algorithm as an importance source.
#' \code{getImpExtraZ} generates default, normalized permutation importance, \code{getImpExtraRaw} raw permutation importance, finally \code{getImpExtraGini} generates Gini impurity importance.
#' @name getImpExtra
#' @rdname getImpExtra
#' @aliases getImpExtraZ getImpExtraGini getImpExtraRaw
#' @param x data frame of predictors including shadows.
#' @param y response vector.
#' @param ntree  Number of trees in the forest; copied into \code{\link{ranger}}'s native num.trees, put to retain transparent compatibility with randomForest.
#' @param num.trees  Number of trees in the forest, as according to \code{\link{ranger}}'s nomenclature. If not given, set to \code{ntree} value. If both are given, \code{num.trees} takes precedence.
#' @param ... parameters passed to the underlying \code{\link{ranger}} call; they are relayed from \code{...} of \code{\link{Boruta}}. Note that these function work just by setting \code{splitrule} to \code{"extratrees"}.
#' @export
getImpExtraZ<-function(x,y,ntree=500,num.trees=ntree,...)
  getImpRfZ(x,y,ntree=ntree,splitrule="extratrees",...)
comment(getImpExtraZ)<-'ranger normalized permutation importance'

#' @rdname getImpExtra
#' @export
getImpExtraGini<-function(x,y,ntree=500,num.trees=ntree,...)
  getImpRfGini(x,y,ntree=ntree,splitrule="extratrees",...)
comment(getImpExtraGini)<-'ranger extra-trees Gini index importance'

#' @rdname getImpExtra
#' @export
getImpExtraRaw<-function(x,y,ntree=500,num.trees=ntree,...)
  getImpRfRaw(x,y,ntree=ntree,splitrule="extratrees",...)
comment(getImpExtraRaw)<-'ranger extra-trees raw permutation importance'


#' Random Ferns importance
#'
#' This function is intended to be given to a \code{getImp} argument of \code{\link{Boruta}} function to be called by the Boruta algorithm as an importance source.
#' @param x data frame of predictors including shadows.
#' @param y response vector.
#' @param ... parameters passed to the underlying \code{\link[rFerns]{rFerns}} call; they are relayed from \code{...} of \code{\link{Boruta}}.
#' @export
#' @note Random Ferns importance calculation should be much faster than using Random Forest; however, one must first optimize the value of the \code{depth} parameter and
#' it is quite likely that the number of ferns in the ensemble required for the importance to converge will be higher than the number of trees in case of Random Forest.
getImpFerns<-function(x,y,...){
  f<-rFerns::rFerns(x,y,
                    saveForest=FALSE,importance=TRUE,...)
  f$importance[,1]
}
comment(getImpFerns)<-'rFerns importance'

#' Xgboost importance
#'
#' This function is intended to be given to a \code{getImp} argument of \code{\link{Boruta}} function to be called by the Boruta algorithm as an importance source.
#' @param x data frame of predictors including shadows.
#' @param y response vector.
#' @param nrounds Number of rounds; passed to the underlying \code{\link[xgboost]{xgboost}} call.
#' @param verbose Verbosity level of xgboost; either 0 (silent) or 1 (progress reports). Passed to the underlying \code{\link[xgboost]{xgboost}} call.
#' @param ... other parameters passed to the underlying \code{\link[xgboost]{xgboost}} call.
#' Similarly as \code{nrounds} and \code{verbose}, they are relayed from \code{...} of \code{\link{Boruta}}.
#' For convenience, this function sets \code{nrounds} to 5 and verbose to 0, but this can be overridden.
#' @note Only dense matrix interface is supported; all predictions given to \code{\link{Boruta}} call have to be numeric (not integer).
#' Categorical features should be split into indicator attributes.
#' This functionality is inspired by the Python package BoostARoota by Chase DeHan.
#' I have some doubts whether boosting importance can be used for all relevant selection without hitting substantial false negative rates; please consider this functionality experimental.
#' @references \url{https://github.com/chasedehan/BoostARoota}
#' @export

getImpXgboost<-function(x,y,nrounds=5,verbose=0,...){
  xgboost::xgb.importance(
    model=xgboost::xgboost(
      data=as.matrix(x),
      label=y,
      nrounds=nrounds,
      verbose=verbose
    )
  )->imp
  rep(0,ncol(x))->ans
  ans[as.numeric(imp$Feature)+1]<-imp$Gain
  ans
}
comment(getImpXgboost)<-'xgboost gain importance'

#' H2O importance
#'
#' This function is intended to be given to a \code{getImp} argument of \code{\link{Boruta}} function to be called by the Boruta algorithm as an importance source.
#' 
#' @param x data frame of predictors including shadows. Will be either H2O Frame or a vector.
#' @param y response vector. Will be either H2O Frame or a vector.
#' @param h2o_model_fn H2O model to use for importance. Note that some models, such as h2o.naiveBayes, cannot be used because 
#' h2o.varimp() does not work for those models.
#' @param importance_type Whether to use relative_importance, scaled_importance, or percentage from the \code{h2o.varimp} result.
#' @param model_id If provided, will pass this model_id to the underlying h2o call, so the model can be retrieved later. Otherwise, the h2o model is removed.
#' @param ... other parameters passed to the underlying h2o call.
#' @export
getImpH2O <- function(x, y, 
                      h2o_model_fn = h2o::h2o.randomForest, 
                      importance_type = c("relative_importance",
                                          "scaled_importance",
                                          "percentage"),
                      model_id = NULL,
                      ...) {
  importance_type <- match.arg(importance_type)
  
  if(!requireNamespace("h2o", quietly = TRUE)) {
    stop("Please install h2o package to use getImpH2O.")
  }
  
  args <- list(...)
  
  if("nfolds" %in% names(args)) message(sprintf("Using nfolds = %d.", args$nfolds))
  
  # if(!h2o::is.h2o(x)) x <- h2o::as.h2o(x, destination_frame = "getImpH2O_x")
  # if(!h2o::is.h2o(y)) y <- h2o::as.h2o(y, destination_frame = "getImpH2O_y")
  
  args$training_frame <- h2o::h2o.cbind(x, y)
  # args$training_frame <- h2o.assign(h2o::h2o.cbind(x, y), "getImpH2O_training_frame")
  # on.exit(h2o::h2o.rm("getImpH2O_training_frame"), add = TRUE)
  args$x <- colnames(x)
  args$y <- colnames(y)
  args$model_id <- model_id
  
  h2o_model.hex <- do.call(h2o_model_fn, args)
  
  imp <- h2o::h2o.varimp(h2o_model.hex)
  out <- imp[[importance_type]]
  names(out) <- imp$variable
  
  stopifnot(names(out) %in% colnames(x))
  out <- out[colnames(x)]
  
  if("nfolds" %in% names(args)) {
    nfolds <- args$nfolds
    if(nfolds > 0) {
      out <- sapply(1:nfolds, function(i, model.hex) {
        model_cv.hex <- h2o::h2o.getModel(model.hex@model$cross_validation_models[[i]]$name)
        imp <- h2o::h2o.varimp(model_cv.hex)
        out <- imp[[importance_type]]
        names(out) <- imp$variable
        out
      }, model.hex = h2o_model.hex)
      
      out <- t(out)
      
      stopifnot(colnames(out) %in% colnames(x))
      out <- out[, colnames(x), drop = FALSE]
    }
  } 
  
  
  
  if(is.null(model_id)) { 
    # drop model and cv models, if applicable
    cv_models <- h2o.cross_validation_models(h2o_model.hex)
    if(!is.null(cv_models)) {
      h2o.rm(cv_models)
    }
    
    h2o::h2o.rm(h2o_model.hex)
  }
  
  # if("getImpH2O_x" %in% h2o::h2o.ls()$key) h2o::h2o.rm("getImpH2O_x")
  # if("getImpH2O_y" %in% h2o::h2o.ls()$key) h2o::h2o.rm("getImpH2O_y")
  
  return(out)
}

comment(getImpH2O)<-'h2o importance'


#' CV importance
#'
#' This function is intended to be given to a \code{getImp} argument of \code{\link{Boruta}} function to be called by the Boruta algorithm as an importance source.
#' The getImp is called repeatedly on cross-validation subsets of the data.
#' 
#' @param x data frame of predictors including shadows. Will be either H2O Frame or a vector.
#' @param y response vector. Will be either H2O Frame or a vector.
#' @param nfolds Number of folds (random data subsets) to use.
#' @param getUnderlyingImp Underlying importance function to call for each cross-validation subset.
#' @param ... other parameters passed to the underlying h2o getImp call.
#' @export
getImpCV <- function(x,y, nfolds = 5, getUnderlyingImp = getImpRfZ, ...) {
  folds <- sample.int(nfolds, size = nrow(x), replace = TRUE)
  
  
  
  out <- sapply(1:nfolds, function(i, getUnderlyingImp, x, y, folds, ...) {
    # if(inherits(x, "H2OFrame")) 
    #   x <- h2o::h2o.assign(x, "getImpCV_x")
    # if(inherits(x, "H2OFrame")) 
    #   y <- h2o::h2o.assign(y, "getImpCV_y")
    
    cv_idx <- which(folds == i)
    if(is.null(dim(y))) {
      y_cv <- y[cv_idx]
    } else {
      y_cv <- y[cv_idx,]
    }
    getUnderlyingImp(x[cv_idx, ], y = y_cv, ...)
  }, getUnderlyingImp = getUnderlyingImp, x = x, y = y, folds = folds, ...)
  
  return(t(out))
}
comment(getImpCV)<-'cross-validation importance'
