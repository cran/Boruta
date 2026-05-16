# Importance sources

#' randomForest importance adapters
#'
#' Those function is intended to be given to a \code{getImp} argument of \code{\link{Boruta}} function to be called by the Boruta algorithm as an importance source.
#' \code{getImpLegacyRfZ} generates default, normalized permutation importance, \code{getImpLegacyRfRaw} raw permutation importance, finally \code{getImpLegacyRfGini} generates Gini index importance, all using \code{\link[randomForest]{randomForest}} as a Random Forest algorithm implementation.
#' @name getImpLegacyRf
#' @rdname getImpLegacyRf
#' @aliases getImpLegacyRfZ getImpLegacyRfGini getLegacyImpRfRaw
#' @note The \code{getImpLegacyRfZ} function was a default importance source in Boruta versions prior to 5.0; since then \code{\link[ranger]{ranger}} Random Forest implementation is used instead of \code{\link[randomForest]{randomForest}}, for speed, memory conservation and an ability to use multithreading.
#' Both importance sources should generally lead to the same results, yet there are differences.
#'
#' Most notably, ranger by default treats factor attributes as ordered (and works very slow if instructed otherwise with \code{respect.unordered.factors=TRUE}); on the other hand it lifts 32 levels limit specific to \code{\link[randomForest]{randomForest}}.
#' To this end, Boruta decision for factor attributes may be different.
#'
#' Random Forest methods has two main parameters, number of attributes tried at each split and the number of trees in the forest; first one is called \code{mtry} in both implementations, but the second \code{ntree} in \code{\link[randomForest]{randomForest}} and \code{num.trees} in \code{\link[ranger]{ranger}}.
#' To this end, to maintain compatibility, \code{getImpRf*} functions still accept \code{ntree} parameter relaying it into \code{num.trees}.
#' Still, both parameters take the same defaults in both implementations (square root of the number all all attributes and 500 respectively).
#'
#' Moreover, \code{\link[ranger]{ranger}} brings some addition capabilities to Boruta, like analysis of survival problems or sticky variables which are always considered on splits.
#'
#' Finally, the results for the same PRNG seed will be different.
#' @param x data frame of predictors including shadows.
#' @param y response vector.
#' @param ... parameters passed to the underlying \code{\link[randomForest]{randomForest}} call; they are relayed from \code{...} of \code{\link{Boruta}}.
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

#' Fru Random Forest importance adapters
#'
#' Those function is intended to be given to a \code{getImp} argument of \code{\link{Boruta}} function to be called by the Boruta algorithm as an importance source.
#' \code{getImpFruZ} generates default, normalized permutation importance while \code{getImpFruRaw} raw permutation importance.
#' Fru does not support Gini index importance.
#' @name getImpFru
#' @rdname getImpFru
#' @aliases getImpFruZ getImFruRaw
#' @param x data frame of predictors including shadows.
#' @param y response vector.
#' @param ntree  Number of trees in the forest; copied into \code{\link[fru]{fru}}'s native \code{trees}, put to retain transparent compatibility with randomForest.
#' @param trees  Number of trees in the forest, as according to \code{\link[fru]{fru}}'s nomenclature. If not given, set to \code{ntree} value. If both are given, \code{trees} takes precedence.
#' @param num.threads Number of computing threads to use; copied into \code{\link[fru]{fru}}'s native \code{threads}, put to retain transparent compatibility with ranger.
#' @param threads Number of computing threads to use, as according to \code{\link[fru]{fru}}'s nomenclature.
#' If not given, set to \code{num.threads}.
#' If both are given, \code{threads} takes precedence.
#' Default value of zero means all available threads.
#' @param ... parameters passed to the underlying \code{\link[fru]{fru}} call; they are relayed from \code{...} of \code{\link{Boruta}}.
#' @note In prior versions of Boruta, other implementations of Random Forest were used as a default; prior to 5.0.0, randomForest package was used, and the default adapter used back then is available as \code{\link{getImpLegacyRf}}.
#' Prior to 10.0, ranger package was used with \code{\link{getImpRfZ}}.
#' @export
getImpFruZ<-function(x,y,ntree=500,trees=ntree,num.threads=0,threads=num.threads,...){
 if(inherits(y,"Surv"))
  stop("Fru doesn't support censored data; try getImpRfZ as an importance source")
 fru::importance(fru::fru(x,y,trees=trees,importance=TRUE,threads=threads,...),scale=TRUE)
}
comment(getImpFruZ)<-'fru normalized permutation importance'

#' @rdname getImpFru
#' @export
getImpFruRaw<-function(x,y,ntree=500,trees=ntree,num.threads=0,threads=num.threads,...){
 if(inherits(y,"Surv"))
  stop("Fru doesn't support censored data; try getImpRfRaw as an importance source")
 fru::importance(fru::fru(x,y,trees=trees,importance=TRUE,threads=threads,...),scale=FALSE)
}
comment(getImpFruZ)<-'fru raw permutation importance'

#' Ranger Random Forest importance adapters
#'
#' Those function is intended to be given to a \code{getImp} argument of \code{\link{Boruta}} function to be called by the Boruta algorithm as an importance source.
#' \code{getImpRfZ} generates default, normalized permutation importance, \code{getImpRfRaw} raw permutation importance, finally \code{getImpRfGini} generates Gini index importance.
#' @name getImpRf
#' @rdname getImpRf
#' @aliases getImpRfZ getImpRfGini getImpRfRaw
#' @param x data frame of predictors including shadows.
#' @param y response vector.
#' @param ntree  Number of trees in the forest; copied into \code{\link[ranger]{ranger}}'s native num.trees, put to retain transparent compatibility with randomForest.
#' @param num.trees  Number of trees in the forest, as according to \code{\link[ranger]{ranger}}'s nomenclature. If not given, set to \code{ntree} value. If both are given, \code{num.trees} takes precedence.
#' @param ... parameters passed to the underlying \code{\link[ranger]{ranger}} call; they are relayed from \code{...} of \code{\link{Boruta}}.
#' @note \code{getImpRfZ} was the default importance source between versions 5.0.0 and 10.0.0. Prior to Boruta 5.0, \code{getImpLegacyRfZ} function was a default; see \link{getImpLegacyRf} for more details.
#' @export
getImpRfZ<-function(x,y,ntree=500,num.trees=ntree,...){
 if(inherits(y,"Surv")){
  return(ranger::ranger(x=x,y=y,
   num.trees=num.trees,importance="permutation",
   scale.permutation.importance=TRUE,
   write.forest=FALSE,...)$variable.importance)
 }
 ranger::ranger(x=x,y=y,
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
 ranger::ranger(x=x,y=y,
  num.trees=num.trees,importance="impurity",
  scale.permutation.importance=FALSE,
  write.forest=FALSE,...)$variable.importance
}
comment(getImpRfGini)<-'ranger Gini index importance'

#' @rdname getImpRf
#' @export
getImpRfRaw<-function(x,y,ntree=500,num.trees=ntree,...){
 if(inherits(y,"Surv")){
  return(ranger::ranger(x=x,y=y,
   num.trees=num.trees,importance="permutation",
   write.forest=FALSE,...)$variable.importance)
 }
 ranger::ranger(x=x,y=y,
  num.trees=num.trees,importance="permutation",
  scale.permutation.importance=FALSE,
  write.forest=FALSE,...)$variable.importance
}
comment(getImpRfRaw)<-'ranger raw permutation importance'

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

