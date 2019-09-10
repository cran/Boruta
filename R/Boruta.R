# Core of Boruta

#' @export
#' @rdname Boruta
Boruta <- function(x, ...)
  UseMethod("Boruta")

#' Feature selection with the Boruta algorithm
#'
#' Boruta is an all relevant feature selection wrapper algorithm, capable of working with any classification method that output variable importance measure (VIM); by default, Boruta uses Random Forest.
#' The method performs a top-down search for relevant features by comparing original attributes' importance with importance achievable at random, estimated using their permuted copies, and progressively eliminating irrelevant features to stabilise that test.
#' @rdname Boruta
#' @method Boruta default
#' @param x data frame of predictors.
#' @param y response vector; factor for classification, numeric vector for regression, \code{Surv} object for survival (supports depends on importance adapter capabilities).
#' @param getImp function used to obtain attribute importance.
#' The default is getImpRfZ, which runs random forest from the \code{ranger} package and gathers Z-scores of mean decrease accuracy measure.
#' It should return a numeric vector of a size identical to the number of columns of its first argument, containing importance measure of respective attributes.
#' Any order-preserving transformation of this measure will yield the same result.
#' It is assumed that more important attributes get higher importance. +-Inf are accepted, NaNs and NAs are treated as 0s, with a warning.
#' @param pValue confidence level. Default value should be used.
#' @param mcAdj if set to \code{TRUE}, a multiple comparisons adjustment using the Bonferroni method will be applied. Default value should be used; older (1.x and 2.x) versions of Boruta were effectively using \code{FALSE}.
#' @param maxRuns maximal number of importance source runs.
#' You may increase it to resolve attributes left Tentative.
#' @param holdHistory if set to \code{TRUE}, the full history of importance is stored and returned as the \code{ImpHistory} element of the result.
#' Can be used to decrease a memory footprint of Boruta in case this side data is not used, especially when the number of attributes is huge; yet it disables plotting of such made \code{Boruta} objects and the use of the \code{\link{TentativeRoughFix}} function.
#' @param doTrace verbosity level. 0 means no tracing, 1 means reporting decision about each attribute as soon as it is justified, 2 means the same as 1, plus reporting each importance source run, 3 means the same as 2, plus reporting of hits assigned to yet undecided attributes.
#' @param ... additional parameters passed to \code{getImp}.
#' @return An object of class \code{Boruta}, which is a list with the following components:
#' \item{finalDecision}{a factor of three value: \code{Confirmed}, \code{Rejected} or \code{Tentative}, containing final result of feature selection.}
#' \item{ImpHistory}{a data frame of importances of attributes gathered in each importance source run.
#' Beside predictors' importances, it contains maximal, mean and minimal importance of shadow attributes in each run.
#' Rejected attributes get \code{-Inf} importance.
#' Set to \code{NULL} if \code{holdHistory} was given \code{FALSE}.}
#' \item{timeTaken}{time taken by the computation.}
#' \item{impSource}{string describing the source of importance, equal to a comment attribute of the \code{getImp} argument.}
#' \item{call}{the original call of the \code{Boruta} function.}
#' @details Boruta iteratively compares importances of attributes with importances of shadow attributes, created by shuffling original ones.
#' Attributes that have significantly worst importance than shadow ones are being consecutively dropped.
#' On the other hand, attributes that are significantly better than shadows are admitted to be Confirmed.
#' Shadows are re-created in each iteration.
#' Algorithm stops when only Confirmed attributes are left, or when it reaches \code{maxRuns} importance source runs.
#' If the second scenario occurs, some attributes may be left without a decision.
#' They are claimed Tentative.
#' You may try to extend \code{maxRuns} or lower \code{pValue} to clarify them, but in some cases their importances do fluctuate too much for Boruta to converge.
#' Instead, you can use \code{\link{TentativeRoughFix}} function, which will perform other, weaker test to make a final decision, or simply treat them as undecided in further analysis.
#' @references Miron B. Kursa, Witold R. Rudnicki (2010). Feature Selection with the Boruta Package.
#' \emph{Journal of Statistical Software, 36(11)}, p. 1-13.
#' URL: \url{http://www.jstatsoft.org/v36/i11/}
#' @export
#' @examples
#' set.seed(777)
#' #Add some nonsense attributes to iris dataset by shuffling original attributes
#' iris.extended<-data.frame(iris,apply(iris[,-5],2,sample))
#' names(iris.extended)[6:9]<-paste("Nonsense",1:4,sep="")
#' #Run Boruta on this data
#' Boruta(Species~.,data=iris.extended,doTrace=2)->Boruta.iris.extended
#' #Nonsense attributes should be rejected
#' print(Boruta.iris.extended)
#'
#' #Boruta using rFerns' importance
#' Boruta(Species~.,data=iris.extended,getImp=getImpFerns)->Boruta.ferns.irisE
#' print(Boruta.ferns.irisE)
#'
#' \dontrun{
#' #Boruta on the HouseVotes84 data from mlbench
#' library(mlbench); data(HouseVotes84)
#' na.omit(HouseVotes84)->hvo
#' #Takes some time, so be patient
#' Boruta(Class~.,data=hvo,doTrace=2)->Bor.hvo
#' print(Bor.hvo)
#' plot(Bor.hvo)
#' plotImpHistory(Bor.hvo)
#' }
#' \dontrun{
#' #Boruta on the Ozone data from mlbench
#' library(mlbench); data(Ozone)
#' library(randomForest)
#' na.omit(Ozone)->ozo
#' Boruta(V4~.,data=ozo,doTrace=2)->Bor.ozo
#' cat('Random forest run on all attributes:\n')
#' print(randomForest(V4~.,data=ozo))
#' cat('Random forest run only on confirmed attributes:\n')
#' print(randomForest(ozo[,getSelectedAttributes(Bor.ozo)],ozo$V4))
#' }
#' \dontrun{
#' #Boruta on the Sonar data from mlbench
#' library(mlbench); data(Sonar)
#' #Takes some time, so be patient
#' Boruta(Class~.,data=Sonar,doTrace=2)->Bor.son
#' print(Bor.son)
#' #Shows important bands
#' plot(Bor.son,sort=FALSE)
#' }
Boruta.default <- 
  function(x,
           y,
           pValue = 0.01,
           mcAdj = TRUE,
           maxRuns = 100,
           doTrace = 0,
           holdHistory = TRUE,
           getImp = getImpRfZ,
           ...) {
    
    #Extract the call to store in output
    cl <- match.call()
    cl[[1]] <- as.name('Boruta')
    
    #Convert x into a data.frame
    if (!is.data.frame(x) & !inherits(x, "H2OFrame")) x <- data.frame(x)
    
    ##Some checks on x & y
    if (length(grep('^shadow', names(x))) > 0)
      stop('Attributes with names starting from "shadow" are reserved for internal use. Please rename them.')
    
    if(anyNA(x)) stop('Cannot process NAs in input. Please remove them.')
    if(anyNA(y)) stop('Cannot process NAs in input. Please remove them.')
  
  
    ##Main loop
    Boruta_internal_main_loop(x = x, 
                              y = y, 
                              pValue = pValue,
                              mcAdj = mcAdj,
                              maxRuns = maxRuns,
                              doTrace = doTrace,
                              holdHistory = holdHistory,
                              getImp = getImp, 
                              
                              cl = cl,
                              ...)
  }


Boruta_internal_main_loop <- function(x, 
                                      y,
                                      pValue,
                                      mcAdj,
                                      maxRuns,
                                      doTrace,
                                      holdHistory,
                                      getImp,
                                      
                                      decReg, 
                                      hitReg, 
                                      runs, 
                                      impHistory = NULL,
                                      cl = NULL,
                                      
                                      ...) {
  #Timer starts... now!
  timeStart <- Sys.time()
  
  ##Creating some useful constants
  nAtt <- ncol(x)
  nObjects <- nrow(x)
  attNames <- names(x)
  confLevels <- c("Tentative", "Confirmed", "Rejected")
  
  if(missing("decReg")) {
    # cat("decReg missing; creating new version.\n")
    decReg <- factor(rep("Tentative", nAtt), levels = confLevels)
  }
  if(missing("hitReg")) {
    # cat("hitReg missing; creating new version.\n")
    hitReg <- rep(0, nAtt)
    names(hitReg) <- attNames
  }
  if(missing("runs")) {
    # cat("runs missing; setting to 0.\n")
    runs <- 0
  }

  

  while (any(decReg == "Tentative") && (runs + 1 -> runs) <= maxRuns) {
    curImp <- addShadowsAndGetImp(decReg, runs, x, y, getImp, doTrace, ...)
    
    # If more than one set of importances (e.g., from cross-validation), increment runs
    runs <- runs + nrow(curImp$imp.mat) - 1
    
    hitReg <- assignHits(hitReg, curImp, doTrace)
    decReg <- doTests(decReg, hitReg, runs, mcAdj, pValue, doTrace, timeStart)
    
    #If needed, update impHistory with scores obtained in this iteration
    if (holdHistory) {
      imp <- cbind(
        curImp$imp.mat,
        shadowMax = apply(curImp$shaImp.mat, 1, max),
        shadowMean = apply(curImp$shaImp.mat, 1, mean),
        shadowMin = apply(curImp$shaImp.mat, 1, min))
      
      impHistory <- rbind(impHistory, imp)
    }
  }
  
  ##Building result
  names(decReg) <- attNames
  ans <- list(
    finalDecision = decReg,
    ImpHistory = impHistory,
    pValue = pValue,
    maxRuns = maxRuns,
    light = TRUE,
    mcAdj = mcAdj,
    timeTaken = Sys.time() - timeStart,
    roughfixed = FALSE,
    call = cl,
    impSource = comment(getImp),
    hits = hitReg
  )
  "Boruta" -> class(ans)
  
  return(ans)
}

#' Resume feature selection from Boruta object.
#' 
#' See \code{\link{Boruta}} for details of the feature selection algorithm. 
#' Run Boruta and then pass in that object to resume feature selection, when
#' features remain tentative.
#' @export
#' @rdname Boruta
#' @title ResumeBoruta
#' @param checkpoint Boruta object with which to resume training..
#' @param maxAdditionalRuns Maximum number of additional importance source runs.
#' @param numPriorRuns Number of runs previously attempted. Not needed if the checkpoint Boruta object contains importance history.
#' @examples
#' set.seed(777)
#' #Add some nonsense attributes to iris dataset by shuffling original attributes
#' iris.extended<-data.frame(iris,apply(iris[,-5],2,sample))
#' names(iris.extended)[6:9]<-paste("Nonsense",1:4,sep="")
#' #Run Boruta on this data
#' Boruta.iris.extended <- Boruta(y = iris.extended$Species, x = iris.extended[,-which(colnames(iris.extended) == "Species")], doTrace=2)
#' #Nonsense attributes should be rejected, except 1
#' print(Boruta.iris.extended)
#' Boruta.iris.extended2 <- ResumeBoruta(y = iris.extended$Species, x = iris.extended[,-which(colnames(iris.extended) == "Species")], checkpoint = Boruta.iris.extended, maxAdditionalRuns=1000, doTrace=2)
#' #Last nonsense attribute shoudl be confirmed important.
#' print(Boruta.iris.extended)
#' print(Boruta.iris.extended2)
ResumeBoruta <-  
  function(checkpoint,
           x,
           y,
           maxAdditionalRuns = 10,
           numPriorRuns = nrow(checkpoint$ImpHistory),
           pValue = 0.01,
           mcAdj = TRUE,
           doTrace = 0,
           holdHistory = TRUE,
           getImp = getImpRfZ,
           ...) {
    if (class(checkpoint) != 'Boruta')
      stop("This is NOT a Boruta object!")
    if (!("hits" %in% names(checkpoint)))
      stop("Sorry, Boruta resume ony works with Boruta objects that include hits.")
    if (checkpoint$roughfixed)
      warning("Boruta resume should only be run on Boruta objects prior to rough fix.")
    
   
    #Convert x into a data.frame
    if (!is.data.frame(x) & !inherits(x, "H2OFrame")) x <- data.frame(x)
    
    ##Some checks on x & y
    if (length(grep('^shadow', names(x))) > 0)
      stop('Attributes with names starting from "shadow" are reserved for internal use. Please rename them.')
    
    if(anyNA(x)) stop('Cannot process NAs in input. Please remove them.')
    if(anyNA(y)) stop('Cannot process NAs in input. Please remove them.')
    
    if (ncol(x) != length(checkpoint$finalDecision))
      stop('Cannot resume with data containing different columns.')
    if (!all(colnames(x) == names(checkpoint$finalDecision)))
      stop('Cannot resume with data containing different columns.')
    
    
    if (is.null(numPriorRuns)) {
      runs <- checkpoint$maxRuns
    } else {
      runs <- numPriorRuns
    }
    
    ##Main loop
    Boruta_internal_main_loop(x = x, 
                              y = y,
                              pValue = pValue,
                              mcAdj = mcAdj,
                              maxRuns = runs + maxAdditionalRuns,
                              doTrace = doTrace,
                              holdHistory = holdHistory,
                              getImp = getImp,
                              
                              decReg = checkpoint$finalDecision, 
                              hitReg = checkpoint$hits, 
                              runs = runs, 
                              impHistory = checkpoint$ImpHistory,
                              cl = NULL,
                              
                              ...)
    
  }

.attListPrettyPrint <- function(x, limit = 5) {
  x <- sort(x)
  if (length(x) < limit + 1)
    return(sprintf("%s;", paste(x, collapse = ", ")))
  sprintf("%s and %s more;",
          paste(utils::head(x, limit), collapse = ", "),
          length(x) - limit)
}

#' @rdname Boruta
#' @method Boruta formula
#' @param formula alternatively, formula describing model to be analysed.
#' @param data in which to interpret formula.
#' @export
Boruta.formula <- function(formula, data = .GlobalEnv, ...) {
  ##Grab and interpret the formula
  stats::terms.formula(formula, data = data) -> t
  x <- eval(attr(t, "variables"), data)
  apply(attr(t, "factors"), 1, sum) > 0 -> sel
  nam <- rownames(attr(t, "factors"))[sel]
  data.frame(x[sel]) -> df
  names(df) <- nam
  x[[attr(t, "response")]] -> dec
  
  ##Run Boruta
  ans <- Boruta.default(df, dec, ...)
  ans$call <- match.call()
  ans$call[[1]] <- as.name('Boruta')
  formula -> ans$call[["formula"]]
  return(ans)
}

#' Print Boruta object
#'
#' Print method for the Boruta objects.
#' @method print Boruta
#' @param x an object of a class Boruta.
#' @param ... additional arguments passed to \code{\link{print}}.
#' @return Invisible copy of \code{x}.
#' @export
print.Boruta <- function(x, ...) {
  if (class(x) != 'Boruta')
    stop("This is NOT a Boruta object!")
  cat(paste(
    'Boruta performed ',
    dim(x$ImpHistory)[1],
    ' iterations in ',
    format(x$timeTaken),
    '.\n',
    sep = ''
  ))
  if (x$roughfixed)
    cat(
      paste(
        'Tentatives roughfixed over the last ',
        x$averageOver,
        ' iterations.\n',
        sep = ''
      )
    )
  if (sum(x$finalDecision == 'Confirmed') == 0) {
    cat(' No attributes deemed important.\n')
  } else {
    writeLines(strwrap(
      paste(
        sum(x$finalDecision == 'Confirmed'),
        ' attributes confirmed important: ',
        .attListPrettyPrint(names(x$finalDecision[x$finalDecision == 'Confirmed']))
      ),
      indent = 1
    ))
  }
  if (sum(x$finalDecision == 'Rejected') == 0) {
    cat(' No attributes deemed unimportant.\n')
  } else {
    writeLines(strwrap(
      paste(
        sum(x$finalDecision == 'Rejected'),
        ' attributes confirmed unimportant: ',
        .attListPrettyPrint(names(x$finalDecision[x$finalDecision == 'Rejected']))
      ),
      indent = 1
    ))
  }
  if (sum(x$finalDecision == 'Tentative') != 0) {
    writeLines(strwrap(
      paste(
        sum(x$finalDecision == 'Tentative'),
        ' tentative attributes left: ',
        .attListPrettyPrint(names(x$finalDecision[x$finalDecision == 'Tentative']))
      ),
      indent = 1
    ))
  }
  invisible(x)
}


#' Shuffle H2O column.
#'
#' Helper function to shuffle a single H2o column.
#' @param dat An H2O frame or something that can be converted to one using as.h2o.
#' @param col_idx Column index for the column to be shuffled.
#' @return Shuffled H2O frame.
#' @export
h2o.shuffle_column <- function(dat, col_idx) {
  col_name <- colnames(dat)[col_idx]
  
  col <- dat[, col_name]
  col <- h2o.cbind(col, h2o.runif(col))
  col <- h2o.arrange(col, "rnd")
  dat[, col_name] <- col[, col_name]
  
  invisible(dat)
}

# h2o.shuffle_column <- function(dat, col_idx) {
# 
#   if(!requireNamespace("h2o", quietly = TRUE)) {
#     stop("Please install h2o package to use getImpH2O.")
#   }
# 
#   nCols <- ncol(dat)
#   nObs <- nrow(dat)
#   column_name <- colnames(dat)[col_idx]
# 
#   if(!h2o::is.h2o(dat)) {
#     dat <- as.h2o(dat, destination_frame = "shuffle_column_dat")
#   } else {
#     dat <- h2o.assign(dat, "shuffle_column_dat")
#   }
# 
#   h2o::h2o.no_progress()
#   tmp.hex <- h2o::h2o.assign(h2o::h2o.createFrame(rows = nObs, cols = 1, randomize = TRUE,
#                                   categorical_fraction = 0,
#                                   integer_fraction = 0,
#                                   binary_fraction = 0,
#                                   binary_ones_fraction = 0,
#                                   missing_fraction = 0),
#                              "shuffle_column_tmp")
#   h2o::h2o.show_progress()
# 
#   # colnames(tmp.hex) <- c("shuffle_column_tmp", "test_tmp") # failing for some reason
#   tmp.hex <- h2o.assign(h2o::h2o.cbind(dat[, col_idx], tmp.hex[, "C1"]), "shuffle_column_tmp")
#   tmp.hex <- h2o.assign(h2o::h2o.arrange(tmp.hex, "C1"), "shuffle_column_tmp")
# 
# 
#   if(col_idx == 1) {
#     res <- h2o.assign(h2o::h2o.cbind(tmp.hex[, column_name],
#                      dat[,(col_idx+1):nCols]), "shuffle_column_result")
# 
#   } else if(col_idx == nCols) {
#     res <- h2o.assign(h2o::h2o.cbind(dat[, 1:(col_idx - 1)],
#                                      tmp.hex[, column_name]), "shuffle_column_result")
#   } else {
#     res <- h2o.assign(h2o::h2o.cbind(dat[, 1:(col_idx - 1)],
#                                      tmp.hex[, column_name],
#                      dat[,(col_idx+1):nCols]), "shuffle_column_result")
#   }
# 
#   h2o::h2o.rm("shuffle_column_tmp")
#   h2o::h2o.rm("shuffle_column_dat")
#   # h2o.rm(sorted_tmp.hex)
# 
#   return(res)
# }
# 
# h2o.shuffle_column2 <- function(dat, col_idx, modify_inline = TRUE) {
#   
#   if(!requireNamespace("h2o", quietly = TRUE)) {
#     stop("Please install h2o package to use getImpH2O.")
#   }
#   
#   nCols <- ncol(dat)
#   nObs <- nrow(dat)
#   column_name <- colnames(dat)[col_idx]
#   
#   if(!h2o::is.h2o(dat)) {
#     dat <- as.h2o(dat, destination_frame = "shuffle_column_dat")
#   } else {
#     if(!modify_inline)
#       dat <- h2o.assign(dat, "shuffle_column_dat")
#   }
#   dat_key <- h2o::h2o.getId(dat)
#   
#   h2o::h2o.no_progress()
#   tmp.hex <- h2o::h2o.createFrame(rows = nObs, cols = 1, randomize = TRUE, 
#                                                   categorical_fraction = 0,
#                                                   integer_fraction = 0,
#                                                   binary_fraction = 0,
#                                                   binary_ones_fraction = 0,
#                                                   missing_fraction = 0)
#   on.exit(h2o::h2o.rm(tmp.hex), add = TRUE)
#   h2o::h2o.show_progress()
#   
#   # colnames(tmp.hex) <- c("shuffle_column_tmp", "test_tmp") # failing for some reason
#   tmp.hex <- h2o::h2o.cbind(dat[, col_idx], tmp.hex[, "C1"])
#   tmp.hex <- h2o::h2o.arrange(tmp.hex, "C1")
#   
#   
#   if(col_idx == 1) {
#     res <- h2o::h2o.cbind(tmp.hex[, column_name],
#                                      dat[,(col_idx+1):nCols])
#     
#   } else if(col_idx == nCols) {
#     res <- h2o::h2o.cbind(dat[, 1:(col_idx - 1)],
#                                      tmp.hex[, column_name])
#   } else {
#     res <- h2o::h2o.cbind(dat[, 1:(col_idx - 1)],
#                                      tmp.hex[, column_name],
#                                      dat[,(col_idx+1):nCols])
#   }
#   
#   if(modify_inline) {
#     res <- h2o.assign(res, dat_key)
#   } else {
#     res <- h2o.assign(res, "shuffle_column_result")
#   }
#   
#   # h2o::h2o.rm(tmp.hex)
#   # h2o::h2o.rm("shuffle_column_dat")
#   # h2o.rm(sorted_tmp.hex)
#   
#   invisible(res)
# }
# 
# h2o.shuffle_column3 <- function(dat, col_idx) {
#   col <- as.vector(dat[, col_idx])
#   col <- sample(col)
#   
#   h2o.no_progress()
#   dat[, col_idx] <- as.h2o(col)
#   h2o.show_progress()
#   invisible(dat)
# }



##Expands the information system with newly built random attributes and calculates importance
addShadowsAndGetImp <- function(decReg, runs, x, y, getImp, doTrace, ...) {
  nAtt <- ncol(x)
  attNames <- names(x)
  
  # xSha is going to be a data frame with shadow attributes; time to init it.
  # also make sure data frames are in H2O if necessary; make copies to avoid changing original
  if(inherits(x, "H2OFrame") | inherits(y, "H2OFrame")) {
    # print(h2o::h2o.ls())
    if(!requireNamespace("h2o", quietly = TRUE)) {
      stop("Please install h2o package to use getImpH2O.")
    }
    x <- h2o::h2o.assign(x, "Boruta_x")
    y <- h2o::h2o.assign(y, "Boruta_y")
    xSha <- h2o::h2o.assign(x[, decReg != "Rejected", drop = F], "Boruta_xSha")
    # print(h2o::h2o.ls())
  } else {
    xSha <- x[, decReg != "Rejected", drop = F]
  }
  
  while (dim(xSha)[2] < 5) { #There must be at least 5 random attributes.
    if(inherits(xSha, "H2OFrame")) {
      xSha <- h2o::h2o.cbind(xSha, xSha)
      # print(h2o::h2o.ls())
    } else {
      xSha <- cbind(xSha, xSha)
    }
  }
   
  
  #Now, we permute values in each attribute (shuffle the shadow features)
  nSha <- ncol(xSha)
  
  if(inherits(xSha, "H2OFrame")) {
    if (doTrace > 2) 
      message(sprintf('Shuffling H2O columns'))
    for(i in seq_len(nSha)) {
      xSha <- h2o.shuffle_column(xSha, i)
    }
    # confirm shuffle
    stopifnot(!all(as.vector(x[,1]) == as.vector(xSha[, 1])))
    # h2o::h2o.rm("shuffle_column_result")
    # print(h2o::h2o.ls())
  } else {
    xSha <- data.frame(lapply(xSha, sample)) 
  }
  namesSha <- paste('shadow', 1:nSha, sep = "")
  names(xSha) <- namesSha

  #Notifying user of our progress
  if (doTrace > 1)
    message(sprintf(' %s. run of importance source...', runs))
  
  if(inherits(xSha, "H2OFrame")) {
    combined_x <- h2o.cbind(x[, decReg != "Rejected"], xSha)
    # print(h2o::h2o.ls())
  } else {
    combined_x <- cbind(x[, decReg != "Rejected"], xSha)
  }
  
  
  
  #Calling importance source; "..." can be used by the user to pass rf attributes (for instance ntree)
  impRaw <- getImp(x = combined_x, y = y, ...)
  
  if(inherits(xSha, "H2OFrame")) {
    # print(h2o::h2o.ls())
    # h2o::h2o.rm("Boruta_combined_x")
    h2o::h2o.rm("Boruta_xSha")
    h2o::h2o.rm("Boruta_x")
    h2o::h2o.rm("Boruta_y")
    # print(h2o::h2o.ls())
  }
  
  if((is.vector(impRaw) & !is.numeric(impRaw)) & !is.matrix(impRaw)) {
    stop("getImp result is not a numeric vector or a matrix. Please check the given getImp function.")
  } 
  
  if(is.vector(impRaw)) {
    if(length(impRaw) != sum(decReg != "Rejected") + nSha)
      stop("getImp result has a wrong length. Please check the given getImp function.")
    
    impRaw <- t(as.matrix(impRaw))
    
  } else if(ncol(impRaw) != sum(decReg != "Rejected") + nSha) {
    stop("getImp result matrix has a wrong number of columns. Please check the given getImp function.")
  }

  if (any(is.na(impRaw) | is.nan(impRaw))) {
    impRaw[is.na(impRaw) | is.nan(impRaw)] <- 0
    warning("getImp result contains NA(s) or NaN(s); replacing with 0(s), yet this is suspicious.")
  }
  
  #Importance must have Rejected attributes put on place and filled with -Infs
  
  imp.mat <- matrix(data = -Inf, nrow = nrow(impRaw), ncol = nAtt + nSha)
  colnames(imp.mat) <- c(attNames, namesSha)
  
  imp.mat[, c(decReg != "Rejected", rep(TRUE, nSha))] <- impRaw
  shaImp.mat <- imp.mat[, (nAtt + 1):ncol(imp.mat), drop = F]
  imp.mat <- imp.mat[,1:nAtt, drop = F]
  
  return(list(imp.mat = imp.mat, shaImp.mat = shaImp.mat))
}

##Assigns hits
assignHits <- function(hitReg, curImp, doTrace) {

  hits.mat <- curImp$imp.mat > apply(curImp$shaImp.mat, 1, max)
  if (doTrace > 2) {
    uncMask <- decReg == "Tentative"
    intHits <- sum(hits.mat[,uncMask])
    if (intHits > 0)
      message(
        sprintf(
          "Assigned hit to %s attribute%s out of %s undecided.",
          sum(colSums(hits.mat) > 0),
          if (intHits == 1)
            ""
          else
            "s",
          sum(uncMask)
        )
      )
    else
      message("None of undecided attributes scored a hit.")
  }
  hitReg <- hitReg + colSums(hits.mat)
  return(hitReg)
}

##Checks whether number of hits is significant
doTests <-
  function(decReg,
           hitReg,
           runs,
           mcAdj,
           pValue,
           doTrace,
           timeStart) {
    pAdjMethod <- ifelse(mcAdj[1], 'bonferroni', 'none')
    #If attribute is significantly more frequent better than shadowMax, its claimed Confirmed
    toAccept <-
      stats::p.adjust(stats::pbinom(hitReg - 1, runs, 0.5, lower.tail = FALSE),
                      method = pAdjMethod) < pValue
    (decReg == "Tentative" & toAccept) -> toAccept
    
    #If attribute is significantly more frequent worse than shadowMax, its claimed Rejected (=irrelevant)
    toReject <-
      stats::p.adjust(stats::pbinom(hitReg, runs, 0.5, lower.tail = TRUE), method =
                        pAdjMethod) < pValue
    (decReg == "Tentative" & toReject) -> toReject
    
    #Update decReg
    decReg[toAccept] <- "Confirmed"
    "Rejected" -> decReg[toReject]
    
    #Report progress
    if (doTrace > 0) {
      names(hitReg) -> attNames
      nAcc <- sum(toAccept)
      nRej <- sum(toReject)
      nLeft <- sum(decReg == "Tentative")
      if (nAcc + nRej > 0)
        message(sprintf(
          "After %s iterations, +%s: ",
          runs,
          format(difftime(Sys.time(), timeStart), digits = 2)
        ))
      if (nAcc > 0)
        message(sprintf(
          " confirmed %s attribute%s: %s",
          nAcc,
          ifelse(nAcc == 1, '', 's'),
          .attListPrettyPrint(attNames[toAccept])
        ))
      if (nRej > 0)
        message(sprintf(
          " rejected %s attribute%s: %s",
          nRej,
          ifelse(nRej == 1, '', 's'),
          .attListPrettyPrint(attNames[toReject])
        ))
      if (nAcc + nRej > 0)
        if (nLeft > 0) {
          message(sprintf(
            " still have %s attribute%s left.\n",
            nLeft,
            ifelse(nLeft == 1, '', 's')
          ))
        } else{
          if (nAcc + nRej > 0)
            message(" no more attributes left.\n")
        }
    }
    return(decReg)
  }

