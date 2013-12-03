# Plot functions for Boruta.
# Author: Miron B. Kursa
###############################################################################

##generateCol is internaly used by plot.Boruta and plotZHistory
generateCol<-function(x,colCode,col,numShadow){
 #Checking arguments
 if(is.null(col) & length(colCode)!=4)
  stop('colCode should have 4 elements.');
 #Generating col
 if(is.null(col)){
  rep(colCode[4],length(x$finalDecision)+numShadow)->cc;
  cc[c(x$finalDecision=='Confirmed',rep(FALSE,numShadow))]<-colCode[1];
  cc[c(x$finalDecision=='Tentative',rep(FALSE,numShadow))]<-colCode[2];
  cc[c(x$finalDecision=='Rejected',rep(FALSE,numShadow))]<-colCode[3];
  col=cc;
 }
 return(col);
}

##' @name plot.Boruta
##' @method plot Boruta
##' @title Plot Boruta object
##' @description default plot method for Boruta objects, showing boxplots of attribute importances over run.
##' @param x an object of a class Boruta.
##' @param colCode a vector containing colour codes for attribute decisions,
##' respectively Confirmed, Tentative, Rejected and shadow.
##' @param sort controls weather boxplots should be ordered, or left in original order.
##' @param whichShadow a logical vector controlling which shadows should be drawn;
##' switches respectively max shadow, mean shadow and min shadow.
##' @param col standard \code{col} attribute. If given, suppresses effects of \code{colCode}.
##' @param xlab X axis label that will be passed to \code{\link{boxplot}}.
##' @param ylab Y axis label that will be passed to \code{\link{boxplot}}.
##' @param ... additional graphical parameter that will be passed to \code{\link{boxplot}}.
##' @note If \code{col} is given and \code{sort} is \code{TRUE}, the \code{col} will be permuted, so that its order corresponds to
##' attribute order in \code{ImpHistory}.
##' @return Invisible copy of \code{x}.
##' @examples
##' \dontrun{
##' library(mlbench); data(HouseVotes84);
##' na.omit(HouseVotes84)->hvo;
##' #Takes some time, so be patient
##' Boruta(Class~.,data=hvo,doTrace=2)->Bor.hvo;
##' print(Bor.hvo);
##' plot(Bor.hvo);
##' }
##' @author Miron B. Kursa
##' @S3method plot Boruta
plot.Boruta<-function(x,colCode=c('green','yellow','red','blue'),sort=TRUE,whichShadow=c(TRUE,TRUE,TRUE),
  col=NULL,xlab='Attributes',ylab='Importance',...){
 #Checking arguments
 if(class(x)!='Boruta') stop('This function needs Boruta object as an argument.');

 #Removal of -Infs and conversion to a list
 lz<-lapply(1:ncol(x$ImpHistory),function(i) x$ImpHistory[is.finite(x$ImpHistory[,i]),i]);
 colnames(x$ImpHistory)->names(lz);

 #Selection of shadow meta-attributes
 numShadow<-sum(whichShadow);
 lz[c(rep(TRUE,length(x$finalDecision)),whichShadow)]->lz;

 #Generating color vector
 col<-generateCol(x,colCode,col,numShadow);

 #Ordering boxes due to attribute median ZScore
 if(sort){
  ii<-order(sapply(lz,median));
  lz[ii]->lz; col<-col[ii];
 }

 #Final plotting
 boxplot(lz,xlab=xlab,ylab=ylab,col=col,...);
 invisible(x);
}

##' @name plotImpHistory
##' @title Plot Boruta object as importance history
##' @description alternative plot method for Boruta objects, showing matplot of attribute importances over run.
##' @param x an object of a class Boruta.
##' @param colCode a vector containing colour codes for attribute decisions,
##' respectively Confirmed, Tentative, Rejected and shadow.
##' @param showRounds if set, to \code{TRUE}, gray lines separating round will be drawn.
##' @param col standard \code{col} attribute, passed to \code{\link{matplot}}.
##' If given, suppresses effects of \code{colCode}.
##' @param type Plot type that will be passed to \code{\link{matplot}}.
##' @param lty Line type that will be passed to \code{\link{matplot}}.
##' @param pch Point mark type that will be passed to \code{\link{matplot}}.
##' @param xlab X axis label that will be passed to \code{\link{matplot}}.
##' @param ylab Y axis label that will be passed to \code{\link{matplot}}.
##' @param ... additional graphical parameter that will be passed to \code{\link{matplot}}.
##' @return Invisible copy of \code{x}.
##' @examples
##' \dontrun{
##' library(mlbench); data(Sonar);
##' #Takes some time, so be patient
##' Boruta(Class~.,data=Sonar,doTrace=2)->Bor.son;
##' print(Bor.son);
##' plotImpHistory(Bor.son);
##' }
##' @author Miron B. Kursa
##' @export plotImpHistory
plotImpHistory<-function(x,colCode=c('green','yellow','red','blue'),showRounds=TRUE,col=NULL,type="l",lty=1,pch=0,
  xlab='Classifier run',ylab='Importance',...){
 #Checking arguments
 if(class(x)!='Boruta')
  stop('This function needs Boruta object as an argument.');
 col<-generateCol(x,colCode,col,3);

 #Final plotting
 matplot(0:(nrow(x$ImpHistory)-1),x$ImpHistory,xlab=xlab,ylab=ylab,col=col,type=type,lty=lty,pch=pch,...);
 if(showRounds) abline(v=(0:3)*x$roundRuns,col="gray");
 invisible(x);
}
