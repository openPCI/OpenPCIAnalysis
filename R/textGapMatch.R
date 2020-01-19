#' Make a text gap match object
#'
#' @param resp A response vector from TAO 
#'
#' @return Returns a text gap match object
#' @export
#'
#' @examples
#' mcresp<-c("['choice_1'; 'choice_5']","['choice_5'; 'choice_6']")
#' mc<-makeMC(mcresp)
makeTextGapMatch<-function(resp) {
  # MC is in a format that is simlar to JSON, but we need to convert it a little...
  resp<-gsub("'","\"",resp)
  resp<-gsub(";",",",resp)
  resp[resp==""]<-"[]"
  tgobj<-(apply(as.array(resp),1,jsonlite::fromJSON))
  class(tgobj)<-"textGapMatchObject"
}
#' Test for text in gap match
#'
#' @param textGapMatchObject a textGapMatchObject 
#' @param text The text to look for
#' @param gap The gap to look in
#' @param no.where.else Only accept it if it is only here
#'
#' @return Returns the scores
#' @export
#'
#' @examples To come
textInGap<-function(textGapMatchObject,text,gap,no.where.else=TRUE) {
  isIn=rep(0,length(textGapMatchObject))
  for(i in gap) {
    isIn<-(isIn + as.logical(lapply(textGapMatchObject,function(x) {text %in% x[[i]]})))
  }
  if(no.where.else) {
    gaps<-names(textGapMatchObject[[1]])
    for(i in gaps) {
      if(!(i %in% gap))
        isIn<-(isIn * (!as.logical(lapply(textGapMatchObject,function(x) {text %in% x[[i]]}))))
    }
  }
  return(isIn)
}
#' The position in gap match
#'
#' @param textGapMatchObject a textGapMatchObject 
#' @param text The text to look for
#' @param gap The gap to look in
#'
#' @return Returns the scores
#' @export
#'
#' @examples To come
posInGap<-function(textGapMatchObject,text,gap) {
  posIn<-as.numeric(lapply(textGapMatchObject,function(x) {pos<-which(x[[gap]]==text);if(length(pos)>0) pos else 0}))
  return(posIn)
}
