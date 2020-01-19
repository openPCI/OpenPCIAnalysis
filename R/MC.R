#' Make a multiple choise object
#'
#' @param resp A response vector from TAO 
#'
#' @return Returns a multiple choice object
#' @export
#'
#' @examples
#' mcresp<-c("['choice_1'; 'choice_5']","['choice_5'; 'choice_6']")
#' mc<-makeMC(mcresp)
makeMC<-function(resp) {
  # MC is in a format that is simlar to JSON, but we need to convert it a little...
  resp<-gsub("'","\"",resp)
  resp<-gsub(";",",",resp)
  resp[resp==""]<-"[]"
  mc<-(apply(as.array(resp),1,jsonlite::fromJSON))
  class(mc)<-"MCObject"
  mc
}
#' Make a single choise object
#'
#' @param resp A response vector from TAO 
#'
#' @return Returns a multiple choice object
#' @export
#'
#' @examples
#' scresp<-c("choice_1","choice_5")
#' sc<-makeSC(scresp)
makeSC<-function(resp) {
  # Do nothing...
  class(resp)<-"SCObject"
}

#' Score single or multiple choice objects
#'
#' @param mc a multiple or single choice object
#' @param correct the correct response(s)
#'
#' @return returns a vector of scores
#' @export
#'
#' @examples
#' 
#' mcresp<-c("['choice_1'; 'choice_5']","['choice_5'; 'choice_6']")
#' mc<-makeMC(mcresp)
#' scoreMC(mc,correct=c("choice_1","choice_5"))
scoreMC<-function(mc,correct=c()) {
  return (as.numeric(lapply(mc,function(x) length(intersect(x,correct)))))
}
#' @rdname scoreMC
scoreSC<-scoreMC