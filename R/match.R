
#' Make a match object
#'
#' @param resp a vector of match responses from TAO
#'
#' @return Returns a matchObject
#' @export
#'
#' @examples
#' matchresponse<-c("[mail1 annoyed; mail4 annoyed; mail2 angry]","[mail1 confused]")
#' matchObject<-makeMatch(matchresponse)
makeMatch<-function(resp) {
  # Match is in a format that is simlar to JSON, but we need to convert it a little...
  resp<-gsub("; ",",",resp)
  resp<-gsub(" ",":",resp)
  resp<-gsub("\\[","{",resp)
  resp<-gsub("\\]","}",resp)
 resp<-gsub("([[:alnum:]_]+)","\"\\1\"",resp)
  resp[resp==""]<-"{}"
  
  matchObject<-apply(as.array(resp),1,jsonlite::fromJSON)
  class(matchObject)<-"matchObject"
  matchObject
}

#' Score match responses
#'
#' @param matchObject A matchObject 
#' @param identifier The variable that has been given (a) corresponding value(s)
#' @param correct The correct values
#'
#' @return A vector of scores
#' @export
#'
#' @examples
#' matchresponse<-c("[mail1 annoyed; mail4 annoyed; mail2 angry]","[mail1 confused]")
#' matchObject<-makeMatch(matchresponse)
#' scoreMatch(matchObject,"mail1",c("happy","annoyed"))
scoreMatch<-function(resp,identifier,correct=c()) {
  return (as.logical(lapply(resp,function(x) {length(intersect(x[names(x)==identifier],correct))})))
}
