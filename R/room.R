#' mergeScores
#'
#' @param result result of scoring script. A matrix. 
#' @param resp Response data.frame from TAO
#' @param column the column to merge with the results
#' @param prefix Text to put before the variable name
#'
#' @return returns a result matrix
#' @export
#'
#' @examples
#' result<-mergeScores(result,prescoredresults,"X2..Responses.RESPONSE","TM2_")
mergeScores<-function(result,resp,column,prefix="") {
  if(column %in% names(resp)) {
    respcolumn<-resp[,column]
    respcolumn[respcolumn==""]<-"[]"
    all<-apply(as.array(respcolumn),1,jsonlite::fromJSON)
    i<-1
    while(length(all[[i]])==0) i<-i+1
    numelm<-length(all[[i]][["score"]])
    names<-names(all[[i]][["score"]])
    scores<-as.data.frame(t(sapply(all,function(x) {y<-unlist(x$score);if(is.null(y)) rep(0,numelm) else y})))
    print(names)
    names(scores)<-paste0(prefix,names)
    testTaker<-data.frame(testTaker=resp[,1])
    scoresAndTaker<-cbind(testTaker,scores)
    #scoresAndTaker<-scoresAndTaker[apply(scores,1,function(x) {sum(is.na(x))!=length(x)}),]
    result<-merge(result,scoresAndTaker,by.x="testTaker",by.y="testTaker")
  }
  return (result) 
}

