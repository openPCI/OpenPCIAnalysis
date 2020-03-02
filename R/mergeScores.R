#' Merge scores from pre-coded items
#'
#' @param result result of scoring script. A matrix. 
#' @param resp Response data.frame from TAO or from pre-coded items.
#' @param test.taker vector of test taker-ids for the response data.frame. If left empty, first column in resp is used as test.taker.
#' @param column String. The beginning of the name of the columns to merge with the results. If more columns start with the provided string, all of them are included.
#' @param prefix Text to put before the variable name
#'
#' @return returns a result matrix
#' @export
#'
#' @examples
#' # Make sure not to import data from item X20
#' result<-mergeScores(result,prescoredresults,"X2[^0-9]","TM2_")
mergeScores<-function(result,resp,column,prefix="",test.taker=NULL) {
  if(!is.null(test.taker)) resp<-cbind(data.frame(test.taker=test.taker),resp)
  cols<-grepl(paste0("^",column),names(resp),ignore.case = T)
  if(any(cols)) {
    for(onecolumn in which(cols)) {
      respcolumn<-resp[,onecolumn]
      if(inherits(respcolumn,"integer")) {
        scores<-as.data.frame(respcolumn)
        colnam<-sub(".*\\.(.*)","\\1",colnames(resp)[onecolumn])
      } else {
        respcolumn[respcolumn==""]<-"[]"
        all<-apply(as.array(respcolumn),1,function(x) {if(nchar(x)==65535) {warning("A json-cell was out of space (had more than 65535 characters). Not included");} else {jsonlite::fromJSON(x)}})
        i<-1
        while(length(all[[i]])==0) i<-i+1
        numelm<-length(all[[i]][["score"]])
        colnam<-names(all[[i]][["score"]])
        scores<-as.data.frame(t(sapply(all,function(x) {y<-unlist(x$score);if(is.null(y)) rep(0,numelm) else y})))
      }
      #print(colnam)
      colnames(scores)<-paste0(prefix,colnam)
      testTaker<-data.frame(testTaker=resp[,1])
      scoresAndTaker<-cbind(testTaker,scores)
      #scoresAndTaker<-scoresAndTaker[apply(scores,1,function(x) {sum(is.na(x))!=length(x)}),]
      result<-merge(result,scoresAndTaker,by.x=colnames(result)[1],by.y=colnames(scoresAndTaker)[1],all.x=T)
    }
  } else warning(paste("Merge Scores: No column starting with",column,"in response data.frame"))
  return (result) 
}

