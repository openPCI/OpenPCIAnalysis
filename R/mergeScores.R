#' Merge scores from pre-coded items
#'
#' @param result result of scoring script. A matrix. 
#' @param resp Response data.frame from TAO or from pre-coded items.
#' @param test.taker vector of test taker-ids for the response data.frame. If left empty, first column in resp is used as test.taker.
#' @param column String. The beginning of the name of the columns to merge with the results. If more columns start with the provided string, all of them are included.
#' @param prefix Text to put before the variable name
#'
#' @details 
#' The data from `resp` are merged into `result` based on the `test.taker` column (either 1st column in `resp` or the `test.taker` vector). Both numeric scores and scores which are part of a JSON object are processed.
#' 
#' MergeScores returns a data.frame with the same number of rows as `result`. 
#' 
#' If there are duplets in the test-takers in `resp`, we first check if the order of test.takers in `result` and `resp` are exactly the same. If so, we `cbind` the scores (one by one).
#' 
#' If not, we unify duplicate `test.takers` by giving them the highest score they have achieved, and then merge with `result` - giving duplicate `result` `test.takers` the same highest value.
#' @return returns a result matrix
#' @export
#'
#' @examples
#' result<-data.frame(id=c("a","b","a","b","c","d","a"))
#' resp<-data.frame(id=c("b","b","a","a","c","d","a"),X1_result=c(1,NA,2,2,1,3,NA),X2_party=c(2,1,3,1,2,3,NA),X20_dont=c(NA,2,1,2,1,3,NA))
#' 
#' result<-mergeScores(result=result,resp=resp,column="X1",prefix="CP_")

#' # Make sure not to import data from item X20
#' result<-mergeScores(result,resp,"X2[^0-9]","TM2_")
mergeScores<-function(result,resp,column,prefix="",test.taker=NULL) {
  if(!is.null(test.taker)) resp<-cbind(data.frame(test.taker=test.taker,stringsAsFactors = F),resp)
  cols<-grepl(paste0("^",column),names(resp),ignore.case = T)
  if(any(cols)) {
    for(onecolumn in which(cols)) {
      respcolumn<-resp[,onecolumn]
      if(inherits(respcolumn,c("integer","numeric"))) {
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
      testTaker<-resp[,1]
      # If the testtakers are in the same order, just cbind (takes care of duplets)
      done<-F
      if(nrow(result)==nrow(scores)) {
        if(all(as.character(testTaker)==as.character(result[,1]))) {
          result<-cbind(result,scores)
          done<-T
        } 
      }
      if(!done) { # Merge combines the data.frames in all possible ways - resulting in more rows if there are duplet testtakers
        # Remove duplicate testTakers in resp by giving them the highest value
        
        dups<-which(duplicated(testTaker))
        if(length(dups)>0){
          revised.testTaker<-testTaker[-dups]
          revised.scores<-scores[-dups,]
          for(x in dups) {
            # Next time you meet a duplicate of the same testtaker, you will do the same calculation - no problem
            tt<-testTaker[x]
            max.score<-apply(scores[testTaker==tt,],2,max,na.rm = T)
            # give the max value to the unified testtaker
            revised.scores[which(revised.testTaker==tt),]<-max.score
          }
          scoresAndTaker<-cbind(data.frame(revised.testTaker),data.frame(revised.scores))
        } else {scoresAndTaker<-cbind(data.frame(testTaker),data.frame(scores))}
        colnames(scoresAndTaker)<-c("id",paste0(prefix,colnam))
        result$recreateorder<-1:nrow(result)
        result<-merge(result,scoresAndTaker,by.x=colnames(result)[1],by.y=colnames(scoresAndTaker)[1],all.x=T)
        # Get the rows back in the same order...
        rownames(result)<-result$recreateorder
        result<-result[order(result$recreateorder),colnames(result)!="recreateorder"]
        
        #colnames(result)[1]
      }
    }
  } else warning(paste("Merge Scores: No column starting with",column,"in response data.frame"))
  return (result) 
}

