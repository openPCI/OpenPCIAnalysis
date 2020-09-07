
#' Make a match object
#'
#' @param resp a vector of match responses from TAO
#' @param columns.as.variables Boolean. Set to use columns as variables instead of rows.
#' @return Returns a matchObject
#' @export
#' @seealso [scoreMatch()], [explodeMatch()]
#' @examples
#' matchresponse<-c("[mail1 annoyed; mail4 annoyed; mail2 angry]","[mail1 confused]")
#' matchObject<-makeMatch(matchresponse)
makeMatch<-function(resp, columns.as.variables=F) {
  # Match is in a format that is simlar to JSON, but we need to convert it a little...
  resp<-gsub("; ",",",resp)
  resp<-gsub(" ",":",resp)
  resp<-gsub("\\[","{",resp)
  resp<-gsub("\\]","}",resp)
 resp<-gsub("([[:alnum:]_]+)","\"\\1\"",resp)
  resp[resp==""]<-"{}"
  matchObject<-rep(NA,length(resp))
  nona<-!is.na(resp)
  matchObject[nona]<-apply(as.array(resp[nona]),1,jsonlite::fromJSON)
  if(columns.as.variables) {
    matchObject[nona]<-lapply(matchObject[nona], function(x) setNames(as.list(names(x)),x))
  }
  class(matchObject)<-"matchObject"
  matchObject
}

#' Score match responses
#'
#' @param matchObject A matchObject 
#' @param identifier The variable that has been given (a) corresponding value(s)
#' @param correct The correct values
#' @param columns.as.variables Boolean. Set to use columns as variables instead of rows.
#' @return A vector of scores
#' @export
#'
#' @examples
#' matchresponse<-c("[mail1 annoyed; mail4 annoyed; mail2 angry]","[mail1 confused]")
#' matchObject<-makeMatch(matchresponse)
#' scoreMatch(matchObject,"mail1",c("happy","annoyed"))
scoreMatch<-function(resp,identifier,correct=c(), columns.as.variables=F) {
  return (as.logical(lapply(resp,function(x) {length(intersect(x[names(x)==identifier],correct))})))
}

#' Explode a Match interaction into a data frame
#'
#' @param resp The response column from TAO or a matchObject from [makeMatch()]
#' @param vals.as.numeric Convert values to numeric (by removing letters and other non-numeric parts of value)
#' @param columns.as.variables Boolean. Set to use columns as variables instead of rows.
#' 
#' @return Returns a data.frame
#' @export
#' @details This function is used when a Match interaction is used to ask a number of questions (in rows) with shared response options (in columns)
#'
#' @examples
#' matchresponse<-c("[agree q1; disagree q2; agree q3; disagree q4]","[agree q1; disagree q2; agree q3; agree q4]")
#' explodeMatch(matchresponse,vals.as.numeric=F)
explodeMatch<-function(resp,vals.as.numeric=T, columns.as.variables=F) {
  match.data<-if(!inherits(resp,"matchObject")) makeMatch(resp) else resp
  cols<-setdiff(unique(unlist(match.data)),NA)
  dummydf<-matrix(ncol=length(cols), dimnames = list(NULL,cols))
  
  do.call(rbind,lapply(match.data,function(x) 
    if(!is.na(x) && length(x)>0) {
      df<-matrix(
        if(vals.as.numeric) as.numeric(gsub("[^0-9]","",names(x))) else names(x),
        ncol=length(x),
        dimnames = list(NULL,x)
      )
      if(ncol(df)<ncol(dummydf)){
        df<-cbind(
          as.data.frame(t(dummydf[,!(colnames(dummydf) %in% x)])),
          df
        ) 
      }
      df[,cols]
    } else dummydf
  ))
  
}
