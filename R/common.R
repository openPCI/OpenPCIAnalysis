#' Initialize result data.frame
#'
#' @param data a data.frame or matrix with a Test.taker column.
#'
#' @return a data.frame for the results
#' @export
#'
#' @examples
#' iniResult(data)
iniResult<-function(data) {
  return(data.frame(testTaker=data$Test.taker))
}
#' Get responses
#'
#' @param data csv-formatted dataset.
#' @param pattern RegExp pattern of the response columns
#'
#' @return Returns a list of responses.
#' @export
#' @description Gets responses from data produced by TAO.
#' @examples 
#' getResponses(data,pattern=".RESPONSE")
getResponses<-function(data,pattern=".RESPONSE") {
  return(lapply(data,function(x) {x[,grep(pattern,colnames(x))]}))
}

#' Get already scored items
#'
#' @param data csv-formatted dataset.
#' @param pattern RegExp pattern of the scoring columns.
#' @export
#' @return Gets scored data produced by TAO.
#' @examples 
#' getScores(data,pattern=".SCORE")
getScores<-function(data,pattern=".SCORE") {
  return(lapply(data,function(x) {x[,grep(pattern,colnames(x))]}))
}

#' Calculate durations
#'
#' @param data List of modules
#' @param exclude Columns containing this text should not be counted in. Use text1|text2 to exclude more than one text
#' 
#' @description Calculate time spent on each item by all students. E.g. use this to set response to NA when the time spent is very few seconds.
#' @return Returns a list durations=durations,median=median,testMedian=testMedian,mean=mean,testMean=testMean,max=max,testMax=testMax
#' @export
#'
#' @examples getDurations(data,exclude="notthiscolumn")
getDurations<-function(data,exclude="") {
  durations<-lapply(data,function(x) {z<-x[,grep("duration",colnames(x))];rownames(z)<-x$Test.taker;return(z)})
  durations<-lapply(durations,apply,1:2,function(x) {
    gsub("0([0-9]+[HM\\.])","\\1",sub("PT(([0-9]+)H)?(([0-9]+)M)?(([0-9]+)\\.([0-9]+)S)?","0\\2H0\\4M0\\6.\\70S",x))
    # sub("PT","",ifelse(!grepl("H",x),paste0("0H",ifelse(!grepl("M",x),paste0("0M",ifelse(!grepl(".",x,fixed = T),paste0(x,"0.0S"),x)),x)),x))
    })
  durations<-lapply(durations,apply,1:2,function(x) {as.difftime(x,format="%HH%MM%OSS",units="secs")})
  # Remove columns with all NA's
  durations<-lapply(durations,function(x) x[,colSums(is.na(x))<nrow(x)])
  # Remove excludes
  if(length(exclude>0))
    durations<-lapply(durations,function(x) x[,!grepl(exclude,colnames(x))])
  
  #Stats
  median<-lapply(durations,apply,2,median,na.rm=T)
  testMedian<-lapply(median,sum)
  mean<-lapply(durations,apply,2,mean,na.rm=T)
  testMean<-lapply(mean,sum)
  max<-lapply(durations,apply,2,max,na.rm=T)
  testMax<-lapply(max,sum)
  return (list(durations=durations,median=median,testMedian=testMedian,mean=mean,testMean=testMean,max=max,testMax=testMax))
}

#' Write results
#'
#' @param result A data.frame with results
#' @param filename Where to write the results
#' @param resultsdir The path to the directory of the file
#'
#' @return Returns the cleaned results.
#' @export
#'
#' @examples writeResult(result,filename,resultsdir)
writeResult<-function(result,filename,resultsdir) {
  resp<-result[,2:ncol(result)]
  resp<-apply(resp,1:2,as.numeric)
  result[,2:ncol(result)]<-resp#apply(result[,2:ncol(result)],1:2,function(x) {ifelse(is.na(x),-1, as.numeric(x))})
  # result<-apply(result,1:2,function(x) x=ifelse(x==-1,NA,x))
  write.csv2(result,paste0(resultsdir,filename,".csv"),row.names = F)
#  print(resp)
  resp<-resp[,colSums(resp,na.rm = T)!=0]
  resp
}
#' @rdname writeResult
#' @export
writeCoded<-function(coded,filename,resultsdir) {
  write.csv2(coded,paste0(resultsdir,filename,".csv"),row.names = F)
}
delist<-function(a) {
  a[sapply(a, is.null)] <- NA
  unlist(a)
}
