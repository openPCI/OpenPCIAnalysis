#' Make a multiple choise object
#'
#' @param resp A response vector from TAO 
#'
#' @return Returns a multiple choice object
#' @export
#'
#' @examples
#' mcresp<-c("['choice_1'; 'choice_5']","['choice_5'; 'choice_6'; 'choice_7']")
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

#' Explode a Multiple Choice interaction into a data frame
#'
#' @param resp The response column from TAO or a MCObject from [makeMC()]
#'
#' @return Returns a data.frame
#' @export
#' @details This function is used when a Multiple Choice interaction allows to choose more than one option
#'
#' @examples
#' mcresp<-c("['choice_1'; 'choice_5']","['choice_5'; 'choice_6'; 'choice_7']")
#' explodeMC(mcresp)
explodeMC<-function(resp) {
  mc.data<-if(!inherits(resp,"MCObject")) makeMC(resp) else resp
  cols<-setdiff(unique(unlist(mc.data)),NA)
  dummydf<-matrix(ncol=length(cols), dimnames = list(NULL,cols))
  
  do.call(rbind,lapply(mc.data,function(x) 
    if(!is.na(x) && length(x)>0) {
      df<-matrix(1,
        ncol=length(x),
        dimnames = list(NULL,x)
      )
      if(ncol(df)<ncol(dummydf)){
        extracols<-colnames(dummydf)[!(colnames(dummydf) %in% x)]
        df<-cbind(
          as.data.frame(matrix(0,nrow = 1,ncol = length(extracols),dimnames = list(NULL,extracols))),
          df
        ) 
      }
      df[,cols]
    } else dummydf
  ))
  
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