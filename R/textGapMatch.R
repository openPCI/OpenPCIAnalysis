#' Make a text gap match object
#'
#' @param resp A response vector from TAO 
#'
#' @return Returns a text gap match object
#' @export
#'
#' @examples
#' matchresp<-c('{"room":["Chair"],"kitchen":["Pot"],"hallway":["Staircase"]}','{"room":["Sofa","table"],"kitchen":["Books"],"hallway":["Lion"]}')
#' matchObj<-makeTextGapMatch(matchresp)
makeTextGapMatch<-function(resp) {
  # MC is in a format that is simlar to JSON, but we need to convert it a little...
  resp<-gsub("'","\"",resp)
  resp<-gsub(";",",",resp)
  resp[resp==""]<-"[]"
  tgobj<-(apply(as.array(resp),1,jsonlite::fromJSON))
  class(tgobj)<-"textGapMatchObject"
  tgobj
}
#' Test for text in gap match
#'
#' @param textGapMatchObject a textGapMatchObject 
#' @param text The text to look for
#' @param gap The gap(s) to look in
#' @param no.where.else Only accept it if it is only here
#'
#' @return Returns the scores
#' @export
#'
#' @examples 
#' resp<-"{\"dropzone_1\":[\"1.Decide size of building\",\"2.Paint the walls\",\"4.Get a quote\"],\"dropzone_2\":[\"1.Move furniture\",\"2.Hire workers\"],\"dropzone_3\":[\"1.Test the construction\",\"2.Move in\",\"3.Paint the walls\"]}"
#' gm<-makeTextGapMatch(resp)
#' textInGap(gm,"Paint the walls","dropzone_1")
#' textInGap(gm,"Paint the walls","dropzone_1",no.where.else=F)
textInGap<-function(textGapMatchObject,text,gap,no.where.else=TRUE) {
  isIn<-lapply(textGapMatchObject,function(x) {
    isInHere<-F
    for(i in 1:length(gap)) {
      isInHere<-isInHere || any(grepl(pattern = paste0("^[0-9.]*",text,"$"),x[[gap[i]]]))
    }
    isInHere
  })
  if(no.where.else) {
    isInOther<-lapply(textGapMatchObject,function(x) {
      isInHere<-F
      gaps<-names(x)
      if(length(gaps)>0) {
        for (i in 1:length(gaps)) {
          if (!(gaps[i] %in% gap)) 
            isInHere <- isInHere || any(grepl(pattern = paste0("^[0-9.]*", text, "$"), x[[gaps[i]]]))
        }
      }
      isInHere
    })
    isIn<-sapply(1:length(isIn), function(x) isIn[[x]] && !isInOther[[x]])
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
#' @examples 
#' resp<-"{\"dropzone_1\":[\"1.Decide size of building\",\"2.Paint the walls\",\"4.Get a quote\"],\"dropzone_2\":[\"1.Move furniture\",\"2.Hire workers\"],\"dropzone_3\":[\"1.Test the construction\",\"2.Move in\"]}"
#' gm<-makeTextGapMatch(resp)
#' posInGap(gm,"Paint the walls",1)
posInGap<-function(textGapMatchObject,text,gap) {
  posIn<-as.numeric(lapply(textGapMatchObject,function(x) {pos<-which(grepl(pattern = paste0("^[0-9.]*",text,"$"),x[[gap]]));if(length(pos)>0) pos else 0}))
  return(posIn)
}
