#' Make an order object
#'
#' @param resp A response vector from TAO 
#'
#' @return Returns an orderObject
#' @export
#'
#' @examples
#' orderresp<-c("<'audition'; 'actor'; 'prof reading'; 'costumes'>", "<'manuscript'; 'prof reading'; 'costumes'; 'actor'>")
#' orderObject<-makeOrder(orderresp)
makeOrder<-function(resp) {
  # Match is in a format that is simlar to JSON, but we need to convert it a little...
  resp<-gsub(";",",",resp,fixed = T)
  resp<-gsub("<","[",resp,fixed = T)
  resp<-gsub(">","]",resp,fixed = T)
  resp<-gsub("'","\\\"",resp)
  #resp<-gsub("([[:alnum:]_]+)","\"\\1\"",resp)
  resp[resp==""]<-"[]"
  
  return (apply(as.array(resp),1,jsonlite::fromJSON))
}
#' Test ordering of elements
#'
#' @param orderObject An orderObject
#' @param a an element
#' @param b an element
#' @param strict All members of b needs to be present
#' @param count How many in b is a before?
#'
#' @return Returns scores
#' @export
#'
#' @examples
#' orderresp<-c("<'audition'; 'actor'; 'prof reading'; 'costumes'>", "<'manuscript'; 'prof reading'; 'costumes'; 'actor'>")
#' orderObject<-makeOrder(orderresp)
#' isOrderSet(orderObject,"audition")+isOrderBefore(orderObject,"audition",c("costumes", "prof reading"),count = T)
isOrderBefore<-function(orderObject,a,b,strict=T,count=F) {
  if(count)
    res<-as.numeric(lapply(orderObject,function(x) {sum(which(x==a)<which(x %in% b),na.rm = T)}))
  else res<-as.logical(lapply(orderObject,function(x) {ifelse(strict,length(intersect(x,b))==length(b),TRUE) & which(x==a)<min(which(x %in% b),Inf,na.rm = T)}))
  ifelse(is.na(res),0,res)
}
#' @rdname isOrderBefore
isOrderAfter<-function(orderObject,a,b,strict=T) {
  if(count)
    res<-as.numeric(lapply(orderObject,function(x) {sum(which(x==a)>which(x %in% b),na.rm = T)}))
  else 
    res<-as.logical(lapply(orderObject,function(x) {ifelse(strict,length(intersect(x,b))==length(b),TRUE) & which(x==a)>max(which(x %in% b),-Inf,na.rm = T)}))
  ifelse(is.na(res),0,res)
}
#' @rdname isOrderBefore
isOrderSet<-function(orderObject,a) {
  res<-as.logical(lapply(orderObject,function(x) {x<-ifelse(is.na(x),0,x); ifelse(length(x)>0,which(x==a)>0,F)}))
  ifelse(is.na(res),0,res)
}
