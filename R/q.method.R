library(ggplot2)
#' Extract Q-method responses from JSON
#'
#' @param qjson A response vector from the OpenPCI wordrank PCI 
#'
#' @return Returns a list of responses, each consisting of a list ranked rows
#' @export
#'
#' @examples
#' #' qjson<-c("[[\"Running\"],[\"Walking\",\"Crawling\"],[\"Jumping\",\"Sprinting\",\"Jogging\"],[\"Strolling\",\"\",\"Standing\",\"Sitting\"]]",
#'          "[[\"Sitting\"],[\"Strolling\",\"Jogging\"],[\"Standing\",\"Sprinting\",\"Crawling\"],[\"Walking\",\"Running\",\"Jumping\"]]")
#' q<-get.q(qjson)
#' q
get.q<-function(qjson) {
  qjson<-as.character(qjson)
  l<-unname(lapply(qjson,function(x) {if(!is.na(x) && nchar(x)>0) jsonlite::fromJSON(x,simplifyVector = T)}))
  lapply(l,sapply,trimws)
}
#' Collect Q-method-data from respondents into rows
#'
#' @param q A Q-method list (from [get.q()])
#'
#' @return Returns a list with one list element for each row in the wordrank PCI, each element contains all statements in that row accross all responcents. 
#' @export
#'
#' @examples
#' #' qjson<-c("[[\"Running\"],[\"Walking\",\"Crawling\"],[\"Jumping\",\"Sprinting\",\"Jogging\"],[\"Strolling\",\"\",\"Standing\",\"Sitting\"]]",
#'          "[[\"Sitting\"],[\"Strolling\",\"Jogging\"],[\"Standing\",\"Sprinting\",\"Crawling\"],[\"Walking\",\"Running\",\"Jumping\"]]")
#' q<-get.q(qjson)
#' qrows<-get.q.rows(q)
#' qrows
get.q.rows<-function(q) {
  maxrow<-max(sapply(q,length))
  r<-sapply(1:maxrow,function(x) unlist(sapply(q,function(y) if(length(y)>=x) y[[x]] else NA)))
}
#' Tabularize Q-method-data
#'
#' @param qrows A list of rows from [get.q.rows()]
#' @param exclude A vector of values to exclude from the tabulation
#'
#' @return Returns a data.frame with three rows: Activity (the statements from wordrank), Frequency of this statement in this row, and the Row for which the Activity has this Frequency
#' @export
#' @seealso [q.plot()]
#' @examples
#' qjson<-c("[[\"Running\"],[\"Walking\",\"Crawling\"],[\"Jumping\",\"Sprinting\",\"Jogging\"],[\"Strolling\",\"\",\"Standing\",\"Sitting\"]]",
#'          "[[\"Sitting\"],[\"Strolling\",\"Jogging\"],[\"Standing\",\"Sprinting\",\"Crawling\"],[\"Walking\",\"Running\",\"Jumping\"]]")
#' q<-get.q(qjson)
#' qrows<-get.q.rows(q)
#' qdist<-q.distribution(qrows)
#' qdist
q.distribution<-function(qrows,exclude=NULL) {
  qdistlist<-lapply(qrows,table)
  nqrow<-length(qdistlist)
  df<-do.call("rbind",lapply(1:nqrow,function(x) {d<-as.data.frame(qdistlist[[x]]);d$Row<-x;d}))
  colnames(df)<-c("Activity","Frequency","Row")
  df<-df[nchar(as.character(df$Activity))>0 & !is.na(df$Activity),]
  if(!is.null(exclude)) df<-df[!(df$Activity %in% exclude),]
  df
}

#' Plot distribution of Q-method ordering
#'
#' @param qdist The distributions of Q-method ordering from [qdist()]
#' @param title The title of the plot
#' @param subtitle The subtitle of the plot
#' @param caption The caption of the plot
#'
#' @return Returns the plot (a ggplot which for example can be saved with [ggsave()])
#' @export
#'
#' @examples
#' qjson<-c("[[\"Running\"],[\"Walking\",\"Crawling\"],[\"Jumping\",\"Sprinting\",\"Jogging\"],[\"Strolling\",\"\",\"Standing\",\"Sitting\"]]",
#'          "[[\"Sitting\"],[\"Strolling\",\"Jogging\"],[\"Standing\",\"Sprinting\",\"Crawling\"],[\"Walking\",\"Running\",\"Jumping\"]]")
#' q<-get.q(qjson)
#' qrows<-get.q.rows(q)
#' qdist<-q.distribution(qrows)
#' qplot(qdist)
q.plot<-function(qdist,title=NULL,subtitle=NULL,caption=NULL) {
  # We need to order activities based on all rows to make sure they are present
  nqrow<-max(qdist$Row)
  l<-c()
  for(i in 1:nqrow) {
    n<-setdiff(qdist$Activity[qdist$Row==i],l)
    l<-c(n[order(qdist$Frequency[(qdist$Activity %in% n) & qdist$Row==i])],l)
  }
  l<-na.omit(l)
  qdist$Activity<-factor(qdist$Activity,levels=l)
  qdist$Percent<-apply(qdist,1,function(x) {as.numeric(x["Frequency"])/sum(qdist$Frequency[qdist$Row==as.numeric(x["Row"])])})
  p<-ggplot(qdist,mapping = aes(x=Activity,y=Percent))+
    facet_wrap(facets = vars(Row),ncol=3)+
    geom_col()+
    scale_y_continuous(labels = scales::percent)+
    coord_flip()+
    labs(title = title,subtitle = subtitle,caption = caption)
  print(p)
  p
}
