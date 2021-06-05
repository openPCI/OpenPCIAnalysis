library(ggplot2)
library(qmethod)
#' Extract Q-method responses from JSON
#'
#' @param qjson A response vector from the OpenPCI wordrank PCI 
#' @param positive Which corner is the most positive value: "top" (default), "bottom", "left", "right".
#'
#' @return Returns a list of responses, each consisting of a list ranked rows
#' @export
#'
#' @examples
#' qjson<-c("[[\"Running\"],[\"Walking\",\"Crawling\"],[\"Jumping\",\"Sprinting\",\"Jogging\"],[\"Strolling\",\"\",\"Standing\",\"Sitting\"]]",
#'          "[[\"Sitting\"],[\"Strolling\",\"Jogging\"],[\"Standing\",\"Sprinting\",\"Crawling\"],[\"Walking\",\"Running\",\"Jumping\"]]")
#' q<-get.q(qjson)
#' q
#' q<-get.q(qjson,"right")
#' q
get.q<-function(qjson,positive=c("top", "bottom", "left", "right")) {
  positive<-match.arg(positive)
  qjson<-as.character(qjson)
  l<-unname(lapply(qjson,function(x) {
    if(!is.na(x) && nchar(x)>0) {
      q1<-jsonlite::fromJSON(x,simplifyVector = T)
      if(positive=="left" || positive=="right") {
        q1<-sapply(data.table::transpose(q1),function(y) y[!is.na(y)])
      }
      if(positive=="bottom" || positive=="right") {
        q1<-rev(q1)
      } 
     q1
    }
  }))
  q<-lapply(l,sapply,trimws)
  class(q)<-"open.pci.q"
  q
}
#' Collect Q-method-data from respondents into rows
#'
#' @param q A Q-method list (from [get.q()])
#'
#' @return Returns a list with one list element for each row in the wordrank PCI, each element contains all statements in that row accross all responcents. 
#' @export
#'
#' @examples
#' qjson<-c("[[\"Running\"],[\"Walking\",\"Crawling\"],[\"Jumping\",\"Sprinting\",\"Jogging\"],[\"Strolling\",\"\",\"Standing\",\"Sitting\"]]",
#'          "[[\"Sitting\"],[\"Strolling\",\"Jogging\"],[\"Standing\",\"Sprinting\",\"Crawling\"],[\"Walking\",\"Running\",\"Jumping\",\"Sleeping\"]]")
#' q<-get.q(qjson)
#' qrows<-get.q.rows(q)
#' qrows
get.q.rows<-function(q) {
  nqrow<-max(sapply(q,length))
  r<-sapply(1:nqrow,function(x) unlist(sapply(q,function(y) if(length(y)>=x) y[[x]] else NA)))
}

#' Produce a data.frame from a open.pci.q object
#'
#' @param q A open.pci.q object
#'
#' @return Returns a data.frame with statements as rows and persons as columns. Each statement has a score on each person equal to the row it is put in by the person. The upper row has the highest value, the bottom row has the value 1. If a statement has not been put in a row, it gets 0 on this person.
#' @export
#'
#' @examples
#' qjson<-c("[[\"Running\"],[\"Walking\",\"Crawling\"],[\"Jumping\",\"Sprinting\",\"Jogging\"],[\"Strolling\",\"\",\"Standing\",\"Sitting\"]]",
#'          "[[\"Sitting\"],[\"Strolling\",\"Jogging\"],[\"Standing\",\"Sprinting\",\"Crawling\"],[\"Walking\",\"Running\",\"Jumping\"]]")
#' q<-get.q(qjson)
#' get.q.df(q)
get.q.df<-function(q) {
  nqrow<-max(sapply(q,length))
  statements<-unique(unlist(q))
  statements<-statements[statements!=""]
  nstat<-length(statements)
  npers<-length(q)
  q.df<-as.data.frame(matrix(rep(NA,nstat*npers),nrow = nstat),row.names=statements)
  res<-rep(0,nstat)
  for(p in 1:npers) q.df[,p]<-if(length(q[[p]])>0) apply(sapply(1:nqrow,function(x) {res[sapply(q[[p]][x],match,statements)]<-nqrow-x+1;res}),1,sum) else NA
  q.df
}

#' Do a Q-Method analysis on the dataset
#'
#' @param q A, open.pci.q object from [get.q()]
#' @param q.df Alternatively to q, provide a data.frame from [get.q.df()]
#' @param only.complete Boolean. Only include persons who have used all statements.
#' @param nfactors The number of factors to extract.
#' @param rotation The type of rotation to use 
#' @param cor.method Which method to use for the correlations in [cor()] ("pearson", "spearmann", "kendall"). 
#' Pearson is default in qmethod, but given that the data is ordinal, not interval scaled, Kendall's tau is default in q.analysis.
#' @param nsteps The number of repetitions to use when bootstrapping (to get estimates of standard errors and bias). See [qmboots()].
#' @param indet Indeterminacy method ("qindtest", "procrustes"). Default is qindtest, use procrustes for more than three factors. See [qmboots()].
#' @param distribution Distribution provided as a vector of numbers, such as c(1, 1, 1, 2, 2, 3), signifying three cells in the lowest row, two cells in the middle row, and one cell in the highest row.
#' @param exclude.cases A vector of cases to exclude (e.g. c("V33", "V38", "V59", "V107"))
#'
#' @return Returns an object of QmethodRes. See [qmethod()] for explanation.
#' @export
#' @note The analysis is done by the qmethod-package.
#' @seealso [qmethod()], [qmboots()], [cor()], [plot.QmethodRes()].
#' @examples
#' qjson<-c("[[\"Running\"],[\"Walking\",\"Crawling\"],[\"Jumping\",\"Sprinting\",\"Jogging\"],[\"Strolling\",\"\",\"Standing\",\"Sitting\"]]",
#'          "[[\"Sitting\"],[\"Strolling\",\"Jogging\"],[\"Standing\",\"Sprinting\",\"Crawling\"],[\"Walking\",\"Running\",\"Jumping\"]]",
#'          "[[\"Strolling\"],[\"Sitting\",\"Standing\"],[\"Jogging\",\"Sprinting\",\"Crawling\"],[\"Walking\",\"Running\",\"Jumping\"]]")
#' q<-get.q(qjson)
#' q.analysis(q)
q.analysis<-function(q=NULL,q.df=get.q.df(q), only.complete=T,nfactors = 3, rotation = "varimax", cor.method="kendall", nsteps=NULL, indet="qindtest",distribution=NULL, exclude.cases=NULL) {
  if(!is.null(q.df)) q.df <- q.df[,!(colnames(q.df) %in% exclude.cases)]
  q.df.no.na<-q.df[,!apply(q.df,2,function(x) all(is.na(x)))]
  
  # If statements has not been put into the q-matrix, they are given the value 0. Only complete leaves persons out that hasn't used all statements
  if(only.complete) q.df.no.na<-q.df.no.na[,apply(q.df.no.na,2,function(x) all(!is.na(x) & x>0))]
  if(is.null(distribution)) {
    if(class(q)!="open.pci.q") stop("You need to provide a q object, if you don't provide a distribution")
    qrows<-get.q.rows(q)
    qdist<-q.distribution(qrows)
    maxval<-max(qdist$Row)
    dist.vector<-rep(0,maxval)
    for(r in 1:nrow(qdist)) dist.vector[qdist$Row[r]]<-dist.vector[qdist$Row[r]]+qdist$Frequency[r]
    dist.vector<-ceiling(dist.vector/(sum(dist.vector)/(nlevels(qdist$Activity)-1)))
    distribution<-rep(1,sum(dist.vector))
    for(i in 2:maxval) distribution[sum(dist.vector[1:(i-1)]):sum(dist.vector[1:i])]<-i
    while(length(distribution)>(nlevels(qdist$Activity)-1)) distribution<-distribution[-1]
  }
  if(!is.null(nsteps))
    qmboots(dataset = q.df.no.na, nfactors = nfactors, rotation = rotation,cor.method = cor.method,forced = F,distribution=distribution,indet = indet,nsteps = nsteps)
  else 
    qmethod(dataset = q.df.no.na, nfactors = nfactors, rotation = rotation,cor.method = cor.method,forced = F,distribution=distribution)
}
#' Tabularize Q-method-data
#'
#' @param qrows A list of rows from [get.q.rows()]
#' @param exclude A vector of values to exclude from the tabulation
#'
#' @return Returns a data.frame with three rows: Activity (the statements from wordrank), Frequency of this statement in this row, and the Row for which the Activity has this Frequency. Rows are numbered from the buttom (1) to the top (number of rows)
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
  df<-do.call("rbind",lapply(1:nqrow,function(x) {d<-as.data.frame(qdistlist[[x]]);d$Row<-nqrow-x+1;d}))
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
  for(i in nqrow:1) {
    n<-setdiff(qdist$Activity[qdist$Row==i],l)
    l<-c(n[order(qdist$Frequency[(qdist$Activity %in% n) & qdist$Row==i])],l)
  }
  l<-na.omit(l)
  qdist$Activity<-factor(qdist$Activity,levels=l)
  qdist$Percent<-apply(qdist,1,function(x) {as.numeric(x["Frequency"])/sum(qdist$Frequency[qdist$Row==as.numeric(x["Row"])])})
  qdist$Row<-factor(qdist$Row,levels = nqrow:1)
  p<-ggplot(qdist,mapping = aes(x=Activity,y=Percent))+
    facet_wrap(facets = vars(Row),ncol=3)+
    geom_col()+
    scale_y_continuous(labels = scales::percent)+
    coord_flip()+
    labs(title = title,subtitle = subtitle,caption = caption)
  print(p)
  p
}

#' Show distribution of statements in factors
#'
#' @param QmethodRes A result object from [qmethod()] or a data.frame with statements as rownames and factors in columns with position of statements.
#' @param marg Margin of rectangles (relative to size of retangle)
#' @param linewidth Linewidth of texts in number of letters (longer lines are split at nearest space)
#' @param show.plot If TRUE, plot is output to active dev.
#' @return Returns a list of the produced plots.
#' @export
#'
#' @examples
#' qjson<-c("[[\"Running\"],[\"Walking\",\"Crawling\"],[\"Jumping\",\"Sprinting\",\"Jogging\"],[\"Strolling\",\"\",\"Standing\",\"Sitting\"]]",
#'          "[[\"Sitting\"],[\"Strolling\",\"Jogging\"],[\"Standing\",\"Sprinting\",\"Crawling\"],[\"Walking\",\"Running\",\"Jumping\"]]")
#' q<-get.q(qjson)
#' result<-q.analysis(q)
#' q.show.distribution(result)
q.show.distribution<-function(QmethodRes,marg=0.03,linewidth=15,show.plot=T) {
  plots<-list()
  if(inherits(QmethodRes,"QmethodRes")) QmethodRes<-QmethodRes$zsc_n
  for(i in 1:ncol(QmethodRes)) {
    nrect<-table(QmethodRes[,i])
    maxnrect<-max(nrect)
    y1<-rev(rep(nrect,names(nrect)))
    y2<-y1+1-marg*2
    x1<-unlist(sapply(nrect,function(x) (1:x)+(maxnrect/2-(x)/2)))
    x2<-x1+1-marg
    txts<-sub("\n$","",gsub(pattern = paste0("(.{",linewidth,"}[^ ]+)"),replacement = "\\1\n",x = rownames(QmethodRes)[order(QmethodRes[,i],decreasing = T)]))
    d=data.frame(x1=x1,x2=x2,y1=y1,y2=y2, t=rev(txts))
    p<-ggplot() + 
      scale_x_continuous(name="x") + 
      scale_y_continuous(name="y") +
      geom_rect(data=d, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color="black", fill="transparent",linejoin = "round") +
      geom_text(data=d, aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=t), size=4) +
      theme_void()+
      theme(legend.position = "none")
    if(show.plot) print(p)
    plots[[length(plots)+1]]<-p
  }
  invisible(plots)
}

