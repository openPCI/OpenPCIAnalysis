#' Make Gantt Object
#'
#' @param gantt The Gantt data from TAO
#' @param names Names of the rows in the Gantt chart (don't have to be the same as in the PCI)
#' @param timespan Duration of a time slot
#' @param time.format The POSSIX time format that the data was formatted (see https://www.stat.berkeley.edu/~s133/dates.html). Use \\%d/0 when the Gantt-output was made using 0 for month
#'
#' @return Returns a Gantt object
#' @export
#'
#' @examples 
#' response<-"{'response':'1/8 10:30;1/8 13:00 - 1/8 14:00;1/8 11:30 - 1/8 12:30'}"
#' makeGantt(response,names=c("waitress","actor","pianist"),timespan=30,time.format="%d/%m %H:%M")
makeGantt<-function(gantt,names,timespan=30,time.format="%d/%m %H:%M") {
  #Clean
  #gantt<-gsub("1/8 ","",gantt)
  # Extract intervals from JSON (or remove \n, in case of legacy gantt)
  gantt<-delist(apply(matrix(gantt),1,function(x) {x<-if(grepl("{",x,fixed = T)) {jsonlite::fromJSON(gsub("'",'"',ifelse(x=="","[]",x)))$response} else gsub("\n","",x)}))
  #Eksplode to list of lists
  gantt<-lapply(gantt,strsplit,";")
  gantt<-lapply(gantt,lapply,lapply,lapply,strsplit,", ")
  # print(summary(gantt))
  #Reduce to 2-level list
  gantt<-lapply(gantt,lapply,lapply,function(x) {x<-x[[1]][[1]]})
  gantt<-lapply(gantt,function(x) x[[1]])
  # Add rows in lists of fewer than max rows
  maxRows<-apply(as.data.frame(lapply(gantt,length)),1,max)
  gantt<-lapply(gantt,function(x) {while(length(x)<maxRows) x[[length(x)+1]]<-character(); return(x)})
  # Add names to rows
  gantt<-lapply(gantt,function(x) {names(x)<-names; return(x)})
  #Transform intervals to times
  ganttUnique<-lapply(gantt,lapply,expandtime,timespan,time.format)
  #Translate times to POSIX
  ganttUnique<-lapply(ganttUnique,lapply,function(x) {if(!is.null(x)) x<-apply(as.array(x),1,function(y) as.POSIXct(strptime(y,time.format)));return(x)})

  go<-list(gantt=ganttUnique,names=names,timespan=timespan,time.format=time.format)
  class(go)<-"gantt"
  go
  #Gantt GanttUnique - which has dissolved intervals
}
tmp<-tmp<-0
#Test if a has been given a time

#' Has this element been given a time slot 
#'
#' @param gantt A gantt object (created by makeGantt)
#' @param a The name or row number of the element
#'
#' @return Returns a vector of booleans
#' @export
#'
#' @examples 
#' response<-"{'response':'1/8 10:30 - 1/8 12:30;1/8 13:00 - 1/8 14:00;1/8 11:30 - 1/8 12:30'}"
#' gantt<-makeGantt(response,names=c("waitress","actor","pianist"),timespan=30,time.format="%d/%m %H:%M")
#' isSet(gantt,a="actor")
isSet<-function(gantt,a=1) {
  as.logical(
    lapply(gantt$gantt,function(x) {return(length(x[[a]])>0)})
  )
}

#' Check relations between elements: isBefore, isAfter, isOverlap, noOverlap, isFirst, isLast
#'
#' @param gantt gantt object (from makeGantt).
#' @param a element or elements in the gantt chart (character or vector of characters).
#' @param b element or elements (character or vector of characters).
#' @param which.a,which.b "all": consider all slots of a/b, "first": only consider the first slot of a/b, "last": only consider the last slot of a/b. If which.a/which.b is a number n, the n'th slot is used in the comparison.
#' @param which "all": consider all slots of element, "first": only consider the first slot of element, "last": only consider the last slot of element. If which is a number n, the n'th slot is used in the comparison.
#' @param orEqual if TRUE, a and b can be at the same time (not just before/after).
#' @param strict if FALSE, an element a is considered before/after even though b is not present.
#'
#' @return When stict is TRUE, returns 0 when b is not set, when strict is FALSE, a is before b, when b is not set.
#' @export
#' 
#' @details 
#' In these functions a and b are compared. Which.a and which.b decides which parts of a an b take part in the comparison. 
#' If which.a is "all", all the slots of a are compared to the which.b slots. If which.a is "first", only the first slot of a is considered in the comparison.
#' If which.b is "last", only the last slot of a is considered in the comparison.
#' The same goes for which.b: if b is "all", all slots of b are considered, if b is "first" or "last", only the first/last slot is considered in the comparison.
#' For example, in isBefore, if which.a is "first" and which.b is last, the first slot of a needs to be before the last slot of b.
#' 
#' In practice this means that for isBefore, when a is "all", the last slot of a is compared to b, and when b is "all", the first slot of b i compared to a.
#' 
#' orEqual relaxes the condition of before or after, first or last, so the selected parts of a and b can be overlapping as well.
#' This result in that which.a="all" and orEqual=T means that the last slot are allowed to overlap.

#'
#' @examples 
#' response<-"{'response':'1/8 10:30 - 1/8 13:00;1/8 13:00 - 1/8 14:00;1/8 11:30 - 1/8 12:30'}"
#' gantt<-makeGantt(response,names=c("waitress","actor","pianist"),timespan=30,time.format="%d/%m %H:%M")
#' isBefore(gantt,"waitress",c("actor"),which.a = "first",which.b = "first")
#' isBefore(gantt,"waitress",c("actor"),which.a = "all",which.b = "first")
#' isBefore(gantt,"waitress",c("actor"),which.a = "all",which.b = "first",orEqual = TRUE)
#' isAfter(gantt,c("pianist","actor"),c("waitress"),which.a = "last",which.b = "all")
#' isLast(gantt,"waitress")
#' isFirst(gantt,"waitress",which="first")
#' isOverlap(gantt,"actor","waitress")
isBefore<-function(gantt,a=gantt$names[1],b=gantt$names[2],which.a="all",which.b="all",orEqual=FALSE,strict=TRUE,strictAfter=TRUE) {
  #strictAfter is only used internally to control strict from isAfter
  if(which.a=="last") which.a="all" #Just to help the designer
  if(which.b=="first") which.b="all" #do.
  if(length(b)==1) b<-c(b)
  as.logical(
    lapply(gantt$gantt,function (x) {
      res<-TRUE
      for(i in a) {
        for(j in b) {
          aval<-switch(which.a,
                       all=x[[i]][length(x[[i]])], # Last element
                       first=x[[i]][1], # First 
                       x[[i]][which.a] # Given number
          )
          bval<-switch(which.b,
                   last=x[[j]][length(x[[j]])], # Last element
                   all=x[[j]][1], # First b 
                   x[[j]][which.b] # Given number
          )
          if(!strict & is.null(bval)) bval<-Inf
          if(!strictAfter & is.null(aval)) aval<-0
          thisres<-ifelse(orEqual,aval <= bval,aval < bval)
          res<-ifelse(is.na(thisres),0,res & thisres)
        }
      }
      return(res)
    })
  )
}
#' @rdname isBefore
#' @export
isFirst<-function(gantt,a=gantt$names[1],which="all",orEqual=FALSE,strict=TRUE) {
  res<-rep(TRUE,length(gantt$gantt))
  for(i in gantt$names) {
    if(a!=i) res<-(res & isBefore(gantt,a=a,b=i,which.a=which,orEqual=orEqual,strict=strict))
  }
  return(res)
}

#' @rdname isBefore
#' @export
isAfter<-function(gantt,a=gantt$names[1],b=gantt$names[2],which.a="all",which.b="all",orEqual=FALSE,strict=TRUE) {isBefore(gantt,b,a,which.b,which.a,orEqual=orEqual,strictAfter = strict)}

#' Title
#' @rdname isBefore
#' @export
isLast<-function(gantt,a=gantt$names[1],which="all",orEqual=FALSE,strict=TRUE) {
    res<-rep(TRUE,length(gantt$gantt))
    for(i in gantt$names) {
        if(a!=i) res<-(res & isAfter(gantt,a=a,b=i,which.a=which,orEqual=orEqual,strict=strict))
    }
    return(res)
}


#' @param which.a/which.b for isOverlap: any: one or more a/b elements overlap, all: all a/b elements need to overlap, first/first: First a/b needs to overlap, last/last: Last a/b needs to overlap, or use number (or sequence)
#' @rdname isBefore
#' @export
isOverlap<-function(gantt,a=gantt$names[1],b=gantt$names[2],which.a="any",which.b="any") {
  if(length(b)==1) b<-c(b)
  as.logical(
    lapply(gantt$gantt,function (x) {
      res<-FALSE
      for(i in b) {
        test<-
          switch(which.a,
             all=x[[a]], # All elements
             any=x[[a]], # Test all elements
             first=x[[a]][1], # First
             last=x[[a]][length(x[[a]])], # Last 
             x[[a]][which.a] # Given number (or sequence)
          ) %in%
          switch(which.b,
             all=x[[i]], # All elements 
             any=x[[i]], # Test all elements 
             first=x[[i]][1], # First
             last=x[[i]][length(x[[i]])], # Last
             x[[i]][which.b] # Given number (or sequence)
          )
        thisres<-ifelse(length(x[[a]])>0 & length(x[[i]])>0,
            ifelse((which.a=="any" || which.b=="any"),any(test),all(test)) &
            ifelse(which.b=="all",length(x[[i]])==length(x[[a]]),TRUE),
          0)
        res<-ifelse(is.na(thisres),res,res | thisres)
      }
      return(res)
    })
  )
}

#noOverlap only if there is actually elements that could have overlapped.
#b: character or vector of characters
#' @rdname isBefore
#' @export
noOverlap<-function(gantt,a=gantt$names[1],b=gantt$names[1]) {
  if(length(b)==1) b<-c(b)
  as.logical(
    lapply(gantt$gantt,function (x) {
      res<-TRUE
      for(i in b) {
          thisres<-ifelse(length(x[[a]])>0 & length(x[[i]])>0,
                    !any(x[[a]] %in% x[[i]]),
                    0)
          res<-ifelse(is.na(thisres),0,res & thisres)
        }
      return (res)    
    })
  )
}

#' numSlots
#'
#' @param gantt a gantt object (created by makeGantt)
#' @param a an element
#'
#' @return Returns number of time slots - also counting non-connected slots
#' @export
#'
#' @examples 
#' response<-"{'response':'1/8 10:30 - 1/8 12:30;1/8 13:00 - 1/8 14:00;1/8 11:30 - 1/8 12:30'}"
#' gantt<-makeGantt(response,names=c("waitress","actor","pianist"),timespan=30,time.format="%d/%m %H:%M")
#' numSlots(gantt,"actor")
numSlots<-function(gantt,a=gantt$names[1]) {
  as.numeric(lapply(gantt$gantt,function(x) length(x[[a]])))
}
#' find the pattern in the row
#'
#' @param gantt 
#' @param a 
#' @param type "used"/"pause"/"pattern"
#' @param stat max, min, sum, length
#'
#' @return used: filled spots, pause: unfilled spots between filled spots, pattern: show the pattern (returns a list)
#' @export
#'
#' @examples
#' response<-"{'response':'1/8 10:30 - 1/8 12:30;1/8 13:00 - 1/8 14:00;1/8 11:30 - 1/8 12:30'}"
#' gantt<-makeGantt(response,names=c("waitress","actor","pianist"),timespan=30,time.format="%d/%m %H:%M")
#' findRows(gantt,"actor")
findRows<-function(gantt,a=gantt$names[1],type="used",stat="max") { #equals=NA,below=NA,above=NA
  numSpotsInRow<-lapply(gantt$gantt,function(x) {
      if(is.null(x[[a]])) return()
      if(length(x[[a]])<2) return (c(1))
      c<-1
      previousTime<-x[[a]][1]
      res<-apply(as.array(x[[a]][2:length(x[[a]])]),1,function(y){
        if(y==(previousTime+c*gantt$timespan*60)) {
          c<<-c+1
          return(0)
        } else {
          pause<--(y-(previousTime+c*gantt$timespan*60))/(gantt$timespan*60)
          previousTime<<-y
          cnow<-c
          c<<-1
          return(c(cnow,pause))
        }
      })
      n<-c(as.matrix(as.data.frame(res)),c)
      return(n[!is.na(n)])
  })
  if(type=="pattern") return (numSpotsInRow)
  use<-lapply(numSpotsInRow,function(x) {
      sign<-ifelse(type=="used",1,-1)
      use<-sign*x[(sign*x>0)]
    })
  lapply(use,stat)
}
#' Get start or end time of an element
#'
#' @param gantt A gantt object (created by makeGantt)
#' @param a an element
#' @param which start, end
#' @param human.readable If TRUE, time is given in human readable format
#'
#' @return
#' @export
#'
#' @examples
#' response<-"{'response':'1/8 10:30;1/8 13:00 - 1/8 14:00;1/8 11:30 - 1/8 12:30'}"
#' gantt<-makeGantt(response,names=c("waitress","actor","pianist"),timespan=30,time.format="%d/%m %H:%M")
#' getTime(gantt,"waitress",which="end",human.readable=T)
#' getTime(gantt,"actor",which="end",human.readable=T)
getTime<-function(gantt,a=gantt$names[1],which="start",human.readable=FALSE) {
  delist(lapply(gantt$gantt,function(x) {
    if(is.null(x[[a]])) return(NA)
    time<-switch (which,
      start = x[[a]][1],
      end = x[[a]][length(x[[a]])]+gantt$timespan*60
    )
    if(human.readable) {
      class(time) = c('POSIXt','POSIXct')
      return(format(time,format=gantt$time.format))
    }
    else ifelse(is.na(time),0,time)
  }))
}

#' Get the minimum/maximum start/end time among elements
#'
#' @param gantt A gantt object (created by makeGantt)
#' @param a a vector of elements to compare
#' @param which Which time to compare ("start" or "end")
#' @param strict If TRUE and an element among the compared is not present, NA is returned. 
#'
#' @return Returns minimum/maximum time in seconds of the earliest/latest element starting/ending
#' @export
#'
#' @examples
#' response<-matrix(c("{'response':'1/8 17:00;;1/8 17:00 - 1/8 18:00;1/8 18:00, 1/8 20:00 - 1/8 21:00;1/8 20:30;1/8 20:00 - 1/8 20:30;1/8 19:30 - 1/8 20:30'}"))
#' gantt<-makeGantt(response,names=c("waitress","actor","pianist","bartender","cleaning","ticketer","musician"),timespan=30,time.format="%d/%m %H:%M")
#' getMinTime(gantt,c("pianist","bartender"),which="start")
#' getMaxTime(gantt,c("actor","bartender"),which="end",strict=F)
#' getMaxTime(gantt,"actor",which="end")
#' as.difftime(getMaxTime(gantt,c("pianist","bartender"),which="end")-getMinTime(gantt,c("pianist","bartender"),which="start"),units = "secs")
#' 
getMinTime<- function (gantt,a,which,strict=T) {
  #Create a list of infinite values to avoid warnings...
  c<-data.frame(max=rep(Inf,length(gantt$gantt)))
  for(i in a) {
    c[i]<-getTime(gantt,i,which)
  }
  minTime<-apply(c,1,min,na.rm=!strict)
  #minTime<-apply(as.data.frame( minTime),1,function(x) ifelse(is.na(x),-Inf,x)) 
    #mapply(min, getTime(gantt,a,which,human.readable = FALSE),getTime(gantt,b,which,human.readable = FALSE),c,MoreArgs = list(na.rm=TRUE))
#  class(minTime)=c('POSIXt','POSIXct')
  return (minTime)
}
# a is a vector of names
#' @rdname getMinTime
#' @export
getMaxTime<- function (gantt,a,which,strict=T) {
  #Create a list of 0 values to avoid warnings...
  c<-data.frame(min=matrix(rep(0,length(gantt$gantt))))
  for(i in a) {
    c[i]<-getTime(gantt,i,which)
  }
  maxTime<-apply(c,1,max,na.rm=!strict)
  #maxTime<-apply(as.data.frame( maxTime),1,function(x) ifelse(is.na(x),Inf,x)) 
  
  #maxTime<-mapply(max, getTime(gantt,a,which,human.readable = FALSE),getTime(gantt,b,which,human.readable = FALSE),c,MoreArgs = list(na.rm=TRUE))
 # class(maxTime)=c('POSIXt','POSIXct')
  return (maxTime)
}

#' Get duration
#'
#' @param gantt A gantt object (created by makeGantt)
#' @param a An element
#'
#' @return Returns duration in seconds
#' @export
#'
#' @examples
#' response<-matrix(c("{'response':'1/8 17:00;1/8 17:00 - 1/8 17:30;1/8 17:00 - 1/8 18:00;1/8 17:00, 1/8 18:00, 1/8 19:00, 1/8 20:00;1/8 20:30;1/8 20:00 - 1/8 20:30;1/8 19:30 - 1/8 20:30'}"))
#' gantt<-makeGantt(response2,names=c("waitress","actor","pianist","bartender","cleaning","ticketer","musician"),timespan=30,time.format="%d/%m %H:%M")
#' getDuration(gantt,"actor")
#' getDuration(gantt,"waitress")
#' getDuration(gantt,"bartender")
#' getDuration(gantt,"cleaning")

getDuration<-function(gantt,a) {
  numSlots(gantt = gantt,a = a)*gantt$timespan*60
  #getTime(gantt,a,"end")-getTime(gantt,a,"start")
}

expandtime<-function(x,timespan,time.format) {
  y<-c()
  for(i in x) {
    if(!grepl("NaN",i) && grepl(" - ",i)) {
      startend<-strsplit(i," - ")
      start<-strptime(startend[[1]][1],time.format)
      end<-strptime(startend[[1]][2],time.format)
      diff<-difftime(end,start,units = "mins")
      numintervals<-round(as.numeric(diff)/timespan) #Round shouldn't be necassary
      y<-c(y,startend[[1]][1])
      for(j in 1:(numintervals)) {
        y<-c(y,format(start+timespan*60*j,time.format))
      }
    } else {y<-c(y,ifelse(grepl("NaN",i),NA,i))}
  }
  return (y)
}

