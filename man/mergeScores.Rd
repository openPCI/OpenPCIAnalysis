% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mergeScores.R
\name{mergeScores}
\alias{mergeScores}
\title{Merge scores from pre-coded items}
\usage{
mergeScores(result, resp, column, prefix = "", test.taker = NULL)
}
\arguments{
\item{result}{result of scoring script. A matrix.}

\item{resp}{Response data.frame from TAO or from pre-coded items.}

\item{column}{String. The beginning of the name of the columns to merge with the results. If more columns start with the provided string, all of them are included.}

\item{prefix}{Text to put before the variable name}

\item{test.taker}{vector of test taker-ids for the response data.frame. If left empty, first column in resp is used as test.taker.}
}
\value{
returns a result matrix
}
\description{
Merge scores from pre-coded items
}
\details{
The data from \code{resp} are merged into \code{result} based on the \code{test.taker} column (either 1st column in \code{resp} or the \code{test.taker} vector). Both numeric scores and scores which are part of a JSON object are processed.

MergeScores returns a data.frame with the same number of rows as \code{result}.

If there are duplets in the test-takers in \code{resp}, we first check if the order of test.takers in \code{result} and \code{resp} are exactly the same. If so, we \code{cbind} the scores (one by one).

If not, we unify duplicate \code{test.takers} by giving them the highest score they have achieved, and then merge with \code{result} - giving duplicate \code{result} \code{test.takers} the same highest value.
}
\examples{
result<-data.frame(id=c("a","b","a","b","c","d","a"))
resp<-data.frame(id=c("b","b","a","a","c","d","a"),X1_result=c(1,NA,2,2,1,3,NA),X2_party=c(2,1,3,1,2,3,NA),X20_dont=c(NA,2,1,2,1,3,NA))

result<-mergeScores(result=result,resp=resp,column="X1",prefix="CP_")
# Make sure not to import data from item X20
result<-mergeScores(result,resp,"X2[^0-9]","TM2_")
}
