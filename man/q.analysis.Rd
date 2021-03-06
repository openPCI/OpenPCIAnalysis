% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/q.method.R
\name{q.analysis}
\alias{q.analysis}
\title{Do a Q-Method analysis on the dataset}
\usage{
q.analysis(
  q = NULL,
  q.df = get.q.df(q),
  only.complete = T,
  nfactors = 3,
  rotation = "varimax",
  cor.method = "kendall",
  nsteps = NULL,
  indet = "qindtest"
)
}
\arguments{
\item{q}{A, open.pci.q object from \code{\link[=get.q]{get.q()}}}

\item{q.df}{Alternatively to q, provide a data.frame from \code{\link[=get.q.df]{get.q.df()}}}

\item{only.complete}{Boolean. Only include persons who have used all statements.}

\item{nfactors}{The number of factors to extract.}

\item{rotation}{The type of rotation to use}

\item{cor.method}{Which method to use for the correlations in \code{\link[=cor]{cor()}} ("pearson", "spearmann", "kendall").
Pearson is default in qmethod, but given that the data is ordinal, not interval scaled, Kendall's tau is default in q.analysis.}

\item{nsteps}{The number of repetitions to use when bootstrapping (to get estimates of standard errors and bias). See \code{\link[=qmboots]{qmboots()}}.}

\item{indet}{Indeterminacy method ("qindtest", "procrustes"). Default is qindtest, use procrustes for more than three factors. See \code{\link[=qmboots]{qmboots()}}.}
}
\value{
Returns an object of QmethodRes. See \code{\link[=qmethod]{qmethod()}} for explanation.
}
\description{
Do a Q-Method analysis on the dataset
}
\note{
The analysis is done by the qmethod-package.
}
\examples{
qjson<-c("[[\"Running\"],[\"Walking\",\"Crawling\"],[\"Jumping\",\"Sprinting\",\"Jogging\"],[\"Strolling\",\"\",\"Standing\",\"Sitting\"]]",
         "[[\"Sitting\"],[\"Strolling\",\"Jogging\"],[\"Standing\",\"Sprinting\",\"Crawling\"],[\"Walking\",\"Running\",\"Jumping\"]]")
q<-get.q(qjson)
q.analysis(q)
}
\seealso{
\code{\link[=qmethod]{qmethod()}}, \code{\link[=qmboots]{qmboots()}}, \code{\link[=cor]{cor()}}, \code{\link[=plot.QmethodRes]{plot.QmethodRes()}}.
}
