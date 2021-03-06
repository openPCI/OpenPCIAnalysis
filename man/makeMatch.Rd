% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/match.R
\name{makeMatch}
\alias{makeMatch}
\title{Make a match object}
\usage{
makeMatch(resp, variables.in.columns = F)
}
\arguments{
\item{resp}{a vector of match responses from TAO}

\item{variables.in.columns}{Boolean. Set to use columns as variables instead of rows.}
}
\value{
Returns a matchObject
}
\description{
Make a match object
}
\examples{
matchresponse<-c("[mail1 annoyed; mail4 annoyed; mail2 angry]","[mail1 confused]")
matchObject<-makeMatch(matchresponse)
}
\seealso{
\code{\link[=scoreMatch]{scoreMatch()}}, \code{\link[=explodeMatch]{explodeMatch()}}
}
