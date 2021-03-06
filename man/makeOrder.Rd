% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/order.R
\name{makeOrder}
\alias{makeOrder}
\title{Make an order object}
\usage{
makeOrder(resp)
}
\arguments{
\item{resp}{A response vector from TAO}
}
\value{
Returns an orderObject
}
\description{
Make an order object
}
\examples{
orderresp<-c("<'audition'; 'actor'; 'prof reading'; 'costumes'>", "<'manuscript'; 'prof reading'; 'costumes'; 'actor'>")
orderObject<-makeOrder(orderresp)
}
