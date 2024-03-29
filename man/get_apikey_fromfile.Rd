% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_apikey_from_file.r
\name{get_apikey_fromfile}
\alias{get_apikey_fromfile}
\title{Get value of algaebase API key from a file}
\usage{
get_apikey_fromfile(keyfile)
}
\arguments{
\item{keyfile}{path to text file}
}
\value{
api key as character string (invisibly)
}
\description{
Get value of algaebase API key from a file
}
\examples{
\dontrun{apikey<-get_apikey_fromfile("keyfile.txt")}
}
