% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{make_filename}
\alias{make_filename}
\title{Create Filename}
\usage{
make_filename(year)
}
\arguments{
\item{year}{An integer or string that corresponds to a year.}
}
\value{
The corresponding filename as a string for a given year.
}
\description{
This function creates the file name for the accident data for a given year.
}
\examples{
make_filename("2013")
make_filename(2015)

}
