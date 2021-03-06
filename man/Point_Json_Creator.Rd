% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Point_Json_Creator.R
\name{Point_Json_Creator}
\alias{Point_Json_Creator}
\title{Point_Json_Creator}
\usage{
Point_Json_Creator(points, geometry = "geometry", wkid = "4326")
}
\arguments{
\item{points}{is expected to be a SF object with boundary data}

\item{geometry}{is expected to be a string with the field name containing the geometry data}

\item{wkid}{a text string indicating the well know id to be used.}
}
\value{
a json string
}
\description{
Takes an SF object with points data and coverts it to a ESRI format json string
}
\examples{

}
