% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{kgss_sample}
\alias{kgss_sample}
\title{A Sample of Korean General Social Survey Data, 2023}
\format{
A data frame with the following variables.
\describe{
\item{\code{year}}{a numeric vector communicating the year of the survey}
\item{\code{respid}}{a numeric vector communicating a unique identifier for
the respondent}
\item{\code{age}}{a numeric vector communicating the age of the respondent}
\item{\code{female}}{a numeric vector communicating whether the respondent
self-identifies as a woman or a man}
\item{\code{employed}}{a dummy variable for whether the respondent is employed or 'not employed'}
\item{\code{unived}}{a dummy variable for whether the respondent has a four-year university degree}
\item{\code{netuse}}{a numeric vector for hours of internet usage for the respondent}
\item{\code{ideo}}{a numeric vector communicating the ideology of the
respondent on a 1-5 scale. 1 = "very liberal". 5 = "very conservative"}
\item{\code{si_gbh}}{a numeric vector of the extent to which the respondent considers
gender-based hatred (toward both men and women) to be a serious issue. 1 = 'very serious'. 5 = 'not serious at all'}
\item{\code{satisfin}}{a numeric vector communicating the respondent's satisfaction
with their financial situation. 1 = 'very dissatisfied'. 5 = 'very satisfied'}
\item{\code{fp_mord}}{a numeric vector for whether the respondent thinks maintaining order is the most important
priority for South Korea.}
\item{\code{fpcat}}{a character vector for what the respondent believes is the
most important priority for South Korea. Possible values: "Maintain Order",
"Fight Rising Prices", "Give People More Say", "Protect Freedom of Speech".}
\item{\code{cntryaffq}}{a character vector for the country to which the respondent
feels closest. Possible values include "USA", "China", "North Korea", "Russia", and "Japan".}
}
}
\usage{
kgss_sample
}
\description{
This is a simple sample of the Korean General Social Survey (KGSS) data from
2023.
}
\details{
Missingness is substantial for one reason or the other. The data are complete
cases only. It's not problematic for this purpose, but I did want to make a
note of it.

Data were created based on the English version of the data made available by
the Survey Research Center at Sung Kyun Kwan University.
}
\keyword{datasets}
