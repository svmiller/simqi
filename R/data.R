#' @importFrom tibble tibble
NULL

#' A Sample of SOM Institute Data, 2019-2020
#'
#' This is a simple sample of the SOM (Society-Opinion-Media) data that is
#' run by the University of Gothenburg. The sample comes from the cumulative data
#' set (v. 2021-1) for observations in 2019-2020. The SOM Institute
#' Cumulative Dataset contains data from the National SOM study, which is an
#' annually repeated cross-sectional self-administered mail survey conducted in
#' Sweden since 1986. I think of it as a Swedish corollary to the General Social
#' Survey in the United States. I'll use it for it various testing purposes.
#'
#' @format A data frame with 2841 observations on the following 14 variables.
#' \describe{
#' \item{\code{year}}{a numeric vector communicating the year of the survey}
#' \item{\code{idnr}}{a numeric vector communicating a unique identifier for
#' the respondent}
#' \item{\code{lan}}{a character vector for the county in which the respondent
#' lives}
#' \item{\code{lptrust}}{a numeric vector communicating what I term "political trust"
#' of the respondent. I include more information about this variable in the details
#' section.}
#' \item{\code{satisdem}}{a numeric vector, ranging from 1-4, communicating
#' satisfaction with democracy in Sweden. 1 = not at all satisfied. 4 =
#' very satisfied.}
#' \item{\code{trust_rf}}{a numeric vector, ranging from 1-5, communicating
#' trust in the royal family of Sweden. 1 = very low trust. 5 = very high trust.}
#' \item{\code{attitude_eu}}{a numeric vector, ranging from 1-5, communicating
#' the attitude of the respondent toward the European Union. 1 = very negative.
#' 5 = very positive.}
#' \item{\code{age}}{a numeric vector communicating the age of the respondent}
#' \item{\code{female}}{a numeric vector communicating whether the respondent
#' self-identifies as a woman or a man}
#' \item{\code{edu3}}{a numeric vector ranging from 1-3 communicating an
#' education-level attained. 1 = "low" (below grade 9). 2 = "medium" (above
#' grade 9, but below university). 3 = "high" (i.e. at least some university)}
#' \item{\code{ideo}}{a numeric vector communicating the ideology of the
#' respondent on a 1-5 scale. 1 = "clearly to the left". 5 = "clearly to the
#' right"}
#' \item{\code{hinc}}{a numeric vector communicating the gross household income
#' of the respondent on a 1-5 scale. 1 = "very low". 5 = "very high".}
#' \item{\code{resarea}}{a numeric vector communicating the area where the
#' respondent lives. 1 = "rural area". 2 = "village". 3 = "city/town". 4 =
#' "Stockholm/Gothenburg/Malmö".}
#' \item{\code{interestp}}{a numeric vector communicating the respondent's
#' interest in politics. 1 = "not at all interested". 4 = "very interested".}
#' }
#'
#' @details
#'
#' Missingness is substantial for one reason or the other. The data are complete
#' cases only. It's not problematic for this purpose, but I did want to make a
#' note of it.
#'
#' The political trust variable is a simple latent estimate derived from a graded
#' response model of the items from the original data on trust in government
#' (`aa10a`), trust in parliament (`aa10n`), trust in the political parties
#' (`aa10q`), and trust in Swedish politicians (`ab12`). The first three items
#' were on 1-5 scales while the last one (about Swedish politicians) is on a 1-4
#' scale. All items were reverse coded from their original scales and the user
#' should interpret the ensuing latent estimate to be communicating higher
#' political trust with higher values on the scale. The user is also free to
#' question just how valid of a measure of political trust this is, though I will
#' only add that the factor loadings for all four items were as low as .81 and as
#' high as .91. The proportional variance is .764.
#'
#' The variables for satisfaction with democracy, trust in the royal family,
#' attitude about the European Union, and interest in politics are reverse coded
#' from their original scale.
#'
#' SOM is unique from other long-standing survey data sets of which I'm aware
#' by allowing respondents to self-identify as some other gender. In 2019 and
#' 2020, only 71 of 21,195 respondents self-identified this way (before any
#' other case-exclusions). I remove these observations from the data.
#'
#' If I understand the codebook correctly, the household income variable is coded
#' by SOM's researchers and is not a self-placement by the respondent.
#'
#' You may want to explicitly factor the residential area variable, though this
#' is basically how it was presented in the data.
#'
"som_sample"


#' A Sample of Korean General Social Survey Data, 2023
#'
#' This is a simple sample of the Korean General Social Survey (KGSS) data from
#' 2023.
#'
#'
#' @format A data frame with the following variables.
#' \describe{
#' \item{\code{year}}{a numeric vector communicating the year of the survey}
#' \item{\code{respid}}{a numeric vector communicating a unique identifier for
#' the respondent}
#' \item{\code{age}}{a numeric vector communicating the age of the respondent}
#' \item{\code{female}}{a numeric vector communicating whether the respondent
#' self-identifies as a woman or a man}
#' \item{\code{employed}}{a dummy variable for whether the respondent is employed or 'not employed'}
#' \item{\code{unived}}{a dummy variable for whether the respondent has a four-year university degree}
#' \item{\code{netuse}}{a numeric vector for hours of internet usage for the respondent}
#' \item{\code{ideo}}{a numeric vector communicating the ideology of the
#' respondent on a 1-5 scale. 1 = "very liberal". 5 = "very conservative"}
#' \item{\code{si_gbh}}{a numeric vector of the extent to which the respondent considers
#' gender-based hatred (toward both men and women) to be a serious issue. 1 = 'very serious'. 5 = 'not serious at all'}
#' \item{\code{satisfin}}{a numeric vector communicating the respondent's satisfaction
#' with their financial situation. 1 = 'very dissatisfied'. 5 = 'very satisfied'}
#' \item{\code{fp_mord}}{a numeric vector for whether the respondent thinks maintaining order is the most important
#' priority for South Korea.}
#' \item{\code{fpcat}}{a character vector for what the respondent believes is the
#' most important priority for South Korea. Possible values: "Maintain Order",
#' "Fight Rising Prices", "Give People More Say", "Protect Freedom of Speech".}
#' \item{\code{cntryaffq}}{a character vector for the country to which the respondent
#' feels closest. Possible values include "USA", "China", "North Korea", "Russia", and "Japan".}
#' }
#'
#' @details
#'
#' Missingness is substantial for one reason or the other. The data are complete
#' cases only. It's not problematic for this purpose, but I did want to make a
#' note of it.
#'
#' Data were created based on the English version of the data made available by
#' the Survey Research Center at Sung Kyun Kwan University.

"kgss_sample"
