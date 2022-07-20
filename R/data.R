globalVariables(c("stations", "tide_station", "earthquake_station"))

#' Japan Meteorological Agency's Stations list
#'
#' @description XXX
#' @format A data frame with 1321 rows 14 variables:
#' \itemize{
#'   \item{area}
#'   \item{station_no}
#'   \item{station_type}
#'   \item{station_name}
#'   \item{address}
#'   \item{elevation}
#'   \item{observation_begin}
#'   \item{note1}
#'   \item{note1}
#'   \item{note2}
#'   \item{katakana}
#'   \item{prec_no}
#'   \item{block_no}
#'   \item{pref_code}
#'   \item{geometry}
#' }
#' @examples
#' head(stations)
#' dim(stations)
"stations"

#' Tidal observation stations of Japan Meteorological Agency
#'
#' @description XXX
#' @format A data frame with 1809 rows 7 variables
#' @examples
#' head(tide_station)
"tide_station"

#' Japan Meteorological Agency's earthquake observe stations
#'
#' @description XXX
#' @format A simple feature data frame with 671 rows  7 variables
#' @examples
#' head(earthquake_station)
"earthquake_station"
