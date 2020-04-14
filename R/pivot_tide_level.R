#' Convert and split tidal level data
#'
#' @param data tidal level data
#' @examples
#' \dontrun{
#' read_tide_level("https://www.data.jma.go.jp/gmd/kaiyou/data/db/tide/suisan/txt/2020/TK.txt") %>%
#'   pivot_tide_level()
#' }
#' @seealso [read_tide_level()]
#' @return List to store two datasets. hourly observation data and tide.
#' @export
pivot_tide_level <- function(data) {
  list(
    hourly = pivot_tide_hourly(data),
    tide = pivot_tide_tide(data)
  )
}

hour <- value <- tide_level <- time <- count <- NULL

pivot_tide_hourly <- function(data) {
  data %>%
    tidyr::pivot_longer(
      tidyselect::starts_with("hry"),
      names_to = "hour",
      values_to = "value",
      names_prefix = "hry_"
    ) %>%
    dplyr::select(date, stn, hour, value) %>%
    dplyr::transmute(datetime = lubridate::as_datetime(paste(date,
                                                      paste0(hour,
                                                             ":00:00"))),
              stn,
              value)
}

pivot_tide_tide <- function(data) {
  dplyr::left_join(
    data %>%
      dplyr::select(-tidyselect::starts_with("hry")) %>%
      tidyr::pivot_longer(tidyselect::contains("tide_hm"),
                          names_to = c("tide_level", "count"),
                          names_pattern = "(.*)_tide_hm_obs([0-9])",
                          values_to = "time",
                          names_prefix = "_tide_hm_obs") %>%
      dplyr::select(date, stn, tide_level, count, time),
    data %>%
      dplyr::select(-tidyselect::starts_with("hry")) %>%
      tidyr::pivot_longer(tidyselect::contains("tidal_level_cm"),
                          names_to = c("tide_level", "count"),
                          names_pattern = "(.*)_tide_tidal_level_cm_obs([0-9])",
                          values_to = "value") %>%
      dplyr::select(date, stn, tide_level, count, value),
    by = c("date", "stn", "tide_level", "count")) %>%
    dplyr::filter(!is.na(time))
}
