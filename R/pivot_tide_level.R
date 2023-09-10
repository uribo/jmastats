#' Convert and split tidal level data
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' @param data tidal level data
#' @examples
#' read_tide_level(system.file("dummy/tide.txt", package = "jmastats")) |>
#'   pivot_tide_level()
#' @seealso [read_tide_level()]
#' @return List to store two datasets containing hourly and tide level data.
#' @export
pivot_tide_level <- function(data) {
  list(
    hourly = pivot_tide_hourly(data),
    tide = pivot_tide_tide(data)
  )
}

hour <- tide_value <- tide_level <- time <- count <- datetime <- NULL

tide_drop_units <- function(data) {
  data |>
    dplyr::mutate(
      dplyr::across(c(tidyselect::num_range("hry_", range = seq.int(0, 23), width = 2),
                      tidyselect::contains("tide_level")),
                    units::drop_units))
}

pivot_tide_hourly <- function(data) {
  data |>
    tide_drop_units() |>
    tidyr::pivot_longer(
      tidyselect::starts_with("hry"),
      names_to = "hour",
      values_to = "tide_value",
      names_prefix = "hry_"
    ) |>
    dplyr::mutate(datetime = lubridate::as_datetime(paste(date,
                                                      paste0(hour,
                                                             ":00:00"))),
              tide_value = units::set_units(tide_value, "cm")) |>
    dplyr::select(datetime, stn, tide_value)
}

pivot_tide_tide <- function(data) {
  d <-
    data |>
    tide_drop_units()
  dplyr::left_join(
    d |>
      dplyr::select(!tidyselect::starts_with("hry")) |>
      tidyr::pivot_longer(tidyselect::contains("tide_hm"),
                          names_to = c("tide_level", "count"),
                          names_pattern = "(.*)_tide_hm_obs([0-9])",
                          values_to = "time",
                          names_prefix = "_tide_hm_obs") |>
      dplyr::select(date, stn, tide_level, count, time),
    d |>
      dplyr::select(!tidyselect::starts_with("hry")) |>
      tidyr::pivot_longer(tidyselect::contains("tide_level"),
                          names_to = c("tide_level", "count"),
                          names_pattern = "(.*)_tide_level_obs([0-9])",
                          values_to = "tide_value") |>
      dplyr::select(date, stn, tide_level, count, tide_value) |>
      dplyr::mutate(tide_value = units::set_units(tide_value, "cm")),
    by = c("date", "stn", "tide_level", "count")) |>
    dplyr::filter(!is.na(time))
}
