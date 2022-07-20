#' Collect JMA Stats Data
#'
#' @param item url
#' @param block_no block number
#' @param year select year
#' @param month select month
#' @param day select date (default `NULL`)
#' @import rlang
#' @importFrom dplyr funs mutate mutate_all mutate_if select
#' @importFrom glue glue
#' @importFrom readr type_convert
#' @importFrom rvest html_table
#' @importFrom stringr str_pad str_remove str_trim
#' @importFrom xml2 read_html
#' @note
#' The parameter `item` chooses one from these:
#' - annually:
#' - monthly:
#' - 3monthly:
#' - 10daily:
#' - mb5daily:
#' - daily:
#' - hourly:
#' - rank:
#' @examples
#' \dontrun{
#' jma_collect(item = "annually", "1284", year = 2017, month = 11)
#' # daily
#' jma_collect(item = "daily", block_no = "0010", year = 2017, month = 11)
#' jma_collect(item = "daily", "0422", year = 2017, month = 11)
#' # hourly
#' jma_collect("hourly", "0010", 2018, 7, 30)
#' # ranking
#' jma_collect("rank", block_no = "47646", year = 2020)
#' }
#' @export
jma_collect <- function(item = NULL,
                        block_no, year, month, day) {

  .blockid <- rlang::enquo(block_no)

  target <-
    jma_url(item, !!.blockid, year, month, day)

  df_raw <-
    xml2::read_html(target$url) %>%
    rvest::html_table(fill = TRUE)

  selected_item <-
    paste0(item, "_", target$station_type)
  vars <- name_sets(selected_item)

  if (item == "annually") {
    df <-
      df_raw[[4]][-c(1:2), ] %>%
      purrr::set_names(vars) %>%
      tweak_df()
  } else if (item == "monthly") {
    df <-
      df_raw[[6]][-c(1:2), ] %>%
      purrr::set_names(vars) %>%
      tweak_df()
  } else if (item == "10daily") {
    df <-
      df_raw[[6]][-c(1:2), ] %>%
      purrr::set_names(vars) %>%
      tweak_df()
  } else if (item == "mb5daily") {
    df <-
      df_raw[[6]][-c(1:2), ] %>%
      purrr::set_names(vars) %>%
      tweak_df()
  } else if (item == "daily") {
    df <-
      .jma_collect_daily(df_raw, vars, target$station_type)
  } else if (item == "hourly") {
    df <-
      .jma_collect_hourly(df_raw, vars)
  } else if (item == "10min") {
    df <-
      .jma_collect_10min(df_raw, vars, target$station_type)
  } else if (item == "rank") {
    value <- period <- NULL
    df <-
      df_raw[[3]]
    df <-
      df %>%
      tidyr::pivot_longer(cols = -c(1, ncol(df)),
                          names_to = "rank") %>%
      tidyr::extract(col = value,
                     into = c("value", "date"),
                     regex = "(.+)\\((.+)\\)") %>%
      purrr::set_names(name_sets(selected_item)) %>%
      dplyr::mutate(period = glue_split_period_char(period,
                                                    collapse = intToUtf8(c(12363L, 12425L))),
                    rank = stringr::str_extract(rank, "[0-9]{1,}")) %>%
      readr::type_convert()
  } else {
    df <- df_raw
  }
  # convert_variable_unit(df) %>%
  #   tibble::as_tibble()
  tibble::as_tibble(df)
}

tweak_df <- function(df) {
  convert_error(df) %>%
    dplyr::mutate(
      dplyr::across(tidyselect::everything(),
                    .fns = list(~ stringr::str_remove_all(., "(]|\\))")),
                    .names = "{.col}")) %>%
    dplyr::mutate(
      dplyr::across(where(is.character),
                    .funs = list(~ stringr::str_trim(., side = "both")),
                    .names = "{.col}")) %>%
    readr::type_convert()
}

.jma_collect_daily <- function(df, vars, station_type) {
  if (station_type == "a1") {
    df <-
      df[[6]][-c(1:2), ]
  } else if (station_type == "s1") {
    df <-
      df[[6]][-c(1:3), ]
  }
  df %>%
    purrr::set_names(vars) %>%
    tweak_df() %>%
    dplyr::mutate(date = as.Date(paste(year,
                                       stringr::str_pad(month, width = 2, pad = "0"),
                                       stringr::str_pad(date, width = 2, pad = "0"), sep = "-"))) %>%
    readr::type_convert()
}

.jma_collect_hourly <- function(df, vars) {
  df <-
    df[[5]][-c(1), ] %>%
    purrr::set_names(vars) %>%
    tweak_df()
  df %>%
    dplyr::mutate(date = lubridate::make_date(year, month, day)) %>%
    dplyr::select(date, dplyr::everything()) %>%
    readr::type_convert()
}

.jma_collect_10min <- function(df, vars, station_type) {
  if (station_type == "a1") {
    df <-
      df[[5]][-c(1:2), ]
  } else if (station_type == "s1") {
    df <-
      df[[5]][-1, ]
  }
  df %>%
    purrr::set_names(vars) %>%
    tweak_df()
}

jma_url <- function(item = NULL,
                    block_no, year, month, day) {
  .blockid <- rlang::enquo(block_no)
  selected_item <- item
  if (identical(selected_item, character(0))) {
    rlang::abort(intToUtf8(c(12371, 12398, 20013, 12363, 12425, 36984, 25246)))
  }
  if (selected_item == "hourly") {
    validate_date(year, month, day)
  }
  if (selected_item %in% c("annually", "rank")) {
    if (rlang::is_missing(year)) {
      year <- ""
      month <- ""
      dummy_year <- 1
      dummy_month <- 1
    } else {
      dummy_year <- year
      dummy_month <- 1
    }
    if (rlang::is_missing(month)) {
      month <- ""
      dummy_month <- 1
    } else {
      dummy_month <- month
    }
    if (selected_item == "rank") {
      if (rlang::is_missing(year)) {
        year <- ""
        dummy_year <- 1
      } else {
        dummy_year <- year
      }
    }
  } else {
    dummy_year <- year
    dummy_month <- month
  }
  if (rlang::is_missing(day)) {
    day <- ""
    dummy_day <- 1
  } else {
    dummy_day <- day
  }
  if (validate_date(dummy_year, dummy_month, dummy_day)) {
    station_info <-
      detect_station_info(.blockid)
    if (!selected_item %in% c("annually", "rank")) {
      station_info$station_type <-
        paste0(station_info$station_type, "1")
    }
    list(
      url = as.character(glue::glue(
        "https://www.data.jma.go.jp/obd/stats/etrn/view/{selected_item}_{station_type}.php?prec_no={prec_no}&block_no={blockid}&year={year}&month={month}&day={day}&view=",
        blockid = rlang::eval_tidy(.blockid),
        station_type = station_info$station_type,
        prec_no = station_info$prec_no
      )),
      station_type = station_info$station_type
    )
  }
}

detect_station_info <- function(.blockid) {
  df_target_station <-
    subset(stations, block_no == rlang::eval_tidy(.blockid)) %>%
    sf::st_drop_geometry() %>%
    dplyr::distinct(block_no, .keep_all = TRUE)
  pref <-
    df_target_station$prec_no
  station_type <-
    ifelse(df_target_station$station_type == intToUtf8(23448), "s", "a")
  station_type <-
    ifelse(df_target_station$station_name %in% c(intToUtf8(c(22825, 22478)),
                                               intToUtf8(c(19982, 35542, 23798)),
                                               intToUtf8(c(23433, 27425, 23994)),
                                               intToUtf8(c(24029, 24179)),
                                               intToUtf8(c(24950, 33391, 38291)),
                                               intToUtf8(c(30427, 23665)),
                                               intToUtf8(c(37857, 21407)),
                                               intToUtf8(c(26481)),
                                               intToUtf8(c(30707, 30000)),
                                               intToUtf8(c(19978, 22823, 27941)),
                                               intToUtf8(c(32654, 27941, 23798)),
                                               intToUtf8(c(38957, 12534, 23798)),
                                               intToUtf8(c(23567, 20516, 36032)),
                                               intToUtf8(c(21338, 22810))
  ),
  # Special pattern
  "a",
  station_type)
  list(
    prec_no = pref,
    station_type = station_type
  )
}

convert_variable_unit <- function(.data) {
  df <-
    dplyr::mutate_at(.data,
                   dplyr::vars(tidyselect::matches("\\(\u2103\\)$")),
                   list(~ units::set_units(., value = "\u2103"))) %>%
    dplyr::mutate_at(dplyr::vars(tidyselect::matches("\\(hPa\\)$")),
                     list(~ units::set_units(., value = "hPa"))) %>%
    dplyr::mutate_at(dplyr::vars(tidyselect::matches("\\(mm\\)$")),
                     list(~ units::set_units(., value = "mm"))) %>%
    dplyr::mutate_at(dplyr::vars(tidyselect::matches("\\(cm\\)$")),
                     list(~ units::set_units(., value = "cm"))) %>%
    dplyr::mutate_at(dplyr::vars(tidyselect::matches("\\(hour\\)$")),
                     list(~ units::set_units(., value = "h"))) %>%
    dplyr::mutate_at(dplyr::vars(tidyselect::matches("\\(m/s\\)$")),
                     list(~ units::set_units(., value = "m/s"))) %>%
    dplyr::mutate_at(dplyr::vars(tidyselect::matches("\\(%\\)$")),
                     list(~ units::set_units(., value = "%")))
  df %>%
    purrr::set_names(stringr::str_remove_all(names(df), "\\(.+\\)"))
}

convert_error <- function(.data) {
  dplyr::mutate_all(.data,
                    .funs = list(~
                      dplyr::if_else(. %in% c(intToUtf8(c(47, 47, 47)), intToUtf8(c(215)), "", intToUtf8(c(35))),
                                     NA_character_, .))) %>%
    dplyr::mutate_all(.funs = list(~
                        dplyr::if_else(. %in% c(intToUtf8(c(45, 45))), "0.0", .)))

}

jma_vars <- list(atmosphere = paste0("atmosphere_",
                        c("land", "surface"), "(hPa)"),
                 precipitation = paste0("precipitation_",
                        c("sum",
                          "max_per_day",
                          "max_1hour",
                          "max_10minutes"), "(mm)"),
                 temperature = paste0("temperature_",
                        c("average",
                          "average_max",
                          "average_min",
                          "max",
                          "min"), "(\u2103)"),
                 humid = paste0("humidity_",
                                c("average", "min"),
                                "(%)"),
                 wind = paste0("wind_",
                          c(paste0(c("average_speed",
                                     "max_speed"),
                                   "(m/s)"),
                            "max_speed_direction",
                            paste0("max_instantaneous_speed", "(m/s)"),
                            "max_instantaneous_direction")),
                 daylight = paste0("daylight_", "(h)"),
                 solar = paste0("solar_irradiance_",
                                c("average"), "(MJ/m^2)"),
                 snow = paste0("snow_",
                        c("fall",
                          "max_fall_day",
                          "depth"),
                        "(cm)"),
                 cloud = paste0("cloud_covering_", c("mean")),
                 condition = paste0("condition_",
                        c("snow_days",
                          "fog_days",
                          "thunder_days")))

name_sets <- function(item) {
    switch(item,
      "daily_a1" = c(
        "date",
        paste0("precipitation_",
              c("sum",
                "max_1hour",
                "max_10minutes"), "(mm)"),
        paste0("temperature_",
               c("average",
                 "max",
                 "min"), "(\u2103)"),
        jma_vars$humid,
        paste0("wind_",
               c("average_speed",
                 "max_speed",
                 "max_speed_direction",
                 "max_instantaneous_speed",
                 "max_instantaneous_direction",
                 "direction_frequency"), "(m/s)"),
        paste0("sunshine_duration_", "(h)"),
        paste0("snow_",
               c("fall",
                 "depth"), "(cm)")
      ),
      "daily_s1" = c(
        "date",
        paste0("pressure_",
               c("average",
                 "sea_level"),
               "(hPa)"),
        paste0("precipitation_",
               c("sum",
                 "max_1hour",
                 "max_10minutes"), "(mm)"),
        paste0("temperature_",
               c("average",
                 "max",
                 "min"), "(\u2103)"),
        jma_vars$humid,
        paste0("wind_",
               c("average_speed",
                 "max_speed",
                 "max_speed_direction",
                 "max_instantaneous_speed",
                 "max_instantaneous_direction"),
               "(m/s)"),
        paste0("sunshine_duration_", "(h)"),
        paste0("snow_",
               c("fall",
                 "depth"),
               "(cm)"),
        paste0("weather",
               c("daytime (06:00-18:00)", "nighttime (18:00-06:00)"))
      ),
      "hourly_a1" = c("time",
                      paste0("precipitation_", "(mm)"),
                      paste0("temperature_", "(\u2103)"),
                      paste0("dew_point_", "(\u2103)"),
                      paste0("vapor_", "(hPa)"),
                      paste0("humidity_", "(%)"),
                      paste0("wind_", c(paste0("speed", "(m/s)"), "direction")),
                      jma_vars$daylight,
                      paste0("snow_", c("fall_moment", "fall_period"), "(cm)")),
      "hourly_s1" = c("time",
                      jma_vars$atmosphere,
                      paste0("precipitation_", "(mm)"),
                      paste0("temperature_", "(\u2103)"),
                      paste0("dew_point_", "(\u2103)"),
                      paste0("vapor_", "(hPa)"),
                      paste0("humidity_", "(%)"),
                      paste0("wind_", c(paste0("speed", "(m/s)"), "direction")),
                      jma_vars$daylight,
                      paste0("solar_irradiance_", "(MJ/m^2)"),
                      paste0("snow_", c("fall_moment", "fall_period"), "(cm)"),
                      paste0("weather"),
                      paste0("cloud_covering"),
                      paste0("visibility_", "(km)")
                      ),
      "10min_a1" = c("time",
                     paste0("precipitation_", "(mm)"),
                     paste0("temperature_", "(\u2103)"),
                     paste0("relative_humidity", "(%)"),
                     paste0("wind_",
                            c(c(paste0("speed", "(m/s)"), "direction"),
                              c(paste0("max_instantaneous_speed", "(m/s)"),
                                "max_instantaneous_direction"))),
                     paste0("daylight_", "(min)")
                     ),
      "10min_s1" = c("time",
                     jma_vars$atmosphere,
                     paste0("precipitation_", "(mm)"),
                     paste0("temperature_", "(\u2103)"),
                     paste0("relative_humidity", "(%)"),
                     paste0("wind_",
                            c(c(paste0("speed", "(m/s)"), "direction"),
                              c(paste0("max_instantaneous_speed", "(m/s)"),
                                "max_instantaneous_direction"))),
                     paste0("daylight_", "(min)")),
      "10daily_a1" = c(
        "month",
        "season",
        jma_vars$precipitation,
        jma_vars$temperature,
        jma_vars$humid,
        jma_vars$wind,
        jma_vars$daylight,
        jma_vars$snow
      ),
      "10daily_s1" = c(
        "month",
        "season",
        jma_vars$atmosphere,
        jma_vars$precipitation,
        jma_vars$temperature,
        jma_vars$humid,
        jma_vars$wind,
        jma_vars$daylight,
        jma_vars$solar,
        jma_vars$snow,
        jma_vars$cloud,
        jma_vars$condition
      ),
      "mb5daily_a1" = c(
        "month",
        "interval_5days",
        "day",
        jma_vars$precipitation,
        jma_vars$temperature,
        jma_vars$humid,
        jma_vars$wind,
        jma_vars$daylight,
        jma_vars$snow),
      "mb5daily_s1" = c(
        "month",
        "interval_5days",
        "day",
        jma_vars$atmosphere,
        jma_vars$precipitation,
        jma_vars$temperature,
        jma_vars$humid,
        jma_vars$wind,
        jma_vars$daylight,
        jma_vars$solar,
        jma_vars$snow,
        jma_vars$cloud,
        jma_vars$condition),
      "monthly_a1" = c("month",
                       jma_vars$precipitation,
                       jma_vars$temperature,
                       jma_vars$humid,
                       jma_vars$wind,
                       jma_vars$daylight,
                       jma_vars$snow),
      "monthly_s1" = c("month",
                       jma_vars$atmosphere,
                       jma_vars$precipitation,
                       jma_vars$temperature,
                       jma_vars$humid,
                       jma_vars$wind,
                       jma_vars$daylight,
                       jma_vars$solar,
                       jma_vars$snow,
                       jma_vars$cloud,
                       jma_vars$condition),
      "annually_a" = c("year",
                       jma_vars$precipitation,
                       jma_vars$temperature,
                       jma_vars$humid,
                       jma_vars$wind,
                       jma_vars$daylight,
                       jma_vars$snow),
      "annually_s" = c("year",
                       jma_vars$atmosphere,
                       jma_vars$precipitation,
                       jma_vars$temperature,
                       jma_vars$humid,
                       jma_vars$wind,
                       jma_vars$daylight,
                       jma_vars$solar,
                       jma_vars$snow,
                       jma_vars$cloud,
                       jma_vars$condition),
    "rank_s" = c("element", "period", "rank", "value", "date"),
    "rank_a" = c("element", "period", "rank", "value", "date"))
}
