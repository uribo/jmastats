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
#' @examples
#' \dontrun{
#' jma_collect(item = "annually", "1284", year = 2017, month = 11)
#' # daily
#' jma_collect(item = "daily", block_no = "0010", year = 2017, month = 11)
#' jma_collect(item = "daily", "0422", year = 2017, month = 11)
#' # hourly
#' jma_collect("hourly", "0010", 2018, 7, 30)
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

  selected_item <- paste0(item, "_", target$station_type)

  if (item == "annually") {
      df <-
        df_raw[[4]][-c(1:2), ]

      names(df) <-
        name_sets(selected_item)

      df <-
        convert_error(df) %>%
        dplyr::mutate_all(.funs = dplyr::funs(stringr::str_remove(., "]"))) %>%
        dplyr::mutate_if(is.character,.funs = dplyr::funs(stringr::str_trim(., side = "both"))) %>%
        readr::type_convert()

  } else if (item == "monthly" & target$station_type == "a1") {
    df <-
      df_raw[[6]][-c(1:2), ]

    names(df) <-
      name_sets(selected_item)

    df <-
      convert_error(df) %>%
      dplyr::mutate_all(.funs = dplyr::funs(stringr::str_remove(., "]"))) %>%
      dplyr::mutate_if(is.character,.funs = dplyr::funs(stringr::str_trim(., side = "both"))) %>%
      readr::type_convert()

  } else if (item == "10daily" & target$station_type == "a1") {

    df <-
      df_raw[[6]][-c(1:2), ]

    names(df) <-
      name_sets(selected_item)

    df <-
      convert_error(df) %>%
      dplyr::mutate_all(.funs = dplyr::funs(stringr::str_remove(., "]"))) %>%
      dplyr::mutate_if(is.character,.funs = dplyr::funs(stringr::str_trim(., side = "both"))) %>%
      readr::type_convert()

  } else if (item == "daily" & target$station_type == "a1") {
    df <-
      df_raw[[6]][-c(1:2), ]

    names(df) <-
      name_sets(selected_item)

    df <-
      convert_error(df) %>%
      dplyr::mutate_all(.funs = dplyr::funs(stringr::str_remove(., "]"))) %>%
      dplyr::mutate_if(is.character,.funs = dplyr::funs(stringr::str_trim(., side = "both"))) %>%
      dplyr::mutate(date = as.Date(paste(year,
                                  stringr::str_pad(month, width = 2, pad = "0"),
                                  stringr::str_pad(date, width = 2, pad = "0"), sep = "-"))) %>%
      readr::type_convert()
  } else if (item == "daily" & target$station_type == "s1") {
    df <-
      df_raw[[6]][-c(1:3), ]

    names(df) <-
      name_sets(selected_item)

    df <-
      convert_error(df) %>%
      dplyr::mutate_all(.funs = dplyr::funs(stringr::str_remove(., "]"))) %>%
      dplyr::mutate_if(is.character,.funs = dplyr::funs(stringr::str_trim(., side = "both"))) %>%
      dplyr::mutate(date = as.Date(paste(year,
                                         stringr::str_pad(month, width = 2, pad = "0"),
                                         stringr::str_pad(date, width = 2, pad = "0"), sep = "-"))) %>%
      readr::type_convert()

  } else if (item == "hourly") {
    if (target$station_type == "a1") {
      df <-
        df_raw[[5]][-c(1), ]

      names(df) <-
        name_sets(selected_item)

      df <-
        convert_error(df) %>%
        dplyr::mutate_all(.funs = dplyr::funs(stringr::str_remove(., "]"))) %>%
        dplyr::mutate_if(is.character,.funs = dplyr::funs(stringr::str_trim(., side = "both"))) %>%
        readr::type_convert()
    } else if (target$station_type == "s1") {
      df <-
        df_raw[[5]][-c(1), ]

      names(df) <-
        name_sets(selected_item)

      df <-
        convert_error(df) %>%
        dplyr::mutate_all(.funs = dplyr::funs(stringr::str_remove(., "]"))) %>%
        dplyr::mutate_if(is.character,.funs = dplyr::funs(stringr::str_trim(., side = "both"))) %>%
        readr::type_convert()
    }

    df <-
      df %>%
      dplyr::mutate(date = lubridate::make_date(year, month, day)) %>%
      dplyr::select(date, dplyr::everything()) %>%
      readr::type_convert()
  } else if (item == "monthly" & target$station_type == "a1") {
    df <-
      df_raw[[6]][-c(1:2), ]

    names(df) <-
      name_sets(selected_item)

    df <-
      convert_error(df) %>%
      dplyr::mutate_all(.funs = dplyr::funs(stringr::str_remove(., "]"))) %>%
      dplyr::mutate_if(is.character,.funs = dplyr::funs(stringr::str_trim(., side = "both"))) %>%
      readr::type_convert()

  } else {
    df <- df_raw
  }
  # convert_variable_unit(df) %>%
  #   tibble::as_tibble()
  tibble::as_tibble(df)
}

jma_url <- function(item = NULL,
                    block_no, year, month, day) {
  .blockid <- rlang::enquo(block_no)

  selected_item <- item

  if (identical(selected_item, character(0))) {
    rlang::abort(intToUtf8(c(12371, 12398, 20013, 12363, 12425, 36984, 25246)))
  }

  if (selected_item == "annually" & rlang::is_missing(year)) {
    year <- ""
    dummy_year <- 1
  } else {
    dummy_year <- year
  }
  if (rlang::is_missing(day)) {
    day <- ""
    dummy_day <- 1
  } else {
    dummy_day <- day
  }

  if (validate_date(dummy_year, month, dummy_day)) {
    df_target_station <-
      subset(stations, block_no == rlang::eval_tidy(.blockid)) %>%
      dplyr::distinct(block_no, .keep_all = TRUE)

    pref <- df_target_station$prec_no
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
                                                   intToUtf8(c(26481))
                                                   ),
             # Special pattern
             "a",
             station_type)

    if (selected_item != "annually") {
      station_type <- paste0(station_type, "1")
    }

    list(
      url = as.character(glue::glue(
        "http://www.data.jma.go.jp/obd/stats/etrn/view/{selected_item}_{station_type}.php?prec_no={pref}&block_no={blockid}&year={year}&month={month}&day={day}&view=",
        blockid = rlang::eval_tidy(.blockid)
      )),
      station_type = station_type
    )
  }

}

convert_variable_unit <- function(.data) {
  df <-
    dplyr::mutate_at(.data,
                   dplyr::vars(dplyr::contains("\u2103")), dplyr::funs(units::set_units(., value = "\u2103"))) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("hPa")), dplyr::funs(units::set_units(., value = "hPa"))) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("mm")), dplyr::funs(units::set_units(., value = "mm"))) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("cm")), dplyr::funs(units::set_units(., value = "cm"))) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("hour")), dplyr::funs(units::set_units(., value = "hour"))) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("m/s")), dplyr::funs(units::set_units(., value = "m/s"))) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("%")), dplyr::funs(units::set_units(., value = "%")))

  names(df) <-
    stringr::str_remove_all(names(df), "\\(.+\\)")

  df
}

convert_error <- function(.data) {
  dplyr::mutate_all(.data,
                    .funs = dplyr::funs(
                      dplyr::if_else(. %in% c(intToUtf8(c(47, 47, 47)), intToUtf8(c(215)), "", intToUtf8(c(35))),
                                     NA_character_, .))) %>%
    dplyr::mutate_all(.funs = dplyr::funs(
                        dplyr::if_else(. %in% c(intToUtf8(c(45, 45))), "0.0", .)))

}

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
        paste0("humidity_",
               c("average",
                 "min"), "(%)"),
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
                      paste0("wind_", c("speed", "direction"), "(m/s)"),
                      paste0("dailight_", "(h)"),
                      paste0("snow_", c("fall_moment", "fall_period"), "(cm)")),
      "hourly_s1" = c("time",
                      paste0("atmosphere_",
                      c("land", "surface"), "(hPa)"),
                      paste0("precipitation_", "(mm)"),
                      paste0("temperature_", "(\u2103)"),
                      paste0("dew_point_", "(\u2103)"),
                      paste0("vapor_", "(hPa)"),
                      paste0("humidity_", "(%)"),
                      paste0("wind_", c("speed", "direction"), "(m/s)"),
                      paste0("daylight_", "(h)"),
                      paste0("solar_irradiance_", "(MJ/m^2)"),
                      paste0("snow_", c("fall_moment", "fall_period"), "(cm)"),
                      paste0("weather"),
                      paste0("cloud_covering"),
                      paste0("visibility_", "(km)")
                      ),
      "10daily_a1" = c(
        "month",
        "season",
        paste0("precipitation_",
               c("sum",
                 "max_per_day",
                 "max_1hour",
                 "max_10minutes"), "(mm)"),
        paste0("temperature_",
               c("average",
                 "average_max",
                 "average_min",
                 "max",
                 "min"), "(\u2103)"),
        paste0("wind_",
               c("average_speed",
                 "max_speed",
                 "max_speed_direction",
                 "max_instantaneous_speed",
                 "max_instantaneous_direction"),
               "(m/s)"),
        paste0("daylight_", "(h)"),
        paste0("snow_",
               c("fall",
                 "max_fall_day",
                 "depth"), "(cm)")
      ),
      "10daily_s1" = c(
        "month",
        "season",
        paste0("atmosphere_",
               c("land", "surface"), "(hPa)"),
        paste0("precipitation_",
               c("max",
                 "max_per_day",
                 "max_1hour",
                 "max_10minutes"), "(mm)"),
        paste0("temperature_",
               c("average",
                 "average_max",
                 "average_min",
                 "max",
                 "min"), "(\u2103)"),
        paste0("humidity_", c("mean", "min"), "(%)"),
        paste0("wind_",
               c("average_speed",
                 "max_speed",
                 "max_speed_direction",
                 "max_instantaneous_speed",
                 "max_instantaneous_direction"),
               "(m/s)"),
        paste0("dailight_", "(h)"),
        paste0("solar_irradiance_",
               c("average"), "(MJ/m^2)"),
        paste0("snow_",
               c("fall",
                 "max_fall_day",
                 "depth"), "(cm)"),
        paste0("cloud_covering_", c("mean")),
        paste0("condition",
               c("snow_days",
                 "fog_days",
                 "thunder_days"))
      ),
      "mb5daily_a1" = c(
        "month",
        "interval_5days",
        "day",
        paste0("precipitation_",
               c("max",
                 "max_per_day",
                 "max_1hour",
                 "max_10minutes"), "(mm)"),
        paste0("temperature_",
               c("average",
                 "average_max",
                 "average_min",
                 "max",
                 "min"), "(\u2103)"),
        paste0("wind_",
               c("average_speed",
                 "max_speed",
                 "max_speed_direction",
                 "max_instantaneous_speed",
                 "max_instantaneous_direction"),
               "(m/s)"),
        paste0("daylight_", "(h)"),
        paste0("snow_",
               c("fall",
                 "max_fall_day",
                 "depth"), "(cm)")
        ),
      "mb5daily_s1" = c(
        "month",
        "interval_5days",
        "day",
        paste0("atmosphere_",
               c("land", "surface"), "(hPa)"),
        paste0("precipitation_",
               c("max",
                 "max_per_day",
                 "max_1hour",
                 "max_10minutes"),
               "(mm)"),
        paste0("temperature_",
               c("average",
                 "average_max",
                 "average_min",
                 "max",
                 "min"),
               "(\u2103)"),
        paste0("humidity", c("mean", "min"), "(%)"),
        paste0("wind_",
               c("average_speed",
                 "max_speed",
                 "max_speed_direction",
                 "max_instantaneous_speed",
                 "max_instantaneous_direction"),
               "(m/s)"),
        paste0("daylight_", "(h)"),
        paste0("solar_irradiance_",
               c("average"), "(MJ/m^2)"),
        paste0("snow_",
               c("fall",
                 "max_fall_day",
                 "depth"),
               "(cm)"),
        paste0("cloud_covering_", c("mean")),
        paste0("condition",
               c("snow_days",
                 "fog_days",
                 "thunder_days"))
      ),
      "monthly_a1" = c("month",
                       paste0("precipitation_",
                              c("max",
                                "max_per_day",
                                "max_1hour",
                                "max_10minutes"),
                              "(mm)"),
                       paste0("temperature_",
                              c("average",
                                "average_max",
                                "average_min",
                                "max",
                                "min"), "(\u2103)"),
                       paste0("wind_",
                              c("average_speed",
                                "max_speed",
                                "max_speed_direction",
                                "max_instantaneous_speed",
                                "max_instantaneous_direction"),
                              "(m/s)"),
                       paste0("daylight_", "(h)"),
                       paste0("snow_",
                              c("fall",
                                "max_fall_day",
                                "depth"),
                              "(cm)")
      ),
      "annually_a" = c("year",
                      paste0("precipitation_",
                             c("max",
                               "max_per_day",
                               "max_1hour",
                               "max_10minutes"),
                             "(mm)"),
                      paste0("temperature_",
                             c("average",
                               "average_max",
                               "average_min",
                               "max",
                               "min"),
                             "(\u2103)"),
                      paste0("wind_",
                             c("average_speed",
                               "max_speed",
                               "max_speed_direction",
                               "max_instantaneous_speed",
                               "max_instantaneous_direction"),
                             "(m/s)"),
                      paste0("daylight_", "(h)"),
                      paste0("snow_",
                             c("fall",
                               "max_fall_day",
                               "depth"),
                             "(cm)")
      ),
      "annually_s" = c("year",
                       paste0("atmosphere_",
                              c("land", "surface"), "(hPa)"),
                       paste0("precipitation_",
                              c("sum",
                                "max_per_day",
                                "max_1hour",
                                "max_10minutes"), "(mm)"),
                       paste0("temperature_",
                              c("average",
                                "average_max",
                                "average_min",
                                "max",
                                "min"), "(\u2103)"),
                       paste0("humidity_",
                              c("average",
                                "min"), "(%)"),
                       paste0("wind_",
                              c("average_speed",
                                "max_speed",
                                "max_speed_direction",
                                "max_instantaneous_speed",
                                "max_instantaneous_direction"),
                              "(m/s)"),
                       paste0("daylight_", "(h)"),
                       paste0("solar_irradiance_", "(MJ/m^2)"),
                       paste0("snow_",
                              c("fall",
                                "max_fall_day",
                                "depth"),
                              "(cm)"),
                       paste0("cloud_covering_", c("mean")),
                       paste0("condition",
                              c("snow_days",
                                "fog_days",
                                "thunder_days"))
    ))
}
