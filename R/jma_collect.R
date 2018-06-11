#' Collect JMA Stats Data
#'
#' @param item url
#' @param block_no block number
#' @param year select year
#' @param month select month
#' @param day select date (default `NULL`)
#' @examples
#' \dontrun{
#' jma_collect(item = "annually", "1284", year = 2017, month = 11, day = NULL)
#' # daily
#' jma_collect(item = "daily", block_no = "0010", year = 2017, month = 11, day = NULL)
#' jma_collect(item = "daily", "0422", year = 2017, month = 11, day = NULL)
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

  if (item == "annually" & target$station_type == "a") {
    df <-
      df_raw[[4]][-c(1:2), ]

    names(df) <-
      name_sets(selected_item)

    df <-
      convert_error(df) %>%
      dplyr::mutate_all(.funs = dplyr::funs(stringr::str_remove(., "]"))) %>%
      readr::type_convert(col_types = readr::cols(.default = readr::col_number()))
  } else if (item == "monthly" & target$station_type == "a1") {
    df <-
      df_raw[[6]][-c(1:2), ]

    names(df) <-
      name_sets(selected_item)

    df <-
      convert_error(df) %>%
      dplyr::mutate_all(.funs = dplyr::funs(stringr::str_remove(., "]"))) %>%
      readr::type_convert(col_types = readr::cols(.default = readr::col_number()))

  } else if(item == "10daily" & target$station_type == "a1") {

    df <-
      df_raw[[6]][-c(1:2), ]

    names(df) <-
      name_sets(selected_item)

    df <-
      convert_error(df) %>%
      dplyr::mutate_all(.funs = dplyr::funs(stringr::str_remove(., "]"))) %>%
      readr::type_convert(col_types = readr::cols(.default = readr::col_number()))

  } else if (item == "daily" & target$station_type == "a1") {
    df <-
      df_raw[[6]][-c(1:2), ]

    names(df) <-
      name_sets(selected_item)

    df <-
      convert_error(df) %>%
      dplyr::mutate_all(.funs = dplyr::funs(stringr::str_remove(., "]"))) %>%
      dplyr::mutate(date = as.Date(paste(year,
                                  stringr::str_pad(month, width = 2, pad = "0"),
                                  stringr::str_pad(date, width = 2, pad = "0"), sep = "-"))) %>%
      readr::type_convert(col_types = readr::cols(.default = readr::col_number()))
  } else if (item == "daily" & target$station_type == "s1") {
    df <-
      df_raw[[6]][-c(1:3), ]

    names(df) <-
      name_sets(selected_item)

    df <-
      convert_error(df) %>%
      dplyr::mutate_all(.funs = dplyr::funs(stringr::str_remove(., "]"))) %>%
      dplyr::mutate(date = as.Date(paste(year,
                                         stringr::str_pad(month, width = 2, pad = "0"),
                                         stringr::str_pad(date, width = 2, pad = "0"), sep = "-"))) %>%
      readr::type_convert(col_types = readr::cols(.default = readr::col_number()))

  } else if (item == "hourly" & target$station_type == "a1") {

    df <-
      df_raw[[5]][-c(1:2), ]

    names(df) <-
      name_sets(selected_item)

    df <-
      convert_error(df) %>%
      dplyr::mutate_all(.funs = dplyr::funs(stringr::str_remove(., "]"))) #%>%
      #readr::type_convert(col_types = readr::cols(.default = readr::col_number()))
  } else if (item == "hourly" & target$station_type == "s1") {
    df <-
      df_raw[[5]][-c(1), ]

    names(df) <-
      name_sets(selected_item)

    df <-
      convert_error(df) %>%
      dplyr::mutate_all(.funs = dplyr::funs(stringr::str_remove(., "]"))) %>%
      readr::type_convert()

  } else if (item == "monthly" & target$station_type == "a1") {
    df <-
      df_raw[[6]][-c(1:2), ]

    names(df) <-
      name_sets(selected_item)

    df <-
      convert_error(df) %>%
      dplyr::mutate_all(.funs = dplyr::funs(stringr::str_remove(., "]")))

  } else {
    df <- df_raw
  }
  # convert_variable_unit(df) %>%
  #   tibble::as_tibble()
  tibble::as_tibble(df)
}

#' jma_url(item = "annually", "0010", year = 2017, month = 11, day = NULL)
jma_url <- function(item = NULL,
                    block_no, year, month, day) {
  .blockid <- rlang::enquo(block_no)

  selected_item <- item

  if (identical(selected_item, character(0))) {
    rlang::abort("この中から選択")
  }

  df_target_station <-
    dplyr::filter(stations, block_no == !! rlang::eval_tidy(.blockid)) %>%
    dplyr::distinct(block_no, .keep_all = TRUE)

  pref <- df_target_station$prec_no
  station_type <-
    dplyr::if_else(df_target_station$station_type == "官", "s", "a")

  station_type <-
    dplyr::if_else(df_target_station$station_name %in% c("天城", "与論島",
                                                         "安次嶺", "川平",
                                                         "慶良間", "盛山",
                                                         "鏡原", "東"),
                   # 官なのにaのやつ
                   "a",
                   station_type)

  if (selected_item != "annually") {
    station_type <- paste0(station_type, "1")
  }

  if (rlang::is_true(rlang::is_missing(day))) {
    day <- ""
  }

  list(
    url = glue::glue(
      "http://www.data.jma.go.jp/obd/stats/etrn/view/{selected_item}_{station_type}.php?prec_no={pref}&block_no={blockid}&year={year}&month={month}&day={day}&view=",
      blockid = rlang::eval_tidy(.blockid)
    ),
    station_type = station_type
  )

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
                      dplyr::if_else(. %in% c("///", "×", "", "#"), NA_character_, .))) %>%
    dplyr::mutate_all(.funs = dplyr::funs(
                        dplyr::if_else(. %in% c("--"), "0.0", .)))

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
                      paste0("dailight_", "(h)"),
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
        paste0("sunshine_duration_", "(h)"),
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
        paste0("sunshine_duration_", "(h)"),
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
        paste0("sunshine_duration_", "(h)"),
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
                       paste0("sunshine_duration_", "(h)"),
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
                      paste0("sunshine_duration_", "(h)"),
                      paste0("snow_",
                             c("fall",
                               "max_fall_day",
                               "depth"),
                             "(cm)")
      )
    )
}
