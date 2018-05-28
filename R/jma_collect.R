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
                        block_no, year, month, day = NULL) {

  .blockid <- rlang::enquo(block_no)

  target <-
    jma_url(item, !!.blockid, year, month, day)

  df_raw <-
    xml2::read_html(target$url) %>%
    rvest::html_table(fill = TRUE)

  if (item == "annually" & target$station_type == "a") {
    fix_names <-
      c("年",
        paste0("降水量_",
               c("合計", "最大日", "最大1時間", "最大10分間"), "(mm)"),
        paste0("気温_",
               c("平均日平均", "平均日最高", "平均日最低", "最高", "最低"), "(\u2103)"),
        paste0("風向・風速_",
               c("平均風速", "最大風速風速", "最大風速風向",
                 "最大瞬間風速風速", "最大瞬間風速風向"), "(m/s)"),
        paste0("日照時間_", "(h)"),
        paste0("雪_",
               c("降雪の合計", "日降雪の最大", "最深積雪"), "(寒候年.cm)")
        )
      # c("年",
      #   paste0("気圧_", c("現地平均", "海面平均"), "(hPa)"),
      #   paste0("降水量_", c("合計", "最大日", "最大1時間", "最大10分間"), "(mm)"),
      #   paste0("気温_", c("平均日平均", "平均日最高", "平均日最低", "最高", "最低"), "(\u2103)"),
      #   paste0("湿度_", c("平均", "最小"), "(%)"),
      #   paste0("風向・風速_", c("平均風速", "最大風速風速", "最大風速風向", "最大瞬間風速風速", "最大瞬間風速風向"), "(m/s)"),
      #   paste0("日照時間_", "(h)"),
      #   paste0("全天日射量_", c("平均"), "(MJ/m^2)"),
      #   paste0("雪_", c("冠雪合計", "冠雪日合計の最大", "最深積雪"), "(寒候年.cm)"),
      #   paste0("雲量_", "平均"),
      #   paste0("大気現象", c("雪日数(寒候年)", "霧日数", "雷日数")))

    df <-
      df_raw[[4]][-c(1:2), ]

    names(df) <- fix_names

    df <-
      convert_error(df) %>%
      dplyr::mutate_all(.funs = dplyr::funs(stringr::str_remove(., "]"))) %>%
      readr::type_convert(col_types = readr::cols(.default = readr::col_number()))
  } else if (item == "monthly" & target$station_type == "a1") {
    df <-
      df_raw[[6]][-c(1:2), ]

    fix_names <- c(
      "月",
      paste0("降水量_", c("合計", "日最大", "最大1時間", "最大10分間"), "(mm)"),
      paste0("気温_", c("平均日平均", "平均日最高", "平均日最低",
                      "最高", "最低"), "(\u2103)"),
      paste0("風向・風速_",
             c("平均風速", "最大風速風速", "最大風速風向",
               "最大瞬間風速風速", "最大瞬間風速風向"), "(m/s)"),
      paste0("日照時間_", "(h)"),
      paste0("雪_", c("降雪の合計", "日降雪の最大", "最深積雪"), "(寒候年.cm)")
    )

    names(df) <- fix_names

    df <-
      convert_error(df) %>%
      dplyr::mutate_all(.funs = dplyr::funs(stringr::str_remove(., "]"))) %>%
      readr::type_convert(col_types = readr::cols(.default = readr::col_number()))

  } else if(item == "10daily" & target$station_type == "a1") {

    df <-
      df_raw[[6]][-c(1:2), ]

    fix_names <- c(
      "月",
      "旬",
      paste0("降水量_", c("合計", "日最大", "最大1時間", "最大10分間"), "(mm)"),
      paste0("気温_", c("平均日平均", "平均日最高", "平均日最低",
                      "最高", "最低"), "(\u2103)"),
      paste0("風向・風速_",
             c("平均風速", "最大風速風速", "最大風速風向",
               "最大瞬間風速風速", "最大瞬間風速風向"), "(m/s)"),
      paste0("日照時間_", "(h)"),
      paste0("雪_", c("降雪の合計", "日降雪の最大", "最深積雪"), "(寒候年.cm)")
    )

    names(df) <- fix_names

    df <-
      convert_error(df) %>%
      dplyr::mutate_all(.funs = dplyr::funs(stringr::str_remove(., "]"))) %>%
      readr::type_convert(col_types = readr::cols(.default = readr::col_number()))

  } else if (item == "daily" & target$station_type == "a1") {
    df <-
      df_raw[[6]][-c(1:2), ]

    fix_names <- c(
      "日",
      paste0("降水量_", c("合計", "最大1時間", "最大10分間"), "(mm)"),
      paste0("気温_", c("平均", "最高", "最低"), "(\u2103)"),
      paste0("風向・風速_",
             c("平均風速", "最大風速風速", "最大風速風向",
               "最大瞬間風速風速", "最大瞬間風速風向", "最多風速"), "(m/s)"),
      paste0("日照時間_", "(h)"),
      paste0("雪_", c("降雪合計", "最深積雪"), "(寒候年.cm)")
    )

    names(df) <- fix_names

    df <-
      convert_error(df) %>%
      dplyr::mutate_all(.funs = dplyr::funs(stringr::str_remove(., "]"))) %>%
      dplyr::mutate(`日` = as.Date(paste(year,
                                  stringr::str_pad(month, width = 2, pad = "0"),
                                  stringr::str_pad(`日`, width = 2, pad = "0"), sep = "-"))) %>%
      readr::type_convert(col_types = readr::cols(.default = readr::col_number()))
  } else if (item == "daily" & target$station_type == "s1") {
    df <-
      df_raw[[6]][-c(1:2), ]

    fix_names <- c(
      "日",
      paste0("気圧_", c("現地平均", "海面平均"), "(hPa)"),
      paste0("降水量_", c("合計", "最大1時間", "最大10分間"), "(mm)"),
      paste0("気温_", c("平均", "最高", "最低"), "(\u2103)"),
      paste0("湿度_", c("平均", "最小"), "(%)"),
      paste0("風向・風速_",
             c("平均風速", "最大風速風速", "最大風速風向",
               "最大瞬間風速風速", "最大瞬間風速風向"), "(m/s)"),
      paste0("日照時間_", "(h)"),
      paste0("雪_", c("降雪合計", "最深積雪"), "(寒候年.cm)"),
      paste0("天気概況", c("昼 (06:00-18:00)", "夜 (18:00-翌日06:00)"))
    )

    names(df) <- fix_names

    df <-
      convert_error(df) %>%
      dplyr::mutate_all(.funs = dplyr::funs(stringr::str_remove(., "]"))) %>%
      readr::type_convert(col_types = readr::cols(.default = readr::col_number()))

  } else if (item == "hourly" & target$station_type == "a1") {

    df <-
      df_raw[[5]][-c(1:2), ]

    names(df) <-
      name_sets(rlang::eval_tidy(item))

    df <-
      convert_error(df) %>%
      dplyr::mutate_all(.funs = dplyr::funs(stringr::str_remove(., "]"))) #%>%
      #readr::type_convert(col_types = readr::cols(.default = readr::col_number()))
  } else if (item == "hourly" & target$station_type == "s1") {
    df <-
      df_raw[[5]][-c(1), ]

    names(df) <-
      name_sets(rlang::eval_tidy(item))

    df <-
      convert_error(df) %>%
      dplyr::mutate_all(.funs = dplyr::funs(stringr::str_remove(., "]"))) %>%
      readr::type_convert()

  } else if (item == "monthly" & target$station_type == "a1") {
    df <-
      df_raw[[6]][-c(1:2), ]

    names(df) <-
      name_sets(rlang::eval_tidy(item))

    df <-
      convert_error(df) %>%
      dplyr::mutate_all(.funs = dplyr::funs(stringr::str_remove(., "]")))

  } else {
    df <- df_raw
  }
  # convert_variable_unit(df) %>%
  #   tibble::as_tibble()
  df
}

#' jma_url(item = "annually", "0010", year = 2017, month = 11, day = NULL)
jma_url <- function(item = NULL,
                    block_no, year, month, day = NULL) {
  .blockid <- rlang::enquo(block_no)

  selected_item <- item

  if (identical(selected_item, character(0))) {
    rlang::abort("この中から選択")
  }

  df_target_station <-
    dplyr::filter(stations, block_no == !! rlang::eval_tidy(.blockid))

  pref <- df_target_station$prec_no
  station_type <-
    dplyr::if_else(df_target_station$station_type == "官", "s", "a")

  station_type <-
    dplyr::if_else(selected_item == "annually",
                   station_type,
                   paste0(station_type, "1"))

  day <- if (rlang::is_true(is.null(day))) {
    ""
  } else {
    day
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

    switch (item,
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
                      paste0("露点温度", "(\u2103)"),
                      paste0("蒸気圧", "(hPa)"),
                      paste0("humidity", "(%)"),
                      paste0("wind_", c("speed", "direction"), "(m/s)"),
                      paste0("dailight_", "(h)"),
                      paste0("全天日射量", "(MJ/m^2)"),
                      paste0("snow_", c("fall_moment", "fall_period"), "(cm)"),
                      paste0("weather"),
                      paste0("雲量"),
                      paste0("視程", "(km)")
                      ),
      "monthly_a1" = c("月",
                       paste0("降水量_", c("合計", "日最大", "最大1時間", "最大10分間"), "(mm)"),
                       paste0("気温_",
                              c("平均日平均", "平均日最高", "平均日最低",
                                "最高", "最低"), "(\u2103)"),
                       paste0("風向・風速_",
                              c("平均風速", "最大風速風速", "最大風速風向",
                                "最大瞬間風速風速", "最大瞬間風速風向"), "(m/s)"),
                       paste0("日照時間_", "(h)"),
                       paste0("雪_", c("降雪の合計", "日降雪の最大", "最深積雪"), "(寒候年.cm)")
      )
    )
}
