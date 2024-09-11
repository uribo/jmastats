#' Collect JMA Historical Weather Data
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Refer to the data available in the JMA Historical Weather Data Search.
#' Executed by specifying the target location and date.
#' Currently, not all types of data acquisition are supported.
#' @param item Type of weather data to be acquired. Mainly specifies
#' the interval between records (e.g. `daily` or `hourly`).
#' See NOTE for details.
#' @param block_no Block number of the location to be observed. It is assumed that
#' block_no is input as a string consisting of a 4- or 5-digit number. If a
#' numeric value is specified, it is processed as a string.
#' @param year select year
#' @param month select month
#' @param day select date (default `NULL`)
#' @param cache use cash and save to cache.  (`TRUE`, the default)
#' @param pack Whether to packing common variables or not.  (`TRUE`, the default)
#' @param quiet Whether to output information on variable and
#' row combinations that were treated as missing values
#' for some reason. (`TRUE`, the default)
#' @import rlang
#' @importFrom dplyr mutate select
#' @importFrom readr type_convert
#' @importFrom rvest html_table
#' @importFrom stringr str_glue str_pad str_remove str_trim
#' @importFrom xml2 read_html url_parse
#' @note
#' The parameter `item` chooses one from these:
#' - annually: Annual value. Please specify a location by `block_no`.
#' - monthly: Monthly value. Please specify location and year.
#' - 3monthly: Value every 3 months. Please specify location and year.
#' - 10daily: Seasonal value. Please specify location and year.
#' - mb5daily: Semi-seasonal value. Please specify location and year.
#' - daily: Daily value. Please specify location, year and month.
#' - hourly: Hourly value. Please specify location, year, month and day.
#' - rank: Values of the largest in the history of observations.
#' - nml_ym: Climatological normals for each year and month.
#' - nml_3m: Climatological normals for each 3 months.
#' - nml_10d: Climatological normals for each season (almost 10 days).
#' - nml_mb5d: Climatological normals for each semi-season (almost 5 days).
#' - nml_daily: Daily climatological normals for specific month.
#' for each location.
#' @examples
#' \donttest{
#' # Annually
#' jma_collect(item = "annually", "1284", year = 2017, month = 11, cache = FALSE)
#' # Daily
#' jma_collect(item = "daily", block_no = "0010", year = 2017, month = 11, cache = FALSE)
#' jma_collect(item = "daily", "0422", year = 2017, month = 11, cache = FALSE)
#' # Hourly
#' jma_collect("hourly", "0010", 2018, 7, 30, cache = FALSE)
#' # Historical Ranking
#' jma_collect("rank", block_no = "47646", year = 2020, cache = FALSE)
#' # Climatological normals
#' jma_collect("nml_ym", block_no = "47646", cache = FALSE, pack = FALSE)
#' jma_collect("nml_3m", "47646", cache = FALSE, pack = FALSE, quiet = TRUE)
#' jma_collect("nml_10d", "0228", cache = FALSE, pack = FALSE, quiet = TRUE)
#' jma_collect("nml_mb5d", "0228", cache = FALSE, pack = FALSE, quiet = FALSE)
#' }
#' @export
#' @return a `tbl` object
jma_collect <- function(item = NULL,
                        block_no, year, month, day,
                        cache = TRUE, pack = TRUE, quiet = FALSE) {
  target <-
    detect_target(item, block_no, year, month, day)
  if (cache) {
    param <-
      xml2::url_parse(target$url)$query
    file_loc <-
      search_cache_file(item, target$station_type, param)
    if (file.exists(file_loc)) {
      out <- readRDS(file_loc)
    } else {
      out <-
        slow_jma_collect(item, block_no, year, month, day, quiet)
      saveRDS(out, file = file_loc)
    }
  } else {
    out <-
      slow_jma_collect(item, block_no, year, month, day, quiet)
  }
  if (pack == TRUE) {
    if (!item %in% c("hourly", "10min")) {
      out <-
        pack_df(out, unpack = FALSE)
    }
  }
  out
}

pack_df <- function(df, unpack = FALSE) {
  if (unpack == FALSE) {
    df <-
      df |>
      tidyr::pack(atmosphere = tidyselect::starts_with("atmosphere"),
                  pressure = tidyselect::starts_with("pressure"),
                  precipitation = tidyselect::starts_with("precipitation"),
                  temperature = tidyselect::starts_with("temperature"),
                  humidity = tidyselect::starts_with("humidity"),
                  wind = tidyselect::starts_with("wind"),
                  sunshine = tidyselect::starts_with("sunshine"),
                  daylight = tidyselect::starts_with("daylight"),
                  snow = tidyselect::starts_with("snow"),
                  solar_irradiance = tidyselect::starts_with("solar_irradiance"),
                  cloud_covering = tidyselect::starts_with("cloud_covering"),
                  condition = tidyselect::starts_with("condition"),
                  weather_time = tidyselect::matches("weather.*time"),
                  .names_sep = "_")
    df[, c(names(purrr::discard(df, tibble::is_tibble)),
           df[, names(purrr::keep(df, tibble::is_tibble))] |>
             purrr::map(~ ncol(.x)) |>
             purrr::discard(~ .x == 0L) |>
             names())]
  } else {
    df |>
      tidyr::unpack(tidyselect::everything(),
                    names_sep = "_")
  }
}

jma_collect_raw <- function(item = NULL, block_no, year, month, day, quiet) {

  target <-
    detect_target(item, block_no, year, month, day)

  cat(
    cli::col_br_blue("Data from:"),
    cli::col_cyan(cli::style_hyperlink(target$url, target$url)))

  df_raw <-
    xml2::read_html(target$url) |>
    rvest::html_table(fill = TRUE)

  selected_item <-
    paste0(item, "_", target$station_type)
  vars <- name_sets(selected_item)

  df <-
    df_raw |>
    discard_ignore_df()

  if (item == "annually") {
    df <-
      .jma_collect_annually(df, vars, quiet)
  } else if (item %in% c("monthly", "3monthly", "10daily", "mb5daily")) {
    df <-
      df[-c(1:2), ] |>
      purrr::set_names(vars) |>
      tweak_df(quiet = quiet)
  } else if (item == "daily") {
    df <-
      .jma_collect_daily(df, vars,
                         year, month, date = day,
                         target$station_type,
                         quiet)
  } else if (item == "hourly") {
    df <-
      .jma_collect_hourly(df, vars, year, month, day, quiet)
  } else if (item == "10min") {
    df <-
      .jma_collect_10min(df, vars, target$station_type, quiet)
  } else if (item == "rank") {
    value <- period <- NULL
    df <-
      df |>
      tidyr::pivot_longer(cols = -c(1, ncol(df)),
                          names_to = "rank") |>
      tidyr::extract(col = value,
                     into = c("value", "date"),
                     regex = "(.+)\\((.+)\\)") |>
      purrr::set_names(name_sets(selected_item)) |>
      dplyr::mutate(period = glue_split_period_char(period,
                                                    collapse = intToUtf8(c(12363L, 12425L))),
                    rank = stringr::str_extract(rank, "[0-9]{1,}")) |>
      readr::type_convert()
  } else if (item %in% c("nml_ym", "nml_3m", "nml_10d", "nml_mb5d", "nml_daily")) {
    element <- NULL
    if (item %in% c("nml_ym", "nml_3m", "nml_daily")) {
      nml_meta <-
        list(years = df[[2]][df[[2]] |>
                               stringr::str_which(intToUtf8(65374))],
             records = df[[2]][df[[2]] |>
                                 stringr::str_which(intToUtf8(65374))+1])
    } else if (item == "nml_10d") {
      nml_meta <-
        list(years = df[[3]][df[[3]] |>
                               stringr::str_which(intToUtf8(65374))],
             records = df[[3]][df[[3]] |>
                                 stringr::str_which(intToUtf8(65374))+1])
    } else if (item == "nml_mb5d") {
      nml_meta <-
        list(years = df[[4]][df[[4]] |>
                               stringr::str_which(intToUtf8(65374))],
             records = df[[4]][df[[4]] |>
                                 stringr::str_which(intToUtf8(65374))+1])
    }
    nml_meta$years <-
      nml_meta$years |>
      stringr::str_split(intToUtf8(65374), simplify = TRUE) |>
      stringr::str_squish()
    cat(
      cli::col_br_blue(
        paste("\nThe record is based on the statistical period from",
              nml_meta$years[1],
              "to",
              nml_meta$years[2],
              paste0("(",
                     nml_meta$records,
                     " years of data).\n"))))
    df <-
      df[-c(seq.int(df[[1]] |>
                      stringr::str_which(intToUtf8(c(36039, 26009, 24180, 25968))))), ] |>
      purrr::set_names(vars) |>
      tweak_df(quiet = quiet)
    if (item %in% c("nml_10d", "nml_mb5d")) {
      df <-
        df |>
        tidyr::unite("element",
                     tidyselect::starts_with("element"),
                     sep = "",
                     remove = TRUE)
    } else if (item == "nml_daily") {
      df <-
        df |>
        dplyr::mutate(element = paste0(month,
                                        intToUtf8(26376),
                                        element))
    }
  }
  tibble::as_tibble(df)
}

slow_jma_collect <-
  purrr::slowly(function(item,
                         block_no, year, month, day, quiet) {
  jma_collect_raw(item = item,
                  block_no = block_no,
                  year = year,
                  month = month,
                  day = day,
                  quiet = quiet)},
  rate = purrr::rate_delay(pause = 7),
  quiet = FALSE)

detect_target <- function(item, block_no, ...) {
  block_no <-
    check_block_no(block_no)
  jma_url(item, block_no, ...)
}

check_block_no <- function(block_no) {
  if (nchar(block_no) > 5) {
    rlang::abort("block_no must be a string consisting of 4 or 5 digits.")
  }
  if (nchar(block_no) == 5L) {
    if (stringr::str_detect(block_no, "^47")) {
      if (!dplyr::between(as.numeric(block_no), 47401, 47991)) {
        rlang::abort("The 5-digit block_no ranges from '47401' to '47991'.")
      }
    } else {
      rlang::abort("The 5-digit block_no must start with '47'.")
    }
  } else if (!dplyr::between(as.numeric(block_no), 2, 1675)) {
    rlang::abort("The 5-digit block_no ranges from '0002' to '1675'.")
  } else if (!is.character(block_no)) {
      rlang::warn(
        "block_no is assumed to be given as a string.\nTreats the input block_no as a string.") # nolint
      if (nchar(block_no) == 3L) {
        block_no <-
          stringr::str_pad(block_no, pad = "0", side = "left", width = 4)
      }
    }
  block_no
}

tweak_df <- function(df, quiet) {
  convert_error(df, quiet) |>
    dplyr::mutate(
      dplyr::across(tidyselect::everything(),
                    .fns = list(~ stringr::str_remove_all(., "(]|\\))")),
                    .names = "{.col}")) |>
    dplyr::mutate(
      dplyr::across(tidyselect::where(is.character),
                    .fns = list(~ stringr::str_trim(., side = "both")),
                    .names = "{.col}")) |>
    readr::type_convert()
}

.jma_collect_annually <- function(df, vars, quiet) {
  df[-c(1:2), ] |>
    purrr::set_names(vars) |>
    tweak_df(quiet = quiet)
}

.jma_collect_daily <- function(df, vars,
                               year, month, date,
                               station_type,
                               quiet) {
  if (station_type == "a1") {
    df <-
      df[-c(1:2), ]
  } else if (station_type == "s1") {
    df <-
      df[-c(1:3), ]
  }
  df |>
    purrr::set_names(vars) |>
    tweak_df(quiet = quiet) |>
    dplyr::mutate(date = as.Date(paste(year,
                                       stringr::str_pad(month, width = 2, pad = "0"),
                                       stringr::str_pad(date, width = 2, pad = "0"), sep = "-")))
}

.jma_collect_hourly <- function(df, vars, year, month, day, quiet) {
  df <-
    df[-c(1), ] |>
    purrr::set_names(vars) |>
    tweak_df(quiet = quiet)
  df |>
    dplyr::mutate(date = lubridate::make_date(year, month, day)) |>
    dplyr::select(date, dplyr::everything())
}

.jma_collect_10min <- function(df, vars, station_type, quiet) {
  if (station_type == "a1") {
    df <-
      df[-c(1:2), ]
  } else if (station_type == "s1") {
    df <-
      df[-1, ]
  }
  df |>
    purrr::set_names(vars) |>
    tweak_df(quiet = quiet)
}

jma_url <- function(item = NULL,
                    block_no, year, month, day, ...) {
  .blockid <- rlang::enquo(block_no)
  selected_item <-
    rlang::arg_match(item,
                     c("annually", "monthly", "3monthly",
                       "10daily", "mb5daily", "daily",
                       "hourly", "10min", "rank",
                       paste("nml", c("ym", "3m", "10d", "mb5d", "daily"),
                             sep = "_")))
  if (identical(selected_item, character(0))) {
    rlang::abort(intToUtf8(c(12371, 12398, 20013, 12363, 12425, 36984, 25246)))
  }
  station_info <-
    detect_station_info(.blockid)
  if (selected_item %in% c(paste("nml", c("ym", "3m", "10d", "mb5d"),
                                 sep = "_"))) {
    list(
      url = as.character(stringr::str_glue(
        "https://www.data.jma.go.jp/stats/etrn/view/{selected_item}.php?prec_no={prec_no}&block_no={blockid}&year=&month=&view=",
        selected_item = stringr::str_replace(selected_item,
                                             "_",
                                             dplyr::if_else(station_info$station_type == "a",
                                                            "_amd_",
                                                            "_sfc_")),
        blockid = rlang::eval_tidy(.blockid),
        prec_no = station_info$prec_no
      )),
      station_type = station_info$station_type
    )
  } else {
    if (selected_item == "hourly") {
      validate_date(year, month, day)
    }
    if (rlang::is_missing(day)) {
      day <- ""
      dummy_day <- 1
    } else {
      dummy_day <- day
    }
    if (rlang::is_missing(month)) {
      month <- ""
      dummy_month <- 1
    } else {
      dummy_month <- month
    }
    if (selected_item %in% c("annually", "rank")) {
      if (rlang::is_missing(year)) {
        year <- ""
        dummy_year <- 1
      } else {
        dummy_year <- year
      }
    } else if (selected_item == "nml_daily") {
        year <- ""
        dummy_year <- 2024
        day <- ""
        dummy_day <- 1
        dummy_month <- month
    } else {
      dummy_year <- year
      dummy_month <- 1
    }
    if (validate_date(dummy_year, dummy_month, dummy_day)) {
      if (!selected_item %in% c("annually", "rank", "nml_daily")) {
        station_info$station_type <-
          paste0(station_info$station_type, "1")
      }
      if (selected_item == "nml_daily") {
        list(
          url = as.character(stringr::str_glue(
            "https://www.data.jma.go.jp/stats/etrn/view/{selected_item}.php?prec_no={prec_no}&block_no={blockid}&year=&month={month}&day=&view=",
            blockid = rlang::eval_tidy(.blockid),
            selected_item = dplyr::if_else(station_info$station_type == "a",
                                           "nml_amd_d",
                                           "nml_sfc_d"),
            prec_no = station_info$prec_no
          )),
          station_type = station_info$station_type
        )
      } else {
        list(
          url = as.character(stringr::str_glue(
            "https://www.data.jma.go.jp/stats/etrn/view/{selected_item}_{station_type}.php?prec_no={prec_no}&block_no={blockid}&year={year}&month={month}&day={day}&view=",
            blockid = rlang::eval_tidy(.blockid),
            station_type = station_info$station_type,
            prec_no = station_info$prec_no
          )),
          station_type = station_info$station_type
        )
      }
    }
  }
}

detect_station_info <- function(.blockid) {
  df_target_station <-
    subset(stations, block_no == rlang::eval_tidy(.blockid)) |>
    sf::st_drop_geometry() |>
    dplyr::distinct(block_no, .keep_all = TRUE)
  pref <-
    df_target_station$prec_no
  station_type <-
    ifelse(df_target_station$station_type == intToUtf8(23448), "s", "a")
  station_type <-
    ifelse(df_target_station$station_name == intToUtf8(c(39640, 26494)) & df_target_station$station_no == 23281,
         "a",
         station_type)
  station_type <-
    ifelse(df_target_station$station_name %in% c(intToUtf8(c(22899, 28288, 21029)),
                                                 intToUtf8(c(26681, 23460, 20013, 27161, 27941)),
                                                 intToUtf8(c(24111, 24195, 27849)),
                                                 intToUtf8(c(31859, 23713)),
                                                 intToUtf8(c(38738, 26862, 22823, 35895)),
                                                 intToUtf8(c(33031, 31070)),
                                                 intToUtf8(c(26481, 26681)),
                                                 intToUtf8(c(29577, 24029)),
                                                 intToUtf8(c(22823, 23798, 21271, 12494, 23665)),
                                                 intToUtf8(c(26032L, 23798L)),
                                                 intToUtf8(c(31070L, 27941L, 23798L)),
                                                 intToUtf8(c(19977L, 23429L, 22378L, 30000L)),
                                                 intToUtf8(c(20843L, 37325L, 35211L, 12534L, 21407L)),
                                                 intToUtf8(c(26494L, 26412L, 20170L, 20117L)),
                                                 intToUtf8(c(12475L, 12531L, 12488L, 12524L, 12450L)),
                                                 intToUtf8(c(25104L, 30000L)),
                                                 intToUtf8(c(26494L, 27996L)),
                                                 intToUtf8(c(31179L, 12534L, 23798L)),
                                                 intToUtf8(c(19977L, 20117L)),
                                                 intToUtf8(c(26149L, 27743L)),
                                                 intToUtf8(c(35914L, 20013L)),
                                                 intToUtf8(c(20843L, 23614L)),
                                                 intToUtf8(c(38306L, 31354L, 23798L)),
                                                 intToUtf8(c(31070L, 25144L, 31354L, 28207L)),
                                                 intToUtf8(c(21335L, 32000L, 30333L, 27996L)),
                                                 intToUtf8(c(26085L, 24540L, 23546L)),
                                                 intToUtf8(c(26412L, 37111L)),
                                                 intToUtf8(c(35199L, 37111L, 23724L)),
                                                 intToUtf8(c(26000L, 24029L)),
                                                 intToUtf8(c(39640L, 27941L)),
                                                 intToUtf8(c(28246L, 23665L)),
                                                 intToUtf8(c(39321L, 21335L)),
                                                 intToUtf8(c(21335L, 22269L, 26085L, 31456L)),
                                                 intToUtf8(c(23431L, 37096L)),
                                                 intToUtf8(c(31354L, 28207L, 21271L, 30010L)),
                                                 intToUtf8(c(27494L, 34101L)),
                                                 intToUtf8(c(22823L, 26449L)),
                                                 intToUtf8(c(24029L, 21103L)),
                                                 intToUtf8(c(30410L, 22478L)),
                                                 intToUtf8(c(20013L, 31278L, 23376L)),
                                                 intToUtf8(c(31520L, 21033L)),
                                                 intToUtf8(c(21916L, 30028L, 23798L)),
                                                 intToUtf8(c(21271L, 21407L)),
                                                 intToUtf8(c(21271L, 22823L, 26481L)),
                                                 intToUtf8(c(26087L, 26481L)),
                                                 intToUtf8(c(19979L, 22320L, 23798L)),
                                                 intToUtf8(c(20210L, 31563L)),
                                                 intToUtf8(c(25152L, 37326L)),
                                                 intToUtf8(c(38596, 21644)),
                                                 intToUtf8(c(21517, 21462)),
                                                 intToUtf8(c(27996, 20013)),
                                                 intToUtf8(c(22825, 22478)),
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
                                               intToUtf8(c(40372, 19992)),
                                               intToUtf8(c(28317, 36794)),
                                               intToUtf8(c(21338, 22810)),
                                               intToUtf8(c(22768, 21839)),
                                               intToUtf8(c(26412, 27850)),
                                               intToUtf8(c(26481, 31070, 27005)),
                                               intToUtf8(c(21315, 27507)),
                                               intToUtf8(c(32011, 21029, 23567, 21521)),
                                               intToUtf8(c(33457, 24059)),
                                               intToUtf8(c(32701, 30000)),
                                               intToUtf8(c(38745, 23713, 31354, 28207)),
                                               intToUtf8(c(26494, 23665, 21335, 21513, 30000)),
                                               intToUtf8(c(36196, 27743))),
  # Special pattern
  "a",
  station_type)
  list(
    prec_no = pref,
    station_type = station_type
  )
}

# see) https://www.data.jma.go.jp/stats//data/mdrr/man/remark.html
convert_error <- function(.data, quiet) {
  if (!quiet) {
    msg <-
      .data |>
      purrr::map(note_message) |>
      purrr::keep(~ length(.x) > 0)
    msg |>
      purrr::map2(names(msg),
                  ~ cat(cli::col_red(paste0(
                    "Treated as missing: lines ",
                    paste0(.x, collapse = ", "), " at ", .y, "\n"))))
  }
  dplyr::mutate(
    .data,
    dplyr::across(tidyselect::everything(),
                  .fns = ~ stringr::str_remove_all(., "(\\)|\\])$") |>
                    stringr::str_squish())) |>
    dplyr::mutate(
      dplyr::across(tidyselect::everything(),
                    .fns = ~ dplyr::if_else(. %in% c(intToUtf8(c(47, 47, 47)),
                                                     intToUtf8(c(215)),
                                                     "",
                                                     intToUtf8(c(35))),
                                            NA_character_, .))) |>
    dplyr::mutate(
      dplyr::across(tidyselect::everything(),
                    .fns = ~ dplyr::if_else(. %in% c(intToUtf8(c(45, 45))), "0.0", .)))
}

note_vars <- function(var) {
  syms <-
    var |>
    stringr::str_which(paste0("(",
                              paste0(c(intToUtf8(c(47, 47, 47)),
                                       intToUtf8(c(215)),
                                       intToUtf8(c(35))), collapse = "|"),
                              "|",
                              "\\+|\\-|\\)|\\]",
                              ")"))
  append(
    syms,
    var |>
      stringr::str_which("^$")
  )
}

note_message <- function(var) {
  note_vars(var) |>
    purrr::keep(\(x) length(x) > 0)
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
                      paste0("precipitation", "(mm)"),
                      paste0("temperature", "(\u2103)"),
                      paste0("dew_point", "(\u2103)"),
                      paste0("vapor", "(hPa)"),
                      paste0("humidity", "(%)"),
                      paste0("wind_", c(paste0("speed", "(m/s)"), "direction")),
                      jma_vars$daylight,
                      paste0("snow_", c("fall_moment", "fall_period"), "(cm)")),
      "hourly_s1" = c("time",
                      jma_vars$atmosphere,
                      paste0("precipitation", "(mm)"),
                      paste0("temperature", "(\u2103)"),
                      paste0("dew_point", "(\u2103)"),
                      paste0("vapor", "(hPa)"),
                      paste0("humidity", "(%)"),
                      paste0("wind_", c(paste0("speed", "(m/s)"), "direction")),
                      jma_vars$daylight,
                      paste0("solar_irradiance", "(MJ/m^2)"),
                      paste0("snow_", c("fall_moment", "fall_period"), "(cm)"),
                      paste0("weather"),
                      paste0("cloud_covering"),
                      paste0("visibility", "(km)")
                      ),
      "10min_a1" = c("time",
                     paste0("precipitation", "(mm)"),
                     paste0("temperature", "(\u2103)"),
                     paste0("relative_humidity", "(%)"),
                     paste0("wind_",
                            c(c(paste0("speed", "(m/s)"), "direction"),
                              c(paste0("max_instantaneous_speed", "(m/s)"),
                                "max_instantaneous_direction"))),
                     paste0("daylight_", "(min)")
                     ),
      "10min_s1" = c("time",
                     jma_vars$atmosphere,
                     paste0("precipitation", "(mm)"),
                     paste0("temperature", "(\u2103)"),
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
      "3monthly_s1" = c("month",
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
    "rank_a" = c("element", "period", "rank", "value", "date"),
    "nml_ym_s" = c("element",
                       jma_vars$atmosphere,
                       jma_vars$precipitation[1],
                       jma_vars$temperature[c(1, 4, 5)],
                       paste0("vapor", "(hPa)"),
                       paste0("relative_humidity", "(%)"),
                       jma_vars$wind[1],
                       "wind_most_frequent_direction",
                       jma_vars$daylight,
                       jma_vars$solar,
                       jma_vars$snow,
                       jma_vars$cloud,
                       jma_vars$condition),
    "nml_ym_a" = c("element",
                       jma_vars$precipitation[1],
                       jma_vars$temperature[c(1, 4, 5)],
                       jma_vars$wind[1],
                       jma_vars$daylight,
                       jma_vars$snow[c(1, 3)]),
    "nml_3m_s" = c("element",
                   stringr::str_remove(jma_vars$precipitation[1], "_sum"),
                   jma_vars$temperature[1],
                   paste0("temperature_",
                          c(rep("min_num_days_", 2),
                            rep("max_num_days_", 4)),
                          c("lt_0.0",
                            "geq_35.0",
                            "lt_0.0",
                            "geq_25.0",
                            "geq_30.0",
                            "geq_35.0"),
                          "(\u2103)"),
                   jma_vars$daylight,
                   jma_vars$snow[c(1, 3)]),
    "nml_3m_a" = c("element",
                   stringr::str_remove(jma_vars$precipitation[1], "_sum"),
                   jma_vars$temperature[1],
                   paste0("temperature_",
                          c(rep("min_num_days_", 2),
                            rep("max_num_days_", 4)),
                          c("lt_0.0",
                            "geq_35.0",
                            "lt_0.0",
                            "geq_25.0",
                            "geq_30.0",
                            "geq_35.0"),
                          "(\u2103)"),
                   jma_vars$daylight,
                   jma_vars$snow[c(1, 3)]),
    "nml_10d_s" = c("element",
                    "element2",
                    jma_vars$atmosphere[2],
                    stringr::str_remove(jma_vars$precipitation[1], "_sum"),
                    jma_vars$temperature[c(1, 4, 5)],
                    paste0("relative_humidity", "(%)"),
                    jma_vars$wind[1],
                    jma_vars$daylight,
                    jma_vars$solar,
                    jma_vars$snow[c(1, 3)],
                    stringr::str_remove(jma_vars$cloud, "_mean")),
    "nml_10d_a" = c("element",
                    "element2",
                    stringr::str_remove(jma_vars$precipitation[1], "_sum"),
                    jma_vars$temperature[c(1, 4, 5)],
                    jma_vars$wind[1],
                    jma_vars$daylight,
                    jma_vars$snow[c(1, 3)]),
    "nml_mb5d_s" = c(paste0("element",
                            c("", 2, 3)),
                     stringr::str_remove(jma_vars$precipitation[1], "_sum"),
                     jma_vars$temperature[c(1, 4, 5)],
                     jma_vars$daylight,
                     jma_vars$solar),
    "nml_mb5d_a" = c(paste0("element",
                            c("", 2, 3)),
                     stringr::str_remove(jma_vars$precipitation[1], "_sum"),
                     jma_vars$temperature[c(1, 4, 5)],
                     jma_vars$daylight),
    "nml_daily_s" = c("element",
                      stringr::str_remove(jma_vars$precipitation[1], "_sum"),
                      jma_vars$temperature[c(1, 4, 5)],
                      jma_vars$daylight,
                      jma_vars$solar,
                      jma_vars$cloud,
                      jma_vars$snow[c(1, 3)]),
    "nml_daily_a" = c("element",
                      stringr::str_remove(jma_vars$precipitation[1], "_sum"),
                      jma_vars$temperature[c(1, 4, 5)],
                      jma_vars$daylight,
                      jma_vars$snow[c(1, 3)])
    )
}

discard_ignore_df <- function(x) {
  x <-
    x |>
    purrr::keep(function(x) nrow(x) > 1)
  x |>
    purrr::pluck(length(x))
}
