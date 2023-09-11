#' Read and parse tide level text data
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' @param path URL or local file path to sea tide level file
#' @param .year A.D. 1997 to present year.
#' @param .month Month number. 1997 only, valid after March.
#' @param .stn Station identification name in uppercase two-digit letters.
#' @param raw If *TRUE*, return raw format data
#' @seealso <https://www.data.jma.go.jp/gmd/kaiyou/db/tide/suisan/readme.html>
#' @examples
#' # Read a local storage file (dummy data)
#' read_tide_level(system.file("dummy/tide.txt", package = "jmastats"))
#' \donttest{
#' # Request from URL
#' read_tide_level("https://www.data.jma.go.jp/gmd/kaiyou/data/db/tide/suisan/txt/2020/TK.txt")
#' # Request from parameters
#' read_tide_level(.year = 2020, .month = 2, .stn = "TK")
#' }
#' @export
#' @return a `tbl` object
read_tide_level <- function(path = NULL, .year, .month, .stn, raw = FALSE) {
  if (is.null(path)) {
    path <-
      request_tide_level_url(.year, .month, .stn)
  }
  d <-
    readr::read_lines(path)
  if (raw == FALSE) {
    d <-
      d |>
      purrr::map(
        ~ .x |>
          stringr::str_split(pattern = stringr::boundary("character"),
                             simplify = TRUE) |>
          as.data.frame(stringsAsFactors = FALSE)
      ) |>
      purrr::list_rbind() |>
      tibble::as_tibble() |>
      parse_tide_file() |>
      dplyr::mutate(
        dplyr::across(c(tidyselect::num_range("hry_",
                                              range = seq.int(0, 23),
                                              width = 2),
                        tidyselect::contains("tide_level")),
                      function(x) units::set_units(x, "cm")))
  }
  d
}

request_tide_level_url <- function(.year, .month, .stn) {
  jma_site <- # nolint
    "https://www.data.jma.go.jp"
  year <-
    check_input_tidal_year(.year)
  month <-
    sprintf("%02d", .month)
  month <-
    rlang::arg_match(month,
                     sprintf("%02d", seq.int(12)))
  if (year == 1997 && month %in% c("01", "02", "03")) {
    rlang::abort("Old format")
  }
  stn_candidate <-
    tide_station |>
    dplyr::filter(year == !!rlang::enquo(year)) |>
    dplyr::pull(stn)
  stn <-
    stn_candidate[stn_candidate %in% .stn]
  if (length(stn) == 0) {
    rlang::abort("In that year, there was no data from the target observatory.")
  }
  stringr::str_glue("{jma_site}/gmd/kaiyou/data/db/tide/genbo/{year}/{year}{month}/hry{year}{month}{stn}.txt") # nolint
}

parse_tide_file <- function(data) {
  year_last2 <- month_last2 <- day_last2 <- stn <- NULL
  seq.int(nrow(data)) |>
    purrr::map(
      function(.x) {
        cbind(parse_hry(data[.x, ]),
              parse_ymd(data[.x, ], "year"),
              parse_ymd(data[.x, ], "month"),
              parse_ymd(data[.x, ], "day"),
              parse_tide_station(data[.x, ]),
              parse_hm(data[.x, ], "low_tide"),
              parse_tide(data[.x, ], "low_tide"),
              parse_hm(data[.x, ], "high_tide"),
              parse_tide(data[.x, ], "high_tide")
        ) |>
          dplyr::mutate(date = lubridate::make_date(year = paste0("20", year_last2),
                                             month_last2,
                                             day_last2)) |>
          dplyr::select(!c(year_last2, month_last2, day_last2)) |>
          tibble::as_tibble()
      }) |>
    purrr::list_rbind() |>
    dplyr::select(
      tidyselect::starts_with("hry"),
      date,
      stn,
      tidyselect::ends_with("obs1"),
      tidyselect::ends_with("obs2"),
      tidyselect::ends_with("obs3"),
      tidyselect::ends_with("obs4")) |>
    dplyr::mutate(
      dplyr::across(c(tidyselect::contains("tide_hm_obs")),
                    function(x) dplyr::na_if(x, "99:99"))) |>
    dplyr::mutate(
      dplyr::across(c(tidyselect::contains("tide_tidal_level_cm_obs")),
                    function(x) dplyr::na_if(x, "99:99"))) |>
    readr::type_convert(col_types = readr::cols(
      stn = readr::col_character(),
      low_tide_hm_obs1 = readr::col_time(format = ""),
      low_tide_level_obs1 = readr::col_double(),
      high_tide_hm_obs1 = readr::col_time(format = ""),
      high_tide_level_obs1 = readr::col_double(),
      low_tide_hm_obs2 = readr::col_time(format = ""),
      low_tide_level_obs2 = readr::col_double(),
      high_tide_hm_obs2 = readr::col_time(format = ""),
      high_tide_level_obs2 = readr::col_double(),
      low_tide_hm_obs3 = readr::col_time(format = ""),
      low_tide_level_obs3 = readr::col_double(),
      high_tide_hm_obs3 = readr::col_time(format = ""),
      high_tide_level_obs3 = readr::col_double(),
      low_tide_hm_obs4 = readr::col_time(format = ""),
      low_tide_level_obs4 = readr::col_double(),
      high_tide_hm_obs4 = readr::col_time(format = ""),
      high_tide_level_obs4 = readr::col_double()
    ))
}

parse_hry <- function(d) {
  purrr::map2_chr(
    .x = seq.int(1, 72, by = 3),
    .y = seq.int(1, 72, by = 3) + 2,
    .f = ~ d[.x:.y] |>
      paste(collapse = "")
  ) |>
    purrr::set_names(paste0("hry_", sprintf("%02d", seq.int(0, 23)))) |>
    as.data.frame() |>
    t() |>
    as.data.frame(stringsAsFactors = FALSE) |>
    dplyr::mutate(
      dplyr::across(tidyselect::everything(),
                    readr::parse_number)) |>
    tibble::as_tibble()
}

parse_ymd <- function(d, type) {
  type <-
    rlang::arg_match(type,
                   c("year", "month", "day"))
  index_list <-
    list(x = 73, y = 74)
  index_list <-
    switch (type,
    "year" = index_list,
    "month" = index_list |>
      purrr::list_modify(x = index_list$x + 2,
                         y = index_list$y + 2),
    "day" = index_list |>
      purrr::list_modify(x = index_list$x + 4,
                         y = index_list$y + 4)
  )
  d[seq.int(index_list$x, index_list$y)] |>
    paste(collapse = "") |>
    purrr::set_names(paste0(type, "_last2")) |>
    as.data.frame() |>
    t() |>
    as.data.frame(stringsAsFactors = FALSE) |>
    dplyr::mutate(
      dplyr::across(tidyselect::everything(),
                    readr::parse_number)) |>
    tibble::as_tibble()
}

parse_tide_station <- function(d) {
  d[seq.int(79, 80)] |>
    paste(collapse = "") |>
    purrr::set_names("stn") |>
    as.data.frame() |>
    t() |>
    as.data.frame(stringsAsFactors = FALSE) |>
    tibble::as_tibble()
}

parse_hm <- function(d, time) {
  time <-
    rlang::arg_match(time,
                     c("low_tide", "high_tide"))
  index_list <-
    switch (time,
          low_tide = list(
            x =
              seq(81, 108, by = 7),
            y =
              seq(84, 108, by = 7)),
          high_tide = list(
            x =
              seq(109, 136, by = 7),
            y =
              seq(112, 136, by = 7)
          ))
  d <-
    purrr::map2(
    index_list$x,
    index_list$y,
    ~ d[seq.int(.x, .y)] |>
      purrr::set_names(c("h1", "h2", "m1", "m2")) |>
      dplyr::mutate(
        across(tidyselect::everything(),
               function(x) stringr::str_replace(x,
                                                pattern = "[[:blank:]]",
                                                replacement = "0"))) |>
      dplyr::mutate(hm = paste(paste0(h1, h2, collapse = ""),
                               paste0(m1, m2, collapse = ""),
                               sep = ":"),
                    .keep = "none")) |>
    purrr::list_cbind()
  d |>
    purrr::set_names(paste0(time,
                            "_hm",
                            "_obs",
                            seq.int(length(d))))
}

parse_tide <- function(d, time) {
  time <-
    rlang::arg_match(time,
                     c("low_tide", "high_tide"))
  index_list <-
    switch (time,
            low_tide = list(
              x =
                seq(85, 108, by = 7),
              y =
                seq(87, 108, by = 7)),
            high_tide = list(
              x =
                seq(113, 136, by = 7),
              y =
                seq(115, 136, by = 7)
            ))
  d <-
    purrr::map2(
    index_list$x,
    index_list$y,
    ~ d[seq.int(.x, .y)] |>
      purrr::set_names(c("obs1", "obs2", "obs3")) |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::everything(),
          function(x) stringr::str_replace(x,
                                           pattern = "[[:blank:]]",
                                           replacement = "0"))) |>
      dplyr::mutate(tidal_level_cm = paste0(obs1, obs2, obs3, collapse = ""),
                    .keep = "none")) |>
    purrr::list_cbind()
  d |>
    purrr::set_names(paste0(time,
                            "_level",
                            "_obs",
                            seq.int(length(d))))
}
