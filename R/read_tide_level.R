#' Read and parse tide level text data
#'
#' @param path URL or local file path to sea tide level file
#' @param .year year
#' @param .month month
#' @param .stn Two uppercase letters of the alphabet to identify the observation point.
#' @param raw If *TRUE*, return raw format data
#' @seealso [https://www.data.jma.go.jp/gmd/kaiyou/db/tide/suisan/readme.html](https://www.data.jma.go.jp/gmd/kaiyou/db/tide/suisan/readme.html)
#' @examples
#' \dontrun{
#' read_tide_level("https://www.data.jma.go.jp/gmd/kaiyou/data/db/tide/suisan/txt/2020/TK.txt")
#'
#' read_tide_level(.year = 2020, .month = 2, .stn = "TK")
#' }
#' @export
read_tide_level <- function(path = NULL, .year, .month, .stn, raw = FALSE) {
  if (is.null(path)) {
    year <-
      as.character(.year)
    month <-
      sprintf("%02d", .month)
    year <-
      rlang::arg_match(year,
                       as.character(seq.int(1997, lubridate::year(lubridate::now()))))
    month <-
      rlang::arg_match(month,
                       sprintf("%02d", seq.int(12)))
    if (year == 1997 && month %in% c("01", "02", "03")) {
      rlang::abort("Old format")
    }

    stn_candidate <-
      tide_station %>%
      dplyr::filter(year == !!rlang::enquo(year)) %>%
      dplyr::pull(stn)
    stn <-
      stn_candidate[stn_candidate %in% .stn]
    if (length(stn) == 0) {
      rlang::abort("In that year, there was no data from the target observatory.")
    }
    path <-
      glue::glue("https://www.data.jma.go.jp/gmd/kaiyou/data/db/tide/genbo/{year}/{year}{month}/hry{year}{month}{stn}.txt") # nolint
  }
  d <-
    readr::read_lines(path)
  if (raw == FALSE) {
    d <-
      d %>%
      purrr::map_dfr(
        ~ .x %>%
          stringr::str_split(pattern = stringr::boundary("character"),
                             simplify = TRUE) %>%
          as.data.frame(stringsAsFactors = FALSE)
      ) %>%
      tibble::as_tibble() %>%
      parse_tide_file()
  }
  d
}

parse_tide_file <- function(data) {
  year_last2 <- month_last2 <- day_last2 <- stn <- NULL
  seq.int(nrow(data)) %>%
    purrr::map_dfr(
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
        ) %>%
          dplyr::mutate(date = lubridate::make_date(year = paste0("20", year_last2),
                                             month_last2,
                                             day_last2)) %>%
          dplyr::select(-year_last2, -month_last2, -day_last2) %>%
          tibble::as_tibble()
      }) %>%
    dplyr::select(
      tidyselect::starts_with("hry"),
      date,
      stn,
      tidyselect::ends_with("obs1"),
      tidyselect::ends_with("obs2"),
      tidyselect::ends_with("obs3"),
      tidyselect::ends_with("obs4")) %>%
    dplyr::mutate_at(dplyr::vars(tidyselect::contains("tide_hm_obs")),
              list(~ dplyr::na_if(., "99:99"))) %>%
    dplyr::mutate_at(dplyr::vars(tidyselect::contains("tide_tidal_level_cm_obs")),
              list(~ dplyr::na_if(., "999"))) %>%
    readr::type_convert(col_types = readr::cols(
      stn = readr::col_character(),
      low_tide_hm_obs1 = readr::col_time(format = ""),
      low_tide_tidal_level_cm_obs1 = readr::col_double(),
      high_tide_hm_obs1 = readr::col_time(format = ""),
      high_tide_tidal_level_cm_obs1 = readr::col_double(),
      low_tide_hm_obs2 = readr::col_time(format = ""),
      low_tide_tidal_level_cm_obs2 = readr::col_double(),
      high_tide_hm_obs2 = readr::col_time(format = ""),
      high_tide_tidal_level_cm_obs2 = readr::col_double(),
      low_tide_hm_obs3 = readr::col_time(format = ""),
      low_tide_tidal_level_cm_obs3 = readr::col_double(),
      high_tide_hm_obs3 = readr::col_time(format = ""),
      high_tide_tidal_level_cm_obs3 = readr::col_double(),
      low_tide_hm_obs4 = readr::col_time(format = ""),
      low_tide_tidal_level_cm_obs4 = readr::col_double(),
      high_tide_hm_obs4 = readr::col_time(format = ""),
      high_tide_tidal_level_cm_obs4 = readr::col_double()
    ))
}

parse_hry <- function(d) {
  purrr::map2_chr(
    .x = seq.int(1, 72, by = 3),
    .y = seq.int(1, 72, by = 3) + 2,
    .f = ~ d[.x:.y] %>%
      paste(collapse = "")
  ) %>%
    purrr::set_names(paste0("hry_", sprintf("%02d", seq.int(0, 23)))) %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    dplyr::mutate_all(readr::parse_number) %>%
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
    "month" = index_list %>%
      purrr::list_modify(x = index_list$x + 2,
                         y = index_list$y + 2),
    "day" = index_list %>%
      purrr::list_modify(x = index_list$x + 4,
                         y = index_list$y + 4)
  )
  d[seq.int(index_list$x, index_list$y)] %>%
    paste(collapse = "") %>%
    purrr::set_names(paste0(type, "_last2")) %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    dplyr::mutate_all(readr::parse_number) %>%
    tibble::as_tibble()
}

parse_tide_station <- function(d) {
  d[seq.int(79, 80)] %>%
    paste(collapse = "") %>%
    purrr::set_names("stn") %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
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
    purrr::map2_dfc(
    index_list$x,
    index_list$y,
    ~ d[seq.int(.x, .y)] %>%
      purrr::set_names(c("h1", "h2", "m1", "m2")) %>%
      dplyr::mutate_all(
        list(~ stringr::str_replace(., pattern = "[:blank:]", replacement = "0"))) %>%
      dplyr::transmute(hm = paste(paste0(h1, h2, collapse = ""),
                            paste0(m1, m2, collapse = ""),
                            sep = ":")))
  d %>%
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
    purrr::map2_dfc(
    index_list$x,
    index_list$y,
    ~ d[seq.int(.x, .y)] %>%
      purrr::set_names(c("obs1", "obs2", "obs3")) %>%
      dplyr::mutate_all(
        list(~ stringr::str_replace(., pattern = "[:blank:]", replacement = "0"))) %>%
      dplyr::transmute(tidal_level_cm = paste0(obs1, obs2, obs3, collapse = "")))
  d %>%
    purrr::set_names(paste0(time,
                            "_tidal_level_cm",
                            "_obs",
                            seq.int(length(d))))
}

