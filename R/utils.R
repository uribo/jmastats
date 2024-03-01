utils::globalVariables("where")

guess_unit <- function(x) {
  stringr::str_extract(x, "\\(.+?\\)") |>
    stringr::str_remove_all("\\(|\\)")
}

validate_date <- function(year, month, day) {
  check_positive <-
    sapply(list(year = year, month = month, day = day), function(x) { x > 0})
  if (sum(check_positive) != 3L) {
    rlang::inform(paste0(
      "Input arguments should be positive\n",
      "Check: ",
      paste(names(which(check_positive == FALSE)), collapse = ", ")))
    return(FALSE)
  }
  x <-
    lubridate::make_date(year, month, day)
  if (is.na(x)) {
    rlang::inform(paste0(
      "Input arguments should be calender format\n",
      "Check: ",
      paste(names(which(c(
        "month" =
          ifelse(is.na(match(13, 1:12)), FALSE, TRUE),
        "day" =
          ifelse(is.na(match(1, 1:31)), FALSE, TRUE)
      ) == FALSE)), collapse = ", ")))
    return(FALSE)
  } else {
    return(TRUE)
  }
}

check_input_coords <- function(longitude, latitude, geometry = NULL) {
  if (!is.null(geometry)) {
    if (!rlang::is_missing(longitude) || !rlang::is_missing(latitude)) {
      rlang::inform("the condition assigned coord and geometry, only the geometry will be used") # nolint
    }
    if (sf::st_is(geometry, "POINT")) {
      coords <-
        list(longitude = sf::st_coordinates(geometry)[1],
             latitude =  sf::st_coordinates(geometry)[2])
    } else {
      coords <-
        list(longitude = sf::st_coordinates(sf::st_centroid(geometry))[1],
             latitude =  sf::st_coordinates(sf::st_centroid(geometry))[2])
    }
  } else {
    coords <-
      list(longitude = rlang::quo_squash(longitude),
           latitude  = rlang::quo_squash(latitude))
  }
  coords
}

check_input_tidal_year <- function(year) {
  year <-
    as.character(year)
  rlang::arg_match(year,
                     as.character(seq.int(1997, lubridate::year(lubridate::now()))))
}

jma_pal <- function(palette = c("absolute", "relative"), .attribute = FALSE) {

  rlang::arg_match(palette)

  res <-
    list(
      absolute = list(
        colors = purrr::pmap_chr(
          list(r = c(180, 255, 255, 250,   0, 33,  160, 242),
               g = c(0,    40, 153, 245,  65, 140, 210, 242),
               b = c(104,   0,   0,   0, 255, 240, 255, 255)),
          ~ rgb(..1, ..2, ..3, max = 255)
        ),
        precipitation = list(
          labels = c("Over 80", "50~80", "30~50", "20~30", "10~20", "5~10", "1~5", "0~1"),
          breaks = c(80, 50, 30, 20, 10, 5, 1, 0)
        ),
        snow = list(
          labels = c("Over 200", "150~200", "100~150", "50~100",
                     "20~50", "5~20", "Under 5"),
          breaks = c(200, 150, 100, 50, 20, 5, 4)
        ),
        wind = list(
          labels = c("Over 25", "20~25", "15~20", "10~15", "5~10", "0~5"),
          breaks = c(25, 20, 15, 10, 5, 0)
        )
      ),
      relative = list(
        colors = purrr::pmap_chr(
          list(
            r = c(180, 255, 255, 250, 255, 255, 185,   0,   0,   0),
            g = c(  0,  40, 153, 245, 255, 255, 235, 150,  65,  32),
            b = c(104,   0,   0,   0, 150, 240, 255, 255, 255, 128)
          ),
          ~ rgb(..1, ..2, ..3, max = 255)
        ),
        amedas = list(
          labels =     c("35~", "30~35", "25~30", "20~25",
                         "15~20", "10~15", "5~10", "0~5",
                         "-5~0", "~-5"),
          limits = c(-5, 40),
          breaks = c(40, 35, 30, 25, 20, 15, 10, 5, 0, -5)
        ),
        forecast = list(
          labels =     c("35~", "30~35", "25~30", "20~24",
                         "15~19", "10~14", "5~9", "0~4",
                         "-5~-1", "~-5"),
          breaks = c(35, 30, 25, 20, 15, 10, 5, 0, -5, -6)
        )
      )
    )

  res <-
    res[[rlang::quo_name(palette)]]

  if (.attribute == FALSE)
    res <-
      res[[1]]

    res

}

split_period_char <- function(x) {
  stringr::str_split(x,
                     paste0("(?=([0-9]{4}/[0-9]{1,2}|[0-9]{4}.+",
                            intToUtf8(24180),
                            "))"),
                     simplify = TRUE) |>
    purrr::discard(function(x) stringr::str_length(x) == 0)
}

glue_split_period_char <- function(x, collapse = "_") {
  stringr::str_glue(
    "{x[1]}{collapse}{x[2]}",
    x = split_period_char(x))
}
