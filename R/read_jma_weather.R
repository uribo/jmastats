#' Read the past weather
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Read the past weather data files downloaded from JMA.
#' @param path The path to the downloaded file.
#' @seealso <https://www.data.jma.go.jp/gmd/risk/obsdl/index.php>,
#' <https://www.data.jma.go.jp/gmd/risk/obsdl/top/help3.html>
#' @examples
#' read_jma_weather(system.file("dummy/dl_data.csv", package = "jmastats"))
#' @export
#' @return a `tbl` object
read_jma_weather <- function(path) {
  var_st_names <-
    station_head_info(path)
  if (identical(class(path), "raw")) {
    path <-
      iconv(rawToChar(path), from = "cp932", to = "utf8")
    var_names <-
      paste0(
        read_jma_single_row(path, 4, enc_convert = FALSE),
        read_jma_single_row(path, 5, enc_convert = FALSE) |>
          stringr::str_c("_", .) |>
          stringr::str_remove("^_$"))
  } else {
    var_names <-
      paste0(
        read_jma_single_row(path, 4),
        read_jma_single_row(path, 5) |>
          stringr::str_c("_", .) |>
          stringr::str_remove("^_$"))
  }
  var_names_combination <-
    paste(var_st_names,
          var_names,
          sep = "_")
  d <-
    readr::read_csv(
      path,
      skip = 5,
      locale = readr::locale(encoding = "cp932"),
      col_names = c("date",
                    var_names_combination),
      col_types = paste0("c",
                         paste(rep(
                           "c", length(var_names_combination)
                         ), collapse = ""))
    ) |>
    readr::type_convert()
  if (stringr::str_length(d$date[1]) == 8) {
    d <-
      d |>
      dplyr::mutate(date = lubridate::as_date(date))
  } else if (stringr::str_length(d$date[1]) == 16) {
    d <-
      d |>
      dplyr::mutate(date = lubridate::as_datetime(date)) |>
      dplyr::rename(datetime = date)
  }
  d
}

read_jma_single_row <- function(path, row, enc_convert = TRUE) {
  if (enc_convert == TRUE) {
    locale <- readr::locale(encoding = "cp932")
  } else {
    locale <- readr::default_locale()
  }
  readr::read_lines(path,
                    skip = row -1,
                    n_max = 1,
                    locale = locale) |>
    stringr::str_split(",", simplify = TRUE) |>
    as.vector() |>
    utils::tail(length(.) - 1)
}

station_head_info <- function(path) {
  if (identical(class(path), "raw")) {
    var_st_names <-
      read_jma_single_row(iconv(rawToChar(path), from = "cp932", to = "utf8"),
                          3,
                          enc_convert = FALSE)
  } else {
    var_st_names <-
      read_jma_single_row(path, 3)
  }
  target_st <-
    unique(var_st_names)
  cat(
    crayon::black(paste0("Selected station",
                         dplyr::if_else(length(target_st) == 1L,
                                        "",
                                        "s"),
                         ":")),
    crayon::cyan(
      stringr::str_c(target_st, collapse = ", ")),
    "\n")
  var_st_names
}
