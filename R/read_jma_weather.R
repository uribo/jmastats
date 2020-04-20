#' Read the past weather
#' @description Read the past weather data files downloaded from JMA.
#' @param path The path to the downloaded file.
#' @seealso [https://www.data.jma.go.jp/gmd/risk/obsdl/index.php](https://www.data.jma.go.jp/gmd/risk/obsdl/index.php),
#' [https://www.data.jma.go.jp/gmd/risk/obsdl/top/help3.html](https://www.data.jma.go.jp/gmd/risk/obsdl/top/help3.html)
#' @export
read_jma_weather <- function(path) {
  var_st_names <-
    station_head_info(path)
  var_names <-
    paste0(
      read_jma_single_row(path, 4),
      read_jma_single_row(path, 5) %>%
        stringr::str_c("_", .) %>%
        stringr::str_remove("^_$")
    )
  var_names_combination <-
    paste(var_st_names,
          var_names,
          sep = "_")
  d <-
    readr::read_csv(
      path,
      skip = 6,
      locale = readr::locale(encoding = "cp932"),
      col_names = c("date",
                    var_names_combination),
      col_types = paste0("c",
                         paste(rep(
                           "c", length(var_names_combination)
                         ), collapse = ""))
    ) %>%
    readr::type_convert() %>%
    dplyr::mutate(date = lubridate::as_date(date))
  d
}

read_jma_single_row <- function(path, row) {
  readr::read_lines(path,
                    skip = row -1,
                    n_max = 1,
                    locale = readr::locale(encoding = "cp932")) %>%
    stringr::str_split(",", simplify = TRUE) %>%
    as.vector() %>%
    utils::tail(length(.) - 1)
}

station_head_info <- function(path) {
  var_st_names <-
    read_jma_single_row(path, 3)
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
