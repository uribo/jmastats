#' Read the csv of the earthquake database
#'
#' @description
#' `r lifecycle::badge("stable")`
#' @param path local file path to earthquake record file.
#' @param show_metadata logical. If *FALSE*, returns only the values
#' observed at each location.
#' @examples
#' read_eqdb_csv(system.file("dummy/eqdb.csv", package = "jmastats"))
#' @rdname read_eqdb_csv
#' @seealso <https://www.data.jma.go.jp/svd/eqdb/data/shindo/index.html>
#' @export
#' @return a `tbl` object
read_eqdb_csv <- function(path, show_metadata = TRUE) {
  if (show_metadata == TRUE) {
    meta <-
      list(
        item = list(c(22320, 38663, 12398, 27010, 35201),
                 c(30330, 29983, 26085, 26178),
                 c(38663, 22830, 22320, 21517),
                 c(32239, 24230),
                 c(32076, 24230),
                 c(28145, 12373),
                 c(12510, 12464, 12491, 12481, 12517, 12540, 12489),
                 c(26368, 22823, 38663, 24230)
        ),
        vars = readr::read_csv(path,
                        n_max = 1,
                        col_types = "Dtccccdc")
      )
    cli::cli_inform(
      c(intToUtf8(meta$item[[1]]),
        "x" = stringr::str_glue("{intToUtf8(meta$item[[2]])}: {meta$vars[[1]]} {meta$vars[[2]]}"),
                  "x" = stringr::str_glue("{intToUtf8(meta$item[[3]])}: {meta$vars[[3]]}"),
                  "*" = stringr::str_glue("{intToUtf8(meta$item[[4]])}: {meta$vars[[4]]}"),
                  "*" = stringr::str_glue("{intToUtf8(meta$item[[5]])}: {meta$vars[[5]]}"),
                  "!" = stringr::str_glue("{intToUtf8(meta$item[[6]])}: {meta$vars[[6]]}"),
                  "!" = stringr::str_glue("{intToUtf8(meta$item[[7]])}: {meta$vars[[7]]}"),
                  "!" = stringr::str_glue("{intToUtf8(meta$item[[8]])}: {meta$vars[[8]]}")
                  ))
  }
  record <-
    readr::read_csv(path,
             skip = 2,
             col_types = "ccc")
  record |>
    tidyr::separate_longer_delim(
      cols = intToUtf8(c(35251, 28204, 28857, 21517)),
      delim = " ") |>
    dplyr::mutate(!!rlang::sym(intToUtf8(c(38663, 24230))) :=
                    stringr::str_remove(!!rlang::sym(intToUtf8(c(38663, 24230))),
                                        "\u9707\u5ea6") |>
                    dplyr::case_match(
                      intToUtf8(65297) ~ "1",
                      intToUtf8(65298) ~ "2",
                      intToUtf8(65299) ~ "3",
                      intToUtf8(65300) ~ "4",
                      intToUtf8(65301) ~ "5",
                      intToUtf8(c(65301, 24369)) ~ intToUtf8(c(53, 24369)),
                      intToUtf8(c(65301, 24375)) ~ intToUtf8(c(53, 24375)),
                      intToUtf8(65302) ~ "6",
                      intToUtf8(c(65302, 24369)) ~ intToUtf8(c(54, 24369)),
                      intToUtf8(c(65302, 24375)) ~ intToUtf8(c(54, 24375)),
                      intToUtf8(65303) ~ "7"),
                  !!rlang::sym(intToUtf8(c(27671, 35937, 24193, 12398, 38663, 24230, 35251, 28204, 28857))) := stringr::str_detect(
                    !!rlang::sym(intToUtf8(c(35251, 28204, 28857, 21517))),
                    "\uff0a",
                    negate = TRUE),
                  !!rlang::sym(intToUtf8(c(35251, 28204, 28857, 21517))) := stringr::str_remove(
                    !!rlang::sym(intToUtf8(c(35251, 28204, 28857, 21517))),
                    "\uff0a"))
}
