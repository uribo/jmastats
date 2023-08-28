#' Read the csv of the earthquake database
#' @param path local file path to earthquake record file.
#' @param show_metadata logical. If *FALSE*, returns only the values
#' observed at each location.
#' @rdname read_eqdb_csv
#' @seealso [https://www.data.jma.go.jp/svd/eqdb/data/shindo/index.html](https://www.data.jma.go.jp/svd/eqdb/data/shindo/index.html)
#' @export
read_eqdb_csv <- function(path, show_metadata = TRUE) {
  `観測点名` <- `震度` <- NULL
  if (show_metadata == TRUE) {
    meta <-
      readr::read_csv(path,
               n_max = 1,
               col_types = "Dtccccdc")
    cli::cli_inform(c("\u5730\u9707\u306e\u6982\u8981",
                  "x" = stringr::str_glue("\u767a\u751f\u65e5\u6642: {meta$地震の発生日} {meta$地震の発生時刻}"),
                  "x" = stringr::str_glue("\u9707\u592e\u5730\u540d: {meta$震央地名}"),
                  "*" = stringr::str_glue("\u7def\u5ea6: {meta$緯度}"),
                  "*" = stringr::str_glue("\u7d4c\u5ea6: {meta$経度}"),
                  "!" = stringr::str_glue("\u6df1\u3055: {meta$深さ}"),
                  "!" = stringr::str_glue("\u30de\u30b0\u30cb\u30c1\u30e5\u30fc\u30c9: {meta$Ｍ}"),
                  "!" = stringr::str_glue("\u6700\u5927\u9707\u5ea6: {meta$最大震度}")
                  ))
  }
  record <-
    readr::read_csv(path,
             skip = 2,
             col_types = "ccc")
  record %>%
    tidyr::separate_rows(`観測点名`,
                         sep = "[[:space:]]") %>%
    dplyr::mutate(`震度` = stringi::stri_trans_general(`震度`, "nfkc") %>%
                    stringr::str_remove("\u9707\u5ea6"),
                  `気象庁の震度観測点` = stringr::str_detect(`観測点名`, "\uff0a", negate = TRUE),
                  `観測点名` = stringr::str_remove(`観測点名`, "\uff0a"))
}
