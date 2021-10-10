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
    cli::cli_inform(c("地震の概要",
                  "x" = glue::glue("発生日時: {meta$地震の発生日} {meta$地震の発生時刻}"),
                  "x" = glue::glue("震央地名: {meta$震央地名}"),
                  "*" = glue::glue("緯度: {meta$緯度}"),
                  "*" = glue::glue("経度: {meta$経度}"),
                  "!" = glue::glue("深さ: {meta$深さ}"),
                  "!" = glue::glue("マグニチュード: {meta$Ｍ}"),
                  "!" = glue::glue("最大震度: {meta$最大震度}")
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
                    stringr::str_remove("震度"),
                  `気象庁の震度観測点` = stringr::str_detect(`観測点名`, "\uff0a", negate = TRUE),
                  `観測点名` = stringr::str_remove(`観測点名`, "\uff0a"))
}
