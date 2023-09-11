#' Remove all cache files
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Remove all package cache files.
#' @importFrom utils askYesNo
#' @examples
#' if (interactive())
#'   reset_cache()
#' @rdname reset_cache
#' @export
#' @return None
reset_cache <- function() {
if (utils::askYesNo("Delete all cache files. Is it OK?"))
  unlink(rappdirs::user_cache_dir("jmastats"),
         recursive = TRUE)
}

pick_out_cache <- function(item = NULL,
                           block_no, year, month, day) {
  target <-
    detect_target(item, block_no, year, month, day)
  param <-
    xml2::url_parse(target$url)$query
  file_loc <-
    search_cache_file(item, target$station_type, param)
  if (file.exists(file_loc)) {
    unlink(file_loc)
  }
}

search_cache_file <- function(item, station_type, param) {
  cache_dir <-
    rappdirs::user_cache_dir("jmastats")
  file.path(
      cache_dir,
      paste0(item, "_",
             station_type,
             "&",
             stringr::str_remove(param, "&view="),
             ".rds"))
}
