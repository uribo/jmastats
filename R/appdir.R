#' Remove cache files
#'
#' @description Remove all package cache files.
#' @rdname reset_cache
#' @export
reset_cache <- function() {
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
  cache_dir <- rappdirs::user_cache_dir("jmastats")
  if (!file.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  file.path(
      cache_dir,
      paste0(item, "_",
             station_type,
             "&",
             stringr::str_remove(param, "&view="),
             ".rds"))
}
