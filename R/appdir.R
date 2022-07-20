#' Remove cache files
#'
#' @description Remove all package cache files.
#' @rdname reset_cache
#' @export
reset_cache <- function() {
  unlink(rappdirs::user_cache_dir("jmastats"),
         recursive = TRUE)
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
