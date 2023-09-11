#' Read Kishou Disaster Prevention Information Feed
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' @param frequency Release frequency. Select either high frequency ("high") or
#' long term ("low")
#' @param type Feed type. Specify the item to be retrieved as a string.
#' See details for the items.
#' @details The following items can be specified in the type argument.
#' - regular: It will be announced on time.
#' - extra: It will be announced at any time.
#' - eqvol: Earthquakes and Volcanoes.
#' - other: Other informations.
#' @seealso <https://xml.kishou.go.jp>
#' @examples
#' \donttest{
#' read_kishou_feed("high", type = "regular")
#' read_kishou_feed("low", "other")
#' }
#' @return a `tbl` object
#' @export
read_kishou_feed <- function(frequency, type) {
  x <-
    create_feed_url(frequency, type) |>
    xml2::read_xml()
  x_entry_index <-
    x |>
    xml2::xml_children() |>
    xml2::xml_name() |>
    stringr::str_which("entry") |>
    range()
  df <-
    seq.int(min(x_entry_index),
            max(x_entry_index)) |>
    purrr::map(
      ~ parse_kishou_xml(x, .x)) |>
    purrr::list_rbind()
  df
}

create_feed_url <- function(frequency, type) {
  freq <-
    rlang::arg_match(frequency,
                     c("high",
                       "low"))
  freq <-
    dplyr::if_else(freq == "low", "_l", "")
  type <-
    rlang::arg_match(type,
                     c("regular",
                       "extra",
                       "eqvol",
                       "other"))
  stringr::str_glue("https://www.data.jma.go.jp/developer/xml/feed/{type}{freq}.xml")
}

parse_kishou_xml <- function(x, index) {
  xx <-
    xml2::xml_child(x, index) |>
    xml2::xml_children()
  xxx <-
    xx |>
    purrr::discard(
      ~ xml2::xml_name(.x) == "link")
  xxx |>
    xml2::xml_text() |>
    purrr::set_names(xxx |>
                       xml2::xml_name()) |>
    as.list() |>
    purrr::flatten_df() |>
    purrr::update_list(link = xx |>
                         purrr::keep(~ xml2::xml_name(.x) == "link") |>
                         xml2::xml_attr("href")) |>
    dplyr::mutate(id = stringr::str_remove_all(id, "urn:uuid:")) |>
    readr::type_convert(
      col_types = readr::cols(
        title   = readr::col_character(),
        id      = readr::col_character(),
        updated = readr::col_datetime(format = ""),
        author  = readr::col_character(),
        content = readr::col_character(),
        link    = readr::col_character()))
}
