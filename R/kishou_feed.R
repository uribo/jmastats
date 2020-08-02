#' Read Kishou Disaster Prevention Information Feed
#' @param url feed url (character)
#' @seealso [http://xml.kishou.go.jp/index.html](http://xml.kishou.go.jp/index.html)
#' @examples
#' \dontrun{
#' read_kishou_feed("http://www.data.jma.go.jp/developer/xml/feed/extra.xml")
#' }
#' @return data.fram
#' @export
read_kishou_feed <- function(url) {
  x <-
    xml2::read_xml(url)
  x_entry_index <-
    x %>%
    xml2::xml_children() %>%
    xml2::xml_name() %>%
    stringr::str_which("entry") %>%
    range()
  df <-
    seq.int(min(x_entry_index),
            max(x_entry_index)) %>%
    purrr::map_dfr(
      ~ parse_kishou_xml(x, .x))
  df
}

parse_kishou_xml <- function(x, index) {
  xx <-
    xml2::xml_child(x, index) %>%
    xml2::xml_children()
  xxx <-
    xx %>%
    purrr::discard(
      ~ xml2::xml_name(.x) == "link")
  xxx %>%
    xml2::xml_text() %>%
    purrr::set_names(xxx %>%
                       xml2::xml_name()) %>%
    as.list() %>%
    purrr::flatten_df() %>%
    purrr::update_list(link = xx %>%
                         purrr::keep(~ xml2::xml_name(.x) == "link") %>%
                         xml2::xml_attr("href")) %>%
    dplyr::mutate(id = stringr::str_remove_all(id, "urn:uuid:")) %>%
    readr::type_convert(
      col_types = readr::cols(
        title   = readr::col_character(),
        id      = readr::col_character(),
        updated = readr::col_datetime(format = ""),
        author  = readr::col_character(),
        content = readr::col_character(),
        link    = readr::col_character()))
}
