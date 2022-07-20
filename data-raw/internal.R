library(tibble)
library(magrittr)

x <-
  xml2::read_html("https://www.data.jma.go.jp/obd/stats/etrn/index.php?prec_no=40&block_no=47646&year=2018&month=5&day=7&view=")

df_jma_pages <-
  data_frame(
  title = x %>%
    rvest::html_nodes(css = '#main > table > tr > td:nth-child(3) > div > table > tr > td > table > tr > td.nwtop > a') %>%
    rvest::html_text() %>%
    .[1:18],
  page_id = x %>%
    rvest::html_nodes(css = '#main > table > tr > td:nth-child(3) > div > table > tr > td > table > tr > td.nwtop > a') %>%
    rvest::html_attr(name = "href") %>%
    stringr::str_remove("view/") %>%
    stringr::str_remove(".php.+") %>%
    .[1:18]
)

usethis::use_data(df_jma_pages, internal = TRUE)
