#####################################
# Stations list
#####################################
library(dplyr)
# # 1. zip archives ---------------------------------------------------------
# if (file.exists(here::here("data-raw", "ame_master.csv")) == FALSE) {
#   zip_file <- "http://www.jma.go.jp/jma/kishou/know/amedas/ame_master.zip"
#   download.file(
#     zip_file,
#     here::here("data-raw", basename(zip_file))
#   )
#   unzip(
#     here::here("data-raw", basename(zip_file)),
#     exdir = here::here("data-raw")
#   )
#
#   d <-
#     read.csv(here::here("data-raw", "ame_master.csv"), fileEncoding = "cp932", stringsAsFactors = FALSE) %>%
#     tibble::as_tibble() %>%
#     select(1:4, 6)
#
#   names(d) <-
#     c("area", "block_no", "station_type", "station_name", "address")
#
#   # mismatch
#   # d %>%
#   #   filter(block_no == 47646)
# }

# 2. scraping  ---------------------------------------------------------------
library(rvest)
read_block_no <- function(prec_no) {
  Sys.sleep(2)
  read_html(glue::glue(
    "http://www.data.jma.go.jp/obd/stats/etrn/select/prefecture.php?prec_no={prec_no}&block_no=&year=&month=&day=&view=")) %>%
    html_nodes(css = "#ncontents2 > map > area") %>%
    html_attrs() %>%
    tibble(
      station = purrr::map_chr(., "alt"),
      prec_no = purrr::map_chr(., "href") %>%
        stringr::str_extract("prec_no=[0-9]{2}") %>%
        stringr::str_remove("prec_no="),
      block_no = purrr::map_chr(., "href") %>%
        stringr::str_extract("block_no=[0-9]{4}") %>%
        stringr::str_remove("block_no=")) %>%
    select(-1) %>%
    filter(!is.na(block_no)) %>%
    distinct()
}

df_prec_no <-
  read_html("http://www.data.jma.go.jp/obd/stats/etrn/select/prefecture00.php?prec_no=&block_no=&year=&month=&day=&view=") %>%
  html_nodes(css = '#main > map > area') %>%
  html_attrs() %>%
  tibble::tibble(
    alt = purrr::map_chr(., "alt"),
    prec_no = purrr::map_chr(., "href") %>%
      stringr::str_extract("prec_no=[0-9]{2}") %>%
      stringr::str_remove("prec_no=")) %>%
  select(-1)

df_stations <-
  df_prec_no$prec_no %>%
  unique() %>%
  purrr::map_df(
    read_block_no
  )

stations <- df_stations

usethis::use_data(stations, overwrite = TRUE)
