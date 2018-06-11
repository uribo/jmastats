#####################################
# Stations list
#####################################
library(dplyr)
# # 1. zip archives ---------------------------------------------------------
# Ref) http://www.data.jma.go.jp/developer/index.html
# http://www.data.jma.go.jp/obd/stats/data/mdrr/chiten/sindex2.html
# https://www.jma.go.jp/jma/kishou/know/amedas/ame_master.pdf
if (file.exists(here::here("data-raw", "ame_master.csv")) == FALSE) {
  zip_file <- "http://www.jma.go.jp/jma/kishou/know/amedas/ame_master.zip"
  download.file(
    zip_file,
    here::here("data-raw", basename(zip_file))
  )
  unzip(
    here::here("data-raw", basename(zip_file)),
    exdir = here::here("data-raw")
  )
}

d <-
  read.csv(
    here::here("data-raw", "ame_master.csv"),
    fileEncoding = "cp932",
    stringsAsFactors = FALSE) %>%
  tibble::as_tibble() %>%
  mutate(
    `都府県振興局` = stringi::stri_trans_general(`都府県振興局`, id = "nfkc"),
    `カタカナ名` = stringi::stri_trans_general(`ｶﾀｶﾅ名`, id = "nfkc"),
    longitude = as.numeric(substr(paste0(`経度.度.`, `経度.分.`), 1, 3)) +
      as.numeric(substr(paste0(`経度.度.`, `経度.分.`), 4, 6)) / 60,
    latitude = as.numeric(substr(paste0(`緯度.度.`, `緯度.分.`), 1, 2)) +
      as.numeric(substr(paste0(`緯度.度.`, `緯度.分.`), 3, 5)) / 60
  ) %>%
  dplyr::select(1:4, 6, 11, 14:19)

names(d) <- c("area", "station_no", "station_type",
              "station_name", "address", "elevation",
              "observation_begin", "note1", "note2",
              "katakana", "longitude", "latitude")

d <-
  d %>%
  dplyr::mutate_at(dplyr::vars(c("note1", "note2")),
                   dplyr::funs(dplyr::if_else(. == "−", NA_character_, .)))

d <-
  d %>%
  dplyr::mutate(area = dplyr::recode(area,
                                     `オホーツク` = "網走・北見・紋別"))

  # mismatch
  # d %>%
  #   filter(block_no == 47646) # 茨城県　つくば（館野）

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
        stringr::str_extract("block_no=[0-9]{3,5}") %>%
        stringr::str_remove("block_no=")) %>%
    select(-1) %>%
    filter(!is.na(block_no)) %>%
    distinct()
}


df_prec_no <-
  read_html("http://www.data.jma.go.jp/obd/stats/etrn/select/prefecture00.php?prec_no=&block_no=&year=&month=&day=&view=") %>%
  html_nodes(css = "#main > map > area") %>%
  html_attrs() %>%
  tibble::tibble(
    area = purrr::map_chr(., "alt"),
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

df_stations <-
  df_stations %>%
  left_join(df_prec_no, by = "prec_no") %>%
  mutate(area = stringr::str_remove(area, "地方"))

stations <-
  d %>%
  left_join(df_stations %>%
              mutate(area = stringr::str_remove(area, "(都|府|県)$"),
                     area = dplyr::if_else(station == "竜王山", "徳島", area),
                     station = stringr::str_remove(station, "（.+）")),
            by = c("station_name" = "station", "area")) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  tibble::new_tibble(subclass = "sf")

usethis::use_data(stations, overwrite = TRUE)
