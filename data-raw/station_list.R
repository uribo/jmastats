#####################################
# Stations list
# Last Update: 2022-07-20
# 1. 地上気象観測地点,地域気象観測所
# 2. 潮位観測地点
# 3. 震度観測点
#####################################
library(dplyr, warn.conflicts = FALSE)
library(sf)
library(rnaturalearth) # for reverse geocoding
library(assertr)
library(polite)
library(rvest)

if (!file.exists("data-raw/amedas_raw.rds")) {
  # 1. 地上気象観測地点 -------------------------------------------------------------
  # # 1.1. zip archives ---------------------------------------------------------
  # Ref) https://www.data.jma.go.jp/developer/index.html
  # 地上気象観測地点 https://www.data.jma.go.jp/obd/stats/data/mdrr/chiten/sindex2.html
  # https://www.jma.go.jp/jma/kishou/know/amedas/ame_master.pdf
  # ame_master.zip はここから https://www.jma.go.jp/jma/kishou/know/amedas/kaisetsu.html
  if (!file.exists(here::here("data-raw/ame_master.csv"))) {
    # "https://www.data.jma.go.jp/developer/index.html" |>
    #   read_html() |>
    #   html_elements(css = "#contents_area2 > div > font > table") |>
    #   purrr::pluck(4) |>
    #   html_element(css = "a")
    zip_file <-
      "https://www.jma.go.jp/jma/kishou/know/amedas/ame_master.zip"
    download.file(
      zip_file,
      here::here("data-raw", basename(zip_file))
    )
    unzip(
      here::here("data-raw", basename(zip_file)),
      exdir = here::here("data-raw"))
  }
  df_amedas_master <-
    read.csv(
      here::here("data-raw/ame_master_20220701.csv"),
      fileEncoding = "cp932",
      stringsAsFactors = FALSE) %>%
    verify(dim(.) == c(1314, 16)) |>
    tibble::as_tibble() |>
    mutate(
      `都府県振興局` = stringi::stri_trans_general(`都府県振興局`, id = "nfkc"),
      `カタカナ名` = stringi::stri_trans_general(`ｶﾀｶﾅ名`, id = "nfkc"),
      longitude = as.numeric(substr(paste0(`経度.度.`, `経度.分.`), 1, 3)) +
        as.numeric(substr(paste0(`経度.度.`, `経度.分.`), 4, 6)) / 60,
      latitude = as.numeric(substr(paste0(`緯度.度.`, `緯度.分.`), 1, 2)) +
        as.numeric(substr(paste0(`緯度.度.`, `緯度.分.`), 3, 5)) / 60
    ) |>
    dplyr::select(seq_len(4), 6, 11, seq.int(14, 19)) |>
    purrr::set_names(
      c("area", "station_no", "station_type",
        "station_name", "address", "elevation",
        "observation_begin", "note1", "note2",
        "katakana", "longitude", "latitude")
    ) |>
    dplyr::mutate(dplyr::across(tidyselect::num_range("note", 1:2),
                                .fns = list(~ dplyr::if_else(.x %in% c("−", "\uff0d"),
                                                      NA_character_,
                                                      .x)),
                                .names = "{.col}")) %>%
    dplyr::mutate(area = dplyr::recode(area,
                                       `オホーツク` = "網走・北見・紋別")) %>%
    verify(ncol(.) == 12L)
  # 1.2. scraping  ---------------------------------------------------------------
  read_block_no <- function(prec_no) {
    url <-
      polite::bow(glue::glue(
      "https://www.data.jma.go.jp/obd/stats/etrn/select/prefecture.php?prec_no={prec_no}&block_no=&year=&month=&day=&view="))
    polite::scrape(url) |>
      rvest::html_elements(css = "#ncontents2 > map > area") |>
      rvest::html_attrs() %>%
      tibble::tibble(
        station = purrr::map_chr(., "alt"),
        prec_no = purrr::map_chr(., "href") |>
          stringr::str_extract("prec_no=[0-9]{2}") |>
          stringr::str_remove("prec_no="),
        block_no = purrr::map_chr(., "href") |>
          stringr::str_extract("block_no=[0-9]{3,5}") |>
          stringr::str_remove("block_no=")) |>
      dplyr::select(-1) |>
      dplyr::filter(!is.na(block_no)) |>
      dplyr::distinct()
  }

  df_prec_no <-
    xml2::read_html("https://www.data.jma.go.jp/obd/stats/etrn/select/prefecture00.php?prec_no=&block_no=&year=&month=&day=&view=") |>
    rvest::html_elements(css = "#main > map > area") |>
    rvest::html_attrs() %>%
    tibble::tibble(
      area = purrr::map_chr(., "alt"),
      prec_no = purrr::map_chr(., "href") |>
        stringr::str_extract("prec_no=[0-9]{2}") |>
        stringr::str_remove("prec_no=")) |>
    dplyr::select(-1) %>%
    verify(dim(.) == c(61, 2))

  # 1.3 Merge ---------------------------------------------------------------
  # ~ 2 mins.
  df_stations <-
    df_prec_no |>
    dplyr::pull(prec_no) |>
    unique() %>%
    ensurer::ensure(length(.) == 61L) |>
    purrr::map_df(
      read_block_no) %>%
    verify(dim(.) == c(1674, 3)) |>
    left_join(df_prec_no, by = "prec_no") |>
    mutate(area = stringr::str_remove(area, "地方")) %>%
    verify(dim(.) == c(1674, 4)) |>
    mutate(area = stringr::str_remove(area, "(都|府|県)$"),
           area = dplyr::if_else(station == "竜王山", "徳島", area),
           station = stringr::str_remove(station, "（.+）")) %>%
    rename(station_name = station)

  # 現在も観測が行われているものに制限される
  # 例) ピヤシリ山 (block_no=0007)は除外
  stations <-
    df_amedas_master |>
    left_join(df_stations,
              by = c("station_name", "area")) |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    dplyr::mutate(
      dplyr::across(.cols = c(area, station_type, station_name,
                              address, observation_begin, note1, note2,
                              katakana),
                    .fns = ~ stringi::stri_conv(.x, to = "UTF8"),
                    .names = "{.col}")) %>%
    tibble::new_tibble(nrow = nrow(.), class = "sf") %>%
    verify(nrow(.) == 1321L)
  stations |>
    readr::write_rds("data-raw/amedas_raw.rds")
} else {
  stations <-
    readr::read_rds("data-raw/amedas_raw.rds")
}

ne_jpn <-
  ne_states(country = "Japan", returnclass = "sf") %>%
  tibble::new_tibble(nrow = nrow(.), class = "sf") |>
  arrange(iso_3166_2) |>
  select(iso_3166_2) |>
  transmute(pref_code = stringr::str_remove(iso_3166_2, "JP-"))

stations <-
  stations |>
  st_join(ne_jpn) %>%
  verify(dim(.) == c(1321, 14L))

# 1.4 Manual fix ------------------------------------------------------------
prefecture_code <- c(`13061` = "01",
                     `13121` = "01",
                     `13146` = "01",
                     `13277` = "01",
                     `13311` = "01",
                     `14071` = "01",
                     `16061` = "01",
                     `17351` = "01",
                     `19432` = "01",
                     `19451` = "01",
                     `22391` = "01",
                     `21261` = "01",
                     `21297` = "01",
                     `21323` = "01",
                     `23281` = "01",
                     `24156` = "01",
                     `24166` = "01",
                     `24217` = "01",
                     `23376` = "01",
                     `31001` = "02",
                     `32056` = "05",
                     `32286` = "05",
                     `32616` = "05",
                     `35002` = "06",
                     `34292` = "04",
                     `34361` = "04",
                     `35246` = "06",
                     `45147` = "12",
                     `45346` = "12",
                     `54012` = "15",
                     `54157` = "15",
                     `36846` = "07",
                     `56401` = "17",
                     `56036` = "17",
                     `56116` = "17",
                     `45371` = "12",
                     `44136` = "13",
                     `44173` = "13",
                     `44172` = "13",
                     `44191` = "13",
                     `44216` = "13",
                     `44226` = "13",
                     `44263` = "13",
                     `44262` = "13",
                     `44263` = "13",
                     `44281` = "13",
                     `44301` = "13",
                     `46141` = "14",
                     `46211` = "14",
                     `50506` = "22",
                     `50416` = "22",
                     `50561` = "22",
                     `50551` = "22",
                     `51261` = "23",
                     `57051` = "18",
                     `65036` = "30",
                     `65106` = "30",
                     `65288` = "30",
                     `65356` = "30",
                     `63517` = "28",
                     `63496` = "28",
                     `63491` = "28",
                     `63551` = "28",
                     `66501` = "33",
                     `68022` = "32",
                     `68046` = "32",
                     `67511` = "34",
                     `67576` = "34",
                     `67461` = "34",
                     `67471` = "34",
                     `81428` = "35",
                     `81436` = "35",
                     `81486` = "35",
                     `72111` = "37",
                     `73168` = "38",
                     `73256` = "38",
                     `73341` = "38",
                     `74271` = "39",
                     `74276` = "39",
                     `74372` = "39",
                     `74447` = "39",
                     `74506` = "39",
                     `74516` = "39",
                     `82068` = "40",
                     `82036` = "40",
                     `85176` = "41",
                     `84122` = "42",
                     `84171` = "42",
                     `84236` = "42",
                     `84266` = "42",
                     `84286` = "42",
                     `84306` = "42",
                     `84356` = "42",
                     `84371` = "42",
                     `84596` = "42",
                     `84597` = "42",
                     `83126` = "44",
                     `83476` = "44",
                     `86396` = "43",
                     `86491` = "43",
                     `86216` = "43",
                     `87492` = "45",
                     `88551` = "46",
                     `88612` = "46",
                     `88746` = "46",
                     `88756` = "46",
                     `88776` = "46",
                     `88821` = "46",
                     `88901` = "46",
                     `88851` = "46",
                     `88931` = "46",
                     `88956` = "46",
                     `88971` = "46",
                     `88781` = "46",
                     `91081` = "47",
                     `91107` = "47",
                     `91236` = "47",
                     `91216` = "47",
                     `91141` = "47",
                     `93051` = "47",
                     `93042` = "47",
                     `93041` = "47",
                     `93012` = "47",
                     `93062` = "47",
                     `94081` = "47",
                     `94101` = "47",
                     `94062` = "47",
                     `92011` = "47",
                     `91011` = "47",
                     `91166` = "47",
                     `91151` = "47",
                     `94036` = "47",
                     `94121` = "47",
                     `94116` = "47")

# stations %>%
#   filter(is.na(pref_code)) %>%
#   nrow()
#
# length(prefecture_code)
#
# stations %>%
#   filter(station_no == 94116) %>%
#   select(area, station_name, pref_code)
#
# stations %>%
#   filter(!is.na(pref_code)) %>%
#   filter(station_no %in% names(prefecture_code)) %>%
#   select(area, station_no, station_name, pref_code)

stations <-
  stations |>
  mutate(pref_code = recode(station_no,
                            !!!prefecture_code,
                            .default = pref_code
                            ))

stations |>
  st_drop_geometry() |>
  filter(is.na(pref_code)) %>%
  assertr::verify(nrow(.) == 0L)

stations <-
  stations |>
  dplyr::mutate(dplyr::across(tidyselect:::where(is.character),
                   .funs = list(~ stringi::stri_conv(str = ., to = "UTF8")))) |>
  select(area,
         station_no,
         station_type,
         station_name,
         address,
         elevation,
         observation_begin,
         note1,
         note2,
         katakana,
         prec_no,
         block_no,
         pref_code,
         geometry) %>%
  verify(dim(.) == c(1321, 14))

usethis::use_data(stations, overwrite = TRUE)

# 2. 潮位観測 ---------------------------------------------------------------------
# ref) https://www.data.jma.go.jp/gmd/kaiyou/db/tide/suisan/station.php
# https://www.jma.go.jp/jp/choi/list1.html
library(parzer)
library(httr)

years <-
  seq.int(1997, lubridate::year(lubridate::now()))

tide_station <-
  years |>
  purrr::set_names(as.character(years)) |>
  purrr::map_df(
    ~ polite::bow(glue::glue("https://www.data.jma.go.jp/gmd/kaiyou/db/tide/genbo/station.php?year={.x}")) %>%
      polite::scrape() %>%
      html_table(fill = TRUE) |>
      purrr::pluck(1) |>
      tibble::repair_names() |>
      tibble::as_tibble(),
    .id = "year")

tide_station <-
  tide_station |>
  filter(stringr::str_detect(`地点番号`, "\uff0a", negate = TRUE),
         `地点番号` != "地点番号") |>
  select(seq.int(8)) |>
  purrr::set_names(c("year", "id", "stn", "station_name", "address",
                     "latitude", "longitude", "type")) |>
  mutate_at(vars(longitude, latitude),
            list(~ stringr::str_replace_all(., c("\u309c" = "\u00b0")))) |>
  mutate(longitude = parzer::parse_lon(longitude),
         latitude = parzer::parse_lat(latitude)) |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  verify(dim(.) == c(1809, 7))

usethis::use_data(tide_station, overwrite = TRUE)


# 3. 震度観測点 ----------------------------------------------------------------
x <-
  read_html("https://www.data.jma.go.jp/eqev/data/kyoshin/jma-shindo.html")
x %>%
  html_element(css = "#main > h1") %>%
  html_text() # 令和4年2月24日現在

earthquake_station <-
  x %>%
  html_elements(css = "#main > table") %>%
  html_table() %>%
  purrr::set_names(x %>%
              html_elements(css = "#main > p > a") %>%
              html_text()) %>%
  purrr::map_dfr(
    ~ mutate(.x,
             across(.cols = everything(),
                    .fns = as.character)),
    .id = "prefecture"
  ) %>%
  verify(dim(.) == c(1112, 10)) %>%
  readr::type_convert(col_types = "ccccididcc") %>%
  purrr::set_names(c("prefecture", "area", "station_name", "address",
                     "lat_do", "lat_fun",
                     "lng_do", "lng_fun",
                     "observation_begin", "observation_end"))

earthquake_station <-
  earthquake_station %>%
  mutate(longitude = purrr::pmap_chr(.,
                                    ~ kuniezu::parse_lon_dohunbyo(paste0("東経", ..7, "度", ..8, "秒"))),
         latitude = purrr::pmap_chr(.,
                                    ~ kuniezu::parse_lat_dohunbyo(paste0("北緯", ..5, "度", ..6, "秒")))) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4612) %>%
  sf::st_transform(crs = 4326) %>%
  select(!c(ends_with("_do"), ends_with("_fun"))) %>%
  verify(dim(.) == c(1112, 7)) %>%
  filter(is.na(observation_end)) %>%
  verify(nrow(.) == 671L)

usethis::use_data(earthquake_station, overwrite = TRUE)

# mapview::mapview(earthquake_station)

# st_point(c(kuniezu::parse_lon_dohunbyo(paste0("東経", earthquake_station$lng_do[1], "度", earthquake_station$lng_fun[1], "秒")),
#            kuniezu::parse_lat_dohunbyo(paste0("北緯", earthquake_station$lat_do[1], "度", earthquake_station$lat_fun[1], "秒")))) %>%
#   st_sfc(crs = 4612) %>%
#   st_transform(crs = 4326) %>%
#   mapview::mapview()
