#####################################
# Stations list
# Last Update: 2025-01-28
# 1. 地上気象観測地点,地域気象観測所 (適用日：2024年12月18日)
# 2. 潮位観測地点（2025-01-01）
# 3. 震度観測点 (2024-11-21)
#####################################
# pak::pkg_install("uribo/kuniezu")
library(dplyr, warn.conflicts = FALSE)
library(sf)
library(rnaturalearth) # for reverse geocoding
library(assertr)
library(polite)
library(rvest)
library(ensurer)
library(pointblank)

if (!file.exists(here::here("data-raw/amedas_raw.rds"))) {
  # 1. 地上気象観測地点 -------------------------------------------------------------
  # # 1.1. zip archives ---------------------------------------------------------
  # Ref) https://www.data.jma.go.jp/developer/index.html
  # 地上気象観測地点 https://www.data.jma.go.jp/stats/data/mdrr/chiten/sindex2.html
  # https://www.jma.go.jp/jma/kishou/know/amedas/ame_master.pdf
  # ame_master.zip はここから https://www.jma.go.jp/jma/kishou/know/amedas/kaisetsu.html
  ame_master <- "ame_master_20241218.csv"
  if (!file.exists(here::here(stringr::str_glue("data-raw/{ame_master}")))) {
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
      here::here(stringr::str_glue("data-raw/{ame_master}")),
      fileEncoding = "cp932",
      stringsAsFactors = FALSE) |>
    pointblank::row_count_match(1316L) |>
    pointblank::col_count_match(17L) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      `都府県振興局` = stringi::stri_trans_general(`都府県振興局`, id = "nfkc"),
      `カタカナ名` = stringi::stri_trans_general(`ｶﾀｶﾅ名`, id = "nfkc"),
      longitude = as.numeric(`経度.度.`) + (as.numeric(`経度.分.`) / 60),
      latitude = as.numeric(`緯度.度.`) + (as.numeric(`緯度.分.`) / 60)
    ) |>
    dplyr::select(seq_len(4), 7, 12, seq.int(15, 20)) |>
    # "都府県振興局", "観測所番号", "種類", "観測所名",
    # "所在地", "海面上の高さ.ｍ.", "観測開始年月日",
    # "備考1", "備考2", "カタカナ名", "longitude", "latitude"
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
                                .names = "{.col}")) |>
    # csv上では「オホーツク」だが「網走・北見・紋別」
    dplyr::mutate(area = dplyr::recode(area,
                                       `オホーツク` = "網走・北見・紋別")) |>
    pointblank::col_count_match(12L)
  # 1.2. scraping  ---------------------------------------------------------------
  read_block_no <- function(prec_no) {
    url <-
      polite::bow(stringr::str_glue(
      "https://www.data.jma.go.jp/stats/etrn/select/prefecture.php?prec_no={prec_no}&block_no=&year=&month=&day=&view="))
    polite::scrape(url) |>
      rvest::html_elements(css = "#ncontents2 > map > area") |>
      rvest::html_attrs() %>% # magrittr
      tibble::tibble(
        station = purrr::map_chr(., "alt"),
        prec_no = purrr::map_chr(., "href") |>
          stringr::str_extract("prec_no=[0-9]{2}") |>
          stringr::str_remove("prec_no="),
        block_no = purrr::map_chr(., "href") |>
          stringr::str_extract("block_no=[0-9]{3,5}") |>
          stringr::str_remove("block_no=")) |>
      dplyr::select(!1) |>
      dplyr::filter(!is.na(block_no)) |>
      dplyr::distinct()
  }

  df_prec_no <-
    xml2::read_html("https://www.data.jma.go.jp/stats/etrn/select/prefecture00.php?prec_no=&block_no=&year=&month=&day=&view=") |>
    rvest::html_elements(css = "#main > map > area") |>
    rvest::html_attrs() %>% # magrittr
    tibble::tibble(
      area = purrr::map_chr(., "alt"),
      prec_no = purrr::map_chr(., "href") |>
        stringr::str_extract("prec_no=[0-9]{2}") |>
        stringr::str_remove("prec_no=")) |>
    dplyr::select(!1) |>
    pointblank::row_count_match(61L) |>
    pointblank::col_count_match(2L)

  # 1.3 Merge ---------------------------------------------------------------
  # ~ 2 mins.
  df_stations_raw <-
    df_prec_no |>
    dplyr::pull(prec_no) |>
    unique() |>
    ensurer::ensure(length(.) == 61L) |>
    purrr::map(
      read_block_no) |>
    purrr::list_rbind() |>
    pointblank::row_count_match(1677L) |>
    pointblank::col_count_match(3L)
  df_stations <-
    df_stations_raw |>
    dplyr::left_join(df_prec_no,
                     by = dplyr::join_by(prec_no)) |>
    dplyr::mutate(area = stringr::str_remove(area, "地方")) |>
    pointblank::row_count_match(1677L) |>
    pointblank::col_count_match(4L) |>
    dplyr::mutate(area = stringr::str_remove(area, "(都|府|県)$"),
                  area = dplyr::if_else(station == "竜王山", "徳島", area),
           station = stringr::str_remove(station, "（.+）")) |>
    dplyr::rename(station_name = station)

  # https://www.data.jma.go.jp/stats/etrn/select/prefecture.php?prec_no=50&block_no=&year=&month=&day=&view=
  df_stations |>
    filter(station_name == "三倉", area == "静岡") |>
    pointblank::row_count_match(2L) # 2地点でOK
  df_stations |>
    filter(station_name == "新島", area == "東京") |>
    pointblank::row_count_match(2L) # 2地点でOK
  df_amedas_master |>
    filter(station_name == "高知", area == "高知") |>
    pointblank::row_count_match(2L) # 2地点でOK

  stations <-
    df_amedas_master |>
    dplyr::left_join(df_stations,
              by = dplyr::join_by(station_name, area),
              multiple = "all",
              relationship = "many-to-many") |>
    pointblank::row_count_match(1323L) |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    dplyr::mutate(
      dplyr::across(.cols = c(area, station_type, station_name,
                              address, observation_begin, note1, note2,
                              katakana),
                    .fns =  \(x) stringi::stri_conv(x, to = "UTF8"),
                    .names = "{.col}")) %>%
    tibble::new_tibble(nrow = nrow(.), class = "sf") |>
    pointblank::row_count_match(1323L)

  # 現在も観測が行われているものに制限される
  # 例) ピヤシリ山 (block_no=0007)は除外
  # df_stations |> filter(station_name == "ピヤシリ山")
  # df_amedas_master |> filter(station_name == "ピヤシリ山")
  # stations |> filter(station_name == "ピヤシリ山")

  stations |>
    filter(is.na(block_no)) |>
    pointblank::row_count_match(0L)

  stations |>
    readr::write_rds("data-raw/amedas_raw.rds")
} else {
  stations <-
    readr::read_rds("data-raw/amedas_raw.rds")
}

# natural earthのポリゴン情報をもとに都道府県コードを付与
# ときどき間違いがあるので手動で修正する
ne_jpn <-
  ne_states(country = "Japan", returnclass = "sf") %>% # magrittr
  tibble::new_tibble(nrow = nrow(.), class = "sf") |>
  arrange(iso_3166_2) |>
  select(iso_3166_2) |>
  mutate(pref_code = stringr::str_remove(iso_3166_2, "JP-"),
         geometry,
         .keep = "none")

stations <-
  stations |>
  st_join(ne_jpn) |>
  pointblank::row_count_match(1323L) |>
  pointblank::col_count_match(14L)

# 1.4 Manual fix ------------------------------------------------------------
prefecture_code <- c(`11001` = "01",
                     `11076` = "01",
                     `12061` = "01",
                     `12086` = "01",
                     `12111` = "01",
                     `12136` = "01",
                     `12161` = "01",
                     `12186` = "01",
                     `12206` = "01",
                     `12231` = "01",
                     `12256` = "01",
                     `12281` = "01",
                     `12306` = "01",
                     `12321` = "01",
                     `12346` = "01",
                     `12361` = "01",
                     `12386` = "01",
                     `12411` = "01",
                     `12436` = "01",
                     `12461` = "01",
                     `12486` = "01",
                     `12511` = "01",
                     `12536` = "01",
                     `13006` = "01",
                     `13031` = "01",
                     `13056` = "01",
                     `13061` = "01",
                     `13181` = "01",
                     `13121` = "01",
                     `13146` = "01",
                     `13277` = "01",
                     `13311` = "01",
                     `14026` = "01",
                     `14071` = "01",
                     `16061` = "01",
                     `17351` = "01",
                     `18281` = "01",
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
                     `24101` = "01",
                     `31001` = "02",
                     `31662` = "02",
                     `32056` = "05",
                     `32286` = "05",
                     `32616` = "05",
                     `35002` = "06",
                     `34292` = "04",
                     `34361` = "04",
                     `35246` = "06",
                     `41011` = "09",
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
                     `45061` = "12",
                     `45148` = "12",
                     `46046` = "14",
                     `46141` = "14",
                     `46211` = "14",
                     `50066` = "22",
                     `50506` = "22",
                     `50416` = "22",
                     `50561` = "22",
                     `50551` = "22",
                     `51077` = "23",
                     `51261` = "23",
                     `54041` = "15",
                     `54166` = "15",
                     `54387` = "15",
                     `54876` = "15",
                     `57051` = "18",
                     `62081` = "27",
                     `65021` = "30",
                     `65036` = "30",
                     `65106` = "30",
                     `65127` = "30",
                     `65288` = "30",
                     `65356` = "30",
                     `63517` = "28",
                     `63496` = "28",
                     `63491` = "28",
                     `63551` = "28",
                     `69006` = "31",
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
                     `72176` = "36",
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
                     `84012` = "42",
                     `86396` = "43",
                     `86491` = "43",
                     `86216` = "43",
                     `87412` = "45",
                     `87492` = "45",
                     `88837` = "46",
                     `88836` = "46",
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
                     `88986` = "46",
                     `91081` = "47",
                     `91107` = "47",
                     `91236` = "47",
                     `91216` = "47",
                     `91141` = "47",
                     `92006` = "47",
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
                     `94116` = "47") |>
  ensurer::ensure(length(.) == 186L)

pref_code_missing <-
  stations |>
  filter(is.na(pref_code)) |>
  pointblank::row_count_match(66L) |>
  pull(station_no)
# NAの例
# stations |>
#   filter(station_no == pref_code_missing[1]) |>
#   select(area, station_name, pref_code)

pref_code_missing[!pref_code_missing %in% names(prefecture_code)] |>
  ensurer::ensure(length(.) == 0L)

pref_code_missing[pref_code_missing %in% names(prefecture_code)]

# 変更によって重複がないか確認
stations |>
  filter(station_no %in% names(prefecture_code)) |>
  filter(!is.na(pref_code)) |>
  select(area, station_no, station_name, pref_code) |>
  st_drop_geometry() |>
  left_join(
    tibble::tibble(
      station_no = as.integer(names(prefecture_code)),
      pref_code_mod = prefecture_code
    ),
    by = join_by(station_no),
    relationship = "many-to-many") |>
  distinct(station_no, station_name, pref_code, .keep_all = TRUE) |>
  count(station_no, station_name, sort = TRUE) |>
  filter(n > 1) |>
  pointblank::row_count_match(0L)

# stations |>
#   filter(!is.na(pref_code)) |>
#   filter(station_no %in% names(prefecture_code)) |>
#   select(area, station_no, station_name, pref_code)

stations <-
  stations |>
  mutate(pref_code = recode(station_no,
                            !!!prefecture_code,
                            .default = pref_code
                            ))

stations |>
  sf::st_drop_geometry() |>
  distinct(area, pref_code) |>
  count(area, sort = TRUE) |>
  filter(n > 1) |>
  pointblank::row_count_match(0L)

# stations |>
#   filter(area == "鹿児島", pref_code == "47") |>
#   distinct(area, pref_code)

stations |>
  st_drop_geometry() |>
  filter(is.na(pref_code)) |>
  pointblank::row_count_match(0L)

stations <-
  stations |>
  dplyr::mutate(dplyr::across(tidyselect::where(is.character),
                              .fns = list(~ stringi::stri_conv(str = ., to = "UTF8")))) |>
  dplyr::select(area,
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
         geometry) |>
  pointblank::row_count_match(1323L) |>
  pointblank::col_count_match(14L)

usethis::use_data(stations, overwrite = TRUE)

# mapview::mapview(stations)

# 2. 潮位観測 ---------------------------------------------------------------------
# ref) https://www.data.jma.go.jp/gmd/kaiyou/db/tide/suisan/station.php
# https://www.jma.go.jp/jp/choi/list1.html
library(parzer)

years <-
  seq.int(1997, lubridate::year(lubridate::now()))

# 数分
tide_station <-
  years |>
  purrr::set_names(as.character(years)) |>
  purrr::map(
    function(.x) {
      Sys.sleep(7)
      polite::bow(stringr::str_glue("https://www.data.jma.go.jp/gmd/kaiyou/db/tide/genbo/station.php?year={.x}")) |>
        polite::scrape() |>
        rvest::html_table(fill = TRUE) |>
        purrr::pluck(1) |>
        tibble::repair_names() |>
        tibble::as_tibble()
    }) |>
  purrr::list_rbind(names_to = "year")

tide_station <-
  tide_station |>
  dplyr::filter(stringr::str_detect(`地点番号`, "\uff0a", negate = TRUE),
         `地点番号` != "地点番号") |>
  dplyr::select(seq.int(8)) |>
  # "year", "地点番号", "地点記号", "観測地点名",
  # "所在地", "緯度（北緯）", "経度（東経）", "観測の方式"
  purrr::set_names(c("year", "id", "stn", "station_name",
                     "address", "latitude", "longitude", "type")) |>
  mutate(across(c(longitude, latitude),
                .fns = list(\(x) stringr::str_replace_all(x,
                                                       c("\u309c" = "\u00b0"))),
                .names = "{.col}")) |>
  mutate(longitude = parzer::parse_lon(longitude),
         latitude = parzer::parse_lat(latitude)) |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  pointblank::row_count_match(2019L) |>
  pointblank::col_count_match(7L)

tide_station |>
  dplyr::filter(year == 2025) |>
  pointblank::row_count_match(70L)

tide_station |>
  st_drop_geometry() |>
  count(year) |>
  pointblank::row_count_match(29L)

usethis::use_data(tide_station, overwrite = TRUE)

# 3. 震度観測点 ----------------------------------------------------------------
x <-
  rvest::read_html("https://www.data.jma.go.jp/eqev/data/kyoshin/jma-shindo.html")
x |>
  rvest::html_element(css = "#main > h1") |>
  rvest::html_text() # 令和6年11月21日現在

earthquake_station <-
  x |>
  html_elements(css = "#main > table") |>
  html_table() |>
  purrr::set_names(x |>
              html_elements(css = "#main > p > a") |>
              html_text()) |>
  purrr::map(
    ~ mutate(.x,
             across(.cols = everything(),
                    .fns = as.character))
  ) |>
  purrr::list_rbind(names_to = "prefecture") |>
  pointblank::row_count_match(1127L) |>
  pointblank::col_count_match(10L) |>
  readr::type_convert(col_types = "ccccididcc") |>
  purrr::set_names(c("prefecture", "area", "station_name", "address",
                     "lat_do", "lat_fun",
                     "lng_do", "lng_fun",
                     "observation_begin", "observation_end"))

earthquake_station <-
  earthquake_station %>% # magrittr
  mutate(longitude = purrr::pmap_chr(.,
                                    ~ as.character(kuniezu::parse_lon_dohunbyo(paste0("東経", ..7, "度", ..8, "秒")))),
         latitude = purrr::pmap_chr(.,
                                    ~ as.character(kuniezu::parse_lat_dohunbyo(paste0("北緯", ..5, "度", ..6, "秒"))))) |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4612) |>
  sf::st_transform(crs = 4326) |>
  select(!c(ends_with("_do"), ends_with("_fun"))) |>
  pointblank::row_count_match(1127L) |>
  pointblank::col_count_match(7L) |>
  filter(is.na(observation_end)) |>
  pointblank::row_count_match(671L)

usethis::use_data(earthquake_station, overwrite = TRUE)

# mapview::mapview(earthquake_station)
