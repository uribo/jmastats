#####################################
# Stations list
#####################################
library(dplyr)
library(jpndistrict)
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
  dplyr::select(1:4, 6, 11, 14:19) %>%
  purrr::set_names(
    c("area", "station_no", "station_type",
      "station_name", "address", "elevation",
      "observation_begin", "note1", "note2",
      "katakana", "longitude", "latitude")
  ) %>%
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
    dplyr::select(-1) %>%
    dplyr::filter(!is.na(block_no)) %>%
    dplyr::distinct()
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
  dplyr::select(-1)

# ~ 2 mins.
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

d <-
  d %>%
  left_join(df_stations %>%
              mutate(area = stringr::str_remove(area, "(都|府|県)$"),
                     area = dplyr::if_else(station == "竜王山", "徳島", area),
                     station = stringr::str_remove(station, "（.+）")),
            by = c("station_name" = "station", "area")) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  tibble::new_tibble(subclass = "sf")

# 30 mins.
stations <-
  d %>%
  mutate(pref = purrr::pmap(., ~ find_pref(geometry = ..13))) %>%
  mutate(pref_null = purrr::pmap_lgl(., ~ is.null(..13)))

stations <-
  rbind(
    stations %>%
    filter(pref_null == TRUE) %>%
    mutate(pref = NA_character_) %>%
    rename(pref_code = pref),
  stations %>%
    filter(pref_null == FALSE) %>%
    tidyr::unnest() %>%
    select(-prefecture, -geometry1)
) %>%
  select(-pref_null) %>%
  arrange(station_no)

# Manual fix ------------------------------------------------------------
stations <-
  stations %>%
  mutate(pref_code = recode(station_no,
                            `11151` = "01",
                            `13146` = "01",
                            `13277` = "01",
                            `16061` = "01",
                            `19432` = "01",
                            `19451` = "01",
                            `22241` = "01",
                            `22327` = "01",
                            `22391` = "01",
                            `21261` = "01",
                            `21297` = "01",
                            `21323` = "01",
                            `24156` = "01",
                            `23376` = "01",
                            `31001` = "02",
                            `32056` = "05",
                            `32286` = "05",
                            `32616` = "05",
                            `35002` = "06",
                            `34292` = "04",
                            `34361` = "04",
                            `54012` = "15",
                            `54157` = "15",
                            `36846` = "07",
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
                            `73168` = "38",
                            `73256` = "38",
                            `73341` = "38",
                            `74271` = "39",
                            `74276` = "39",
                            `74372` = "39",
                            `74447` = "39",
                            `74506` = "39",
                            `74516` = "39",
                            `82036` = "40",
                            `84122` = "42",
                            `84171` = "42",
                            `84236` = "42",
                            `84266` = "42",
                            `84371` = "42",
                            `84596` = "42",
                            `83126` = "44",
                            `83476` = "44",
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
                            .default = pref_code
                            ))

usethis::use_data(stations, overwrite = TRUE)
