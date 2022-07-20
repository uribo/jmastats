######################################
# 多言語辞書データ
######################################
library(rvest)
library(ensurer)
library(assertr)
x <-
  "https://www.data.jma.go.jp/developer/multilingual.html" %>%
  read_html()

# 2020年3月17日更新：11か国語から14か国語に拡充
x %>%
  html_nodes(css = "#ncontents2 > font > div > strong > font") %>%
  html_text()

if (!file.exists(here::here("data-raw/", basename(target_url)))) {
  target_url <-
    x %>%
    html_nodes(css = "#ncontents2 > font > div > a:nth-child(1)") %>%
    html_attr("href") %>%
    xml2::url_relative("https://www.data.jma.go.jp/") %>%
    stringr::str_c("https://www.data.jma.go.jp/developer/", .)
  download.file(
    target_url,
    destfile = here::here("data-raw/", basename(target_url))
  )
}

target_file <-
  here::here("data-raw/", basename(target_url))

readxl::excel_sheets(target_file) %>%
  ensure(length(.) == 2L)

df_langdict <-
  readxl::read_xlsx(target_file, sheet = 2) %>%
  verify(dim(.) == c(7032, 22))

df_langdict %>%
  dplyr::pull(`情報種別等`) %>%
  unique()

df_langdict %>%
  dplyr::filter(`情報種別等` == "アメダス地点名") %>%
  assert(nrow(.) == 1299)

d2 <-
  stations %>%
  sf::st_drop_geometry() %>%
  select(area, station_name)

d2 %>%
  dplyr::anti_join(d %>%
                     select(4, 5),
                   by = c("station_name" = "日本語")) %>%
  verify(nrow(.) == 3L)

d2 %>%
  dplyr::filter(area == "長野")

d %>%
  dplyr::filter(stringr::str_detect(`日本語`, "薮"))
