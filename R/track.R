#' Read RSMC Tokyo-Typhoon Center's best track data
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Tidy formatting best track data and combine each point to line.
#' @details
#' * `read_rsmc_besttrack()`: Read single best track data into [sf][sf::st_sf]
#' contains observation record as point.
#' * `track_combine()`: Combine track data to line by id
#' (such as international_number and storm_name).
#' @param path path to best track data (`.txt`). Give the path as a directory
#' in the user's computer or the URL.
#' @import rlang
#' @importFrom dplyr across arrange bind_rows group_by lead join_by select
#' left_join if_else mutate ungroup rowwise
#' @importFrom forcats fct_inorder
#' @importFrom lubridate ymd_h
#' @importFrom purrr map set_names
#' @importFrom readr read_lines
#' @importFrom sf st_as_sf st_as_sfc st_as_text st_cast st_sf st_union
#' @importFrom stringr str_detect str_split str_subset
#' @importFrom tibble as_tibble
#' @importFrom tidyr extract
#' @importFrom utils read.table
#' @seealso <https://www.jma.go.jp/jma/jma-eng/jma-center/rsmc-hp-pub-eg/RSMC_HP.htm>
#' @rdname track
#' @examples
#' read_rsmc_besttrack(path = system.file("dummy/bst.txt", package = "jmastats"))
#'
#' read_rsmc_besttrack(path = system.file("dummy/bst.txt", package = "jmastats")) |>
#'   track_combine()
#' @export
#' @return a `tbl` object
read_rsmc_besttrack <- function(path) {

  last_update <- international_number <- storm_name <-
  `central_pressure(hPa)` <- `maximum_sustained_wind_speed(knot)` <-
    datetime <- latitude <- longitude <- NULL
  v8 <- v10 <- NULL

  lines <- readr::read_lines(path)
  xx <-
    lines[stringr::str_detect(lines, "^66666")]
  df_header <-
    seq.int(length(xx)) |>
    purrr::map(
      function(x) {
        parse_x <- xx[x] |>
          stringr::str_split("[[:space:]]", simplify = TRUE) |>
          stringr::str_subset(".+")
        if (length(parse_x) == 7L) {
          parse_x <-
            c(parse_x[1:4],
              NA_character_,
              parse_x[5:6],
              NA_character_,
              parse_x[7])
        } else if (length(parse_x) == 8L) {
          parse_x <-
            c(parse_x[1:4],
              NA_character_,
              parse_x[5:8])
        }
        parse_x |>
          purrr::set_names(c("indicator_66666",
                      "international_number", "nrow",
                      "tropical_cyclone_number",
                      "international_number_copy",
                      "flag_last_data_line",
                      "DTM", "storm_name", "last_update"))
      }
    ) |>
    dplyr::bind_rows() |>
    tibble::as_tibble() |>
    readr::type_convert(col_types = "dcdcccdcc") |>
    dplyr::mutate(last_update = lubridate::ymd(last_update))

  if (df_header |>
      dplyr::pull(international_number) |>
      stringr::str_sub(1, 2) |>
      dplyr::n_distinct() == 1) {
    df_header <-
      df_header |>
      dplyr::arrange(international_number) |>
      dplyr::mutate(storm_name = forcats::fct_inorder(storm_name))
  } else {
    df_header <-
      df_header |>
      dplyr::mutate(storm_name = forcats::fct_inorder(storm_name))
  }
  data_common_vars <-
    c("datetime", "indicator_002", "grade",
      "latitude", "longitude",
      "central_pressure(hPa)",
      "maximum_sustained_wind_speed(knot)")
  data_typhoon_vars <-
    paste0(c("_direction_of_the_longest_radius_of_",
             "_the_longest_radius_of_",
             "_the_shortest_radius_of_"),
           rep(c("50kt_winds_or_greater",
                 "30kt_winds_or_greater"), each = 3),
           c("", "(nm)", "(nm)"))
  xx <-
    lines[stringr::str_detect(lines, "^66666", negate = TRUE)]
  df_record <-
    seq.int(1, length(xx)) |>
    purrr::map(
      function(x) {
        parse_x <- xx[x] |>
          stringr::str_split("[[:space:]]", simplify = TRUE) |>
          stringr::str_subset(".+")
        if (length(parse_x) <= 7L) {
          tmp_d <-
            as.data.frame(parse_x) |>
            t() |>
            tibble::as_tibble(.name_repair = "minimal")
          if (length(parse_x) == 6L) {
            tmp_d |>
              purrr::set_names(data_common_vars[-length(data_common_vars)])
          } else if (length(parse_x) == 7L) {
            tmp_d |>
              purrr::set_names(data_common_vars)
          }
        } else {
          tmp_d <-
            parse_x |>
            as.data.frame() |>
            t() |>
            tibble::as_tibble(.name_repair = "minimal") |>
            purrr::set_names(paste0("v", seq.int(length(parse_x)))) |>
            tidyr::extract(v8,
                           into = c("H", "I"),
                           regex = "([0-9]{1})([0-9]{4})") |>
            tidyr::extract(v10,
                           into = c("K", "L"),
                           regex = "([0-9]{1})([0-9]{4})")
          if (length(parse_x) == 11L) {
            tmp_d |>
              purrr::set_names(c(data_common_vars,
                                 data_typhoon_vars))
          } else if (length(parse_x) == 12L) {
            tmp_d |>
              purrr::set_names(c(data_common_vars,
                                 data_typhoon_vars,
                                 "indicator_of_landfall_or_passage"))
          }
        }
      },
      .progress = TRUE
    ) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      datetime = lubridate::ymd_h(
        paste0(
          dplyr::if_else(as.numeric(stringr::str_sub(datetime, 1, 2)) <= 19,
                         "20",
                         "19"),
          datetime), tz = "UTC"),
      latitude = as.numeric(latitude) / 10,
      longitude = as.numeric(longitude) / 10,
      international_number = rep(df_header$international_number,
                                 df_header$nrow)) |>
    dplyr::mutate(
      dplyr::across(c(`central_pressure(hPa)`,
                      `maximum_sustained_wind_speed(knot)`),
                    as.numeric)) |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
    dplyr::left_join(df_header,
                     by = dplyr::join_by(international_number)) |>
    dplyr::arrange(datetime)

  df_record
}

#' @rdname track
#' @param data Import data using [read_rsmc_besttrack]
#' @param group_vars To combine track variables.
#' @param keep_vars Keep variables.
#' @param geometry geometry column name (default `geometry`).
#' @export
track_combine <- function(data, group_vars = c("international_number", "storm_name"),
                          keep_vars = NULL, geometry = geometry) {
  aa <- bb <- NULL
  data |>
    dplyr::select(group_vars, keep_vars, geometry) |>
    dplyr::group_by(!!! rlang::syms(group_vars)) |>
    dplyr::mutate(aa  = sf::st_as_text(geometry),
                  bb = dplyr::lead(sf::st_as_text(geometry))) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(bb)) |>
    dplyr::rowwise() |>
    dplyr::mutate(geometry = sf::st_union(sf::st_as_sfc(aa),
                                          sf::st_as_sfc(bb)) |>
                    sf::st_cast("LINESTRING")) |>
    dplyr::ungroup() |>
    dplyr::select(!c(aa, bb)) |>
    sf::st_sf(crs = 4326)
}
