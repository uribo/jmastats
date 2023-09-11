#' Parse data variable units
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' @param data data
#' @param rename *logical*
#' @importFrom dplyr bind_cols select
#' @importFrom purrr keep map map2_dfc reduce set_names
#' @importFrom stringr str_subset
#' @importFrom tidyselect all_of where
#' @importFrom units as_units
#' @examples
#' # For data retrieved with jma_collect(), here is a minimal example.
#' d <-
#' tibble::tibble(date = as.Date(c(17742, 17742, 17742, 17742, 17742, 17742), origin = "1970-01-01"),
#'                time = c(1, 2, 3, 4, 5, 6),
#'                `precipitation(mm)` = c(0, 0, 0, 0, 0, 0),
#'                `temperature(℃)` = c(22.4, 22.1, 21, 20.2, 20.4, 23.5))
#' d |> parse_unit(rename = TRUE)
#' @export
#' @return a `tbl` object
parse_unit <- function(data, rename = TRUE) {

  original_vars <-
    names(data)
  candidate_vars <-
    original_vars[original_vars != c("date")]
  candidate_vars <-
    candidate_vars[stringr::str_detect(candidate_vars, "-", negate = TRUE)]
  candidate_vars <-
    candidate_vars[stringr::str_detect(candidate_vars, "\\(.+\\)$")]

  data_candidate <-
    data |>
    dplyr::select(tidyselect::all_of(candidate_vars)) |>
    dplyr::select(tidyselect::where(function(x) sum(is.na(x)) != nrow(data)))

  var_units <-
    names(data_candidate) |>
    purrr::map_vec(guess_unit) |>
    purrr::keep(function(x) !is.na(x))

  df_drop <-
    data[, c(which(is.na(var_units)),
             which(!names(data) %in% names(data_candidate)))]

  df_units <-
    purrr::map2_dfc(
      seq.int(ncol(data_candidate)),
      var_units,
      function(x, y) units::as_units(as.numeric(data_candidate[[x]]),
                                     value = y)
    ) |>
    purrr::set_names(names(data_candidate)[!names(data_candidate) %in% names(df_drop)])

  df_res <-
    dplyr::bind_cols(df_drop, df_units) |>
    dplyr::select(names(data))

  if (rename == TRUE) {
    df_res <-
      df_res |>
      purrr::set_names(colnames(df_res) |>
                         stringr::str_remove("\\(.+\\)") |>
                         stringr::str_remove("_$"))
  }

  return(df_res)
}
