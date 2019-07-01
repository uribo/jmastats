#' Parse data variable units
#'
#' @param data data
#' @param rename *logical*
#' @importFrom dplyr bind_cols select select_if
#' @importFrom janitor clean_names
#' @importFrom purrr keep map map2_dfc reduce set_names
#' @importFrom readr type_convert
#' @importFrom units as_units
#' @export
parse_unit <- function(data, rename = TRUE) {

  . <- NULL

  data <-
    readr::type_convert(data)

  original_vars <-
    names(data)

  data_candidate <-
    data %>%
    dplyr::select(grep(pattern = "\\(.+\\)$", original_vars)) %>%
    dplyr::select_if(~ !is.character(.)) %>%
    dplyr::select_if(~ sum(is.na(.)) != nrow(data))

  var_units <-
    names(data_candidate) %>%
    purrr::map(guess_unit) %>%
    purrr::reduce(c) %>%
    purrr::keep(~ !is.na(.x))

  df_drop <-
    data[, c(which(is.na(var_units)),
             which(!names(data) %in% names(data_candidate)))]

  df_units <-
    purrr::map2_dfc(
      which(!names(data_candidate) %in% names(df_drop)),
      var_units[which(!names(data_candidate) %in% names(df_drop))],
      ~ units::as_units(data_candidate[[.x]], value = .y)
    ) %>%
    purrr::set_names(names(data_candidate)[!names(data_candidate) %in% names(df_drop)])

  df_res <-
    dplyr::bind_cols(df_drop, df_units) %>%
    dplyr::select(names(data))

  if (rename == TRUE) {
    df_res <-
      df_res %>%
      janitor::clean_names(case = "snake")
  }

  return(df_res)
}
