#' Parse data variable units
#'
#' @param data data
#' @param rename *logical*
#' @export
parse_unit <- function(data, rename = TRUE) {

  original_vars <-
    names(data)

  var_units <-
    names(data) %>%
    purrr::map_chr(guess_unit)

  target_vars <-
    names(data)[-which(is.na(var_units))]

  df_drop <-
    data[, which(is.na(var_units))]

  df_units <-
    purrr::map2_dfc(
      var_units[!is.na(var_units)],
      1:length(var_units[!is.na(var_units)]),
      ~ data %>%
        dplyr::select(!! rlang::sym(target_vars[.y])) %>%
        dplyr::mutate_at(vars(!! rlang::sym(target_vars[.y])),
                         .funs = units::set_units, value = .x)
    )

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
