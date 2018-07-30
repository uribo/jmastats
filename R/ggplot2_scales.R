#' Scales to use for ggplot2
#'
#' @param type Display item
#' @param ... Arguments to pass on to [`ggplot2::scale_colour_gradientn()`]() or
#' `ggplot2::scale_fill_gradientn()`
#'
#' @importFrom ggplot2 scale_color_gradientn scale_fill_gradientn
#' @importFrom purrr flatten
#'
#' @return A `ScaleContinuous` object that can be added to a `ggplot` object
#'
#' @name ggplot2-scales
#' @rdname ggplot2-scales
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' d <-
#' jma_collect("daily", block_no = "47646", year = 2017, month = 9) %>%
#' select(date, `precipitation_sum(mm)`, starts_with("temperature"), starts_with("sunshine")) %>%
#' parse_unit()
#'
#' library(ggforce)
#' ggplot(d, aes(date, precipitation_sum_mm,
#'           color = units::drop_units(precipitation_sum_mm),
#'           fill = units::drop_units(precipitation_sum_mm))) +
#'   geom_bar(stat = "identity") +
#'   scale_color_jma_absolute(type = "precipitation") +
#'   scale_fill_jma_absolute(type = "precipitation")
#' }
scale_color_jma_absolute <- function(type = "precipitation", ...) {

  pal <-
    jma_pal(pallet = "absolute", .attribute = TRUE) %>%
    purrr::flatten()

  ggplot2::scale_color_gradientn(colors = rev(pal$colors),
                        labels = rev(switch(type,
                                        precipitation = pal$precipitation$labels,
                                        snow          = pal$snow$labels,
                                        wind          = pal$wind$labels)),
                        breaks = rev(switch(type,
                                        precipitation = pal$precipitation$breaks,
                                        snow          = pal$snow$breaks,
                                        wind = pal$wind$breaks)
                        ),
                        ...
  )
}

#' @rdname ggplot2-scales
#' @export
scale_fill_jma_absolute <- function(type = "precipitation", ...) {

  pal <-
    jma_pal(pallet = "absolute", .attribute = TRUE) %>%
    purrr::flatten()

  ggplot2::scale_fill_gradientn(colors = rev(pal$colors),
                        labels = rev(switch(type,
                                        precipitation = pal$precipitation$labels,
                                        snow          = pal$snow$labels,
                                        wind          = pal$wind$labels)),
                        breaks = rev(switch(type,
                                        precipitation = pal$precipitation$breaks,
                                        snow          = pal$snow$breaks,
                                        wind = pal$wind$breaks)
                        ),
                        ...
  )
}

#' @rdname ggplot2-scales
#' @export
scale_fill_jma_relative <- function(type = "amedas", ...) {

  pal_relative <-
    jma_pal(pallet = "relative", .attribute = TRUE) %>%
    purrr::flatten()

  ggplot2::scale_fill_gradientn(colors = rev(pal_relative$colors),
                       labels = rev(switch (type,
                                            amedas = pal_relative$amedas$labels,
                                            forecast = pal_relative$forecast$labels
                       )),
                       breaks  = rev(switch (type,
                                             amedas = pal_relative$amedas$breaks,
                                             forecast = pal_relative$forecast$breaks
                       )),
                       limits = c(-5, 35),
                       ...)
}