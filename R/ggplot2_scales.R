#' Scales to use for ggplot2
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param type Display item
#' @param ... Arguments to pass on to [ggplot2::scale_color_gradientn] or
#' [ggplot2::scale_fill_gradientn]
#' @importFrom ggplot2 scale_color_gradientn scale_fill_gradientn
#' @return A `ScaleContinuous` object that can be added to a `ggplot` object
#' @name ggplot2-scales
#' @rdname ggplot2-scales
#' @examples
#' d <-
#' data.frame(
#'   date = as.Date(c(17410, 17411, 17412, 17413, 17414, 17415), origin = "1970-01-01"),
#'   precipitation_sum = units::set_units(c(3.5, 9.5, 0, 0, 0, 5), "mm"))
#'
#' library(ggplot2)
#' library(units)
#' ggplot(d, aes(date, precipitation_sum,
#'           color = drop_units(precipitation_sum),
#'           fill = drop_units(precipitation_sum))) +
#'   geom_bar(stat = "identity") +
#'   scale_color_jma_absolute(type = "precipitation") +
#'   scale_fill_jma_absolute(type = "precipitation")
#' @export
scale_color_jma_absolute <- function(type = "precipitation", ...) {

  pal <-
    jma_pal(palette = "absolute", .attribute = TRUE)

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
scale_color_jma_relative <- function(type = "amedas", ...) {

  pal <-
    jma_pal(palette = "relative", .attribute = TRUE)

  ggplot2::scale_color_gradientn(colors = rev(pal$colors),
                                 labels = switch(type,
                                                 amedas = pal$amedas$labels),
                                 limits = switch(type,
                                                 amedas = pal$amedas$limits
                                 ),
                                 breaks = switch(type,
                                                 amedas = pal$amedas$breaks),
                                 ...
  )

}

#' @rdname ggplot2-scales
#' @export
scale_fill_jma_absolute <- function(type = "precipitation", ...) {

  pal <-
    jma_pal(palette = "absolute", .attribute = TRUE)

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
    jma_pal(palette = "relative", .attribute = TRUE)

  ggplot2::scale_fill_gradientn(colors = rev(pal_relative$colors),
                       labels = rev(switch(type,
                                            amedas = pal_relative$amedas$labels,
                                            forecast = pal_relative$forecast$labels
                       )),
                       breaks  = rev(switch(type,
                                             amedas = pal_relative$amedas$breaks,
                                             forecast = pal_relative$forecast$breaks
                       )),
                       limits = c(-5, 35),
                       ...)
}
