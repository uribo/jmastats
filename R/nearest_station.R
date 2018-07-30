#' Find out neighborhood station
#'
#' @description Return the nearest [stations] information
#' to the given coordinates.
#' @param longitude longitude
#' @param latitude latitude
#' @importFrom dplyr select mutate
#' @importFrom purrr map_dbl
#' @importFrom sf st_distance st_point st_set_geometry st_sfc
#' @importFrom units set_units
#' @examples
#' nearest_station(142.9313, 43.70417)
#' @name nearest_station
#' @export
nearest_station <- function(longitude, latitude) {

  m <- NULL

  distances <-
    1:nrow(stations) %>%
    purrr::map_dbl(
      ~ sf::st_distance(sf::st_point(c(longitude, latitude)) %>%
                          sf::st_sfc(crs = 4326),
                        stations[.x, ]))

  min_row <- which.min(distances)

  dplyr::select(stations[min_row, ], c(1, 2, 4)) %>%
    dplyr::mutate(distance = distances[min_row] %>%
                    units::set_units(m)) %>%
    sf::st_set_geometry(value = NULL)
}
