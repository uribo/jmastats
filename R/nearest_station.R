#' Find out neighborhood station
#'
#' @param longitude longitude
#' @param latitude latitude
#' @example
#' nearest_station(142.9313, 43.70417)
#' @name nearest_station
#' @export
nearest_station <- function(longitude, latitude) {

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
    sf::st_set_geometry(NULL)

}
