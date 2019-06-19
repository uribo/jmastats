#' Find out neighborhood stations
#'
#' @description Return the nearest [stations] information
#' to the given coordinates.
#'
#' @details
#' * `nearest_station()`: Return single station data.
#' * `pick_neighbor_stations()`: Pick-up neighbourhood stations.
#' Filter by distance from target point.
#' @return sf
#' @param longitude Longitude.
#' @param latitude Latitude.
#' @param geometry XY [sf::sf] object.
#' @param distance Distance from station to station to adjustment.
#' @param .unit Unit used for extraction from the point of interest. Default *m* (meters).
#' This value is passed to [units::as_units].
#' @importFrom dplyr filter select mutate transmute
#' @importFrom purrr map_dbl
#' @importFrom rlang enquo quo_name
#' @importFrom sf st_distance st_point st_set_geometry st_sfc
#' @importFrom units as_units set_units
#' @examples
#' \dontrun{
#' nearest_station(142.9313, 43.70417)
#'
#' pick_neighbor_stations(140.10, 36.08, 300000)
#'
#' d <-
#'   pick_neighbor_stations(140.10, 36.08, 30, "km")
#' pick_neighbor_stations(geometry = sf::st_point(c(140.1833, 36.23333)),
#'                        distance = 100)
#' }
#' @name nearest_station
NULL

. <- m <- block_no <- geometry <- station_name <- NULL

#' @rdname nearest_station
#' @export
nearest_station <- function(longitude, latitude, geometry = NULL) {

  coords <-
    check_input_coords(longitude, latitude, geometry)

  distances <-
    seq.int(nrow(stations)) %>%
    purrr::map_dbl(
      ~ sf::st_distance(sf::st_point(c(coords$longitude,
                                       coords$latitude)) %>%
                          sf::st_sfc(crs = 4326),
                        stations[.x, ]))

  min_row <- which.min(distances)

  dplyr::select(stations[min_row, ], c(1, 2, 4)) %>%
    dplyr::mutate(distance = distances[min_row] %>%
                    units::set_units(m)) %>%
    dplyr::select(dplyr::everything(), geometry)
}

#' @rdname nearest_station
#' @export
pick_neighbor_stations <- function(longitude, latitude, distance = 1, .unit = "m", geometry = NULL) {

  distance <- rlang::enquo(distance)
  unit = rlang::quo_name(.unit)

  coords <-
    check_input_coords(longitude, latitude, geometry)

  stations %>%
    dplyr::transmute(station_name,
                     block_no,
                     distance = sf::st_distance(
                       geometry,
                       sf::st_sfc(sf::st_point(c(coords$longitude,
                                                 coords$latitude)),
                                  crs = 4326))[, 1]) %>%
  dplyr::filter(distance <= units::as_units(!! distance, value = !! unit))

}
