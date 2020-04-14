#' Find out neighborhood stations
#'
#' @description Return the nearest [stations] information
#' to the given coordinates.
#'
#' @details
#' * `nearest_station()`: Return single station data.
#' * `pick_neighbor_stations()`: Pick-up neighbourhood stations.
#' * `pick_neighbor_tide_stations()`: Pick-up neighbourhood tidal observation stations.
#' Filter by distance from target point.
#' @return sf
#' @param longitude Longitude.
#' @param latitude Latitude.
#' @param geometry XY [sf::sf] object.
#' @param distance Distance from station to station to adjustment.
#' @param .unit Unit used for extraction from the point of interest. Default *m* (meters).
#' This value is passed to [units::as_units].
#' @param year For tide level data. Restricted to the observation points in the target year.
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
#'
#' pick_neighbor_tide_stations(longitude = 133.4375, latitude = 34.45833,
#'                             year = 2020,
#'                             distance = 100, .unit = "km")
#' }
#' @name nearest_station
NULL

. <- m <- station_no <-
  address <- id <- stn <- type <-
  block_no <- geometry <- station_name <-
  area <- distance <- NULL

#' @rdname nearest_station
#' @export
nearest_station <- function(longitude, latitude, geometry = NULL) {
  coords <-
    check_input_coords(longitude, latitude, geometry)
  res <- pick_neighbor_stations(coords$longitude,
                                coords$latitude,
                                distance = 10,
                                .unit = "km")
  if (nrow(res) > 1)
    res <- res %>%
    dplyr::top_n(1, dplyr::desc(distance))
  else
    res <- pick_neighbor_stations(coords$longitude,
                                  coords$latitude,
                                  distance = 100,
                                  .unit = "km")
  if (nrow(res) > 1)
    res <- res %>%
    dplyr::top_n(1, dplyr::desc(distance))
  else
    res <- pick_neighbor_stations(coords$longitude,
                                  coords$latitude,
                                  distance = 500,
                                  .unit = "km")
  if (nrow(res) > 1)
    res <- res %>%
    dplyr::top_n(1, dplyr::desc(distance))
  else
    res <- pick_neighbor_stations(coords$longitude,
                                  coords$latitude,
                                  distance = 1000,
                                  .unit = "km")
  if (nrow(res) > 1)
    res <- res %>%
    dplyr::top_n(1, dplyr::desc(distance))
  else
    res <- pick_neighbor_stations(coords$longitude,
                                  coords$latitude,
                                  distance = 3200,
                                  .unit = "km") %>%
    dplyr::top_n(1, dplyr::desc(distance))
  if (nrow(res) == 0)
    rlang::inform("Check input coordinates.\nThe distance to stations is too far.")
  else
  res %>%
    dplyr::select(area,
                  station_no,
                  station_name,
                  block_no,
                  distance,
                  geometry)
}

#' @rdname nearest_station
#' @export
pick_neighbor_stations <- function(longitude, latitude, distance = 1, .unit = "m", geometry = NULL) {
  unit <- rlang::quo_name(.unit)
  coords <-
    check_input_coords(longitude, latitude, geometry)
  stations[which(sf::st_is_within_distance(st_sfc(st_point(c(coords$longitude,
                                                             coords$latitude)),
                                                  crs = 4326),
                                           stations,
                                           dist = units::as_units(distance, value = unit),
                                           sparse = FALSE)[1, ]), ] %>%
    dplyr::transmute(
      area,
      station_no,
      station_name,
      block_no,
      distance = sf::st_distance(
        geometry,
        sf::st_sfc(sf::st_point(c(coords$longitude, coords$latitude)),
                   crs = 4326))[, 1]) %>%
    dplyr::arrange(distance)
}

#' @rdname nearest_station
#' @export
pick_neighbor_tide_stations <- function(year, longitude, latitude,
                                        distance = 100, .unit = "km", geometry = NULL) {
  unit <- rlang::quo_name(.unit)
  yr <-
    rlang::enquo(year)
  stations <-
    tide_station %>% filter(year == !!yr)
  coords <-
    check_input_coords(longitude, latitude, geometry)
  stations[which(sf::st_is_within_distance(st_sfc(st_point(c(coords$longitude,
                                                             coords$latitude)),
                                                  crs = 4326),
                                           stations,
                                           dist = units::as_units(distance, value = unit),
                                           sparse = FALSE)[1, ]), ] %>%
    dplyr::transmute(
      year,
      id,
      stn,
      station_name,
      address,
      type,
      distance = sf::st_distance(
        geometry,
        sf::st_sfc(sf::st_point(c(coords$longitude, coords$latitude)),
                   crs = 4326))[, 1],
      geometry
    ) %>%
    dplyr::arrange(distance)
}
