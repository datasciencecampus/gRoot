#' A dataset containing 110034 points around the road network of Cardiff
#' 
#' @format A data frame with 110034 rows and 9 variables:
#' \describe{
#'     \item{left_green}{Percentage vegetation on left side of road}
#'     \item{right_green}{Percentage vegetation on right side of road}
#'     \item{road}{Name of the road}
#'     \item{way_id}{Unique id of road}
#'     \item{seq}{Sequence of recordings along the specific road}
#'     \item{max_green}{Value of the highest amount of green (left or right) for the point}
#'     \item{parish}{Parish this point fall in}
#'     \item{lon}{Longitude of that point}
#'     \item{lat}{Latitude of that point}}
#' @source Data will be available on github after it has been published. 
"tree_points"
#' 
#' Files for displaying polygons in leaflet map
#' 
#' @format A SpatialPolygonsDataFrame with the shapes of Cardiff Parishes and they're mean vegetation value
#' 
#' @source Combination of Parish boundaries from ONS geohub combined with urban forests data. Created using functions found in CityProfileR.
"tree_polygons"
#' 
#' A hexmap object for the hexmaps used in city-profile app prototype. 
#' 
#' Ideally this will be converted into a JSON eventually so there can be an interactive component.
#' 
#' @format currently this is in the form of a data frame it has 1498 obs and 51 variables.
#' 
#' @source an amalgamation of NRW estimates of canopy cover in Welsh towns and 
#' cities, WIMD 2014 data, urban forest data and LSOA 2001 boundaries. The object 
#' was created using the geogrid package.
#' 
"tree_hex"