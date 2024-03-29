#' mapLT
#'
#' @description
#' Maps hunt area and/or herd unit polygons, cutouts (if applicable), and survey lines (if applicable).
#'
#' @param herdPoly sf object, herd unit and/or hunt area boundary (sample frame).
#' @param cutoutPoly optional sf object, area excluded from sample frame.
#' @param lines optional sf object generated from \code{\link{makeLines}}, transect lines.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(dplyr)
#'
#' # get hunt area polygons
#' poly <- huntAreas %>%
#'   filter(HERDNAME %in% c("Badwater", "North Natrona"))
#'
#' # map
#' mapLT(poly)
#'
#' # get cutout
#' x <- st_read("example_cutout.kml")
#'
#' # map poly and cutouts
#' mapLT(poly, x)
#'
#' # get poly with cutouts
#' poly <- cutPoly(poly, x)
#'
#' # calculate recommended line length
#' recLength <- calcLineLength(
#'   poly,
#'   N = 20000
#' )
#'
#' # makeLines
#' lines <- makeLines(
#'   poly,
#'   recLength
#' )
#'
#' # map
#' mapLT(poly, x, lines$lines)
#' }
#'
#' @author Garrett Catlin
#'
#' @import leaflet
#' @importFrom htmltools HTML
#' @importFrom sf st_transform st_as_sf
#' @rawNamespace import(data.table, except = shift)
#'
#' @export

mapLT <- function(herdPoly, cutoutPoly = NULL, lines = NULL) {

  # transform poly
  herdPoly <- sf::st_transform(herdPoly, '+proj=longlat +datum=WGS84')

  # map
  map <- leaflet() %>%
    addTiles() %>%
    addPolygons(data = herdPoly,
                opacity = 1,
                weight = 1,
                color = "black",
                fillOpacity = 0.05,
                fillColor = "black")

  # add cutout if applicable
  if(!is.null(cutoutPoly)) {

    # transform poly
    cutoutPoly <- sf::st_transform(cutoutPoly, '+proj=longlat +datum=WGS84')

    # add cutouts
    map <- map %>%
      addPolygons(data = cutoutPoly,
                  opacity = 1,
                  weight = 1,
                  color = "red",
                  fillOpacity = 0.05,
                  fillColor = "red")
  }

  # add lines if applicable
  if (!is.null(lines)) {
    # transform
    lines <- sf::st_transform(lines, '+proj=longlat +datum=WGS84')

    # add label
    lines <- data.table::as.data.table(lines)
    lines[, leaflet_label := htmltools::HTML(paste0(
      "<b>", LineID, "</b> <br/>",
      "<b> Length (km): </b>", round(lengthKm, 2), "<br/>",
      "<b> Length (mi): </b>", round(lengthMi,2)
    )), by = LineID]
    lines <- sf::st_as_sf(lines)

    # add lines
    map <- map %>%
      addPolylines(
        data = lines,
        weight = 2,
        color = "blue",
        label = ~ leaflet_label,
        opacity = 1
      )
  }

  # return
  return(map)
}
