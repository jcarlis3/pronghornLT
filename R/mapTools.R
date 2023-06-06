#' mapHerd
#'
#' @import leaflet
#' @import sf
#'
#' @export

mapHerd <- function(herdPoly) {

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

  # return
  return(map)
}

#' mapHerdWithCutouts
#'
#' @import leaflet
#' @import sf
#'
#' @export

mapHerdWithCutouts <- function(herdPoly, cutoutPoly) {

  # transform poly
  cutoutPoly <- sf::st_transform(cutoutPoly, '+proj=longlat +datum=WGS84')

  # initial map
  map <- mapHerd(herdPoly)

  # add cutouts
  map <- map %>%
    addPolygons(data = cutoutPoly,
                opacity = 1,
                weight = 1,
                color = "red",
                fillOpacity = 0.05,
                fillColor = "red")

  # return
  return(map)
}

#' mapHerdWithLines
#'
#' @import leaflet
#' @import sf
#' @importFrom htmltools HTML
#' @rawNamespace import(data.table, except = shift)
#'
#' @export

mapHerdWithLines <- function(herdPoly, cutoutPoly = NULL, lines) {

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

  # initial map
  map <- mapHerd(herdPoly)

  # if cutouts
  if (!(is.null(cutoutPoly))) {
    map <- mapHerdWithCutouts(herdPoly, cutoutPoly)
  }

  # add lines
  map <- map %>%
    addPolylines(
      data = lines,
      weight = 2,
      color = "blue",
      label = ~ leaflet_label,
      opacity = 1
    )

  # return
  return(map)
}
