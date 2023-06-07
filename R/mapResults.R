#' mapResults
#' @description
#' quick wrapper for leaflet maps
#'
#' @import leaflet
#' @import sf
#' @importFrom htmltools HTML
#' @importFrom lubridate as_date
#' @rawNamespace import(data.table, except = shift)
#'
#' @export

mapResults = function(preppedData) {

  # extract lines, points
  l <- sf::st_transform(preppedData$sfLines, crs = 4326)
  l <- dplyr::arrange(l, begin)
  p <- sf::st_transform(preppedData$sfPoints, crs = 4326)

  # add date, time
  l$Date <- lubridate::as_date(l$begin)
  l$Begin <- format(as.POSIXct(l$begin), format = "%H:%M")
  l$End <- format(as.POSIXct(l$end), format = "%H:%M")
  p$Time <- format(as.POSIXct(p$DateTime), format = "%H:%M")
  p$Date <- lubridate::as_date(p$DateTime)


  # label points
  p$popup_text <-
    paste0(
      "Transect ID: ", p$siteID, "<br/>",
      "Altitude: <strong>", p$agl, '</strong> <br/>',
      'Band: ', '<strong>', p$band, '</strong> <br/>',
      'Group Size: ', '<strong>', p$s, '</strong> <br/>',
      'Date: ', p$Date, "<br/>",
      "Time: ", p$Time) %>%
    lapply(htmltools::HTML)

  # label transects
  l$popup_text <- paste0(
    "Transect ID: <strong>",
    l$siteID,
    "</strong> <br/>",
    "Transect Length: <strong>",
    round(l$Length, 2),
    " km </strong> <br/>",
    "Date: ", l$Date, "<br/>",
    "Start: ", l$Begin, "<br/>",
    "End: ", l$End
  ) %>%
    lapply(htmltools::HTML)

  # color transects
  l$color <- viridisLite::magma(nrow(l), end = .8)

  # radius of points
  p$radius <- data.table::fcase(
    p$s %in% 1:3, 3,
    p$s %in% 4:10, 5,
    p$s %in% 11:20, 7,
    p$s >= 21, 9
  )

  # leaflet
  map = leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addPolylines(data = l,
                 weight = 4,
                 label = ~popup_text,
                 color = ~color,
                 opacity = .75) %>%
    leaflet::addCircleMarkers(data = p,
                     radius = ~radius,
                     label = ~popup_text,
                     stroke = F,
                     fillOpacity = .45)

  # return
  return(map)
}
