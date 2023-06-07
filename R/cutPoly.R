#' cutPoly
#'
#' @description
#' This function cuts out holes in a given polygon
#'
#' @param sPoly \code{sf} object, the input polygon within which line transects
#'  will be generated. The coordinate system
#'  must use meters as the linear unit (e.g. EPSG=26913)
#' @param xPoly \code{sf} object, optional input polygon that defines an
#' exclusion area where line transects will not be generated.
#'
#' @importFrom sf st_crs st_transform st_combine st_difference st_agr
#'
#' @return A polygon with cutouts
#'
#' @export

cutPoly <- function(sPoly, xPoly) {

  # transform to same crs
  xPoly <- sf::st_transform(xPoly, sf::st_crs(sPoly))

  # combine xPoly
  xPoly <- sf::st_combine(xPoly)

  # take difference
  sf::st_agr(sPoly) = "constant"
  diffPoly <- sf::st_difference(sPoly, xPoly)

  # return
  return(diffPoly)
}
