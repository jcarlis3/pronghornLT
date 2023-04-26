#' Calculate the total length of transects to survey
#'
#' Calculate the total length of transects to survey to reach a user-provided
#' target number of pronghorn groups detected.  All else being equal, herd units
#' with higher densities of pronghorn will require less survey effort to meet
#' the target number of pronghorn groups.
#'
#' @param occupiedPolygon sf object, spatial polygon of portion of herd area
#' expected to be occupied by pronghorn.  The area of this polygon is used to
#' calculate the expected density of pronghorn in the survey area.
#' Expects the units of the sf object to be meters, so UTM NAD83 Zone 12 or 13
#' would be safe bets for the CRS to use.
#' @param N Scalar, expected abundance of pronghorn in the herd unit (more
#' specifically, in the area represented by the `occupiedPolygon`).  Estimates
#' from past surveys are likely your best option here.  Used to calculate the
#' expected density of pronghorn in the survey area.
#' @param p Scalar, expected probability of detecting pronghorn in the herd
#' unit.  Defaults to 0.58, the average p from surveys conducted before 2022.
#' @param w Scalar, width (m) of the survey strip.  Defaults to 200 m, the
#' standard width using the Wyoming survey protocol.
#' @param targetGroups Scalar, number of groups to detect.  Defaults to 300
#' pronghorn groups, which is a bit subjective, but should provide plenty of
#' detections to fit a robust detection model.
#' @param avgGroupSize Scalar, expected average number of pronghorn in each
#' group detected.  Defaults to 2.3, the average group size from surveys
#' conducted before 2022.
#' @author Jason Carlisle, based on some clever back-of-the-napkin math by Trent
#' McDonald and Greg Hiatt at a WGFD training on pronghorn LT surveys in Laramie
#' in April 2022.
#'
#' @return Scalar, the total length (km) of transects to survey.
#' @importFrom sf st_area st_union
#' @export
#'
#'
#' @examples
#' \dontrun{
#' # Read in a sf polygon (here for Rattlesnake herd unit less unoccupied area)
#' occupiedPolygon <- sf::st_read("U:/My Drive/PronghornLT/Rattlesnake/GIS",
#'                                "Rattlesnake_HU")
#'
#' calcLineLength(occupiedPolygon = occupiedPolygon,
#'                N = 12000)
#' }


calcLineLength <- function(occupiedPolygon,
                           N,
                           p = 0.58,
                           w = 200,
                           targetGroups = 300,
                           avgGroupSize = 2.3) {


  # Special handling for polygons with multiple features
  if (nrow(occupiedPolygon) > 1) {

    # Issue warning
    warning("Your input polygon has >1 feature and has been dissolved into 1 feature.")

    # Union for multiple features
    occupiedPolygon <- sf::st_union(occupiedPolygon)

  }


  # Calculate area
  area <- sf::st_area(occupiedPolygon)

  # Calculate length of transects in m
  l.m <- (targetGroups*area*avgGroupSize)/(N*p*w)

  # Convert to km
  l.km <- l.m/1e3

  return(as.numeric(l.km))

}
