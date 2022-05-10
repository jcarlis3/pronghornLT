#' makeLines
#'
#' @description This function optimizes the placement and spacing of
#' line transects for a given polygon area and desired total
#' transect length.
#'
#' @param sPoly  \code{sf} object, the input polygon within which line transects
#'  will be generated. The coordinate system
#'  must use meters as the linear unit (e.g. EPSG=26913)
#' @param xPoly  \code{sf} object, optional input polygon that defines an
#' exclusion area where line transects will not be generated.
#' @param totalLengthKm Total length of transects to generate (in kilometers)
#' @param angle Orientation of the transects. \code{angle = 0} produces
#' transects oriented North-South (the default), \code{angle = 90} produces
#' East-West transects.
#' @param minLengthKm Minimum length required for an individual transect line
#' (in kilometers)
#' @param offset An offset to use when placing transects. By default the central
#' transect is aligned with the center of the polygon.
#' @param minSpace Optional minimal line spacing (in kilometers) to consider when
#' optimizing transect placement (default is NULL).
#' @param maxSpace Optional maximum line spacing (in kilometers) to consider when
#' optimizing transect placement (default is NULL).
#' @param optimTol Optional tolerance value indicating the maximum allowable
#' deviation from \code{totalLengthKm} expressed as a proportion (default is 0.01).
#' When the function fails to find a solution within the specified tolerance an
#' error is returned.
#' @param outFile Optional output filename for a saving a spatial layer containing
#' the line transects. Typically, this will be a shapefile
#' (e.g. "C:/Temp/myLines.shp")
#' The file extension can be any accepted by \code{sf::st_write()}
#' @param overwrite If \code{outFile} is specified, and a file already exists
#' with that name, should that existing file be overwritten by the new one?
#' (default is TRUE)
#'
#' @return Returns a list object containing the transect lines and
#' a summary dataframe:
#' \describe{
#'   \item{lines}{\code{sf} object, simple feature collection of transect lines}
#'   \item{summary}{\code{data.frame} summary of transect lines}
#' }
#'
#' @author Tom Prebyl, minor contributions by Jason Carlisle
#' @importFrom sf st_difference st_union st_coordinates st_sfc st_bbox st_crs st_length st_write st_linestring
#' @import spatstat
#' @import spatstat.geom
#' @export
#'
#' @examples
#' \dontrun{
#' #Read in herd unit polygon
#' hu <- sf::st_read("W:/My Drive/Projects/Pronghorn_ltds/herd_units/Rattlesnake_HU.shp")
#'
#' # Make lines
#' testLines <- makeLines(sPoly = hu,
#'      xPoly = NULL,
#'      totalLengthKm = 1000,
#'      angle = 0,
#'      outFile = NULL,
#'      minSpace = NULL,
#'      maxSpace = NULL)
#'
#' # View output summary
#' print(testLines$summary)
#'
#' # Plot output
#' plot(hu["geometry"])
#' plot(testLines$lines['lineID'], add = T, col = 'red')
#' }


makeLines <- function(sPoly,
              xPoly = NULL,
              totalLengthKm,
              angle = 0,
              minLengthKm = 2.0,
              offset = 0,
              minSpace = NULL,
              maxSpace = NULL,
              optimTol = 0.01,
              outFile = NULL,
              overwrite = TRUE) {

  # Functions below interpret angle = 90 to be North-South and 0 East-West
  # Convert so user inputs are more intuitive 0 = N-S, 90 = E-W
  angle <- (angle*-1)-90


  # Convert km to m
  targLenM <- totalLengthKm*1000
  minLength <- minLengthKm*1000

  # Erase exclusion polygon
  if(!is.null(xPoly)) {
    xPoly <- sf::st_transform(xPoly, sf::st_crs(sPoly))
    diffPoly <- sf::st_difference(sf::st_union(sPoly),
                              sf::st_union(xPoly))
  }
  else {
    diffPoly <- sf::st_union(sPoly)
  }

  # Convert polygon to spatstat window
  polyCoord <- sf::st_coordinates(diffPoly)
  polyWin <- spatstat.geom::owin(poly = list(x = rev(polyCoord[, 1]),
                                             y = rev(polyCoord[, 2])))


  # Estimate approx line spacing (using area)
  totSqKM <- as.numeric(sf::st_area(sPoly)*1e-06)
  targKmPerSqKm <- totalLengthKm/totSqKM
  approxSpace <- exp(-1*log(targKmPerSqKm))*1000  # approximate line spacing (m)


  # Set spacing optimization bounds (if not supplied by user)
  if(is.null(minSpace)) {
    minSpace <- approxSpace*0.75
  }
  if(is.null(maxSpace)) {
    maxSpace <- approxSpace*1.1
  }

  optimSpace <- stats::optimize(
    f = evalLines,
    lower = minSpace,
    upper = maxSpace,
    # tol = 0.1,
    angle = angle,
    win = polyWin,
    minLength = minLength,
    targLenM = targLenM,
    offset = offset
  )

  # If quick optim failed, try more intensive manual search
  if(optimSpace$objective > targLenM*optimTol) {
    op <- options("warn")
    on.exit(options(op))
    options(warn=1)
    warning("Quick optimization failed, trying intensive search")
    optimSpace <- manualOptim(
      minSpace = minSpace,
      maxSpace = maxSpace,
      angle = angle,
      win = polyWin,
      minLength = minLength,
      targLenM = targLenM,
      offset = offset,
      optimTol = optimTol
    )
  }


  if(optimSpace$objective > targLenM*optimTol) {
    stop(paste0("Optimization failed to generate lines with supplied parameters\n",
                "Consider setting minSpace and maxSpace parameters\n",
                "or increasing optimTol.",
                "The best spacing found was: ", round(optimSpace$minimum/1000), "km\n"))
  }

  # print(approxSpace / optimSpace$minimum)

  # Make lines using the optimization results
  keepLines <- genLines(
    angle = angle,
    spacing = optimSpace$minimum,
    win = polyWin,
    offset = offset,
    minLength = minLength)

  # Convert lines to sf object
  matLines <- as.matrix(keepLines)
  lines <- lapply(1:nrow(matLines), function(x) {
    sf::st_linestring(matrix(matLines[x,],ncol=2, byrow = TRUE))
  })
  sfLines <- sf::st_sfc(lines, crs = sf::st_crs(diffPoly))

  # Add attributes
  sfLines <- sf::st_sf(sfLines)
  sfLines$lineID <- 1:nrow(sfLines)
  sfLines$lengthKM <- as.numeric(sf::st_length(sfLines)/1000)

  # Make summary info
  outSumm <- list(
    nLines = nrow(sfLines),
    genTotalLengthKm = sum(sfLines$lengthKM),
    spacingKm = optimSpace$minimum/1000
    # approxRatio = optimSpace$minimum/approxSpace
    )

  # Write shapefile
  if(!is.null(outFile)) {
    sf::st_write(sfLines, outFile, append = !overwrite)
  }

  # Return sf object and summary
  print(outSumm)
  return(list(lines = sfLines, summary = outSumm))
}



# Helper Functions--------------------------------------------------------------
# Define a function to generate lines (modified from spatstat.geom::rlinegrid())
genLines <- function (spacing,
                      win,
                      offset,
                      minLength,
                      angle = 0) {
  # TODO validate inputs
  win <- spatstat.geom::as.owin(win)
  width <- diff(win$xrange)
  height <- diff(win$yrange)
  rmax <- sqrt(width ^ 2 + height ^ 2) / 2 #length of diagonal divided by 2
  xmid <- mean(win$xrange)
  ymid <- mean(win$yrange)
  # u <- runif(1, min = 0, max = spacing) - rmax
  u <- offset - rmax
  if (u >= rmax)
    return(
      spatstat.geom::psp(
        numeric(0),
        numeric(0),
        numeric(0),
        numeric(0),
        window = win,
        check = FALSE
      )
    )
  p <- seq(from = u, to = rmax, by = spacing)
  q <- sqrt(rmax ^ 2 - p ^ 2)
  theta <- pi * ((angle - 90) / 180)
  co <- cos(theta)
  si <- sin(theta)
  X <- spatstat.geom::psp(
    x0 = xmid + p * co + q * si,
    y0 = ymid + p * si - q * co,
    x1 = xmid + p * co - q * si,
    y1 = ymid + p * si + q * co,
    window = spatstat.geom::owin(xmid + c(-1, 1) * rmax, ymid + c(-1, 1) * rmax),
    check = FALSE
  )
  X <- X[win]$ends
  xlengths <- eval(expression(sqrt((x1 - x0) ^ 2 + (y1 - y0) ^ 2)), envir = X)
  subX <- X[xlengths >= minLength, ]
  return(subX)
}

# Define a function to evaluate generated line length with respect to target length
evalLines <- function(spacing, angle, win, targLenM, offset, minLength){
  tlines <- genLines(
    angle = angle,
    spacing = spacing,
    win = win,
    offset = offset,
    minLength = minLength
  )
  tlengths <- eval(expression(sqrt((x1 - x0)^2 + (y1 - y0)^2)), envir = tlines)
  totLen <- sum(tlengths)
  err <- abs(targLenM-totLen)
  # print(paste0(spacing, ":", err))
  return(err)
}


# Manual optimization function to run if stats::optim fails.
  # Identifies local minimums in spacing-error curve and iteratively searches
    # each minima for acceptable spacing solution. Explores different offsets
    # if necessary.
manualOptim <- function(minSpace, maxSpace,
                        angle, win, targLenM,
                        offset, minLength,
                        optimTol,
                        roughIter = 50,
                        fineIter = 150) {

  # Initialize output
  optimMan <- list(objective = Inf,
                   minimum = mean(c(minSpace, maxSpace)))

  # Run rough optimization
  tryOffs <- c(0, 500, 1000, 1500) #offsets to try if necessary
  minInd <- NA
  for(offInd in 1:length(tryOffs)) {
    if(offInd > 1) {
      warning("Failed to find solution with supplied offset, trying: ",
        "offset = ", tryOffs[offInd])
      offset <- tryOffs[offInd]
      fineIter = fineIter+100
    }
    while(any(c(is.na(minInd),
                minInd==1,
                minInd==roughIter))) {
      roughSpace <- seq(minSpace, maxSpace, length.out = roughIter)
      roughRes <- sapply(roughSpace, function(x) {
        evalLines(spacing = x,
                  angle = angle,
                  win = win,
                  targLenM = targLenM,
                  offset = offset,
                  minLength = minLength)})
      if(any(roughRes < targLenM*optimTol)){
             roughOptim <- list(minimum = roughSpace[which.min(roughRes)],
                               objective = min(roughRes))
             return(roughOptim) # return if satisfies criteria
      }
      # get rough local minima
      roughlocMins <- localmin(roughRes)$ind
      roughlocMins <- sort(unique(c(roughlocMins, which.min(roughRes))))
      minInd <- min(roughlocMins)
      if(minInd == 1) {
        minSpace <- minSpace*0.75
      }
      else if(minInd == length(roughSpace)) {
        maxSpace <- maxSpace*0.25
      }
    }
    # plot(roughSpace, roughRes)
    # points(roughSpace[roughlocMins], roughRes[roughlocMins],
      # pch = 16, col = 'red')

    # run fine optimization on each local minima
    fineOut <- do.call(rbind, lapply(roughlocMins, function(roughInd) {
      roughOptim <- roughSpace[roughInd]
      # roughMin <- max(roughOptim*0.2, roughSpace[max(c(1,(roughInd-2)))])
      # roughMax <- min(roughOptim*2, roughSpace[min(c(length(roughSpace),(roughInd+2)))])
      roughMin <- roughSpace[max(c(1,(roughInd-4)))]
      roughMax <- roughSpace[min(c(length(roughSpace),(roughInd+4)))]

      # Run fine optimization
      fineSpace <- seq(roughMin, roughMax, length.out = fineIter)
      fineRes <- sapply(fineSpace, function(y) {
        evalLines(spacing = y,
                  angle = angle,
                  win = win,
                  targLenM = targLenM,
                  offset = offset,
                  minLength = minLength)
        })
      # plot(fineSpace, fineRes)
      tempOptim <- list(minimum = fineSpace[which.min(fineRes)],
                         objective = min(fineRes))
      return(tempOptim)
      }))

    fineOut <- fineOut[which.min(fineOut[,2]),]
    fineOptim <- list(minimum = fineOut$minimum,
                       objective = fineOut$objective)
    if(fineOptim$objective < targLenM*optimTol) {
      optimMan <- fineOptim
      return(fineOptim) # return if satisfies criteria
    }
    # update best solution
    if(fineOptim$objective < optimMan$objective) {
      optimMan <- fineOptim
    }

  } # end offset loop
  return(optimMan)
}


# Local minima function (modified from R package erpR)
localmin <- function (x, n.points = 2) {
  vet = x
  dat = data.frame(index = 1:length(vet))
  test = function(i, vet, n.points) {
    indices = c((i - n.points):(i - 1), (i + 1):(i + n.points))
    indices = indices[indices > 0 & indices < (length(vet))]
    response = ((!any(vet[indices] <= vet[i])) & length(indices) ==
                  (n.points * 2))
    return(response)
  }
  candidates.indices = apply(dat, 1, function(k) {
    test(i = k, vet, n.points)
  })
  if (any(candidates.indices)) {
    candidates = vet[candidates.indices]
    out <- list(ind = which(candidates.indices),
                vals = candidates)
    # return(min(candidates))
    return(out)
  }
  else {
    return(NA)
  }
}
