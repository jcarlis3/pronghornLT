#' Fit distance-sampling model to pronghorn line-transect data
#'
#' Estimate abundance of pronghorn using distance sampling.
#' Distance-sampling models are fit using the Rdistance package.
#' This is a thin wrapper around \code{Rdistance::dfuncEstim} and
#' \code{Rdistance::abundEstim}.
#'
#' @param ddf Detection data.  See \code{Rdistance::dfuncEstim} and
#' \code{Rdistance::abundEstim}.
#' @param sdf Site data.  See \code{Rdistance::abundEstim}.
#' @param keyFun Shape of detection curve.  See \code{Rdistance::dfuncEstim}.
#' @param wHi Right truncation distance.  See \code{Rdistance::dfuncEstim}.
#' Default is 200 m, given the pronghorn survey strip is 200 m wide.
#' @param sidesSurveyed Sides of the aircraft surveyed.  Default is 1, given the
#' current protocol specifies 1 observer looking out 1 side of the aircraft.
#' Rdistance assumes 2 sides of the line are surveyed, so this allows the survey
#' strip area to be adjusted accordingly.
#' @param areaMi2 Size of the area (in square miles) that the abundance estimate
#' is extrapolated to.  This is often the total area of the herd unit, but areas
#' not occupiable by pronghorn (e.g., forested areas) should be excluded.
#' @param bootIterations Number of bootstrap iterations to use to calculate
#' confidence intervals.  See \code{Rdistance::abundEstim}.  If 0 (the default),
#' no bootstrap is run and no CIs are generated.
#'
#' @return An 'abundance estimate' object.  Same as returned by
#' \code{Rdistance::abundEstim}.
#'
#' @author Jason Carlisle
#'
#' @importFrom units set_units
#' @importFrom Rdistance dfuncEstim abundEstim
#' @export
#'
#' @examples
#' \dontrun{
#' # Prep data
#' dataPath <- "C:/Users/jadcarlisle/Desktop/demo"
#' x <- prepDataForAnalysis(inputFile = file.path(dataPath,
#'                                               "Data_PronghornLT_Rattlesnake_2022.xlsx"),
#'                        inputSheet = 1)
#'
#' # Fit distance-sampling model to estimate abundance
#' # Key input data are the x$ddf and x$sdf data.frames from prepDataForAnalysis
#' fit <- fitDistSampModel(ddf = x$ddf,
#'                        sdf = x$sdf,
#'                        keyFun = "hazrate",
#'                        sidesSurveyed = 1,
#'                        areaMi2 = 884,
#'                        bootIterations = 50)
#'
#' # Print results
#' fit
#' }

fitDistSampModel <- function(ddf,
                             sdf,
                             keyFun = "halfnorm",
                             wHi = 200,
                             sidesSurveyed = 1,
                             areaMi2,
                             bootIterations = 0) {

  # Set seed to make bootstrapping results reproducible
  set.seed(82070)


  # Adjust transect lengths to adjust total surveyed area.
  # Only one side of transect typically surveyed, but Rdistance expects both
  # sides are.
  sampFrac <- sidesSurveyed / 2
  sdf$length <- sampFrac * sdf$lengthKm
  sdf$lengthKm <- NULL


  # Set units (required by Rdistance beginning with version 2.2.0)
  # And Rdistance works best if the distance column is named "dist"
  wHi <- units::set_units(wHi, "m")
  ddf$dist <- units::set_units(ddf$adjustedDist, "m")
  ddf$adjustedDist <- NULL
  sdf$length <- units::set_units(sdf$length, "km")
  areaMi2 <- units::set_units(areaMi2, "mi2")



  # Fit detection function
  # Specifying outputUnits = "m" will provide distance measures in m
  # and density estimate in mi^2 (we'll convert those to mi^2 in the app)
  dfunc <- Rdistance::dfuncEstim(formula = dist ~ 1 + groupsize(s),
                                 detectionData = ddf,
                                 likelihood = keyFun,
                                 expansions = 0,
                                 w.hi = wHi,
                                 outputUnits = "m")


  # Estimate abundance
  # Turn off the bootstrap feature if bootIterations input is 0
  if (bootIterations == 0) {
    ci <- NULL  # Rdistance input to skip bootstrap and not calculate CIs
  } else {
    ci <- 0.95  # default 95% CI
  }

  fit <- Rdistance::abundEstim(dfunc,
                               detectionData = ddf,
                               siteData = sdf,
                               area = areaMi2,
                               ci = ci,
                               R = bootIterations,
                               plot.bs = FALSE,
                               showProgress = FALSE)

  return(fit)

}
