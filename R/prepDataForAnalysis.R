#' prepDataForAnalysis
#'
#' @description This function reads in raw survey data collected during
#' aerial line transect surveys for pronghorn (exported directly from
#' CyberTracker data-collection software) and prepares it for distance-sampling
#' analysis in Program R or Program DISTANCE.
#' @param inputFile  The survey data output from CyberTracker.  Excel files with
#'  extension \code{.xls} or \code{.xlsx} are
#' allowed. If \code{inputFile = NULL} (the default), the user is
#' prompted to select the input file interactively. Alternatively, the user can
#' specify the path and name of the Excel file containing the input data.
#' Note the direction of slashes in the path must follow R expectations
#' (use \code{/} instead of the Windows default of \code{\\}),
#' that the file name and extension must be included,
#' and that the path must start and end with quotes.
#' Example:  \code{"C:/Users/jcarlisle/Desktop/myInputFile.xlsx"}.
#' This function expects the following columns (named as shown) in the
#' \code{inputFile}.  The order of the columns does not matter (except in the
#' case of the "Cluster size" columns, see below), and
#' any additional columns are ignored:
#' \describe{
#'  \item{Transect number}{Unique name of each transect.}
#'  \item{Flight control}{Indicates transect starts/stops from detections.}
#'  \item{Latitude}{Y coordinate from GPS.}
#'  \item{Longitude}{X coordinate from GPS.}
#'  \item{Range}{Height of airplane (above ground level [AGL]) in feet.}
#'  \item{Cluster size}{For some reason, the CyberTracker output contains two
#'  columns named "Cluster size".  This function expects the first one to be
#'  the number of pronghorn in the detected group.}
#'  \item{Cluster size}{For some reason, the CyberTracker output contains two
#'  columns named "Cluster size".  This function expects the second one to be
#'  the distance band the pronghorn were detected in.}
#'  \item{Date}{Date of survey.  Can be blank, but issues a warning if so.}
#'  \item{Time}{Time of survey.  Can be blank, but issues a warning if so.}
#' }
#' @param inputSheet  The sheet (or tab) within the above-specified Excel file
#' that should be read in. If \code{inputSheet = NULL} (the default), the names
#' of the sheets in the \code{inputFile} are printed to the console, and the
#' user is prompted to select the sheet name interactively.  Alternatively,
#' specify as the name of the sheet (e.g., \code{"Sheet1"} or \code{"inputData"})
#' or as the numbered position of the sheet within the file (e.g., \code{1}).
#' @param shpCRS Numeric, the EPSG code for the coordinate reference system
#' (projection) of the coordinates in the input data.  Common projections for
#' Wyoming:
#' \itemize{
#' \item 26912:  UTM NAD83 Zone 12N
#' \item 26913:  UTM NAD83 Zone 13N (default)
#' }
#'
#' @return Returns a list with six elements:
#' \describe{
#'  \item{ddf}{A data.frame of detection data.  One row for each group of
#'    pronghorn detected.  See the \code{detectionData} argument of
#'    \code{Rdistance::dfuncEstim} and \code{Rdistance::abundEstim}.}
#'  \item{sdf}{A data.frame of site data.  One row for each site (transect)
#'    surveyed.  See the \code{siteData} argument of
#'    \code{Rdistance::dfuncEstim} and \code{Rdistance::abundEstim}.}
#'  \item{txt}{A data.frame in the "flat file" format expected by Program
#'    DISTANCE.  Ready to be written to a tab-delimited text file which can be
#'    imported into Program DISTANCE.  The column names are
#'    standardized to use names similar to Program DISTANCE (making import less
#'    error-prone) and to clarify units:
#'    \describe{
#'    \item{lineLabel}{Unique name of each transect.}
#'    \item{lineLengthKm}{Length of each transect in kilometers.}
#'    \item{coordsX}{X coordinate from GPS.}
#'    \item{coordsY}{Y coordinate from GPS.}
#'    \item{flightHeightFt}{Height of airplane (AGL) in feet.}
#'    \item{clusterSize}{Number of pronghorn in detected group.}
#'    \item{distBand}{Distance band the pronghorn were detected in.}
#'    \item{adjustedDistM}{Estimated distance from transect to the detected
#'    pronghorn group in meters. Based on the midpoint of the distance band
#'    containing the detected group, adjusted for the flight height at the time
#'    of detection.}
#'    }}
#'  \item{sumTable}{A data.frame of summaries of the input dataset, meant to aid
#'    in QAQC and troubleshooting.}
#'  \item{sfLines}{An sf object of the transect lines as surveyed.
#'    The transects are reconstructed from the transect start and stop records
#'    in the input data.
#'    See the \code{shpCRS} argument to specify a coordinate reference system.}
#'  \item{sfPoints}{An sf object of the detection points.  The points are
#'    created from the sighting records in the input data.  Note, these
#'    locations represent the location of the plane when a pronghorn
#'    group was detected, not the location of the pronghorn group itself.
#'    See the \code{shpCRS} argument to specify a coordinate reference system.}
#' }
#'
#'
#' @author Jason Carlisle
#' @references Guenzel, R.J. 2007. Procedures for Estimating Pronghorn Abundance
#' in Wyoming Using Aerial Line Transect Sampling. Wyoming Game and Fish Department.
#' Cheyenne, WY, USA.

#' @importFrom dplyr select group_by summarize %>%
#' @importFrom readxl excel_sheets read_excel
#' @importFrom rlang .data
#' @importFrom sf st_as_sf st_cast
#' @importFrom stats dist na.omit
#' @importFrom utils menu
#' @importFrom lubridate ymd_hms ymd as_date
#' @export
#'
#' @examples
#' \dontrun{
#' # Read in Sheet1 of myInputFile.xlsx
#' x <- prepDataForAnalysis(
#'   inputFile = "C:/Users/myUserName/Desktop/myInputFile.xlsx",
#'   inputSheet = "Sheet1"
#' )
#'
#' # Detection data (one row per group detected)
#' head(x$ddf)
#'
#' # Site data (one row per transect surveyed)
#' head(x$sdf)
#'
#' # Both detection and site data (flat file) ready for import to Program DISTANCE
#' head(x$txt)
#'
#' # Summaries of the data (to aid QAQC)
#' x$sumTable
#'
#' # Map of transects as flown
#' plot(sf::st_geometry(x$sfLines))
#'
#' # Map of pronghorn detection locations (plane location)
#' plot(sf::st_geometry(x$sfPoints))
#'
#' # Interactive map of both lines and points
#' # Points are sized based on group size
#' mapview::mapview(x$sfLines, color = "blue", legend = FALSE) +
#'   mapview::mapview(x$sfPoints, cex = "s", label = "s")
#' }
prepDataForAnalysis <- function(inputFile = NULL,
                                inputSheet = NULL,
                                shpCRS = 26913) {
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # File paths ----
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # Allow the user to interactively select the input Excel file
  if (is.null(inputFile)) {
    inputFile <- file.choose()
  }


  # Have the user select which sheet of the selected input Excel file to use
  if (is.null(inputSheet)) {
    sheetNames <- readxl::excel_sheets(inputFile)
    sheetIndex <- menu(sheetNames,
      title = "Which sheet in the Excel file contains the input data?"
    )
    inputSheet <- sheetNames[sheetIndex]
  }


  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Read in Excel file exported by CyberTracker ----
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # Read Excel file
  # The .name_repair argument suppresses the following message caused by
  # duplicated column names in the CyberTracker output
  # New names:
  # • `Cluster size` -> `Cluster size...13`
  # • `Cluster size` -> `Cluster size...16`
  x <- readxl::read_excel(
    path = inputFile,
    sheet = inputSheet,
    .name_repair = "unique_quiet"
  )

  # Check that expected column names exist
  expectedNames <- c(
    "Transect number",
    "Flight control",
    "Latitude",
    "Longitude",
    "Range",
    "Date",
    "Time"
  )
  for (i in expectedNames) {
    if (!i %in% names(x)) {
      stop(
        "This function requires the input file to have a column named '",
        i,
        "' but none exists."
      )
    }
  }


  # Make some column names easier to work with
  names(x)[names(x) == "Transect number"] <- "siteID"
  names(x)[names(x) == "Flight control"] <- "event"
  names(x)[names(x) == "Latitude"] <- "y"
  names(x)[names(x) == "Longitude"] <- "x"
  names(x)[names(x) == "Range"] <- "agl"

  # Multiple columns were named "Cluster size" in input sheet
  # read_excel modifies the names with column index, but make more flexible
  # such that first one is cluster size, second is distance band.
  # This is not a very robust way to handle this, but works on the example
  # datasets provided by WGFD.
  clusterNames <- names(x)[grepl("Cluster size", names(x))]
  names(x)[names(x) == clusterNames[1]] <- "s"
  names(x)[names(x) == clusterNames[2]] <- "band"


  # Fix odd text formatting from CyberTracker which added "; Cluster size" into
  # the band column
  x$band <- gsub("; Cluster size", "", x$band)

  # time format date/time cols
  x$Date <- lubridate::ymd(x$Date)
  x$Time <- lubridate::ymd_hms(x$Time)
  x$Time <- format(as.POSIXct(x$Time), format = "%H:%M:%S")
  x$DateTime <- lubridate::ymd_hms(paste0(x$Date, " ", x$Time))


  # Keep only needed columns
  x <- x[c("siteID", "event", "x", "y", "agl", "s", "band", "DateTime")]


  # Drop the "End survey" event
  x <- x[x$event != "End survey", ]

  # Drop any rows with NA in event
  # An example dataset from Lee for Centennial included a row with date and
  # time, but all other rows were NAs
  x <- x[!is.na(x$event), ]


  # Make siteID a factor (might come in as numeric or character)
  x$siteID <- as.factor(x$siteID)

  # Populate rest of siteID column
  # Go row by row, copy down value in the previous row when an NA is encountered
  # The first value cannot be NA
  if (!is.na(x$siteID[1])) {
    for (i in 2:nrow(x)) {
      if (is.na(x$siteID[i])) {
        x$siteID[i] <- x$siteID[i - 1]
      } else {
        next
      }
    }
  } else {
    stop("The first row of the 'Transect number' column cannot be blank.")
  }


  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Split data into sites and detections ----
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Easier to handle data in two sets.
  # Sites data.frame will have one row for each transect surveyed
  # (analogous to "Line transect" layer in Program DISTANCE)
  # Detections data.frame will have one for each group detected
  # (analogous to "Observation" layer in Program DISTANCE)

  # Sites data.frame
  sdf <- x[
    x$event %in% c("Start new transect", "End transect"),
    c("siteID", "event", "x", "y", "DateTime")
  ]

  # sdf currently has two rows for each transect surveyed (a row for each start
  # and stop location), save that aside as tdf to build transect shp off of.
  # sdf will be collapsed into traditional form with one row for each transect
  # surveyed in the line length calculation below
  tdf <- sdf


  # Check that all sites have start/end
  unique.sites <- unique(as.character(sdf$siteID))
  fail.sites <- NULL

  for (i in 1:length(unique.sites)) {
    nrow.start <-
      nrow(sdf[sdf$siteID == unique.sites[i] &
        sdf$event == "Start new transect", ])
    nrow.end <-
      nrow(sdf[sdf$siteID == unique.sites[i] &
        sdf$event == "End transect", ])
    if (!(nrow.start == 1 & nrow.end == 1)) {
      fail.sites <- c(fail.sites, unique.sites[i])
    }
  }
  if (!is.null(fail.sites)) {
    stop(
      cat(
        "These transect numbers do not have one start record and one end record: ",
        fail.sites
      )
    )
  }



  # Detections data.frame
  ddf <-
    x[x$event == "Record sighting", names(x[names(x) != "event"])]

  # # Note that the desired behavior here is for ddf$siteID to contain empty levels
  # # if there were transects where no pronghorn were detected
  # length(unique(ddf$siteID))
  # length(levels(ddf$siteID))  # will be greater if any transects had no pronghorn detected



  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Calculate length of each transect ----
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # Use euclidean distance function to calculate length of transect (in km)
  # Assumes projected coordinated system (e.g., not decimal degrees of lat/lon)
  sdf <- do.call(rbind, lapply(unique.sites, function(i) {
    p <- sdf[sdf$siteID == i, ]

    l <- as.numeric(dist(p[c("x", "y")], method = "euclidean")) / 1e3

    r <- data.frame(
      siteID = i,
      lengthKm = l
    )

    return(r)
  }))



  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Create sf object of transects as surveyed ----
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # Spatial data - transects
  # Create end points
  e <- sf::st_as_sf(
    x = tdf,
    coords = c("x", "y"),
    crs = shpCRS
  )

  # Convert end points to lines
  # I think the summarize step is needed, so added a dummy variable s that
  # is then dropped
  l <- e %>%
    dplyr::group_by(siteID) %>%
    dplyr::summarize(s = unique(siteID), begin = min(DateTime), end = max(DateTime)) %>%
    sf::st_cast("LINESTRING") %>%
    dplyr::select(siteID, begin, end, geometry)

  # add transect length
  lLength <- sf::st_length(l)
  lLength <- units::set_units(lLength, "km")
  l$Length <- lLength

  # Check
  # plot(sf::st_geometry(e))
  # plot(sf::st_geometry(l), add=TRUE)

  # get appx. transect spacing
  transectSpacing <- l |>
    dplyr::group_by(siteID) |>
    sf::st_line_sample(n = 100) |>
    sf::st_as_sf() |>
    sf::st_distance() |>
    as.data.frame.table(responseName = "Dist") |>
    dplyr::filter(Var1 != Var2) |>
    dplyr::group_by(Var1) |>
    dplyr::slice_min(order_by = Dist) |>
    dplyr::ungroup() |>
    dplyr::filter(Dist == max(Dist)) |>
    dplyr::slice_sample(n = 1) |>
    getElement("Dist") |>
    units::set_units("km") |>
    units::drop_units()

  # Spatial data - detections
  # Create points for pronghorn detections (where the plane was when detection
  # was recorded)

  # Create points
  p <- sf::st_as_sf(
    x = ddf,
    coords = c("x", "y"),
    crs = shpCRS
  )



  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Impute missing flight heights ----
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Mean AGL for all data (that have AGLs, before imputing)
  agl.all <- mean(ddf$agl, na.rm = TRUE)

  # Summary of AGL values (NA vs not) by transect
  agl.site <- ddf %>%
    dplyr::group_by(.data$siteID, .drop = FALSE) %>%
    dplyr::summarize(
      n = length(.data$agl),
      nNA = sum(is.na(.data$agl)),
      nGood = .data$n - .data$nNA
    )



  # Step through algorithm to impute missing flight heights:
  # Use the average height of the one previous and one subsequent detections.
  # Special cases:
  # If missing agl is the 1st detection of the transect, we use the height of 2nd detection.
  # If missing agl is the last detection, use the height of the 2nd-to-last detection.
  # If all detections on a transect are missing agl (hopefully rare), use the average agl
  # from the previous and subsequent transects.


  # First, add detection ID within each transect
  ddf <- do.call(rbind, lapply(unique.sites, function(i) {
    if (i %in% ddf$siteID) {
      p <- ddf[ddf$siteID == i, ]
      p$id <- 1:nrow(p)
      return(p)
    } else {
      return(NULL)
    }
  }))


  # Loop through transects and apply the data imputing rules
  # Rebuilds the ddf by appending d (the subset of ddf) one transect at a time
  # Outer loop through sites,
  # Inner loop through detections

  ddf <- do.call(rbind, lapply(1:length(unique.sites), function(i) {
    # Skip over transects where no detections were recorded
    if (agl.site$n[agl.site$siteID == unique.sites[i]] == 0) {
      return(NULL)
    }


    # Detections for the transect of interest
    d <- ddf[ddf$siteID == unique.sites[i], ]


    # If all agls are valid (no NAs)
    # No need to impute anyting
    if (sum(is.na(d$agl)) == 0) {
      return(d)
    } else {
      # If all agls are NA
      if (sum(!is.na(d$agl)) == 0) {
        # this site and those with usable agl values
        good.sites <-
          agl.site[(agl.site$nGood > 0 |
            agl.site$siteID == unique.sites[i]), ]
        good.sites$id <-
          1:nrow(good.sites) # numeric way to index sites, clunky
        use.sites.id <-
          c(
            good.sites$id[good.sites$siteID == unique.sites[i]] - 1,
            good.sites$id[good.sites$siteID == unique.sites[i]] + 1
          )
        use.sites <-
          as.character(good.sites$siteID[good.sites$id %in% use.sites.id])
        d$agl <-
          mean(ddf$agl[ddf$siteID %in% use.sites], na.rm = TRUE) # mean at two transects

        return(d)
      } else {
        # Cases with some NAs (but not all) in agl
        good.agls <- na.omit(d$agl)

        # NA in first detection
        if (is.na(d$agl[1])) {
          d$agl[1] <- good.agls[1]
        }

        # NA in last detection
        if (is.na(d$agl[nrow(d)])) {
          d$agl[nrow(d)] <- good.agls[length(good.agls)]
        }

        if (nrow(d) >= 3) {
          # Helper indices (row numbers) of rows with NAs and without NAs in agl
          na.rows <- which(is.na(d$agl))
          good.rows <- which(!is.na(d$agl))

          for (j in na.rows) {
            pre.agl <-
              d$agl[j - 1] # previous agl (in order, so any NA should already be resolved)
            post.agl <-
              d$agl[min(good.rows[good.rows > j])] # next good agl

            d$agl[j] <- mean(c(pre.agl, post.agl))
          }
        }
        return(d)
      }
    }
  }))


  # Issue a warning if, for some reason, there are still NAs in the agl column
  if (anyNA(ddf$agl)) {
    warning("The flight height contains NA values even after attempting to replace the NAs.")
  }


  # Mean AGL for all data (after imputing)
  agl.obs <- mean(ddf$agl, na.rm = TRUE)

  # Difference in mean AGL after imputing compared to before imputing
  # agl.obs - agl.all

  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Calculate adjusted distances based on bin midpoints and flight height ----
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # Nominal flight height (ft)
  agl.target <- 300

  # Nominal right cut points of each distance band
  cuts <- c(0, 20, 45, 80, 145, 200)

  # Midpoint distance of each nominal band
  cutMids <- cuts[-length(cuts)] + diff(cuts) / 2


  # Insert nominal distance for band midpoint
  ddf$adjustedDist <- NA
  ddf$adjustedDist[grepl("Alpha", ddf$band)] <- cutMids[1]
  ddf$adjustedDist[grepl("Bravo", ddf$band)] <- cutMids[2]
  ddf$adjustedDist[grepl("Charlie", ddf$band)] <- cutMids[3]
  ddf$adjustedDist[grepl("Delta", ddf$band)] <- cutMids[4]
  ddf$adjustedDist[grepl("Echo", ddf$band)] <- cutMids[5]

  # Adjust based on ratio of observed flight height to nominal flight height
  # for each detection
  ddf$adjustedDist <- ddf$adjustedDist * (ddf$agl / agl.target)



  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Calculate adjusted distance band cutpoints based on flight height ----
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # Overall ratio of observed flight height to nominal flight height
  agl.ratio <- agl.obs / agl.target

  # Adjust based on overall ratio of flight heights
  cutsNew <- cuts * agl.ratio



  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Format for .txt file ready for DISTANCE import ----
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # Merge into what Program DISTANCE calls a "flat file"
  # A row for each detection, that includes the transect length
  # Plus a row for each transect where nothing was detected (with NAs for most cols)
  distData <- merge(sdf, ddf, all = TRUE)
  distData$id <- NULL
  distData$DateTime <- NULL


  # Make some column names easier to work with
  names(distData)[names(distData) == "siteID"] <- "lineLabel"
  names(distData)[names(distData) == "lengthKm"] <- "lineLengthKm"
  names(distData)[names(distData) == "x"] <- "coordsX"
  names(distData)[names(distData) == "y"] <- "coordsY"
  names(distData)[names(distData) == "agl"] <- "flightHeightFt"
  names(distData)[names(distData) == "s"] <- "clusterSize"
  names(distData)[names(distData) == "band"] <- "distBand"
  names(distData)[names(distData) == "adjustedDist"] <- "adjustedDistM"


  # Do some rounding to make text files easier to work with
  # (when aligning columns in DISTANCE)
  distData$lineLengthKm <- round(distData$lineLengthKm, 3)
  distData$coordsX <- round(distData$coordsX, 0)
  distData$coordsY <- round(distData$coordsY, 0)
  distData$flightHeightFt <- round(distData$flightHeightFt, 2)
  distData$adjustedDistM <- round(distData$adjustedDistM, 2)



  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Calculate study summaries to aid troubleshooting ----
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # Number of transects
  n.trans <- length(unique(sdf$siteID))

  # Number of detections
  n.detects <- nrow(ddf)

  # Number of pronghorn detected
  n.detects.ind <- sum(ddf$s)

  # Number of detections after truncating at right-most cutpoint
  n.detects.trunc <- sum(ddf$adjustedDist <= max(cutsNew))

  # Number of pronghorn detections after truncating at right-most cutpoint
  n.detects.ind.trunc <- sum(ddf$s[ddf$adjustedDist <= max(cutsNew)])

  # Mean group size (after truncation)
  mean.group <- mean(ddf$s[ddf$adjustedDist <= max(cutsNew)])

  # Max group size (after truncation)
  max.group <- max(ddf$s[ddf$adjustedDist <= max(cutsNew)])

  # Total length of surveyed transects
  total.km <- sum(sdf$lengthKm)

  # Number of missing AGLs
  n.NA <- sum(agl.site$nNA)

  # Mean AGL before imputing
  agl.all

  # Mean AGL after imputing
  agl.obs

  # Total rows in output file
  n.rows <- nrow(distData)

  # Number of transects with no detections
  n.trans.no <- sum(is.na(distData$adjustedDistM))



  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Create table of key data summaries ----
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # Key summaries in data.frame format easier to display in shiny app
  sumTable <- data.frame(
    Summary = c(
      "Number of transects",
      "Number of transects with pronghorn detected",
      "Number of transects without pronghorn detected",
      "Total transect length surveyed (km)",
      "Total transect length surveyed (mi)",
      "Approximate transect spacing (km)",
      "Number of groups detected (any distance)",
      "Number of groups detected (within adjusted survey strip)",
      "Number of individuals detected (any distance)",
      "Number of individuals detected (within adjusted survey strip)",
      "Mean group size (within adjusted survey strip)",
      "Maximum group size (within adjusted survey strip)",
      "Number of groups missing flight height",
      "Mean flight height (ft) at detections after imputing missing values",
      "Ratio of actual/nominal flight height to adjust bin cutpoints"
    ),
    Value = c(
      n.trans,
      n.trans - n.trans.no,
      n.trans.no,
      total.km,
      total.km * 0.621371,
      transectSpacing,
      n.detects,
      n.detects.trunc,
      n.detects.ind,
      n.detects.ind.trunc,
      mean.group,
      max.group,
      n.NA,
      agl.obs,
      agl.obs / agl.target
    )
  )


  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Return list of R objects (nothing written to file) ----
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # Return prepped data objects and table of key summaries
  return(list(
    ddf = as.data.frame(ddf),
    sdf = sdf,
    txt = distData,
    sumTable = sumTable,
    sfLines = l,
    sfPoints = p
  ))
}
