#' prepDataForAnalysis
#'
#' @description This function reads in raw data exported from CyberTracker
#' software and prepares it for input to Program DISTANCE for a
#' distance-sampling analysis.
#' The prepped data is written to file, and some summaries of the data are
#' printed to the R console.
#' @param inputFile  The data output from CyberTracker to format for input to
#' Program DISTANCE.  Excel files with extension \code{.xls} or \code{.xlsx} are
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
#' }
#' @param inputSheet  The sheet (or tab) within the above-specified Excel file
#' that should be read in. If \code{inputSheet = NULL} (the default), the names
#' of the sheets in the \code{inputFile} are printed to the console, and the user
#' is prompted to select the sheet name interactively.  Alternatively,
#' specify as the name of the sheet (e.g., \code{"Sheet1"} or \code{"inputData"})
#' or as the numbered position of the sheet within the file (e.g., \code{1}).
#' @param outputFile The path and name of the tab-delimited text file to write
#' out containing the output data ready for import into Program DISTANCE.
#' If \code{outputFile = NULL} (the default), the file will be written to the
#' same directory as the \code{inputFile}, with \code{"PreppedData_"} prepended
#' to the file name of the \code{inputFile}.  E.g, if
#' \code{inputFile} is \code{"C:/Users/jcarlisle/Desktop/myInputFile.xlsx"} and
#' \code{outputFile = NULL}, the output will be written to
#' \code{"C:/Users/jcarlisle/Desktop/PreppedData_myInputFile.txt"}.
#' Alternatively, the user can specify the path and name of the text file.
#' Note the direction of slashes in the path must follow R expectations
#' (use \code{/} instead of the Windows default of \code{\\}),
#' that the file name and extension must be included,
#' and that the path must start and end with quotes.
#' #' Example:  \code{"C:/Users/jcarlisle/Desktop/myOutputFile.txt"}.
#' The column names in the output file are standardized to use names similar to
#' Program DISTANCE (making import less error-prone) and to clarify units:
#' \describe{
#' \item{lineLabel}{Unique name of each transect.}
#' \item{lineLengthKm}{Length of each transect in kilometers.}
#' \item{coordsX}{X coordinate from GPS.}
#' \item{coordsY}{Y coordinate from GPS.}
#' \item{flightHeightFt}{Height of airplane (AGL) in feet.}
#' \item{clusterSize}{Number of pronghorn in detected group.}
#' \item{distBand}{Distance band the pronghorn were detected in.}
#' \item{adjustedDistM}{Estimated distance from transect to the detected
#' pronghorn group in meters. Based on the midpoint of the distance band
#' containing the detected group, adjusted for the flight height at the time of
#' detection.}
#' }
#' @param shpCreate Logical, should a shapefile of the surveyed transects be
#' written out?  The transects are reconstructed from the transect
#' start and stop records in the input data.
#' @param shpCRS Numeric, the EPSG code for the coordinate reference system
#' (projection) of the coordinates in the input data.  Common projections for
#' Wyoming:
#' \itemize{
#' \item 26912:  UTM NAD83 Zone 12N
#' \item 26913:  UTM NAD83 Zone 13N
#' }
#' @param shpFile The path and name of the shapefile to write
#' out containing the surveyed transects.
#' If \code{shpFile = NULL} (the default), the file will be written to the
#' same directory as the \code{inputFile}, with \code{"SurveyedTransects_"}
#' prepended to the file name of the \code{inputFile}.  E.g, if
#' \code{inputFile} is \code{"C:/Users/jcarlisle/Desktop/myInputFile.xlsx"} and
#' \code{shpFile = NULL}, the output will be written to
#' \code{"C:/Users/jcarlisle/Desktop/SurveyedTransects_myInputFile.shp"}.
#' Alternatively, the user can specify the path and name of the shapefile.
#' @param mapCreate Logical, should an interactive map of the surveyed transects and
#' pronghorn observations be created and written to html file?
#' @param mapFile The path and name of the html map to write
#' out showing the surveyed transects and pronghorn observation locations.
#' If \code{mapFile = NULL} (the default), the file will be written to the
#' same directory as the \code{inputFile}, with \code{"Map_"}
#' prepended to the file name of the \code{inputFile}.  E.g, if
#' \code{inputFile} is \code{"C:/Users/jcarlisle/Desktop/myInputFile.xlsx"} and
#' \code{mapFile = NULL}, the output will be written to
#' \code{"C:/Users/jcarlisle/Desktop/Map_myInputFile.html"}.
#'
#' @return Writes a tab-delimited text file to \code{outputFile}.  This file is
#' ready for import into Program DISTANCE.  Also prints the
#' following summaries in the R console:
#' \itemize{
#' \item Number of transects.
#' \item Total transect length surveyed (km).
#' \item Number of groups (clusters) detected.
#' \item Assuming right-truncation at largest adjusted distance interval cutpoint, number of groups (clusters) in analysis.
#' \item Number of individuals detected.
#' \item Assuming right-truncation at largest adjusted distance interval cutpoint, number of individuals in analysis.
#' \item Number of groups (clusters) that had missing flight heights
#' \item Mean flight height (ft) at detections - before imputing missing values
#' \item Mean flight height (ft) at detections - after imputing missing values
#' \item Nominal distance interval cutpoints (m)
#' \item Adjusted distance interval cutpoints (m)
#' }
#'
#' @author Jason Carlisle
#' @references Guenzel, R.J. 2007. Procedures for Estimating Pronghorn Abundance
#' in Wyoming Using Aerial Line Transect Sampling. Wyoming Game and Fish Department.
#' Cheyenne, WY, USA.
#' @importFrom readxl excel_sheets read_excel
#' @importFrom dplyr select group_by summarize %>%
#' @importFrom stats dist na.omit
#' @importFrom utils menu write.table
#' @importFrom sf st_as_sf st_cast st_write
#' @importFrom mapview mapview mapshot
#' @export
#'
#' @examples
#' \dontrun{
#' # Read in Sheet1 of myInputFile.xlsx, and write out myOutputFile.txt
#' # Skip option to write out shapefile of transects as surveyed
#' prepDataForAnalysis(inputFile = "C:/Users/myUserName/Desktop/myInputFile.xlsx",
#'                     inputSheet = "Sheet1",
#'                     outputFile = "C:/Users/myUserName/Desktop/myOutputFile.txt",
#'                     shpCreate = FALSE)
#' }


prepDataForAnalysis <- function(inputFile = NULL,
                                inputSheet = NULL,
                                outputFile = NULL,
                                shpCreate = TRUE,
                                shpCRS = 26913,
                                shpFile = NULL,
                                mapCreate = TRUE,
                                mapFile = NULL) {


  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # File paths ----
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # Allow the user to interactively select the input Excel file
  if (is.null(inputFile)) {
    inputFile <- file.choose()
  }


  # Have the user select which sheet of the selected input Excel file to use
  if (is.null(inputSheet)) {
    sheetNames <- readxl::excel_sheets(inputFile)
    sheetIndex <- menu(sheetNames,
                       title = "Which sheet in the Excel file contains the input data?")
    inputSheet <- sheetNames[sheetIndex]
  }


  # Set output file path to same as input file path if none provided
  # And default file name that prepends "PreppedData_" to the input filename
  if (is.null(outputFile)) {
    inputPath <- dirname(inputFile)
    inputFileBase <- sub('\\..*$', '', basename(inputFile))
    outputFile <- file.path(inputPath,
                            paste0("PreppedData_", inputFileBase, ".txt"))
  }


  # Set shapefile output file path to same as input file path if none provided
  # And default file name
  if (shpCreate) {
    if (is.null(shpFile)) {
      inputPath <- dirname(inputFile)
      inputFileBase <- sub('\\..*$', '', basename(inputFile))
      shpFile <- file.path(inputPath,
                           paste0("SurveyedTransects_", inputFileBase, ".shp"))
    }

  }


  # Set shapefile output file path to same as input file path if none provided
  # And default file name
  if (mapCreate) {
    if (is.null(mapFile)) {
      inputPath <- dirname(inputFile)
      inputFileBase <- sub('\\..*$', '', basename(inputFile))
      mapFile <- file.path(inputPath,
                           paste0("Map_", inputFileBase, ".html"))
    }

  }



  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Read in Excel file exported by CyberTracker ----
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # Read Excel file
  x <- readxl::read_excel(path = inputFile,
                          sheet = inputSheet)

  # Check that expected column names exist
  expectedNames <- c("Transect number",
                     "Flight control",
                     "Latitude",
                     "Longitude",
                     "Range")
  for (i in expectedNames) {
    if (!i %in% names(x)) {stop("This function requires the input file to have a column named '",
                               i,
                               "' but none exists.")}
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

  # Keep only needed columns
  x <- x[c("siteID", "event", "x", "y", "agl", "s", "band")]


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


  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Split data into sites and detections ----
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Easier to handle data in two sets.
  # Sites data.frame will have one row for each transect surveyed
  # (analogous to "Line transect" layer in Program DISTANCE)
  # Detections data.frame will have one for each group detected
  # (analogous to "Observation" layer in Program DISTANCE)

  # Sites data.frame
  sdf <- x[x$event %in% c("Start new transect", "End transect"),
           c("siteID", "event", "x", "y")]

  # sdf currently has two rows for each transect surveyed (a row for each start
  # and stop location), save that aside as tdf to build transect shp off of.
  # sdf will be collapsed into traditional form with one row for each transect
  # surveyed in the line length calculation below
  tdf <- sdf


  # Check that all sites have start/end
  unique.sites <- unique(as.character(sdf$siteID))
  fail.sites <- NULL

  for (i in 1:length(unique.sites)) {
    nrow.start <- nrow(sdf[sdf$siteID == unique.sites[i] & sdf$event == "Start new transect", ])
    nrow.end <- nrow(sdf[sdf$siteID == unique.sites[i] & sdf$event == "End transect", ])
    if (!(nrow.start == 1 & nrow.end == 1)) {
      fail.sites <- c(fail.sites, i)
    }
  }
  if (!is.null(fail.sites)) {
    stop(cat("These transect numbers do not have one start record and one end record: ", fail.sites))
  }

  # Remove extra objects
  rm(fail.sites, nrow.end, nrow.start)




  # Detections data.frame
  ddf <- x[x$event == "Record sighting", names(x[names(x) != "event"])]

  # Note that the desired behavior here is for ddf$siteID to contain empty levels
  # if there were transects where no pronghorn were detected
  length(unique(ddf$siteID))
  length(levels(ddf$siteID))  # will be greater if any transects had no pronghorn detected



  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Calculate length of each transect ----
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # Use euclidean distance function to calculate length of transect (in km)
  # Assumes projected coordinated system (e.g., not decimal degrees of lat/lon)
  sdf <- do.call(rbind, lapply(unique.sites, function(i) {
    p <- sdf[sdf$siteID == i, ]

    l <- as.numeric(dist(p[c("x", "y")], method = "euclidean"))/1e3

    r <- data.frame(siteID = i,
                    lengthKm = l)

    return(r)

  }))



  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Create shapefile of transects as surveyed ----
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # Spatial data - transects
  if (shpCreate | mapCreate) {

    # Create end points
    e <- sf::st_as_sf(x = tdf,
                      coords = c("x", "y"),
                      crs = shpCRS)

    # Convert end points to lines
    # I think the summarize step is needed, so added a dummy variable s that
    # is then dropped
    l <- e %>%
      dplyr::group_by(siteID) %>%
      dplyr::summarize(s = unique(siteID)) %>%
      sf::st_cast("LINESTRING") %>%
      dplyr::select(siteID, geometry)

    # Check
    # plot(sf::st_geometry(e))
    # plot(sf::st_geometry(l), add=TRUE)


    # (Over)write shapefile of surveyed transects
    if (shpCreate) {
      sf::st_write(l, shpFile, append = FALSE)
    }


  }

  # Spatial data - detections
  if (mapCreate) {
    # Create points for pronghorn detections (where the plane was when detection
    # was recorded)

    # Create points
    p <- sf::st_as_sf(x = ddf,
                      coords = c("x", "y"),
                      crs = shpCRS)

  }


  # Interactive map
  if (mapCreate) {
    m <- mapview::mapview(l, color = "darkgrey", legend = FALSE) +
      # mapview::mapview(e) +  # transect end points
      mapview::mapview(p, legend = FALSE)  # color by group size

    # Display map
    # m

    # Save map as interactive html
    mapview::mapshot(m, url = mapFile)

  }



  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Impute missing flight heights ----
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Mean AGL for all data (that have AGLs, before imputing)
  agl.all <- mean(ddf$agl, na.rm = TRUE)

  # Summary of AGL values (NA vs not) by transect
  agl.site <- ddf %>%
    dplyr::group_by(siteID, .drop = FALSE) %>%
    dplyr::summarize(n = length(agl),
                     nNA = sum(is.na(agl)),
                     nGood = n - nNA)



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
        good.sites <- agl.site[(agl.site$nGood > 0 | agl.site$siteID == unique.sites[i]), ]
        good.sites$id <- 1:nrow(good.sites)  # numeric way to index sites, clunky
        use.sites.id <- c(good.sites$id[good.sites$siteID == unique.sites[i]] - 1,
                          good.sites$id[good.sites$siteID == unique.sites[i]] + 1)
        use.sites <- as.character(good.sites$siteID[good.sites$id %in% use.sites.id])
        d$agl <- mean(ddf$agl[ddf$siteID %in% use.sites], na.rm = TRUE)  # mean at two transects

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
          na.rows <-  which(is.na(d$agl))
          good.rows <- which(!is.na(d$agl))

          for (j in na.rows) {
            pre.agl <- d$agl[j - 1]  # previous agl (in order, so any NA should already be resolved)
            post.agl <- d$agl[min(good.rows[good.rows > j])]  # next good agl

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
  agl.obs - agl.all


  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Calculate adjusted distances based on bin midpoints and flight height ----
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  ddf$adjustedDist <- NA

  # Insert nominal distance for band midpoint
  ddf$adjustedDist[grepl("Alpha", ddf$band)] <- 10
  ddf$adjustedDist[grepl("Bravo", ddf$band)] <- 32.5
  ddf$adjustedDist[grepl("Charlie", ddf$band)] <- 62.5
  ddf$adjustedDist[grepl("Delta", ddf$band)] <- 112.5
  ddf$adjustedDist[grepl("Echo", ddf$band)] <- 172.5

  # Adjust based on ratio of observed flight height to nominal flight height
  # for each detection
  ddf$adjustedDist <- ddf$adjustedDist * (ddf$agl / 300)


  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Calculate adjusted distance and cutpoints based on flight height ----
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # Overall ratio of observed flight height to nominal flight height
  agl.ratio <- agl.obs / 300

  # Nominal right cut points of each distance band
  cuts <- c(0, 20, 45, 80, 145, 200)

  # Adjust based on overall ratio of flight heights
  cuts <- cuts * agl.ratio


  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Do some QAQC (check for extreme values) ----
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # No checks implemented at this time


  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Format to have format expected by DISTANCE ----
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # Merge into what Program DISTANCE calls a "flat file"
  # A row for each detection, that includes the transect length
  # Plus a row for each transect where nothing was detected (with NAs for most cols)
  distData <- merge(sdf, ddf, all = TRUE)
  distData$id <- NULL


  # Make some column names easier to work with
  names(distData)[names(distData) == "siteID"] <- "lineLabel"
  names(distData)[names(distData) == "lengthKm"] <- "lineLengthKm"
  names(distData)[names(distData) == "x"] <- "coordsX"
  names(distData)[names(distData) == "y"] <- "coordsY"
  names(distData)[names(distData) == "agl"] <- "flightHeightFt"
  names(distData)[names(distData) == "s"] <- "clusterSize"
  names(distData)[names(distData) == "band"] <- "distBand"
  names(distData)[names(distData) == "adjustedDist"] <- "adjustedDistM"


  # Fix odd text formatting from CyberTracker which added "; Cluster size" into
  # the band column
  distData$distBand <- gsub("; Cluster size", "", distData$distBand)


  # Do some rounding to make text files easier to work with
  # (when aligning columns in DISTANCE)
  distData$lineLengthKm <- round(distData$lineLengthKm, 3)
  distData$coordsX <- round(distData$coordsX, 0)
  distData$coordsY <- round(distData$coordsY, 0)
  distData$flightHeightFt <- round(distData$flightHeightFt, 2)
  distData$adjustedDistM <- round(distData$adjustedDistM, 2)


  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Calculate study summaries to aid troubleshooting ----
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#


  # Number of transects
  n.trans <- length(unique(sdf$siteID))

  # Number of detections
  n.detects <- nrow(ddf)

  # Number of pronghorn detected
  n.detects.ind <- sum(ddf$s)

  # Number of detections after truncating at right-most cutpoint
  n.detects.trunc <- sum(ddf$adjustedDist <= max(cuts))

  # Number of pronghorn detections after truncating at right-most cutpoint
  n.detects.ind.trunc <- sum(ddf$s[ddf$adjustedDist <= max(cuts)])

  # # Mean group size (after truncation)
  # mean.group <- mean(ddf$s[ddf$adjustedDist <= max(cuts)])

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



  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Write out .txt file ready for DISTANCE import ----
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # Tab-delimited text file
  write.table(distData,
              file = outputFile,
              sep = "\t",
              na = "",  # Program DISTANCE needs blanks instead of NAs
              row.names = FALSE)

  # Print location of output file
  cat("The dataset formatted for analysis in Program DISTANCE was written to:\n",
      outputFile)



  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Print summary results ----
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  cat("\n#%%%%%%%%%%#\n")
  cat("DATA SUMMARY")
  cat("\n#%%%%%%%%%%#\n")

  cat("Total number of rows (excluding column names) to import to DISTANCE: ", n.rows, "\n")
  cat("Number of transects: ", n.trans, "\n")
  cat("Number of transects with no detections: ", n.trans.no, "\n")
  cat("Total transect length surveyed (km): ", total.km, "\n")
  cat("Number of groups (clusters) detected: ", n.detects, "\n")

  cat("Assuming right-truncation at largest adjusted distance interval cutpoint, number of groups (clusters) in analysis: ", n.detects.trunc, "\n")

  cat("Number of individuals detected: ", n.detects.ind, "\n")
  cat("Assuming right-truncation at largest adjusted distance interval cutpoint, number of individuals in analysis: ", n.detects.ind.trunc, "\n")
  # cat("Mean group size (number of individuals per group) after right-truncation: ", mean.group, "\n")


  cat("Number of groups (clusters) that had missing flight heights: ", n.NA, "\n")
  cat("Mean flight height (ft) at detections - before imputing missing flight heights: ", round(agl.all, 2), "\n")
  cat("Mean flight height (ft) at detections - after imputing missing flight heights: ", round(agl.obs, 2), "\n")

  cat("Nominal distance interval cutpoints (m): ", c(0, 20, 45, 80, 145, 200), "\n")
  cat("Adjusted distance interval cutpoints (m): ", round(cuts, 2), "\n")



  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Write out separate .txt file reporting study summaries and QAQC report ----
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Not implemented at this time

}
