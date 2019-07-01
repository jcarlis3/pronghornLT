#' prepDataForAnalysis
#'
#' @description This function reads in raw data exported from CyberTracker
#' software and prepares it for input to Program DISTANCE for a
#' distance-sampling analysis.
#' The prepped data is written to file, and some summaries of the data are
#' printed to the R console.
#' @param inputFile  The path and name of the Excel file containing the input
#' data.  The input data is the data output from CyberTracker.  Excel files with
#' extension \code{.xls} or \code{.xlsx} are allowed.  Note the direction of
#' slashes in the path needs to follow R expectations (use \code{/} instead of
#' the Windows default of \code{\\}), that the file name and extension are included,
#' and that the path needs to start and end with quotes.
#' Example:  \code{"C:/Users/jcarlisle/Desktop/myInputFile.xlsx"}.
#' This function expects the following columns (named as shown) in the
#' \code{inputFile}.  The order of the columns does not matter, and
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
#' that should be read in.  Specify as the name of the sheet (e.g., \code{"Sheet1"}
#' or \code{"inputData"}) or as the numbered position of the sheet within the file
#' (e.g., \code{1}).
#' @param outputFile The path and name of the tab-delimited text file to write
#' out containing the output data ready for import into Program DISTANCE.
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
#'
#'
#' }
#' @author Jason D. Carlisle, \email{jcarlisle@@west-inc.com}
#' @references Guenzel, R.J. 2007. Procedures for Estimating Pronghorn Abundance
#' in Wyoming Using Aerial Line Transect Sampling. Wyoming Game and Fish Department.
#' Cheyenne, WY, USA.
#' @importFrom readxl read_excel
#' @importFrom plyr ddply summarize
#' @importFrom stats dist na.omit
#' @importFrom utils write.table
#' @export
#' @examples
#' \donttest{
#' # Read in Sheet1 of myInputFile.xlsx, and write out myOutputFile.txt
#' prepDataForAnalysis(inputFile = "C:/Users/myUserName/Desktop/myInputFile.xlsx",
#'                     inputSheet = "Sheet1",
#'                     outputFile = "C:/Users/myUserName/Desktop/myOutputFile.txt")
#' }




prepDataForAnalysis <- function(inputFile, inputSheet, outputFile) {


  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Read in Excel file exported by CyberTracker
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  x <- readxl::read_excel(path=inputFile, sheet=inputSheet)

  # Make some column names easier to work with
  names(x)[names(x) == "Transect number"] <- "siteID"
  names(x)[names(x) == "Flight control"] <- "event"
  names(x)[names(x) == "Latitude"] <- "y"
  names(x)[names(x) == "Longitude"] <- "x"
  names(x)[names(x) == "Range"] <- "agl"

  # Multiple columns were named "Cluster size" in input sheet
  # read_excel modifies the names with column index, but make more flexible
  # such that first one is cluster size, second is distance band.
  clusterNames <- names(x)[grepl("Cluster size", names(x))]
  names(x)[names(x) == clusterNames[1]] <- "s"
  names(x)[names(x) == clusterNames[2]] <- "band"

  # Keep only needed columns
  x <- x[c("siteID", "event", "x", "y", "agl", "s", "band")]


  # Drop the "End survey" event
  x <- x[x$event != "End survey", ]


  # Make siteID a factor (might come in as numeric or character)
  x$siteID <- as.factor(x$siteID)

  # Populate rest of siteID column
  # Go row by row, copy down value in the previous row when an NA is encountered
  # The first value cannot be NA
  if(!is.na(x$siteID[1])) {
    for(i in 2:nrow(x)) {
      if(is.na(x$siteID[i])){
        x$siteID[i] <- x$siteID[i-1]
      } else {
        next
      }
    }
  } else {
    stop("The first row of the 'Transect number' column cannot be blank.")
  }


  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Split data into sites and detections
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Easier to handle data in two sets.
  # Sites data.frame will have one row for each transect surveyed
  # (analogous to "Line transect" layer in Program DISTANCE)
  # Detections data.frame will have one for each group detected
  # (analogous to "Observation" layer in Program DISTANCE)

  # Sites data.frame
  sdf <- x[x$event %in% c("Start new transect", "End transect"),
           c("siteID", "event", "x", "y")]


  # Check that all sites have start/end
  unique.sites <- unique(as.character(sdf$siteID))
  fail.sites <- NULL

  for(i in 1:length(unique.sites)) {
    nrow.start <- nrow(sdf[sdf$siteID == unique.sites[i] & sdf$event == "Start new transect", ])
    nrow.end <- nrow(sdf[sdf$siteID == unique.sites[i] & sdf$event == "End transect", ])
    if(!(nrow.start == 1 & nrow.end == 1)) {
      fail.sites <- c(fail.sites, i)
    }
  }
  if(!is.null(fail.sites)) {
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
  # Calculate length of each transect
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # Use euclidean distance function to calculate length of transect (in km)
  # Assumes projected coordinated system (e.g., not decimal degrees of lat/lon)
  sdf <- do.call(rbind, lapply(unique.sites, function (i) {
    p <- sdf[sdf$siteID == i, ]

    l <- as.numeric(dist(p[c("x", "y")], method="euclidean"))/1e3

    r <- data.frame(siteID = i,
                    lengthKm = l)

    return(r)

  }))




  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Impute missing flight heights
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Mean AGL for all data (that have AGLs, before imputing)
  agl.all <- mean(ddf$agl, na.rm=TRUE)

  # Summary of AGL values (NA vs not) by transect
  agl.site <- plyr::ddply(ddf, "siteID", plyr::summarize,
                          n = length(agl),
                          nNA = sum(is.na(agl)),
                          # meanAgl = mean(agl, na.rm=TRUE),
                          nGood = n - nNA,
                          .drop=FALSE)


  # Step through algorithm to impute missing flight heights:
  # Use the average height of the one previous and one subsequent detections.
  # Special cases:
  # If missing agl is the 1st detection of the transect, we use the height of 2nd detection.
  # If missing agl is the last detection, use the height of the 2nd-to-last detection.
  # If all detections on a transect are missing agl (hopefully rare), use the average agl
  # from the previous and subsequent transects.


  # First, add detection ID within each transect
  ddf <- do.call(rbind, lapply(unique.sites, function (i) {
    if(i %in% ddf$siteID) {
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
    if(agl.site$n[agl.site$siteID == unique.sites[i]] == 0) {
      return(NULL)
    }


    # Detections for the transect of interest
    d <- ddf[ddf$siteID == unique.sites[i], ]


    # If all agls are valid (no NAs)
    # No need to impute anyting
    if(sum(is.na(d$agl)) == 0) {
      return(d)
    } else {
      # If all agls are NA
      if(sum(!is.na(d$agl)) == 0) {

        # this site and those with usable agl values
        good.sites <- agl.site[(agl.site$nGood > 0 | agl.site$siteID == unique.sites[i]), ]
        good.sites$id <- 1:nrow(good.sites)  # numeric way to index sites, clunky
        use.sites.id <- c(good.sites$id[good.sites$siteID == unique.sites[i]]-1,
                          good.sites$id[good.sites$siteID == unique.sites[i]]+1)
        use.sites <- as.character(good.sites$siteID[good.sites$id %in% use.sites.id])
        d$agl <- mean(ddf$agl[ddf$siteID %in% use.sites], na.rm=TRUE)  # mean at two transects

        return(d)


      } else {

        # Cases with some NAs (but not all) in agl
        good.agls <- na.omit(d$agl)

        # NA in first detection
        if(is.na(d$agl[1])) {
          d$agl[1] <- good.agls[1]
        }

        # NA in last detection
        if(is.na(d$agl[nrow(d)])) {
          d$agl[nrow(d)] <- good.agls[length(good.agls)]
        }

        if(nrow(d) >= 3) {

          # Helper indices (row numbers) of rows with NAs and without NAs in agl
          na.rows <-  which(is.na(d$agl))
          good.rows <- which(!is.na(d$agl))

          for(j in na.rows){
            pre.agl <- d$agl[j-1]  # previous agl (in order, so any NA should already be resolved)
            post.agl <- d$agl[min(good.rows[good.rows > j])]  # next good agl

            d$agl[j] <- mean(c(pre.agl, post.agl))

          }
        }
        return(d)
      }
    }
  }))


  # Issue a warning if, for some reason, there are still NAs in the agl column
  if(anyNA(ddf$agl)) {
    warning("The flight height contains NA values even after attempting to replace the NAs.")
  }


  # Mean AGL for all data (after imputing)
  agl.obs <- mean(ddf$agl, na.rm=TRUE)

  # Difference in mean AGL after imputing compared to before imputing
  agl.obs - agl.all


  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Calculate adjusted distances based on bin midpoints and flight height
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
  # Calculate adjusted distance and cutpoints based on flight height
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # Overall ratio of observed flight height to nominal flight height
  agl.ratio <- agl.obs / 300

  # Nominal right cut points of each distance band
  cuts <- c(0, 20, 45, 80, 145, 200)

  # Adjust based on overall ratio of flight heights
  cuts <- cuts * agl.ratio


  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Do some QAQC (check for extreme values)
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # No checks implemented at this time



  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Calculate study summaries to aid troubleshooting
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

  # Total length of surveyed transects
  total.km <- sum(sdf$lengthKm)

  # Number of missing AGLs
  n.NA <- sum(agl.site$nNA)

  # Mean AGL before imputing
  agl.all

  # Mean AGL after imputing
  agl.obs


  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Format to have format expected by DISTANCE
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # Merge into what Program DISTANCE calls a "flat file"
  # A row for each detection, that includes the transect length
  # Plus a row for each transect where nothing was detected (with NAs for most cols)
  distData <- merge(sdf, ddf, all=TRUE)
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



  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Write out .txt file ready for DISTANCE import
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#

  # Tab-delimited text file
  write.table(distData,
              file=outputFile,
              sep="\t", row.names=FALSE)

  # Print location of output file
  cat("Data formatted for analysis in Program DISTANCE was written to:\n", outputFile)



  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Print summary results
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  cat("\n#%%%%%%%%%%%%%%%%%%%%%%%%%%%#\n")
  cat("DATA SUMMARY")
  cat("\n#%%%%%%%%%%%%%%%%%%%%%%%%%%%#\n")

  cat("Number of transects: ", n.trans, "\n")
  cat("Total transect length surveyed (km): ", total.km, "\n")
  cat("Number of groups (clusters) detected: ", n.detects, "\n")
  cat("Assuming right-truncation at largest adjusted distance interval cutpoint, number of groups (clusters) in analysis: ", n.detects.trunc, "\n")

  cat("Number of individuals detected: ", n.detects.ind, "\n")
  cat("Assuming right-truncation at largest adjusted distance interval cutpoint, number of individuals in analysis: ", n.detects.ind.trunc, "\n")

  cat("Number of groups (clusters) that had missing flight heights: ", n.NA, "\n")
  cat("Mean flight height at detections - before imputing missing flight heights (ft): ", round(agl.all, 2), "\n")
  cat("Mean flight height at detections - after imputing missing flight heights (ft): ", round(agl.obs, 2), "\n")

  cat("Nominal distance interval cutpoints (m): ", c(0, 20, 45, 80, 145, 200), "\n")
  cat("Adjusted distance interval cutpoints (m): ", round(cuts, 2), "\n")



  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Write out separate .txt file reporting study summaries and QAQC report
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
  # Not implemented at this time

}
