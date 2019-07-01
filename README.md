# pronghornLT
*Utilities to Prepare Pronghorn Line-transect Survey Data for Analysis*

This simple package currently contains just one function (`prepDataForAnalysis`).  This function reads in raw data exported from CyberTracker software and prepares it for input to Program DISTANCE for a distance-sampling analysis. The prepped data is written to file, and some summaries of the data are printed to the R console.


## How to install
To install and load `pronghornLT` in R, run the following code.  Note, the `remotes` package is needed to install `pronghornLT` directly from GitHub, and the first line will install the `remotes` package if you don't already have it.

```
# Check whether the remotes package is installed, if not, install it from CRAN
if("remotes" %in% rownames(installed.packages()) == FALSE) {install.packages("remotes")}

# Install pronghornLT from GitHub
remotes::install_github("jcarlis3/pronghornLT@master")

# Load pronghornLT for use
require(pronghornLT)
```

## How to use
Run `?prepDataForAnalysis` in the R console to see the help file and directions for using the package's only function.
