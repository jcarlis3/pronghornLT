# pronghornLT
*Utilities for Pronghorn Line-transect Surveys in Wyoming*

## How to install
To install and load `pronghornLT` in R, run the following code.  Note, the 
`remotes` package is needed to install `pronghornLT` directly from GitHub, and 
the first line will install the `remotes` package if you don't already have it.

```
# Check whether the remotes package is installed, if not, install it from CRAN
if("remotes" %in% rownames(installed.packages()) == FALSE) {install.packages("remotes")}

# Install pronghornLT from GitHub
remotes::install_github("jcarlis3/pronghornLT@master")

# Load pronghornLT for use
require(pronghornLT)
```

## How to use
See the documentation and examples for the following functions:

### Survey design - Create transect lines within a herd unit
- `calcLineLength`
- `makeLines`

### Data prep and QAQC - Prep raw survey data for analysis and print QAQC summaries
- `prepDataForAnalysis`

### Analysis - Fit distance-sampling model to estimate abundance
- `fitDistSampModel`
