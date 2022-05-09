# pronghornLT
*Utilities to Prepare Pronghorn Line-transect Survey Data for Analysis*

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

### Prep raw survey data for analysis
See the documentation for the following functions:

- `prepDataForAnalysis`

### Create transect lines within a herd unit
See the documentation for the following functions:

- `calcLineLength`
- `makeLines`
