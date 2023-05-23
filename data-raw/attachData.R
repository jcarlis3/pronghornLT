# Save herd unit and hunt area shapefiles into the package as exported data

library(sf)
library(dplyr)
library(units)
library(usethis)


# Download latest herd unit and hunt area shapefiles from WGFD website
# https://wgfd.wyo.gov/Wildlife-in-Wyoming/Geospatial-Data/Big-Game-GIS-Data

# Remember to update the "current as of" date in herdUnits.R and huntAreas.R
# each time new shapefiles are downloaded and placed into the package.

shp_path <- ("C:/Users/jadcarlisle/My Drive/Pronghorn/GIS")


# HERD UNITS
herdUnits <- st_read(file.path(shp_path, "AntelopeHerdUnits.shp"))
dim(herdUnits)  # 41 5

# There's a tiny sliver of a 2nd feature for North Black Hills
# Remove it (filter out herds smaller than 10 square meters)
herdUnits <- herdUnits %>%
  filter(st_area(herdUnits) > units::as_units(10, "m^2"))
dim(herdUnits)  # 40 5


# HUNT AREAS
huntAreas <- st_read(file.path(shp_path, "AntelopeHuntAreas.shp"))
dim(huntAreas)  # 102 8


# Remove some columns to minimize size of package data
herdUnits <- herdUnits %>%
  select(HERDUNIT, HERDNAME, geometry)

huntAreas <- huntAreas %>%
  select(HUNTAREA, HUNTNAME, HERDUNIT, HERDNAME, geometry)


# Save into package
# Creates .rda file for each in /data directory
usethis::use_data(huntAreas, herdUnits,
                  internal = FALSE,
                  overwrite = TRUE,
                  compress = "bzip2")

