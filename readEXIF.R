library(exifr)
library(dplyr)
library(leaflet)
library(tmap)

setwd("~/Users/alessandrooggioni/Desktop/photo/2019/2019-08-01")
files <- list.files(
  pattern = "*.xmp",
  recursive = TRUE,
  full.names = TRUE
)

exifinfoSel <- exifr::read_exif(
  files,
  tags = c(
    "SourceFile",
    "DateTimeOriginal",
    "FNumber",
    "MeasuredEV",
    # "ExposureProgram",
    "ISO",
    # "ShutterSpeedValue",
    # "Flash",
    "FocalLength",
    "CameraType",
    "OwnerName",
    "GPSLongitude",
    "GPSLatitude"
  )
)

leaflet(exifinfoSel) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addMarkers(~ GPSLongitude, ~ GPSLatitude)  
