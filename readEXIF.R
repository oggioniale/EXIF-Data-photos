library(exifr)
library(dplyr)
library(leaflet)
library(tmap)

# LIST OF ALL EXIF TAGS
# exifinfoAll <- exifr::read_exif(
#   files)
# 
# names(exifinfoAll)

yearFolder <- "2019"
dirFolders <- dir(path = paste0("../../../../Desktop/photo/", yearFolder))
folderSel <- as.list(c("2019-07-23", "2019-07-27", "2019-07-28", "2019-08-01", "2019-08-03"))

fListFile <- function(folder) {
  list.files(
    path = paste0("../../../../Desktop/photo/", yearFolder, "/", folder),
    pattern = "*.xmp",
    recursive = TRUE,
    full.names = TRUE
  )
}

files <- lapply(
  folderSel,
  fListFile
)

fExifInfoSel <- function(files) {
  exifr::read_exif(
    files,
    tags = c(
      # more info https://github.com/adobe/xmp-docs/blob/master/XMPNamespaces/exif.md
      "SourceFile",
      "DateTimeOriginal",
      "ExposureTime", # second
      "Aperture", #
      "ISO",
      "FocalLength",
      "ExposureMode", # 0 = Auto exposure, 1 = Manual exposure, 2 = Auto bracket
      "ExposureProgram", # 0 = not defined, 1 = Manual, 2 = Normal program, 3 = Aperture priority, 4 = Shutter priority, 5 = Creative program, 6 = Action program, 7 = Portrait mode, 8 = Landscape mode
      "Lens",
      "Model",
      "Rights",
      "UsageTerms",
      "GPSLatitude",
      "GPSLongitude"#,
      # "Subject"
    )
  )
}

exifinfoSel <- lapply(
  files,
  fExifInfoSel
)

exifinfoSel <- do.call("rbind", exifinfoSel)
exifinfoSel <- exifinfoSel[!is.na(exifinfoSel$GPSLatitude),]
exifinfoSel <- tibble::add_column(
  exifinfoSel,
  .before = "SourceFile",
  photo = stringr::str_replace(sub(".*/", "", exifinfoSel$SourceFile), ".xmp", ""),
  PhotoFile = stringr::str_replace(exifinfoSel$SourceFile, ".xmp", ".CR2"),
  Exposure = paste0("1/", round(1/exifinfoSel$ExposureTime, 2), " sec at f/", exifinfoSel$Aperture)
)


exifinfoSelSP <- sf::st_as_sf(
  x = exifinfoSel, 
  coords = c("GPSLongitude", "GPSLatitude"), 
  crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
)

tmap::tmap_mode(mode = "view")

tm_basemap(leaflet::providers$OpenStreetMap.Mapnik) +
tm_shape(exifinfoSelSP) +
  tm_dots(
    col = NA,
    size = 0.01,
    shape = 16,
    title = NA,
    legend.show = FALSE,
    group = "Geoinfo of Photo",
    popup.vars = c(
      "Capture Date and Time" = "DateTimeOriginal",
      "Exposure" = "Exposure",
      "Focal Length" = "FocalLength",
      "ISO Speed Rating" = "ISO",
      # "Flash" = ,
      "Model" = "Model",
      "Lens" = "Lens"
     ), 
    popup.format = list()
  )


