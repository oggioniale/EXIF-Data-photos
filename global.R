library(dplyr)

###
# Photos
###
yearFolder <- "2019"
dirFolders <- dir(path = paste0("../../../../Desktop/photo/multimedia/", yearFolder))
folderSel <- as.list(c("2019-01-13"))

fListFile <- function(folder) {
  list.files(
    path = paste0("../../../../Desktop/photo/multimedia/", yearFolder, "/", folder),
    pattern = "*.jpg",
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
      "GPSLongitude",
      "Subject"
    )
  )
}

exifinfoSel <- lapply(
  files,
  fExifInfoSel
)

exifinfoSel <- dplyr::bind_rows(exifinfoSel)
# exifinfoSel <- exifinfoSel[!is.na(exifinfoSel$GPSLatitude),]
# exifinfoSel <- exifinfoSel[!is.na(exifinfoSel$DateTimeOriginal),]
exifinfoSel <- tibble::add_column(
  exifinfoSel,
  .before = "SourceFile",
  photo = stringr::str_replace(sub(".*/", "", exifinfoSel$SourceFile), ".jpg", ""),
  Exposure = paste0("1/", round(1/exifinfoSel$ExposureTime, 2), " sec at f/", exifinfoSel$Aperture)
)
exifinfoSel$DateTimeOriginal <- lubridate::ymd_hms(exifinfoSel$DateTimeOriginal)
exifinfoSel$Years <- as.numeric(format(exifinfoSel$DateTimeOriginal,"%Y"))
exifinfoSel <- as.data.frame(exifinfoSel)

listYears <- c("All years", as.list(unique(format(exifinfoSel$DateTimeOriginal,"%Y"))))
listCameraModels <- c("All cameras model", as.list(unique(exifinfoSel$Model)))
listLens <- c("All lens", as.list(unique(exifinfoSel$Lens)))
listOfSubjects <- c("All subjects", as.list(unique(unlist(exifinfoSel$Subject))[!grepl(' - ', unique(unlist(exifinfoSel$Subject)))]))
# listAlbum <- c("All albums", as.list(unique(unlist(exifinfoSel$Subject))[grepl(' - ', unique(unlist(exifinfoSel$Subject)))]))

# allExif <- sf::st_as_sf(
#   x = exifinfoSel,
#   coords = c("GPSLongitude", "GPSLatitude"),
#   crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
# )

###
# Tracks 
###
# yearFolder <- c("2016", "2017", "2018", "2019", "2020")
# dirFolders <- dir(path = paste0("../../../../Documents/Alessandro/Traks/tracce/"))
listGPX <- list.files(
  path = paste0("../../../../Documents/Alessandro/Traks/tracce"),
  pattern = "*.gpx",
  recursive = TRUE,
  full.names = TRUE
)
wpS <- list()
trackS <- list()
for (i in 1:5){#length(listGPX)) {
  GPX_file <- listGPX[i]
  trackName <- substring(GPX_file, 61)
  wpS[[trackName]] <- rgdal::readOGR(GPX_file, layer = "track_points")
  head(wpS[[trackName]][,c('ele', 'time')])
  wpS[[trackName]]$climbed <- list(max(wpS[[trackName]]$ele) - min(wpS[[trackName]]$ele)) # height climbed in meters
  hike.dists <- sp::spDists(wpS[[trackName]], segments=TRUE)
  wpS[[trackName]]$distance <- sum(hike.dists) # distance in km
  wpS[[trackName]]$time <- lubridate::ymd_hms(wpS[[trackName]]$time)
  trackS[[trackName]] <- rgdal::readOGR(GPX_file, layer = "tracks", verbose = FALSE)
}
