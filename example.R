# modified by Maarten Hermans
# http://archived.mhermans.net/hiking-gpx-r-leaflet.html

library(dplyr)
# Adding tracks to the map
wpS <- list()
trackS <- list()
map <- leaflet::leaflet() %>% 
  leaflet::addTiles() %>% 
  leaflet::addLayersControl(position = 'bottomright',
                            overlayGroups = c('Tracks', 'Photo markers'),
                            options = leaflet::layersControlOptions(collapsed = FALSE))

GPX_file <- "http://raw.githubusercontent.com/oggioniale/EXIF-Data-photos/master/data/20190903_RifugioPisciadu.gpx"
trackName <- substring(GPX_file, 64)
download.file(GPX_file, "tracker.gpx")
rgdal::ogrListLayers("tracker.gpx")
wpS[[trackName]] <- rgdal::readOGR("tracker.gpx", layer = "track_points")
head(wpS[[trackName]][,c('ele', 'time')])
wpS[[trackName]]$climbed <- list(max(wpS[[trackName]]$ele) - min(wpS[[trackName]]$ele)) # height climbed in meters
hike.dists <- sp::spDists(wpS[[trackName]], segments=TRUE)
wpS[[trackName]]$distance <- sum(hike.dists) # distance in km
wpS[[trackName]]$time <- lubridate::ymd_hms(wpS[[trackName]]$time)
trackS[[trackName]] <- rgdal::readOGR("tracker.gpx", layer = "tracks", verbose = FALSE)
map <- map %>% leaflet::addPolylines(
  data = trackS[[trackName]],
  color = 'red', 
  group = 'Tracks')

map

# Adding photo-popups to the tracks
image_file <- 'http://github.com/oggioniale/EXIF-Data-photos/raw/master/data/_MG_9957-Pano.jpg'
download.file(image_file, "image.jpg")
allExif <- exifr::read_exif(
  "image.jpg",
  recursive = FALSE,
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

photoIcon <- leaflet::makeIcon(
  iconAnchorX = 12, iconAnchorY = 12, # center middle of icon on track,
  # instead of top corner  
  iconUrl = shiny::icon("camera")
)

map <- map %>% leaflet::addMarkers(map, lng = allExif$GPSLongitude, lat = allExif$GPSLatitude,  
                                   popup = 
                                     leafpop::popupImage(
                                       img = paste0(
                                         allExif$SourceFile,
                                         "<br/>",
                                         "Capture Date and Time: ", allExif$DateTimeOriginal, "<br/>",
                                         "Exposure: ", allExif$ExposureMode, "<br/>",
                                         "Focal Length: ", allExif$FocalLength, "<br/>",
                                         "ISO Speed Rating: ", allExif$ISO, "<br/>",
                                         # "Flash" = , "<br/>",
                                         "Model: ", allExif$Model, "<br/>",
                                         "Lens: ", allExif$Lens, "<br/>"
                                       )
                                     ),
                                   icon = photoIcon, # function providing custom marker-icons
                                   group = 'Photo markers')
map
