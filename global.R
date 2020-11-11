library(dplyr)
library(fontawesome) #devtools::install_github("rstudio/fontawesome")

# data preparation
# saveRDS(dt_Metropolis,"dt_Metropolis.rds")
# dt_Metropolis<-readRDS("cityGeojson/dt_Metropolis.rds")

###
# Photos
###
# exifinfoSel <- exifr::read_exif(
#   list.files(
#     path = paste0("../../../../Desktop/photo/multimedia"),
#     pattern = "*.jpg",
#     recursive = TRUE,
#     full.names = TRUE
#   ),
#   tags = c(
#     # more info https://github.com/adobe/xmp-docs/blob/master/XMPNamespaces/exif.md
#     "SourceFile",
#     "DateTimeOriginal",
#     "ExposureTime", # second
#     "Aperture", #
#     "ISO",
#     "FocalLength",
#     "ExposureMode", # 0 = Auto exposure, 1 = Manual exposure, 2 = Auto bracket
#     "ExposureProgram", # 0 = not defined, 1 = Manual, 2 = Normal program, 3 = Aperture priority, 4 = Shutter priority, 5 = Creative program, 6 = Action program, 7 = Portrait mode, 8 = Landscape mode
#     "Lens",
#     "Model",
#     "Rights",
#     "UsageTerms",
#     "GPSLatitude",
#     "GPSLongitude",
#     "Subject"
#   )
# )
# 
# exifinfoSel <- dplyr::bind_rows(exifinfoSel)
# exifinfoSel <- tibble::add_column(
#   exifinfoSel,
#   .before = "SourceFile",
#   photo = stringr::str_replace(sub(".*/", "", exifinfoSel$SourceFile), ".jpg", ""),
#   Exposure = paste0("1/", round(1/exifinfoSel$ExposureTime, 2), " sec at f/", exifinfoSel$Aperture)
# )
# exifinfoSel$DateTimeOriginal <- lubridate::ymd_hms(exifinfoSel$DateTimeOriginal)
# exifinfoSel$Years <- as.numeric(format(exifinfoSel$DateTimeOriginal,"%Y"))
# exifinfoSel <- as.data.frame(exifinfoSel)
# exifinfoSel$Album <- lapply(exifinfoSel$Subject, '[', 1)
# exifinfoSel$Subject <- lapply(exifinfoSel$Subject, '[', -1)
# 
# saveRDS(exifinfoSel, "data/dt_exifinfoSel.rds")
exifinfoSel <- readRDS("data/dt_exifinfoSel.rds")

listYears <- c("All years", as.list(unique(format(exifinfoSel$DateTimeOriginal,"%Y"))))
listCameraModels <- c("All cameras model", as.list(unique(exifinfoSel$Model)))
listLens <- c("All lens", as.list(unique(exifinfoSel$Lens)))

###
# Tracks 
###
listGPX <- list.files(
  path = c(
    "../../../../Documents/Alessandro/Tracks/2020",
    "../../../../Documents/Alessandro/Tracks/2019",
    "../../../../Documents/Alessandro/Tracks/2018",
    "../../../../Documents/Alessandro/Tracks/2017",
    "../../../../Documents/Alessandro/Tracks/2016",
    # "../../../../Documents/Alessandro/Tracks/2015",
    "../../../../Documents/Alessandro/Tracks/2014",
    "../../../../Documents/Alessandro/Tracks/2013",
    "../../../../Documents/Alessandro/Tracks/2012",
    "../../../../Documents/Alessandro/Tracks/2011",
    "../../../../Documents/Alessandro/Tracks/2009"
  ),
  pattern = "*.gpx",
  recursive = TRUE,
  full.names = TRUE
)
# wpS <- list()
# trackS <- list()
# waypointS <- list()
# for (i in 1:length(listGPX)) {
#   GPX_file <- listGPX[i]
#   trackName <- sub('.*_', '', GPX_file)
#   wpS[[trackName]] <- rgdal::readOGR(GPX_file, layer = "track_points")
#   # head(wpS[[trackName]][,c('ele', 'time')])
#   if (!is.na(wpS[[trackName]]$time[1])) {
#     wpS[[trackName]]$climbed <- list(max(wpS[[trackName]]$ele) - min(wpS[[trackName]]$ele)) # height climbed in meters
#     for (j in 1:length(wpS[[trackName]]$ele)) {
#       wpS[[trackName]]$gainElev[j] <- wpS[[trackName]]$ele[j+1] - wpS[[trackName]]$ele[j]
#     }
#     a <- wpS[[trackName]]$gainElev
#     wpS[[trackName]]$negGain <- sum(a[a<0], na.rm = TRUE)
#     wpS[[trackName]]$posGain <- sum(a[a>0], na.rm = TRUE)
#     hike.dists <- sp::spDists(wpS[[trackName]], segments=TRUE)
#     wpS[[trackName]]$distance <- sum(hike.dists) # distance in km
#     wpS[[trackName]]$time <- lubridate::ymd_hms(wpS[[trackName]]$time)
#     trackS[[trackName]] <- rgdal::readOGR(GPX_file, layer = "tracks", verbose = FALSE)
#   } else {
#     wpS[[trackName]]$climbed <- list(max(wpS[[trackName]]$ele) - min(wpS[[trackName]]$ele)) # height climbed in meters
#     for (j in 1:length(wpS[[trackName]]$ele)) {
#       wpS[[trackName]]$gainElev[j] <- wpS[[trackName]]$ele[j+1] - wpS[[trackName]]$ele[j]
#     }
#     a <- wpS[[trackName]]$gainElev
#     wpS[[trackName]]$negGain <- sum(a[a<0], na.rm = TRUE)
#     wpS[[trackName]]$posGain <- sum(a[a>0], na.rm = TRUE)
#     hike.dists <- sp::spDists(wpS[[trackName]], segments=TRUE)
#     wpS[[trackName]]$distance <- sum(hike.dists) # distance in km
#     trackS[[trackName]] <- rgdal::readOGR(GPX_file, layer = "tracks", verbose = FALSE)
#   }
# }
# 
# for (i in 1:length(listGPX)) {
#   GPX_file <- listGPX[i]
#   trackName <- sub('.*_', '', GPX_file)
#   trS <- plotKML::readGPX(GPX_file, metadata = TRUE, waypoints = TRUE, bounds = TRUE, tracks = TRUE, routes = TRUE)
#   if (length(trS$waypoints) == 0) {
#     waypointS[[trackName]] <- NULL
#   } else {
#     waypointS[[trackName]] <- rgdal::readOGR(GPX_file, layer = "waypoints", verbose = FALSE)
#   }
# }
# 
# saveRDS(wpS, "data/dt_wpS.rds")
# saveRDS(trackS, "data/dt_trackS.rds")
# saveRDS(waypointS, "data/dt_waypointS.rds")

wpS <- readRDS("data/dt_wpS.rds")
trackS <- readRDS("data/dt_trackS.rds")
waypointS <- readRDS("data/dt_waypointS.rds")

###
# Type of Activities
###
# activityType <- tibble::tribble(
#                     ~activityType,
#                 "Mountain biking",
#                          "Hiking",
#                  "Mountaineering",
#                         "Running",
#                         "Walking",
#                       "Motorboat",
#                            "Car"#,
#   #                 "Trail running",
#   #               "Bicycle touring",
#   #                       "Cycling",
#   #                  "Motorcycling",
#   #           "Back country skiing",
#   #                    "Trail bike",
#   #                           "ATV",
#   #           "Kayaking - Canoeing",
#   #                       "Sailing",
#   #                   "Snowshoeing",
#   #          "Cross country skiing",
#   #                 "Alpine skiing",
#   #                        "Flying",
#   #              "Horseback riding",
#   #                  "Dog sledging",
#   #                 "Rock climbing",
#   #                "Inline skating",
#   #                       "Skating",
#   #                         "Train",
#   #                  "Canyoneering",
#   #                        "Diving",
#   #                        "Caving",
#   #                  "Hang gliding",
#   #                    "Ballooning",
#   #                  "Snowboarding",
#   #                  "Ice climbing",
#   #                  "Snowmobiling",
#   #                    "Accessible",
#   #                    "Offroading",
#   #                        "Rowing",
#   #                  "Kiteboarding",
#   #                   "Kite skiing",
#   #                        "Sledge",
#   #                      "Kickbike",
#   #                   "Paragliding",
#   #                     "For blind",
#   #                "Nordic walking",
#   #             "Motorcycle trials",
#   #                        "Enduro",
#   #                   "Via ferrata",
#   #                      "Swimming",
#   #                  "Orienteering",
#   #                    "Multisport",
#   #         "Stand up paddle (SUP)",
#   #                      "Barefoot",
#   #                     "Canicross",
#   #                 "Roller skiing",
#   #                  "Longboarding",
#   #           "Mountain unicycling",
#   #                          "Golf",
#   #          "Recreational vehicle",
#   #                       "Airboat",
#   #                        "Segway",
#   #                         "Camel",
#   #                      "Freeride",
#   # "Unmanned aerial vehicle (UAV)",
#   #        "Birdwatching - Birding",
#   #                  "Trailer bike",
#   #           "Water scooter (PWC)",
#   #                      "Handbike",
#   #                       "Rafting",
#   # "Downhill mountain biking (DH)",
#   #                         "eBike",
#   #                  "BASE jumping",
#   #                      "Joëlette",
#   #            "With baby carriage",
#   #                    "Splitboard",
#   #                   "Gravel Bike"
#   )
# saveRDS(activityType, "data/dt_activityType.rds")
activityType <- readRDS("data/dt_activityType.rds")
activityType$color <- RColorBrewer::brewer.pal(nrow(activityType), "Dark2")
