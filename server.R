function(input, output, session) {

  ## Interactive Map ##########################################
  
  # Create the map
  observeEvent(input$year, {
    if (input$year == 'All years') {
      shinyjs::disable('album')
      shinyjs::disable('subject')
      updateSelectizeInput(
        session,
        'album',
        choices = c("All albums", as.list(unique(unlist(exifinfoSel$Album))))[[1]],
        server = TRUE
      )
      updateSelectizeInput(
        session,
        'subject',
        choices = c("All subjects", as.list(unique(unlist(exifinfoSel$Album))))[[1]],
        server = TRUE
      )
    } else if (input$year != 'All years') {
      shinyjs::enable('album')
      shinyjs::disable('subject')
      updateSelectizeInput(
        session,
        'album',
        choices = append('All albums', as.list(unique(unlist((exifinfoSel %>% filter(Years == input$year))$Album)))),
        server = TRUE
      )
      observeEvent(input$album, {
        if (input$album != 'All albums') {
          shinyjs::enable('subject')
          updateSelectizeInput(
            session,
            'subject',
            choices = append('All subjects', as.list(unique(unlist((exifinfoSel %>% filter(Years == input$year) %>% filter(Album == input$album))$Subject)))),
            # as.list(unique(unlist((exifinfoSel %>% filter(Years == '2020') %>% filter(Album == '2020 - Rifugio Alpinisti Monzesi'))$Subject)))
            server = TRUE
          )
        }
      })
    }
  })
  
  selectedPhotos <- reactive(
    if (input$year == 'All years') {
      exifinfoSel
    } else if (input$year != 'All years' & input$album == 'All albums') {
      exifinfoSel %>% filter(Years == input$year)
    } else if (input$year != 'All years' & input$album != 'All albums' & input$subject == 'All subjects') {
      exifinfoSel %>% filter(Years == input$year) %>% filter(Album == input$album)
    } else if (input$year != 'All years' & input$album != 'All albums' & input$subject != 'All subjects') {
      (exifinfoSel %>%
         filter(Years %in% input$year,
                Album %in% input$album))[grepl(input$subject, 
                                                            (exifinfoSel %>%
                                                               filter(Years %in% input$year,
                                                                      Album %in% input$album))$Subject),]
    }
    # (exifinfoSel %>%
    #     filter(Years %in% '2020',
    #            Album %in% '2020 - Salse di Nirano'))[grepl('Caterina Bergami', 
    #                                                        (exifinfoSel %>%
    #                                                           filter(Years %in% '2020',
    #                                                                  Album %in% '2020 - Salse di Nirano'))$Subject),]
  )
  
  output$map <- leaflet::renderLeaflet({
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Please wait", value = 0)
    
    map <- leaflet::leaflet() %>% 
      leaflet::addTiles() %>% 
      leaflet::addLayersControl(position = 'bottomright',
                                overlayGroups = c("Tracks", 'Photo markers', 'Waypoints'),
                                options = leaflet::layersControlOptions(collapsed = FALSE)
      ) %>%
      leaflet::addEasyButton(
        leaflet::easyButton(
          icon = "fa-crosshairs", title = "Locate Me",
          onClick = leaflet::JS("function(btn, map){ map.locate({setView: true}); }")
        )
      ) %>%
      leaflet::addEasyButton(
        leaflet::easyButton(
          icon = 'fa-globe', title = 'Zoom to max extension',
          onClick = leaflet::JS("function(btn, map){ map.setZoom(2);}")
        )
      ) %>% 
      leaflet::addLegend(
        "bottomleft", 
        colors = activityType$color,
        labels = activityType$activityType,
        title= "Activity types",
        opacity = 1
      )
    
    # fitBound of phtos selected
    observeEvent(input$year, {
      maxLong = max(selectedPhotos()$GPSLongitude)
      maxLat = max(selectedPhotos()$GPSLatitude)
      minLong = min(selectedPhotos()$GPSLongitude)
      minLat = min(selectedPhotos()$GPSLatitude)
      mapProxy <- leaflet::leafletProxy("map")
      mapProxy %>%
        leaflet::fitBounds(minLong, minLat, maxLong, maxLat)
    })
    
    # add tracks to the map
    tracksIcon <- leaflet::awesomeIconList(
      "Mountain biking" = leaflet::makeAwesomeIcon(
        text = fa("biking"), markerColor = 'green', iconColor = 'white', library = "fa"
      ),
      Hiking = leaflet::makeAwesomeIcon(
        text = fa("hiking"), markerColor = 'green', iconColor = 'white', library = "fa"
      ),
      Mountaineering = leaflet::makeAwesomeIcon(
        text = fa("mountain"), markerColor = 'green', iconColor = 'white', library = "fa"
      ),
      Running = leaflet::makeAwesomeIcon(
        text = fa("running"), markerColor = 'green', iconColor = 'white', library = "fa"
      ),
      Walking = leaflet::makeAwesomeIcon(
        text = fa("shoe-prints"), markerColor = 'green', iconColor = 'white', library = "fa"
      ),
      Motorboat = leaflet::makeAwesomeIcon(
        text = fa("ship"), markerColor = 'green', iconColor = 'white', library = "fa"
      ),
      Car = leaflet::makeAwesomeIcon(
        text = fa("car"), markerColor = 'green', iconColor = 'white', library = "fa"
      )
    )
    
    for (i in 1:length(listGPX)) {
      GPX_file <- listGPX[i]
      trackName <- sub('.*_', '', GPX_file)
      if (!is.na(wpS[[trackName]]$time[1])) {
        map <- map %>% leaflet::addPolylines(
          data = trackS[[trackName]],
          color = activityType[activityType$activityType == as.character(trackS[[trackName]]$type),]$color, 
          group = 'Tracks',
          layerId = trackName,
          opacity = 1,
          popup = paste(
            "Track name: <b>", trackS[[trackName]]$name, "</b></br>",
            "Activity types: <b>", trackS[[trackName]]$type, "</b></br>",
            "Start date and time: <b>", as.character(min(wpS[[trackName]]$time)), "</b></br>",
            "Trip time: <b>", round(max(wpS[[trackName]]$time) - min(wpS[[trackName]]$time), digits = 2), " hours</b></br>",
            "Altitude difference: <b>", round(max(wpS[[trackName]]$ele) - min(wpS[[trackName]]$ele), digits = 2), " m</b></br>",
            "Positive Gain (D+): <b>", round(wpS[[trackName]]$posGain[1], digits = 2), " m</b></br>",
            "Negative Gain (D-): <b>", round(wpS[[trackName]]$negGain[1], digits = 2), " m</b></br>",
            p(),
            actionLink(
              "showmodal",
              "Show plot",
              onclick = 'Shiny.onInputChange(\"button_click\",  Math.random())'
            )
          ),
          highlightOptions = leaflet::highlightOptions(color = "white",
                                              weight = 2,
                                              bringToFront = TRUE),
          label = trackS[[trackName]]$type
        )
      } else if (is.na(wpS[[trackName]]$time[1])) {
        map <- map %>% leaflet::addPolylines(
          data = trackS[[trackName]],
          color = activityType[activityType$activityType == as.character(trackS[[trackName]]$type),]$color, 
          group = 'Tracks',
          layerId = trackName,
          opacity = 1,
          popup = paste(
            "Track name: <b>", trackS[[trackName]]$name, "</b></br>",
            "Activity types: <b>", trackS[[trackName]]$type, "</b></br>",
            "For this activity time and date are not been collected.</br>",
            "Altitude difference: <b>", round(max(wpS[[trackName]]$ele) - min(wpS[[trackName]]$ele), digits = 2), " m</b></br>",
            "Positive Gain (D+): <b>", round(wpS[[trackName]]$posGain[1], digits = 2), " m</b></br>",
            "Negative Gain (D-): <b>", round(wpS[[trackName]]$negGain[1], digits = 2), " m</b></br>",
            p(),
            actionLink(
              "showmodal",
              "Show plot",
              onclick = 'Shiny.onInputChange(\"button_click\",  Math.random())'
            )
          ),
          highlightOptions = leaflet::highlightOptions(color = "white",
                                              weight = 2,
                                              bringToFront = TRUE),
          label = trackS[[trackName]]$type
        )
      } else if (trackS[[trackName]]$type == 'Motorboat') {
        map <- map %>% leaflet::addPolylines(
          data = trackS[[trackName]],
          color = activityType[activityType$activityType == as.character(trackS[[trackName]]$type),]$color, 
          group = 'Tracks',
          layerId = trackName,
          opacity = 1,
          popup = paste(
            "Track name: <b>", trackS[[trackName]]$name, "</b></br>",
            "Activity types: <b>", trackS[[trackName]]$type, "</b></br>",
            "Start date and time: <b>", as.character(min(wpS[[trackName]]$time)), "</b></br>",
            "Trip time: <b>", round(max(wpS[[trackName]]$time) - min(wpS[[trackName]]$time), digits = 2), " hours</b></br>",
            p(),
            actionLink(
              "showmodal",
              "Show plot",
              onclick = 'Shiny.onInputChange(\"button_click\",  Math.random())'
            )
          ),
          highlightOptions = leaflet::highlightOptions(color = "white",
                                              weight = 2,
                                              bringToFront = TRUE),
          label = trackS[[trackName]]$type
        )
      }
    }
    
    # add waypoints to the map
    waypointsIcon <- leaflet::awesomeIconList(
      resturant = leaflet::makeAwesomeIcon(
        text = fa("utensils"), markerColor = 'green', iconColor = 'white', library = "fa"
        ),
      beach = leaflet::makeAwesomeIcon(
        text = fa("umbrella-beach"), markerColor = 'blue', iconColor = 'white', library = "fa"
        ),
      touristAttraction = leaflet::makeAwesomeIcon(
        text = fa("landmark"), markerColor = 'red', iconColor = 'white', library = "fa"
      ),
      village = leaflet::makeAwesomeIcon(
        text = fa("home"), markerColor = 'orange', iconColor = 'white', library = "fa"
      )
    )
    for (i in 1:length(waypointS)) {
      map <- map %>%
        # leaflet::addMarkers(
        leaflet::addAwesomeMarkers(
          data = waypointS[[i]],
          popup = paste0(
            "Name: <b>", waypointS[[i]]$name, "</b><br/>",
            "Elevation: <b>", waypointS[[i]]$ele, " (m asl)</b><br/>",
            "Description: <b>", waypointS[[i]]$desc, "</b><br/>",
            "Link: <b><a target = 'blank' href = '", waypointS[[i]]$link1_href, "'>", waypointS[[i]]$link1_text, "</a></b><br/>",
            "Map: <b><a target = 'blank' href = '", waypointS[[i]]$link2_href, "'>", waypointS[[i]]$link2_text, "</a></b><br/>"
          ),
          icon = ~waypointsIcon[waypointS[[i]]$sym], # function providing custom marker-icons
          group = 'Waypoints',
          clusterOptions = leaflet::markerClusterOptions()
        )
    }
    
    # add photos to the map
    photoIcon <- leaflet::awesomeIcons(icon = 'camera', library = 'fa')
    
    map <- map %>%
    leaflet::addAwesomeMarkers(
      lat = selectedPhotos()$GPSLatitude,
      lng = selectedPhotos()$GPSLongitude,  
      popup = paste0(
        "File: <b>", selectedPhotos()$photo, "</b><br/>",
        "Capture Date and Time: <b>", selectedPhotos()$DateTimeOriginal, "</b><br/>",
        "Exposure: <b>", selectedPhotos()$ExposureMode, "</b><br/>",
        "Focal Length: <b>", selectedPhotos()$FocalLength, "</b><br/>",
        "ISO Speed Rating: <b>", selectedPhotos()$ISO, "</b><br/>",
        # "Flash: <b>" = , "</b><br/>",
        "Model: <b>", selectedPhotos()$Model, "</b><br/>",
        "Lens: <b>", selectedPhotos()$Lens, "</b><br/>",
        p(),
        actionLink(
          "showmodalPhoto",
          "Show all selected photos",
          onclick = 'Shiny.onInputChange(\"link_to_tabpanel_Photo\",  Math.random())'
        )
      ),
      icon = photoIcon, # function providing custom marker-icons
      group = 'Photo markers',
      clusterOptions = leaflet::markerClusterOptions()
    )
      
    return(map)
  })
  
  # click on photo
  observeEvent(input$link_to_tabpanel_Photo, {
    newvalue <- "Photos"
    updateNavbarPage(session, "nav", newvalue)
  })
  
  # return from Photos tabPanel to Map tabPanel
  observeEvent(input$link_to_tabpanel_map, {
    newvalue <- "Map"
    updateNavbarPage(session, "nav", newvalue)
  })
  
  # slideshow of photos selected
  # (exifinfoSel %>%
  #     filter(Years %in% '2020',
  #            Album %in% '2020 - Salse di Nirano'))[grepl('Caterina Bergami',
  #                                                        (exifinfoSel %>%
  #                                                           filter(Years %in% '2020',
  #                                                                  Album %in% '2020 - Salse di Nirano'))$Subject),]
  output$slider <- renderImage({
    list.files(
      "../../../../Desktop/photo/multimedia/2020/2020-03-01",
      # system.file(
      #   "../../../../Desktop/photo/multimedia/2020/2020-03-01",
      #   package = "EBImage"
      # ),
      pattern = "*.jpg",
      full.names = TRUE
    ) %>%
      purrr::map(EBImage::readImage) %>%
      EBImage::resize(w = 300) %>%
      EBImage::writeImage(files = "outfile.png", quality = 80)
    # return list containing filename
    list(src = "outfile.png",
         contentType = 'image/png')
    
    # # return list containing filename
    # list(src = "outfile.png",
    #      contentType = 'image/png')
    # list.files(
    #   "../../../../Desktop/photo/multimedia/2020/2020-03-01",
    #   pattern = "*.jpg",
    #   full.names = TRUE
    # ) %>%
    #   purrr::map(magick::image_read) %>%     # read each path file
    #   magick::image_join() %>%               # joins image
    #   magick::image_scale("x500") %>%       # scale image
    #   magick::image_animate(fps = 1) %>%    # animates (option exists for number of loops)
    #   magick::image_write("outfile.png")
    # 
    # # return list containing filename
    # list(src = "outfile.png",
    #      contentType = 'image/png')
  }, deleteFile = TRUE)
  
  # click on track
  mapShapeClickID <- reactive(
    input$map_shape_click$id
  )
  
  observeEvent(input$map_shape_click, {
    observeEvent(input$button_click, {
      trackSelect <- mapShapeClickID()
      if (!is.na(wpS[[trackSelect]]$time[1])) {
        output$trackPlot <- plotly::renderPlotly({
          # Plotly chart with date and time
          plotly::plot_ly(data = as.data.frame(wpS[[trackSelect]]), 
                          x = ~time, 
                          y = ~ele, 
                          type = "scatter", 
                          mode = "points"
          ) %>% 
            layout(
              xaxis = list(title = ''),
              yaxis = list(title = 'Elevation (m)')
            )
        })
      } else if (is.na(wpS[[trackName]]$time[1])) {
        output$trackPlot <- plotly::renderPlotly({
          # Plotly chart without date and time
          plotly::plot_ly(data = as.data.frame(wpS[[trackSelect]]),
                          x = ~track_seg_point_id,
                          y = ~ele, 
                          type = "scatter", 
                          mode = "lines"
          ) %>% 
            layout(
              xaxis = list(title = ''),
              yaxis = list(title = 'Elevation (m)')
            )
        })
      } else if (trackS[[trackName]]$type == 'Motorboat') {
        output$trackPlot <- plotly::renderPlotly({
          # Plotly chart with date and time
          plotly::plot_ly(data = as.data.frame(wpS[[trackSelect]]), 
                          x = ~time, 
                          y = ~vel, 
                          type = "scatter", 
                          mode = "points"
          ) %>% 
            layout(
              xaxis = list(title = ''),
              yaxis = list(title = 'Velocity (km/h)')
            )
        })
      } 
      
    ## track popup ##########################################
    if (trackS[[trackSelect]]$type == 'Motorboat') {
      showModal(modalDialog(
        title = HTML(
          "Track name: <b>", as.character(trackS[[trackSelect]]$name), "</b></br>",
          "Activity types: <b>", as.character(trackS[[trackSelect]]$type), "</b></br>"
        ),
        easyClose = TRUE,
        plotly::plotlyOutput("trackPlot")
      ))
    } else if (!is.na(wpS[[trackSelect]]$time[1])) {
        showModal(modalDialog(
          title = HTML(
            "Track name: <b>", as.character(trackS[[trackSelect]]$name), "</b></br>",
            "Activity types: <b>", as.character(trackS[[trackSelect]]$type), "</b>"
          ),
          HTML("Start date and time: <b>", as.character(min(wpS[[trackSelect]]$time)), "</b>",
               "Trip time: <b>", round(max(wpS[[trackSelect]]$time) - min(wpS[[trackSelect]]$time), digits = 2), " hours</b>",
               "Altitude difference: <b>", round(max(wpS[[trackSelect]]$ele) - min(wpS[[trackSelect]]$ele), digits = 2), " m</b></br>",
               "Positive Gain (D+): <b>", round(wpS[[trackSelect]]$posGain[1], digits = 2), " m</b>",
               "Negative Gain (D-): <b>", round(wpS[[trackSelect]]$negGain[1], digits = 2), " m</b></br>"),
          easyClose = TRUE,
          plotly::plotlyOutput("trackPlot")
        ))
      } else if (is.na(wpS[[trackName]]$time[1])) {
        showModal(modalDialog(
          title = HTML(
            "Track name: <b>", as.character(trackS[[trackSelect]]$name), "</b></br>",
            "Activity types: <b>", as.character(trackS[[trackSelect]]$type), "</b></br>",
            "For this activity time and date are not been collected.</br>"
          ),
          HTML("Altitude difference: <b>", round(max(wpS[[trackSelect]]$ele) - min(wpS[[trackSelect]]$ele), digits = 2), " m</b></br>",
               "Positive Gain (D+): <b>", round(wpS[[trackSelect]]$posGain[1], digits = 2), " m</b>",
               "Negative Gain (D-): <b>", round(wpS[[trackSelect]]$negGain[1], digits = 2), " m</b></br>"),
          easyClose = TRUE,
          plotly::plotlyOutput("trackPlot")
        ))
      }
    })
  })
}
