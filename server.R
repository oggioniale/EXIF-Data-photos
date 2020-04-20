function(input, output, session) {

  ## Interactive Map ##########################################
  
  # Create the map
  selectedPhotos <- reactive(
    if (input$year == 'All years' || input$album == 'All albums' || input$subject == 'All subjects') {
      exifinfoSel
    } else if (input$year != 'All years'|| input$album != 'All albums' || input$subject != 'All subjects') {
      exifinfoSel[which(grepl(input$subject, exifinfoSel$Subject)),][exifinfoSel$Years == input$year]
    }
  )
  
  observeEvent(input$year, {
    if (input$year == 'All years') {
      updateSelectizeInput(
        session,
        'album', 
        choices = c("All albums", as.list(unique(unlist(exifinfoSel$Subject))[grepl(' - ', unique(unlist(exifinfoSel$Subject)))]))[[1]],
        server = TRUE
      )
    } else {
      updateSelectizeInput(
        session,
        'album', 
        choices = as.list(unique(unlist(exifinfoSel$Subject))[grepl(paste0(input$year,' - '), unique(unlist(exifinfoSel$Subject)))]),
        server = TRUE
      )
    }
  })
  
  output$map <- leaflet::renderLeaflet({
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Please wait", value = 0)
    
    map <- leaflet::leaflet() %>% 
      leaflet::addTiles() %>% 
      leaflet::addLayersControl(position = 'bottomright',
                                overlayGroups = c("Tracks", 'Photo markers'),
                                options = leaflet::layersControlOptions(collapsed = FALSE)
      )
    
    # add tracks to the map
    for (i in 1:5){#length(listGPX)) {
      GPX_file <- listGPX[i]
      trackName <- substring(GPX_file, 61)
      map <- map %>% leaflet::addPolylines(
        data = trackS[[trackName]],
        color = 'red', 
        group = 'Tracks',
        layerId = trackName,
        popup = paste(
          "Track name: <b>", trackS[[trackName]]$name, "</b></br>",
          "Start date and time: <b>", min(wpS[[trackName]]$time), "</b></br>",
          "Trip time: <b>", round(max(wpS[[trackName]]$time) - min(wpS[[trackName]]$time), digits = 2), " hours</b></br>",
          "Altitude difference: <b>", round(max(wpS[[trackName]]$ele) - min(wpS[[trackName]]$ele), digits = 2), " m</b></br>",
          p(),
          actionLink(
            "showmodal",
            "Show plot",
            onclick = 'Shiny.onInputChange(\"button_click\",  Math.random())'
          )
        )
      )
    }
    
    # add photos to the map
    photoIcon <- leaflet::awesomeIcons(icon = 'camera', library = 'fa')
    
    # observeEvent(input$year, {
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
              "Show plot",
              onclick = 'Shiny.onInputChange(\"button_click\",  Math.random())'
            )
          ),
          icon = photoIcon, # function providing custom marker-icons
          group = 'Photo markers',
          clusterOptions = leaflet::markerClusterOptions()
        )
    # })
    
    return(map)
  })
  
  # click on photo
  observeEvent(input$button_click_photo, {
    output$img <- renderImage({
      image <- exifinfoSel$SourceFile
    })
  })
  
  # click on track
  mapShapeClickID <- reactive(
    input$map_shape_click$id
  )
  
  observeEvent(input$map_shape_click, {
    observeEvent(input$button_click, {
      trackSelect <- mapShapeClickID()
      output$trackPlot <- plotly::renderPlotly({
        # Plotly chart
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
      
      showModal(modalDialog(
        title = HTML("Track name: <b>", as.character(trackS[[trackSelect]]$name), "</b>"),
        HTML("Start date and time: <b>", min(wpS[[trackSelect]]$time), "</b>",
        "Trip time: <b>", round(max(wpS[[trackSelect]]$time) - min(wpS[[trackSelect]]$time), digits = 2), " hours</b>",
        "Altitude difference: <b>", round(max(wpS[[trackSelect]]$ele) - min(wpS[[trackSelect]]$ele), digits = 2), " m</b></br>"),
        easyClose = TRUE,
        plotly::plotlyOutput("trackPlot")
      ))
    })
  })
}
