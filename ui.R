library(shinyjs)
library(plotly)

navbarPage("FotoAle©", id = "nav",

  tabPanel("Map",
    div(class = "outer",
        useShinyjs(),
        tags$head(
          tags$link(rel = "stylesheet",
                    type = "text/css",
                    href = "css/style.css")
        ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leaflet::leafletOutput("map", width = "100%", height = "100%"),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("Filter photos by:"),
        # TODO add multiple selection
        selectizeInput('year',
                       label = 'Years', 
                       choices = listYears,
                       multiple = FALSE,
                       selected = listYears[[1]]),
        selectizeInput('album',
                       label = 'Album',
                       choices = NULL, # listAlbum,
                       multiple = FALSE,
                       selected = c("All albums", as.list(unique(unlist(exifinfoSel$Subject))[grepl(' - ', unique(unlist(exifinfoSel$Subject)))]))[[1]]),
        selectizeInput('subject',
                       label = 'Subjects',
                       choices = listOfSubjects,
                       multiple = FALSE,
                       selected = listOfSubjects[[1]])
      )
    )
  ),
  conditionalPanel("false", icon("crosshair"))
)
