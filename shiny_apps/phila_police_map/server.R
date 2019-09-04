# server

server <- function(input, output, session) {
  
  data <- reactive({
    x <- df
  })
  
  output$mymap <- renderLeaflet({
    df <- data()
  
  # create leaflet object  
  m <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    
    # set map center & zoom
    setView(lng = -75.15,
            lat = 39.975,
            zoom = 12) %>%
    
    #define conditional markers and hovertext
    addCircleMarkers(lat = map_data$lat,
                     lng = map_data$lng,
                     stroke = TRUE, 
                     opacity = .75,
                     weight = 2,
                     radius = 3,
                     fill = sample_data$arrest_made, fillOpacity = sample_data$arrest_made*0.75,
                     popup = paste0(sample_data$date,
                                    ' - ',
                                    sample_data$subject_race,
                                    ' ',
                                    sample_data$type,
                                    ifelse(sample_data$arrest_made, '; arrested', '; stopped')),
                     color = ifelse(sample_data$subject_race=='black', 'black', 'red'),
    )
  m
  })
  }