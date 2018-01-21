library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library('ReporteRs') # Load
library(ggmap)
library(sp)
library(eeptools)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
zipdata <- sarasota
colorData <- zipdata$age
pal <- colorBin("viridis", colorData, 7, pretty = FALSE)

function(input, output, session) {

  ## Interactive Map ###########################################


  
  # Create the map
  output$map <- renderLeaflet({
    leaflet(data=zipdata[sample(1:250000, 10000),]) %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -82.42, lat = 27.1, zoom = 11) %>%
      clearShapes() %>%
      addCircles(~lon, ~lat, radius=1, layerId=~V2,
                 stroke=TRUE, color="red", fillOpacity=0.1, fillColor="red") 
  })

  # A reactive expression that returns the set of zips that are
  # in bounds right now
  votersInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(zipdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(sarasota,
      lat >= latRng[1] & latitude <= latRng[2] &
        lon >= lngRng[1] & longitude <= lngRng[2])
  })

  # Precalculate the breaks we'll need for the two histograms



  #output$scatterCollegeIncome <- renderPlot({
    # If no zipcodes are in view, don't plot
  #  if (nrow(votersInBounds()) == 0)
  #    return(NULL)

  #  print(xyplot(income ~ college, data = votersInBounds(), xlim = range(allzips$college), ylim = range(allzips$income)))
  #})


  # Show a popup at the given location
  showZipcodePopup <- function(voter_id, lat, lng) {
    selectedZip <- sarasota[sarasota[,'V2'] == voter_id,]
    print(selectedZip)
    content <- as.character(tagList(
      tags$h4("Name: ", selectedZip$`V5`, selectedZip$`V6`, selectedZip$`V3`),
      print(paste("Address: ", selectedZip$`street_name`, selectedZip$`city`, 'FL', selectedZip$`zip_code`)), tags$br(),
      print(paste("Gender: ", selectedZip$`V20`)), tags$br(),
      print(paste("Birthdate: ", selectedZip$birthdate)), tags$br()
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = voter_id) %>%
    setView(lng = lng, lat = lat, zoom = 15) %>%
      addCircles(lng, lat, radius=1, layerId=voter_id,
                 stroke=TRUE, color="red" ,fillOpacity=0.4, fillColor="red") 
  }

  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    print(event)
    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })


  

  ## Data Explorer ###########################################

  

  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.1
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })

  output$ziptable <- DT::renderDataTable({
    #geocode and find nearest neighbors
    # source files
    source("functions.R")
    
    # Geocode address
    voter <- geocode(input$address)
    
    # find nearest neighbors
    neighbors <- nearestNeighbors(voter, sarasota, num_neighbors = 100, party = "REP" )
    neighbors$lat <- jitter(neighbors$lat, factor=300)
    neighbors$lon <- jitter(neighbors$lon, factor=300)
    
    # closest republican
    closest <- neighbors[1,]
    
    
    df <- neighbors %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="',lat, '" data-long="', lon, '" data-zip="', V2, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)

    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}
