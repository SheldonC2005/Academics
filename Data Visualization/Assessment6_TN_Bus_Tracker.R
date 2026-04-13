packages <- c("shiny", "leaflet", "dplyr", "httr", "jsonlite")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(packages[!installed])

library(shiny)
library(leaflet)
library(dplyr)
library(httr)
library(jsonlite)

api_key <- Sys.getenv("ORS_API_KEY")
if (nchar(api_key) == 0) {
  api_key <- "5b3ce3597851110001cf6248726b078769f948a98bbf70821ef7eca7"
}

get_route <- function(start_lat, start_lng, end_lat, end_lng, api_key) {
  url <- paste0(
    "https://api.openrouteservice.org/v2/directions/driving-car?",
    "api_key=", api_key,
    "&start=", start_lng, ",", start_lat,
    "&end=",   end_lng,   ",", end_lat
  )
  response <- tryCatch(GET(url), error = function(e) NULL)
  
  if (!is.null(response) && status_code(response) == 200) {
    json_text <- content(response, "text", encoding = "UTF-8")
    json      <- tryCatch(
      fromJSON(json_text, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(json)) return(NULL)
    coords <- json$features[[1]]$geometry$coordinates
    latlng <- data.frame(
      lng = sapply(coords, function(x) x[[1]]),
      lat = sapply(coords, function(x) x[[2]])
    )
    return(latlng)
  } else {
    warning("Failed to fetch route for a bus.")
    return(NULL)
  }
}

bus_points <- list(
  list(name  = "Bus 1 | Anna Univ -> NIT Trichy",
       start = c(13.0101, 80.2350),
       end   = c(10.7590, 78.8165)),
  
  list(name  = "Bus 2 | VIT Vellore -> PSG Coimbatore",
       start = c(12.9692, 79.1559),
       end   = c(11.0234, 77.0005)),
  
  list(name  = "Bus 3 | SRM Chennai -> Amrita Coimbatore",
       start = c(12.8231, 80.0444),
       end   = c(10.9026, 76.9019)),
  
  list(name  = "Bus 4 | SASTRA Thanjavur -> IIT Madras",
       start = c(10.7869, 79.1325),
       end   = c(12.9916, 80.2336)),
  
  list(name  = "Bus 5 | TCE Madurai -> Bharathidasan Trichy",
       start = c(9.8822,  78.0824),
       end   = c(10.7640, 78.7016))
)

bus_routes <- lapply(bus_points, function(x) {
  route <- get_route(x$start[1], x$start[2], x$end[1], x$end[2], api_key)
  if (!is.null(route)) {
    route$bus <- x$name
  }
  return(route)
})

valid_routes <- Filter(Negate(is.null), bus_routes)
max_steps    <- if (length(valid_routes) > 0) {
  max(sapply(valid_routes, nrow))
} else {
  0
}

ui <- fluidPage(
  titlePanel("Tamil Nadu College Bus Tracker"),
  leafletOutput("map", height = "650px"),
  tags$head(tags$script(HTML("
    setInterval(function(){
      Shiny.onInputChange('tick', new Date().getTime());
    }, 1000);
  ")))
)

server <- function(input, output, session) {
  bus_index <- reactiveVal(1)
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lat = 11.0, lng = 78.8, zoom = 7)
  })
  
  observeEvent(input$tick, {
    if (max_steps == 0) return()
    
    i <- bus_index()
    
    position_list <- lapply(bus_routes, function(route) {
      if (!is.null(route) && i <= nrow(route)) {
        data.frame(
          lat = route$lat[i],
          lng = route$lng[i],
          bus = route$bus[1],
          stringsAsFactors = FALSE
        )
      } else {
        NULL
      }
    })
    
    position_list <- Filter(Negate(is.null), position_list)
    
    if (length(position_list) > 0) {
      positions <- do.call(rbind, position_list)
      leafletProxy("map") %>%
        clearMarkers() %>%
        addMarkers(data = positions, ~lng, ~lat, popup = ~bus)
    }
    
    i <- i + 5
    if (i > max_steps) i <- 1
    bus_index(i)
  })
}

shinyApp(ui, server)