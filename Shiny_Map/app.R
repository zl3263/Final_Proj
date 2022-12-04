library(shiny)
library(rsconnect)
library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(sf)
library(shinyWidgets)
library(geojsonio)

#block_edge = rgdal::readOGR("data/2000 Census Blocks.geojson") 


# Define UI for application that draws a histogram
ui <- bootstrapPage(
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css?family=Oswald", rel = "stylesheet"),
    tags$style(type = "text/css", "html, body {width:100%;height:100%; font-family: Oswald, sans-serif;}"),
    tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                type="text/javascript"),
    tags$script('
                $(document).ready(function () {
                  navigator.geolocation.getCurrentPosition(onSuccess, onError);
                
                  function onError (err) {
                    Shiny.onInputChange("geolocation", false);
                  }
                
                  function onSuccess (position) {
                    setTimeout(function () {
                      var coords = position.coords;
                      console.log(coords.latitude + ", " + coords.longitude);
                      Shiny.onInputChange("geolocation", true);
                      Shiny.onInputChange("lat", coords.latitude);
                      Shiny.onInputChange("long", coords.longitude);
                    }, 1100)
                  }
                });
                ')
  ),
  
  leafletOutput("map", width = "100%", height = "100%"),
  

)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$map <- renderLeaflet({
        leaflet()%>%
        addProviderTiles("CartoDB.Positron")%>%
        setView(-3, 54.3, zoom = 6)
        #addTopoJSON(block_edge, weight = 1, color = "#444444", fill = FALSE)
    })
}

# Run the application 


shinyApp(ui = ui, server = server)
