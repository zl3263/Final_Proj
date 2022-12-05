library(shiny)
library(rsconnect)
library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(sf)
library(shinyWidgets)
library(geojsonio)
library(geojsonsf)

# Now avaliable on https://nuc-rental-income.shinyapps.io/shiny_map/
#setwd("Shiny_Map")
load("cleaned_data.RData")

#block_edge = geojsonio::geojson_read("zoning_boundary.json",what  = "sp")
#block_edge =  geojsonio::geojson_read("E:/Data_Science/Final_Proj/data/2000 Census Blocks.geojson",what  = "sp") 
#block_edge <- readLines("zoning_boundary.json") %>% paste(collapse = "\n") 
#load("cache.RData")
block_edge=read_sf("DTM_Tax_Block_Polygon.shp") %>%
  st_transform("NAD83") %>%
  mutate(borough=as.numeric(BORO)) %>%
  select(borough,everything())%>%
  nest(data=OBJECTID:geometry) %>%
  mutate(data = map(data,sf_geojson)) %>%
  na.omit() %>%
  arrange(borough)



# Define UI for application
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
  
  leafletOutput("map", width = "100%", height = "70%"),
  textOutput("test"),
  dataTableOutput("record"),
  absolutePanel(
    top = 10, left = 10, style = "z-index:500; text-align: left;",
    tags$h2("Browse our dataset by Borough-Block"),
    tags$a("Back to Main Page", href="https://zl3263.github.io/Final_Proj/"),
    selectInput(
      "selected_boro",
      label = "Select Borough",
      choices = list(
        Manhattan = 1,
        Bronx = 2,
        Brooklyn = 3,
        Queens = 4,
        Staten_Island = 5
      ),
      selected = 1,
      width = "30%"
    )
  ),


)

# Define server logic
server <- function(input, output) {

    output$map <- renderLeaflet({
      leaflet(block_edge,options = leafletOptions(zoomControl = FALSE))%>%
      addProviderTiles("CartoDB.Positron")%>%
      setView(lng = -73.974, lat = 40.762, zoom = 12) %>%
      #addPolygons(
      #  smoothFactor = 0.3,
      #  color = "#444444",
      #  weight=1,
      #  fillColor = "transparent",
      #  label = ~paste(as.character(BORO),"-",as.character(BLOCK))
      #)
      addGeoJSON(block_edge$data[[1]], weight = 1, color = "#444444", fillColor = "transparent")
    })
    output$test <- renderText({
      "Please select block"
    })
    output$record <- renderDataTable({
      transformed_rental_income %>%
        filter(boro_block == 100011) %>%
        select(
          boro_block,
          address,
          neighborhood,
          building_classification,
          total_units,
          year_built,
          gross_sq_ft,
          gross_income_per_sq_ft,
          full_market_value,
          report_year
        ) 
    })
    observe({
      selected_boro = as.numeric(input$selected_boro)
      output$map <- renderLeaflet({
        leaflet(block_edge,options = leafletOptions(zoomControl = FALSE))%>%
          addProviderTiles("CartoDB.Positron")%>%
          setView(lng = -73.974, lat = 40.762, zoom = 12) %>%
          #addPolygons(
          #  smoothFactor = 0.3,
          #  color = "#444444",
          #  weight=1,
          #  fillColor = "transparent",
          #  label = ~paste(as.character(BORO),"-",as.character(BLOCK))
          #)
          addGeoJSON(block_edge$data[[selected_boro]], weight = 1, color = "#444444", fillColor = "transparent")
      })
    })
    observe({
      leafletProxy("map") %>% clearPopups()
      event <- input$map_geojson_click
      if (is.null(event))
        return()
      
      isolate({
        output$test <- renderText({
          paste0(
            "You are looking at records for boro-block: ",
            as.character(input$map_geojson_click$properties$BORO),
            "-",
            as.character(input$map_geojson_click$properties$BLOCK)
          )
          
        })
      })
      
      output$record <- renderDataTable({
        transformed_rental_income %>%
          filter(boro_block == as.numeric(input$map_geojson_click$properties$BORO)*100000+as.numeric(input$map_geojson_click$properties$BLOCK)) %>%
          select(
            boro_block,
            address,
            neighborhood,
            building_classification,
            total_units,
            year_built,
            gross_sq_ft,
            gross_income_per_sq_ft,
            full_market_value,
            report_year
          ) 
      })
    })
    
}

# Run the application 


shinyApp(ui = ui, server = server)

