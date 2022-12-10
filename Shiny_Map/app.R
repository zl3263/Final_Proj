library(shiny)
library(rsconnect)
library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(sf)
library(shinyWidgets)
library(geojsonio)
library(geojsonsf)
library(raster)
library(terra)
library(rgdal)

# Now avaliable on https://nuc-rental-income.shinyapps.io/shiny_map/
#setwd("Shiny_Map")
load("cleaned_data.RData")
load("heatmap.RData")

#block_edge = geojsonio::geojson_read("zoning_boundary.json",what  = "sp")
#block_edge =  geojsonio::geojson_read("E:/Data_Science/Final_Proj/data/2000 Census Blocks.geojson",what  = "sp") 
#block_edge <- readLines("zoning_boundary.json") %>% paste(collapse = "\n") 
#load("cache.RData")
block_edge=read_sf("DTM_Tax_Block_Polygon.shp") %>%
  st_transform("+proj=longlat +datum=WGS84") %>%
  mutate(BORO=as.numeric(BORO),BLOCK=as.numeric(BLOCK)) %>%
  arrange(BORO)

this_year= as.numeric(substr(Sys.Date(),1,4))

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
  span(textOutput("test"), style="color:red"),
  dataTableOutput("record"),
  absolutePanel(
    top = 10, left = 10, style = "z-index:500; text-align: left; background-color: rgba(255,255,255,0.4);",
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
    ),
    radioButtons(
      "heatmap_option",
      label = "Select heatmap",
      choices = list(
        "None" = 1,
        "Rental Income" = 2
      ),
      selected = 1,
      inline = TRUE
    ),
    radioButtons(
      "display_option",
      label = "Select display option",
      choices = list(
        "Display all blocks" = 1,
        "Display blocs with data" = 2
      ),
      selected = 1,
      inline = TRUE
    ),
    sliderInput(
      "built_year",
      label = "Select building built year",
      min = 1840,
      max = this_year,
      value = c(1840,this_year)
    ),
    sliderInput(
      "report_year",
      label = "Select data report year",
      min = 2012,
      max = this_year,
      value = c(2012,this_year)
    )
  ),


)

# Define server logic
server <- function(input, output) {
    block_edge_new = 
      block_edge %>%
      filter(BORO==1) %>%
      sf_geojson()
      
    

    pal <- colorNumeric("viridis", values(heatmap),na.color = "transparent")
    
    
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -73.974, lat = 40.762, zoom = 12) %>%
        #addPolygons(
        #  smoothFactor = 0.3,
        #  color = "#444444",
        #  weight=1,
        #  fillColor = "transparent",
        #  label = ~paste(as.character(BORO),"-",as.character(BLOCK))
        #) 
        addGeoJSON(block_edge_new, weight = 1, color = "#444444", fillColor = "transparent")
    })
    output$test <- renderText({
      "Please select block"
    })
    output$record <- renderDataTable({
      transformed_rental_income %>%
        filter(boro_block == 100011) %>%
        dplyr::select(
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
      if(input$heatmap_option == 1){
        leafletProxy("map") %>%
          clearImages()
      }
      else if(input$heatmap_option == 2){
        leafletProxy("map") %>%
          addRasterImage(heatmap,colors = pal,opacity = 0.3)
      }
    })
    
    
    
    observe({
      selected_boro = as.numeric(input$selected_boro)
      display_option = as.numeric(input$display_option)
      #block_edge_selected
      
      if(display_option == 1){
        block_edge_selected =
          block_edge %>%
          filter(
            BORO == selected_boro
          ) %>%
          sf_geojson()
      }
      else{
        selected_block = 
          transformed_rental_income %>%
          filter(
            as.integer(boro_block/100000) == selected_boro,
            year_built >= input$built_year[1],
            year_built <= input$built_year[2],
            report_year >= input$report_year[1],
            report_year <= input$report_year[2]
          ) %>%
          distinct(block_id) %>%
          pull(block_id)
        
        
        block_edge_selected =
          block_edge %>%
          filter(
            BORO == selected_boro,
            BLOCK %in% selected_block
          ) %>%
          sf_geojson()
      }
      
      leafletProxy("map") %>%
        clearGeoJSON() %>%
        addGeoJSON(block_edge_selected, weight = 1, color = "#444444", fillColor = "transparent")
        
      #output$map <- renderLeaflet({
      #  leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      #    addProviderTiles("CartoDB.Positron") %>%
      #    setView(lng = -73.974, lat = 40.762, zoom = 12) %>%
      #    #addPolygons(
      #    #  smoothFactor = 0.3,
      #    #  color = "#444444",
      #    #  weight=1,
      #    #  fillColor = "transparent",
      #    #  label = ~paste(as.character(BORO),"-",as.character(BLOCK))
      #    #)
      #    addGeoJSON(block_edge_selected, weight = 1, color = "#444444", fillColor = "transparent")
      #})
    })
    observe({
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
      
      selected_block =
        block_edge %>%
        filter(
          BORO == as.numeric(input$map_geojson_click$properties$BORO),
          BLOCK ==  as.numeric(input$map_geojson_click$properties$BLOCK)
        )
      
      leafletProxy("map") %>%
        clearShapes() %>%
        addPolygons(stroke = FALSE, fillColor  = "red", fillOpacity=0.1,data= selected_block)
      
      output$record <- renderDataTable({
        transformed_rental_income %>%
          filter(boro_block == as.numeric(input$map_geojson_click$properties$BORO)*100000+as.numeric(input$map_geojson_click$properties$BLOCK)) %>%
          filter(
            year_built >= input$built_year[1],
            year_built <= input$built_year[2],
            report_year >= input$report_year[1],
            report_year <= input$report_year[2]
          ) %>%
          dplyr::select(
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

