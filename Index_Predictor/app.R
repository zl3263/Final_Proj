#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

#setwd("Index_Predictor")
load("modeling_result.RData")
load("block_location.RData")
n_neighbours=20

this_year= as.numeric(substr(Sys.Date(),1,4))
smooth_location_resid=smooth_location_resid
block_location=block_location
predict_rental = function(new_data){
  if(nrow(new_data)!=1){
    return("nrow must be 1")
  }
  location_resid =
    smooth_location_resid %>%
    mutate(
      d=(longitude-new_data$longitude)^2+(latitude-new_data$latitude)^2
    ) %>%
    arrange(d) %>% 
    head(n_neighbours) %>%
    lm(resid_smooth~longitude+latitude,data=.) %>%
    predict(newdata = new_data)
  
  predict(linear_model,newdata=new_data) + location_resid
}
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    #titlePanel("Predicting using Our Model!"),

    # Sidebar with a slider input for number of bins 
    fluidRow(
      column(2,
             textInput(
               "boro_block",
               label = "Boro-Block",
               value  = "1-11"
             ),
             tags$a("which boro-block?", href="https://nuc-rental-income.shinyapps.io/shiny_map/", target="_blank"),
      ),
      
      column(2,
             selectInput(
               "is_elevator",
               label = "Elevator",
               choices = list(
                 ELEVATOR = 1,
                 WALK_UP = 0
               ),
               selected = 0
             ),
      ),
      column(2,
             textInput(
               "total_units",
               label = "Total Units",
               value  = "100"
             ),
      ),
      column(2,
             textInput(
               "year_built",
               label = "Year Built",
               value  = "1900"
             ),
      ),
      column(2,
             textInput(
               "gross_sq_ft",
               label = "Building Area",
               value  = "100000"
             ),
      ),
      column(2,
             selectInput(
               "report_year",
               label = "Estimate Year",
               choices = 2012:this_year,
               selected = this_year
             ),
      )
    ),
    span(textOutput("result"), style="font-size:36px"),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$result <- renderText({
        "Calculate your gross income pre suqare feet!"
    })
    
    observe({
      boro_block_predict = str_split(input$boro_block,"\\D")[[1]]
      boro_block_predict = as.integer(boro_block_predict[1])*100000+as.integer(boro_block_predict[2])
      location = block_location %>%
        filter(boro_block ==boro_block_predict)
      is_elevator = as.logical(as.integer(input$is_elevator))
      total_units = as.numeric(input$total_units)
      year_built = as.numeric(input$year_built)
      gross_sq_ft = as.numeric(input$gross_sq_ft)
      report_year = as.numeric(input$report_year)
      
      new_data = tibble(
        is_elevator = is_elevator,
        total_units = total_units,
        year_built = year_built,
        gross_sq_ft = gross_sq_ft,
        report_year = report_year,
        longitude = location$longitude[1],
        latitude = location$latitude[1],
      )
      
      predicted = predict_rental(new_data)
      
      output$result <- renderText({
        paste0("Your gross rent income per suqare feet is ", as.character(round(predicted,2)))
        #as.character(location)
      })
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
