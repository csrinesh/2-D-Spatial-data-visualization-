library(shiny)
library(leaflet)
library(RColorBrewer)

# setwd("C:\\Users\\srinesh\\Documents\\R\\Shiny_app")
model_data = read.csv("std_srm_sample.csv")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
          sliderInput(inputId = "time",
                      label ="Time period",
                      min = min(model_data$time_hours),
                      max = max(model_data$time_hours),
                      value = min(model_data$time_hours),
                      step = 6, animate = animationOptions(interval = 5000)),
          selectInput(inputId = "colors", 
                      label =  "Color Scheme",
                      rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                      )
               )
)

server <- function(input, output, session) {
  
  a <- reactive({
    model_data[model_data$time_hours == input$time,]
  })
  
  output$map <- renderLeaflet({
    leaflet(model_data) %>% addTiles() %>%
      fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude)) %>%
      addProviderTiles(providers$Stamen.TerrainBackground)
  })
  
  colorpal <- reactive({
    colorNumeric(input$colors, model_data$max_arg)
  })
  
  observe({
    pal <- colorpal()
    leafletProxy("map", data = a()) %>%
    clearShapes() %>%
    addCircles(radius = ~std_mag*20000, weight = 1, color = "#777777", fillColor = ~pal(max_arg)
                 ,fillOpacity = 0.8, popup = ~paste('std mag = ',std_mag,'\nmax phase diff= ', max_arg)
      )
  })
  
  observe({
    proxy <- leafletProxy("map", data = model_data)
    proxy %>% clearControls()
    pal <- colorpal()
    proxy %>% addLegend(position = "bottomright",
                        pal = pal, values = ~max_arg)
  })
}

shinyApp(ui, server)