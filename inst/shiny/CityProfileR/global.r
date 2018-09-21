#global-file - when used locally with CityProfiler
#packages

library(shinydashboard)
library(leaflet)
library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(gRoot)


#palettes
pal1 <- colorNumeric("Greens", domain = NULL) #note: this currently scales so that the full spectrum is displayed each time 
pal_street <- colorNumeric(palette = "YlGn", domain = seq(0,1,0.1))
pal2 <- brewer.pal(n = 9, name = "YlGn")
pal3 <- brewer.pal(n = 9, name = "YlOrRd")
#function
magic_markers <- function(data = tree_points, input, group_name){
    
    observeEvent(
        eventExpr = input, 
        handlerExpr = {
            leafletProxy("map") %>%
                clearGroup("circles_1")
            print("Attempt to clear Marks")
        })
    
    observeEvent(
        eventExpr = input, 
        handlerExpr = {
            leafletProxy("map") %>%
                clearMarkers() %>%
                addCircles(data = tree_points[tree_points$road==input$roadselect,], 
                           lng = ~lon, lat = ~lat, 
                           label = ~paste0(road, seq), group = "circles_1", color = ~pal1(max_green),
                           popup = ~paste0('<a href="http://maps.google.com/maps?q=&layer=c&cbll=', lat,",", lon,'">Google Maps</a>'))
            print("Placing new road markers")
        })
    
    observeEvent(
        eventExpr = input$roadselect,
        handlerExpr = {
            print("Graph Activated")
            output$plot <- renderPlot(expr =  tree_plot(my_data = tree_points, 
                                                        my_road = as.character(input$roadselect)))
        })
}
