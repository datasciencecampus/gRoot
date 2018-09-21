server <- function(input, output){
    
    green_op <- eventReactive(eventExpr = input$green_opacity, 
                              {input$green_opacity*1}, 
                              ignoreNULL = FALSE)
    selected_par <- eventReactive(eventExpr = input$parishselect, 
                                  {tree_points[tree_points$parish==input$parishselect,]}, 
                                  ignoreNULL = FALSE)
    output$map <- renderLeaflet({
        leaflet(height = 800) %>%
            addProviderTiles(providers$OpenStreetMap.BlackAndWhite, group = "leaf")%>%
            setView(lat=51.5,
                    lng=-3.16,
                    zoom=13)%>%
            addMapPane("polygons", zIndex = 410)%>%
            addMapPane("points", zIndex = 420)%>%
            addPolygons(data = tree_polygons, stroke = TRUE, weight = 1.5,
                        color= "Green", fillOpacity = 0.4,
                        group = "Parish Polygons",
                        fillColor = ~pal1(log10(green_vector)),
                        label = ~paste0(par17nm), options = pathOptions(pane = "polygons"))%>%
            addCircles(data = tree_points %>% filter(parish == "Adamsdown"), 
                       lng = ~lon, 
                       lat = ~lat, 
                       label = ~paste(road, seq, way_id), group = "Street Marks", 
                       color = ~pal_street(max_green), 
                       popup = ~paste0('<a href="http://maps.google.com/maps?q=&layer=c&cbll=', 
                                       lat,",", lon,'">Google Maps</a>'), 
                       options = pathOptions(pane = "points"))%>%
            addCircles(data = tree_points %>% 
                           filter(parish == "Adamsdown")%>%
                           filter(road == "Broadway"),
                       lng = ~lon,
                       lat = ~lat,
                       label = ~paste(road, seq, way_id), group = "circles_1", 
                       color = ~pal_street(max_green), 
                       popup = ~paste0('<a href="http://maps.google.com/maps?q=&layer=c&cbll=', 
                                       lat,",", lon,'">Google Maps</a>'))%>%
            addLayersControl(
                overlayGroups = c("Parish Polygons", "Street Marks"),
                options = layersControlOptions(collapsed = FALSE)) %>% 
            hideGroup("Street Marks")})
    
    observe({leafletProxy("map")%>%
            clearGroup("Parish Polygons")%>%
            addPolygons(data = tree_polygons, stroke = TRUE, weight = 1.5,
                        color= "Green", fillOpacity = green_op(),
                        group = "Parish Polygons",
                        fillColor = ~pal1(log10(green_vector)),
                        label = ~paste0(par17nm), options = pathOptions(pane = "polygons"))%>%
            clearGroup("Street Marks")%>%
            addCircles(data = selected_par(), 
                       lng = ~lon, 
                       lat = ~lat, 
                       label = ~paste(road, seq, way_id), group = "Street Marks", 
                       color = ~pal_street(max_green), 
                       popup = ~paste0('<a href="http://maps.google.com/maps?q=&layer=c&cbll=', 
                                       lat,",", lon,'">Google Maps</a>'), 
                       options = pathOptions(pane = "points"))
        
    })
    
    #This is the selector for the road select.      
    road_choice <- eventReactive(input$parishselect, {tree_points$road[tree_points$parish==input$parishselect]}, ignoreNULL = F)
    output$road_1 <- renderUI({
        selectInput(inputId = "roadselect_1", label = "Road_1:", 
                    choices = as.character(
                        road_choice()), 
                    selected = 1)
    })
    output$road_2 <- renderUI({
        selectInput(inputId = "roadselect_2", label = "Road_2:", 
                    choices = as.character(
                        road_choice()), 
                    selected = 4)
    })
    #This output is the text for bounding box and zoom level.
    output$hist <- renderPlot(tree_hist(my_data = tree_points, 
                                        map_coords = input$map_bounds))
    #note:histogram errors are due to non-finite numbers due to log10
    #transformation 
    output$hex1 <- renderPlot(expr = ggplot(tree_hex) +
                                  geom_polygon( aes(x=long, y=lat, fill = new_col, group = group)) +
                                  #geom_text(aes(V1, V2, label = substr(lsoa11cd,4,8)), size=2,color = "black") +
                                  ggtitle(label = "Hexagon plot of PSPnet Greenery", subtitle = "Ranks split into 9 bins") +
                                  coord_equal() +
                                  scale_fill_brewer(palette = pal2, direction = 1,type = "seq", na.value = "grey50", guide_legend(title = "Key")) +
                                  theme_void())
    
    output$hex2 <- renderPlot(expr = ggplot(tree_hex) +
                                  geom_polygon( aes(x=long, y=lat, fill = total_quantile, group = group)) +
                                  #geom_text(aes(V1, V2, label = substr(lsoa11cd,4,8)), size=2,color = "black") +
                                  ggtitle(label = "Hexagon plot of NRW Greenery", subtitle = "Ranks split into 9 bins") +
                                  coord_equal() +
                                  scale_fill_brewer(palette = pal2, direction = 1,type = "seq", na.value = "grey50", guide_legend(title = "Key")) +
                                  theme_void())
    #-----------------------------------------------------------------------------
    
    #The following observe events deal with plotting the street level graph and
    #placing the road marker based on input$roadselect
    
    observeEvent(
        eventExpr = input$roadselect_1, 
        handlerExpr = {
            leafletProxy("map") %>%
                clearGroup("circles_1")
            print("Attempt to clear Marks_1")
        })
    
    observeEvent(
        eventExpr = input$roadselect_1, 
        handlerExpr = {
            leafletProxy("map") %>%
                clearMarkers() %>%
                addCircles(data = tree_points%>%filter(parish==input$parishselect)%>%filter(road==input$roadselect_1),
                           lng = ~lon, lat = ~lat, 
                           label = ~paste(road, seq, way_id), group = "circles_1", color = ~pal_street(max_green),
                           popup = ~paste0('<a href="http://maps.google.com/maps?q=&layer=c&cbll=', 
                                           lat,",", lon,'">Google Maps</a>'),
                           options = pathOptions(pane = "points"))
            print("Placing new road markers_1")
        })
    
    observeEvent(
        eventExpr = input$roadselect_1,
        handlerExpr = {
            print("Graph Activated")
            output$plot <- renderPlotly(expr =  tree_plotly(column_filter = as.character(input$roadselect_1)))
            output$shape_1 <- renderPlot(expr = tree_road_shape(data = tree_points, 
                                                                column_filter = input$roadselect_1, 
                                                                colour_adjust = way_find))
        })
    
    #============================================================================
    observeEvent(
        eventExpr = input$roadselect_2, 
        handlerExpr = {
            leafletProxy("map") %>%
                clearGroup("circles_2")
            print("Attempt to clear Marks_2")
        })
    
    observeEvent(
        eventExpr = input$roadselect_2, 
        handlerExpr = {
            leafletProxy("map") %>%
                clearMarkers() %>%
                addCircles(data = tree_points%>%filter(parish==input$parishselect)%>%filter(road==input$roadselect_2), 
                           lng = ~lon, lat = ~lat, 
                           label = ~paste(road, seq, way_id), group = "circles_2", color = ~pal_street(max_green),
                           popup = ~paste0('<a href="http://maps.google.com/maps?q=&layer=c&cbll=', lat,",", lon,'">Google Maps</a>'),
                           options = pathOptions(pane = "points"))
            print("Placing new road markers_2")
        })
    #=================================================================================
    
}