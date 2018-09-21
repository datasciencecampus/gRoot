#'@title data_in_view
#'  
#'@description subsets the data by given coordinates.
#'  
#'@details This function can be used to subset tabular data using coordinates 
#'  from map data. Developed for use with leaflet maps.
#'  
#'@param my_data a dataframe with a \code{lon}(longitude) and \code{lat} 
#'  (latitude) column.
#'@param map_coords this should be a list of four numbers, in order: North, 
#'  East, South, West. \code{input$map_bounds} outputs in this order. The
#'  assumption is made that these numbers represent the top left corner and
#'  bottom right corner. 
#'@return a new dataframe only containing data within the given coordinates.
#'@author Joe Peskett
#'@export
data_in_view <- function(my_data, map_coords){
    data = my_data[my_data$lon < map_coords[2]
                   & my_data$lon > map_coords[4]
                   & my_data$lat <map_coords[1]
                   & my_data$lat > map_coords[3],]
}
#' 
#' @title tree_plot
#'   
#' @description  Area plot to show amount of variable on two sides of a road.
#'   
#' @details  Data is manipulated using dplyr::mutate, adds a new column by 
#'   multiplying the second variable by 1. Then, subsets by the 
#'   \code{column_name} that has been selected in \code{column_filter}. The 
#'   final bit of manipulation uses melt from the reshape2 package to transform 
#'   dataframe into long format which is required for ggplot2.
#'   
#'   The end product of this is used as the data for ggplot2. Add the 
#'   aesthetics, geoms and labels required for the plot.
#'   
#'   This function was created for use with the city profile prototype however 
#'   has been adjusted so that other data sets with alternative column names may
#'   also be chosen.
#'   
#'   Note that currently the x axis used is the absolute value of longitude * 
#'   lattitude. This is an quick and ultimately flawed attempt to deal with the 
#'   fact with the fact that roads are rarely completely straight or all facing 
#'   the same direction.
#'   
#' @param my_data The dataframe you would like to plot. Default is tree_points.
#' @param column_name The name of column you want to subset the data by. Default
#'   is road.
#' @param column_filter The value of the selected column to filter the data by.
#' @param plotting_vars This should be a character vector of length 2. These are
#'   the variables that will be plotted above and below. The first value will be
#'   plotted above zero, the second will be plotted below. They should be in
#'   approximately in the same scale. Default is left_green and right_green. 
#' @param lon_val the value for longitude. Default is lon.
#' @param lat_val the value for lattitude. Default is lat.
#' @return A ggplot graphic showing left side of the road above the line, right 
#'   side on the bottom of the graph.
#' @author Joe Peskett
#' @export

tree_plot <- function(my_data = tree_points, column_name = road, column_filter, 
                      plotting_vars = c("left_green", "right_green"), 
                      lon_val = "lon", lat_val = "lat"){
  require(ggplot2)
  require(dplyr)
  require(reshape2)
  ggplot(data = 
           my_data %>%
           mutate(inv_val = eval(as.symbol(plotting_vars[2])) * -1, 
                  id = abs(seq)) %>%
           filter(!!enquo(column_name) == column_filter) %>%
           melt(id.vars = "id", measure.vars = c(plotting_vars[1], "inv_val"),
                variable.name = "side"),aes(x=id, y = value)) + 
    geom_area(aes(fill = side)) +
    geom_hline(yintercept = 0) +
    ggtitle(paste("Values along", as.character(column_filter))) +
    xlab("Recording Number") +ylab("Green Value") +
    ylim(c(-1, 1)) +
    theme_classic()
}
#' @title tree_hist
#'   
#' @description  Creates a histogram of data that falls within a given bounding 
#'   box
#'   
#' @details For this function it is important that columns of the data frame are
#'   correctly labeled "x_coord" and "y_coord" and that in map_coords the order
#'   of coordinates is correct. This is used to subset my_data
#'   
#'   my_data is then placed as the data argument in ggplot2. \$max_green is 
#'   selected for the aesthetic and style arguments are given to geom_histogram.
#'   
#'   Note: tree_hist currently throws a small error in the console - this is 
#'   because there is a log1- transformation causing some non-finite values.
#'   
#' @param my_data A dataframe containing tree points. This should have columns 
#'   for max_green, x_coord and y_coord.
#' @param map_coords A list containing 4 coordinates, representing the top left 
#'   and bottom right points of a bounding box.
#' @return A histogram of data that lies within the defined bounding box.
#' @author Joe Peskett
#' @export
tree_hist <- function(my_data, map_coords, column_name = max_green){
  require(ggplot2)
  ggplot(data = data_in_view(my_data, map_coords), 
         aes(!!enquo(column_name))) +
    geom_histogram(fill = "green", alpha = 0.8, bins = 50) +
    labs(title = "Histogram of data in  map view") +
    labs(x="Value", y= "Freq")

}

#' @title  tree_table
#'   
#' @description  Creates a table that displays the data falls within a given 
#'   bounding box
#'   
#' @details This function first subsets 'my_data' by comparing 'x_coords' and 
#'   'y_coords'with 'map_coords'. It is important that  columns of the data 
#'   frame are correctly labeled "x_coord" and "y_coord" and that in map_coords 
#'   the order of coordinates is correct. This should be in a list.
#'   
#'   The data is then grouped by \code{col_name}. A summary of this data is then
#'   create to show number of records, mean left_green, mean right_green and 
#'   mean max_green. Note: It is important to note that these summaries will be 
#'   for whatever is currently in view of the map. For particularly long roads, 
#'   the summary statistics will be for stretch of road that is in view of the 
#'   map.
#' @param my_data A dataframe the full data set to be displayed. Default is 
#'   tree_points.
#' @param  col_name The name of the column to group by. Usually this will be 
#'   "road" or "parish"
#' @param map_coords this should be a list of four numbers, in order: North, 
#'   East, South, West. \code{input$map_bounds} outputs in this order. The 
#'   assumption is made that these numbers represent the top left corner and 
#'   bottom right corner.
#' @param col_1 a column from the data set to be summarised. Default is max_green.
#' @param col_2 a column from the data set to be summarised. Default is left_green.
#' @param col_3 a column from the data set to be summarised. Default is right_green.
#' @return A new dataframe that can then be rendered in a shiny app. Columns
#'   values will just be named 1-3.
#'   
#' @author Joe Peskett
#' @export
tree_table <- function(my_data = tree_points, col_name, map_coords, 
                       col_1 = max_green, col_2 = left_green, 
                       col_3 = right_green){
  require(dplyr)
  #data_in_view(my_data, map_coords)%>%
    my_data %>%
    group_by(!!enquo(col_name)) %>%
    summarise(Records = n(),
              column_1 = round(mean(!!enquo(col_1)), digits = 3),
              column_2 = round(mean(!!enquo(col_2)), digits = 3),
              column_3 = round(mean(!!enquo(col_3)), digits = 3))
}

#' @title tree_hex_map
#'   
#' @description Creates a hexmap for Cardiff - the data for this is stored 
#'   within a object. The script for making this will be included in the 
#'   raw_data
#'   
#' @details Takes data found within the CityProfileR package. Needs to be passed
#'   new_col or total_quantile
#'   
#' @param data This MUST be the hex shapefile supplied with 
#'   CityProfileR(default). Other similar files may be created using the 
#'   supplied script.
#'   
#' @param level What data should be displayed on the hexmap. Currently this 
#'   should be new_col or total_quantile. Can also be any other quantitative
#'   value.
#' @param palette_options Any palette that will take quantitative values. 
#'   
#' @return a hexmap displaying the requested information.
#' @author Joe Peskett
#' @export
tree_hex_map <- function(data, level, palette_options = "Greens"){
    ggplot(data) +
        geom_polygon( aes(x=long, y=lat, fill = level, group = group)) +
        ggtitle(label = "Hexagon plot of PSPnet Greenery", 
                subtitle = "Ranks split into 9 bins") +
        coord_equal() +
        scale_fill_brewer(palette = palette_options, 
                          direction = 1, 
                          type = "seq", 
                          na.value = "grey50", 
                          guide_legend(title = "Key")) +
        theme_void()
}

#' @title tree_road_shape
#'   
#' @description Creates a plot where size and colour are based on the max_green 
#'   value. X and Y are taken as the lon and lat values.
#'   
#' @details Data is found within the package CityProfileR. Can take the argument
#'   to specify a street.
#'   
#' @param data Dateframe containing geospatial data. Should have columns 
#'   containing longitude and lattitude data. Default is tree_points.
#' @param column_name the column that you want to filter data by. Default is
#'   road.
#' @param column_filter a value from the specified column that you want to keep.
#' @param colour_adjust a variable in the dataset that you want to vary the
#'   colours of points by. Default is max_green.
#' @param size_adjust a variable in the dataset that you want to vary the size
#'   of points by. Default is max_green.
#' @return a ggplot with the shape of the selected street.
#' @author Joe Peskett
#' @export
tree_road_shape <- function(data = tree_points, column_name = road, 
                            column_filter, colour_adjust = way_id, 
                            size_adjust = max_green){
    require(ggplot2)
    require(dplyr)
    ggplot(data = data %>% 
               filter(!!enquo(column_name) == column_filter) %>% 
               mutate(way_find = factor(x = way_id, labels = c(1:n_distinct(way_id)))), 
           aes(x = lon, y=lat))+
        geom_point(aes(col = !!enquo(colour_adjust), size = !!enquo(size_adjust)))+
        scale_color_brewer(palette = "Paired") +
        theme_void()
}

##' tree_plotly
##' 
##' DNA plot of selected road, broken down by way_id. 
##' 
##' @author Joe Peskett
##' @export
tree_plotly <- function(my_data = tree_points, column_name = road, column_filter){
    require(dplyr)
    require(ggplot2)
    require(plotly)
    require(reshape2)
    test <- my_data %>% filter(!!enquo(column_name) == column_filter) %>% 
        mutate(length = 1, way_find = factor(x = way_id, labels = c(1:n_distinct(way_id))))
    
    test_mut <- melt(test, id.vars = c("seq", "way_find", "parish", "road", "lat", "lon"),measure.vars = "max_green") %>% mutate(length = 1)
    
    g <- ggplot(test_mut, aes(x=way_find, y=length, fill = value, label = seq)) + geom_bar(stat = "identity") + 
        scale_fill_continuous(low = "khaki", high = "green4") + coord_flip() + facet_wrap(~road)
    gg <- ggplotly(g)
    gg
}
