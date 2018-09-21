ui <- dashboardPage(
  dashboardHeader(title = "City Profiler"),
  dashboardSidebar(selectInput(inputId = "parishselect", 
                               choices = tree_polygons$par17nm, 
                               multiple = FALSE, 
                               label = "Select a Parish", selected = "Adamsdown"),
                   sliderInput(inputId = "green_opacity", label = "Green Opacity", 
                               min = 0, max = 1, value = 0.4, ticks = TRUE),
                   uiOutput(outputId = "road_1"),
                   uiOutput(outputId = "road_2")),
  
  dashboardBody(
    fluidRow(
      tabBox(id = "maptab", title = "City Profile for Urban Forest", selected = 1,height = "800px",width = 12,
             tabPanel(tabName = "Tab 0", title = "Info", status = "primary", includeMarkdown("welcome.Rmd"), value = 1),
             tabPanel(tabName = "Tab 1",  title = "Map", 
                      tags$style(type = "text/css", "#map {height: 800px !important;}"),
                      leafletOutput("map"), value = 7),
             tabPanel(tabName = "Tab 2", title = "Hexmaps", plotOutput("hex1"), plotOutput("hex2"), value = 2),
             #tabPanel(tabName = "Tab 1", title = "Summary table of green levels in map view", value = 3,
             #         DT::dataTableOutput("table")),
             tabPanel(tabName = "Plot", title = "Street-level Green View", value = 4,
                      plotOutput(outputId = "shape_1"),
                      plotlyOutput(outputId = "plot")),
             tabPanel(tabname = "Tab 3", title = "Distribution of Green in map view", value = 5,
                      tags$style(type = "text/css", "#hist {height: 800px !important;}"),
                      plotOutput(outputId = "hist"))#,
             #tabPanel(tabname = "streetshape", title = "Street Shape", value = 6, 
                      #plotOutput(outputId = "shape_1"),
                      #plotOutput(outputId = "shape_2"))
    )
  )
)
)