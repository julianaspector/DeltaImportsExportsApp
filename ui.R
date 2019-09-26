#ui.R


ui <- fluidPage(
  tabsetPanel(
  tabPanel(title="Maps",
  titlePanel("Delta Water Imports and Exports"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("sliders")),
    mainPanel(
    fluidRow(
      column(4, leafletOutput("map_one")),
      column(4, leafletOutput("map_two")),
      column(4, leafletOutput("map_three"))),
    fluidRow(
      column(6, leafletOutput("map_four")),
      column(6, leafletOutput("map_five")))
    )
  )
  ),
  tabPanel(title="Water Year Information",
           fluidRow(
             column(4, tableOutput("table_one")),
             column(4, tableOutput("table_two")),
             column(4, tableOutput("table_three")),
             column(4, tableOutput("table_four"))
            )
  )
  )
)
      
    
