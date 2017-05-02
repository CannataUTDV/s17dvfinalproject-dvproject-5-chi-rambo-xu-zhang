# ui.R
require(shiny)
require(shinydashboard)
require(DT)
require(leaflet)
require(plotly)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Box Plots", tabName = "boxplot", icon = icon("dashboard")),
      menuItem("Histograms", tabName = "histogram", icon = icon("dashboard")),
      menuItem("Scatter Plots", tabName = "scatter", icon = icon("dashboard")),
      menuItem("Crosstabs, KPIs, Parameters", tabName = "crosstab", icon = icon("dashboard")),
      menuItem("Barcharts, Table Calculations", tabName = "barchart", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),    
    tabItems(
      # Begin Box Plots tab content.
      tabItem(tabName = "boxplot",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb5", "Get data from:",
                                      c("SQL" = "SQL",
                                        "CSV" = "CSV"), inline=T),
                         uiOutput("BoxplotLocations"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html,
                         actionButton(inputId = "click5",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("boxplotData1")
                ),
                tabPanel("Box Plot", 
                         sliderInput("boxTotalVictimsRange1", "Total Victims Range:", # See https://shiny.rstudio.com/articles/sliders.html
                                     min = min(globals$TotalVictims), max = max(globals$TotalVictims), 
                                     value = c(min(globals$TotalVictims), max(globals$TotalVictims))),
                         plotlyOutput("boxplotPlot1", height=500))
              )
      ),
      # End Box Plots tab content.
      # Begin Histogram tab content.
      tabItem(tabName = "histogram",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb4", "Get data from:",
                                      c("SQL" = "SQL",
                                        "CSV" = "CSV"), inline=T),
                         actionButton(inputId = "click4",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("histogramData1")
                ),
                tabPanel("Histogram", plotlyOutput("histogramPlot1", height=1000))
              )
      ),
      # End Histograms tab content.
      # Begin Scatter Plots tab content.
      tabItem(tabName = "scatter",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb3", "Get data from:",
                                      c("SQL" = "SQL",
                                        "CSV" = "CSV"), inline=T),
                         uiOutput("scatterStates"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html,
                         actionButton(inputId = "click3",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("scatterData1")
                ),
                tabPanel("Scatter Plot", plotlyOutput("scatterPlot1", height=1000))
              )
      ),
      # End Scatter Plots tab content.
      # Begin Crosstab tab content.
      tabItem(tabName = "crosstab",
        tabsetPanel(
            tabPanel("Data",  
              radioButtons("rb1", "Get data from:",
                c("SQL" = "SQL",
                  "CSV" = "CSV"), inline=T),
              sliderInput("KPI1", "FatalRate_Low:", 
                          min = 0, max = .5,  value = .5),
              sliderInput("KPI2", "FatalRate_Medium:", 
                          min = .5, max = .7,  value = .7),
              actionButton(inputId = "click1",  label = "To get data, click here"),
              hr(), # Add space after button.
              DT::dataTableOutput("data1")
            ),
            tabPanel("Crosstab", plotOutput("plot1", height=1000))
          )
        ),
      # End Crosstab tab content.
      # Begin Barchart tab content.
      tabItem(tabName = "barchart",
        tabsetPanel(
          tabPanel("Data",  
             radioButtons("rb2", "Get data from:",
                          c("SQL" = "SQL",
                            "CSV" = "CSV"), inline=T),
             uiOutput("locations2"), # See http://shiny.rstudio.com/gallery/dynamic-ui.html
             actionButton(inputId = "click2",  label = "To get data, click here"),
             hr(), # Add space after button.
             'Here is data for the "Barchart with Table Calculation" tab',
             hr(),
             DT::dataTableOutput("barchartData1"),
             hr(),
             'Here is data for the "High Fatality Cases tab data',
             hr(),
             DT::dataTableOutput("barchartData2"),
             hr(),
             'Here is data for the "High Total Victims Case tab data',
             hr(),
             DT::dataTableOutput("barchartData3")
          ),
          tabPanel("Barchart with Table Calculation", "Black = Sum of Sales per Region, Red = Average Sum of Sales per Category, and  Blue = (Sum of Sales per Region - Average Sum of Sales per Category)", plotOutput("barchartPlot1", height=1500)),
          tabPanel("High Discount Orders", leafletOutput("barchartMap1"), height=900 ),
          tabPanel("High Sales Customers", plotlyOutput("barchartPlot2", height=700) )
        )
      )
      # End Barchart tab content.
    )
  )
)

