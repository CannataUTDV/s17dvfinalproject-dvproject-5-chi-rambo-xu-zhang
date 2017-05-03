# server.R
require(ggplot2)
require(dplyr)
require(shiny)
require(shinydashboard)
require(data.world)
require(readr)
require(DT)
require(leaflet)
require(plotly)
require(lubridate)

online0 = TRUE

# The following query is for the select list in the Boxplots -> Simple Boxplot tab, and Barcharts -> Barchart with Table Calculation tab.
if(online0) {
  locations = query(
    data.world(propsfile = "www/.data.world"),
    dataset="andyzhang/final-project", type="sql",
    query="select distinct LocationType as D, LocationType as L
    from MassShooting
    order by 1"
  )
  # View(locations)
} else {
  print("Getting Locations from csv")
  file_path = "www/MassShooting.csv"
  df <- readr::read_csv(file_path) 
  tdf1 = df %>% dplyr::distinct(LocationType) %>% arrange(LocationType) %>% dplyr::rename(D = LocationType)
  tdf2 = df %>% dplyr::distinct(LocationType) %>% arrange(LocationType) %>% dplyr::rename(L = LocationType)
  locations = bind_cols(tdf1, tdf2)
}
location_list <- as.list(locations$D, locations$L)
location_list <- append(list("All" = "All"), location_list)
location_list5 <- location_list

# The following queries are for the Barcharts -> High Fatality Cases tab data.
if(online0) {
# Step 1:
  highFatalities <- query(
  data.world(propsfile = "www/.data.world"),
  dataset="andyzhang/final-project", type="sql",
  query="
  SELECT distinct Year, sum(Fatalities) as sumFatalities, sum(NumWeapons) as sumNumWeapons
  FROM MassShooting
  group by Year
  having sum(Fatalities) > 35"
)
  # View(highFatalities )

# Step 2
  highFatalitiesCase <- query(
    data.world(propsfile = "www/.data.world"),
    dataset="andyzhang/final-project", type="sql",
    query="
    SELECT distinct Year, `Case`, City, State
    FROM MassShooting
    where year(Year) in (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    order by Year",
    queryParameters = highFatalities$Year
  )
    # View(highFatalitiesCase)
    
# Step 3
    stateAbreviations <- query(
      data.world(propsfile = "www/.data.world"),
      dataset="andyzhang/final-project", type="sql",
      query="SELECT distinct State, Abbreviation
      FROM StateAbbrev
      where State in (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      order by State",
      queryParameters = highFatalitiesCase$State
    )
    # View(stateAbreviations )
    
# Step 4
    highFatalitiesCase2 <- left_join(highFatalitiesCase,
                                        stateAbreviations, by="State")
    # View(highFatalitiesCase2)
    
# Step 5
    longLat <- query(
      data.world(propsfile = "www/.data.world"),
      dataset="andyzhang/final-project", type="sql",
      query="SELECT distinct City, State as Abbreviation, Latitude, Longitude
      FROM LatLong
      where City in (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      order by City",
      queryParameters = highFatalitiesCase$City
    )
    # View(longLat)
    
# Step 6
    highFatalitiesCase2LongLat <- 
      inner_join(highFatalitiesCase2, longLat, by = c("City", "Abbreviation")) 
    # View(highFatalitiesCase2LongLat)
    
# Step 7
    fatalities <- 
      inner_join(highFatalitiesCase2LongLat, highFatalities, by="Year")
    # View(fatalities)
}

# The following query is for the Barcharts -> High Total Victims Case tab data.
if(online0) {
  # Step 1:
  highFatalities <- query(
    data.world(propsfile = "www/.data.world"),
    dataset="andyzhang/final-project", type="sql",
    query="
    SELECT distinct Year, sum(Fatalities) as sumFatalities
    FROM MassShooting
    group by Year
    having sum(Fatalities) > 35"
  )
  # View(highFatalities)
  
  # Step 2
  totalVictims <- query(
    data.world(propsfile = "www/.data.world"),
    dataset="andyzhang/final-project", type="sql",
    query="
    select `Case`, sum(TotalVictims) as sumTotalVictims
    FROM MassShooting
    where year(Year) in (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    group by `Case`",
    queryParameters = highFatalities$Year
    )
  # View(totalVictims)
} else {
  print("Getting Total Victims from csv")
  file_path = "www/MassShooting.csv"
  df <- readr::read_csv(file_path) 
  
  # Step 1
  highFatalities <- df %>% dplyr::group_by(Year) %>% dplyr::summarize(sumFatalities = sum(Fatalities)) %>% dplyr::filter(sumFatalities >= 30)
  # View(highFatalities)
  
  # Step 2
  totalVictims <- df %>% dplyr::filter(Year %in% highDiscounts$Year) %>% dplyr::select(Case, City, State, Year, TotalVictims) %>% dplyr::group_by(Case, City, State, Year) %>% dplyr::summarise(sumTotalVictims = sum(TotalVictims))
  # View(totalVictims)
}

############################### Start shinyServer Function ####################

shinyServer(function(input, output) {   
  # These widgets are for the Box Plots tab.
  online5 = reactive({input$rb5})
  output$BoxplotLocations <- renderUI({selectInput("selectedBoxplotLocations", "Choose Locations:", location_list5, multiple = TRUE, selected='All') })
  
  # These widgets are for the Histogram tab.
  online4 = reactive({input$rb4})
  
  # These widgets are for the Scatter Plots tab.
  online3 = reactive({input$rb3})
  
  # These widgets are for the Crosstabs tab.
  online1 = reactive({input$rb1})
  FatalRate_Low = reactive({input$KPI1})     
  FatalRate_Medium = reactive({input$KPI2})
  
  # These widgets are for the Barcharts tab.
  online2 = reactive({input$rb2})
  output$locations2 <- renderUI({selectInput("selectedLocations", "Choose Locations:", location_list, multiple = TRUE, selected='All') })
  
  # Begin Box Plot Tab ------------------------------------------------------------------
  dfbp1 <- eventReactive(input$click5, {
    if(input$selectedBoxplotLocations == 'All') location_list5 <- input$selectedBoxplotLocations
    else location_list5 <- append(list("Skip" = "Skip"), input$selectedBoxplotLocations)
    if(online5() == "SQL") {
      print("Getting from data.world")
      df <- query(
        data.world(propsfile = "www/.data.world"),
        dataset="andyzhang/final-project", type="sql",
        query="select Race, TotalVictims, LocationType, Year
        from MassShooting
        where (? = 'All' or LocationType in (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?))",
        queryParameters = location_list5)
    }
    else {
      print("Getting from csv")
      file_path = "www/MassShooting.csv"
      df <- readr::read_csv(file_path)
      df %>% dplyr::select(Race, TotalVictims, LocationType, Year) %>% dplyr::filter(LocationType %in% input$selectedBoxplotLocations | input$selectedBoxplotLocations == "All")
    }
    })
  
  output$boxplotData1 <- renderDataTable({DT::datatable(dfbp1(), rownames = FALSE,
                                                extensions = list(Responsive = TRUE, 
                                                FixedHeader = TRUE)
  )
  })
  
  dfbp2 <- eventReactive(c(input$click5, input$boxTotalVictimsRange1), {
    dfbp1() %>% dplyr::filter(TotalVictims >= input$boxTotalVictimsRange1[1] & TotalVictims <= input$boxTotalVictimsRange1[2])
  })
    
  output$boxplotPlot1 <- renderPlotly({
    p <- ggplot(dfbp2()) + 
      geom_boxplot(aes(x=Race, y=TotalVictims, colour=LocationType)) + 
      ylim(0, input$boxTotalVictimsRange1[2]) +
      theme(axis.text.x=element_text(angle=90, size=10, vjust=0.5))
    ggplotly(p)
  })
  # End Box Plot Tab ___________________________________________________________
   
  # Begin Histgram Tab ------------------------------------------------------------------
  dfh1 <- eventReactive(input$click4, {
    if(online4() == "SQL") {
      print("Getting from data.world")
      query(
        data.world(propsfile = "www/.data.world"),
        dataset="andyzhang/final-project", type="sql",
        query="select NumWeapons, MentalIllness, LegalWeapon
        from MassShooting
        where MentalIllness = 'true' and LegalWeapon = 'true'"
      )
    }
    else {
      print("Getting from csv")
      file_path = "www/SuperStoreOrders.csv"
      df <- readr::read_csv(file_path)
      df %>% dplyr::select(NumWeapons, MentalIllness, LegalWeapon) %>% dplyr::filter(MentalIllness == 'true' & LegalWeapon == 'true')
    }
    })
  
  output$histogramData1 <- renderDataTable({DT::datatable(dfh1(), rownames = FALSE,
                                                  extensions = list(Responsive = TRUE, 
                                                  FixedHeader = TRUE)
  )
  })
  
  output$histogramPlot1 <- renderPlotly({p <- ggplot(dfh1()) +
      geom_histogram(aes(x=NumWeapons)) +
      theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5))
      ggplotly(p)
  })
  # End Histogram Tab ___________________________________________________________
  
  # Begin Scatter Plots Tab ------------------------------------------------------------------
  dfsc1 <- eventReactive(input$click3, {
    if(online3() == "SQL") {
      print("Getting from data.world")
      query(
        data.world(propsfile = "www/.data.world"),
        dataset="andyzhang/final-project", type="sql",
        query="select Fatalities, NumWeapons, State, LocationType
        from MassShooting"
      )
    }
    else {
      print("Getting from csv")
      file_path = "www/MassShooting.csv"
      df <- readr::read_csv(file_path)
      df %>% dplyr::select(Fatalities, NumWeapons, State, LocationType)
    }
  })
  output$scatterData1 <- renderDataTable({DT::datatable(dfsc1(), rownames = FALSE,
                                                 extensions = list(Responsive = TRUE, 
                                                 FixedHeader = TRUE)
  )
  })
  output$scatterPlot1 <- renderPlotly({p <- ggplot(dfsc1()) + 
      theme(axis.text.x=element_text(angle=90, size=16, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=16, hjust=0.5)) +
      geom_point(aes(x=Fatalities, y=NumWeapons, colour=LocationType), size=2)
      ggplotly(p)
  })
  # End Scatter Plots Tab ___________________________________________________________
  
# Begin Crosstab Tab ------------------------------------------------------------------
  dfct1 <- eventReactive(input$click1, {
      if(online1() == "SQL") {
        print("Getting from data.world")
        query(
            data.world(propsfile = "www/.data.world"),
            dataset="andyzhang/final-project", type="sql",
            query="select LocationType, State, 
            sum(Fatalities) as sumFatalities, 
            sum(TotalVictims) as sumTotalVictims, 
            sum(Fatalities) / sum(TotalVictims) as FatalRate,
            
            case
            when sum(Fatalities) / sum(TotalVictims) < ? then 'Low Fatal Rate'
            when sum(Fatalities) / sum(TotalVictims) < ? then 'Medium Fatal Rate'
            else 'High Fatal Rate'
            end AS KPI
            
            from MassShooting
            where LocationType in ('Workplace', 'Military', 'Religious', 'School')
            group by LocationType, State
            order by LocationType, State",
            queryParameters = list(FatalRate_Low(), FatalRate_Medium())
          )
      }
      else {
        print("Getting from csv")
        file_path = "www/MassShooting.csv"
        df <- readr::read_csv(file_path)
        df %>% 
          dplyr::filter(LocationType %in% c('Workplace', 'Military', 'Religious', 'School')) %>%
          dplyr::group_by(LocationType, State) %>% 
          dplyr::summarize(sumFatalities = sum(Fatalities), sumTotalVictims = sum(TotalVictims),
                           FatalRate = sum(Fatalities) / sum(TotalVictims),
                           KPI = if_else(FatalRate <= FatalRate_Low(), '03 Low',
                           if_else(FatalRate <= FatalRate_Medium(), '02 Medium', '01 High')))
      }
  })
  output$data1 <- renderDataTable({DT::datatable(dfct1(), rownames = FALSE,
                                extensions = list(Responsive = TRUE, FixedHeader = TRUE)
  )
  })
  output$plot1 <- renderPlot({ggplot(dfct1()) + 
    theme(axis.text.x=element_text(angle=90, size=16, vjust=0.5)) + 
    theme(axis.text.y=element_text(size=16, hjust=0.5)) +
    geom_text(aes(x=LocationType, y=State, label=sumTotalVictims), size=6) +
    geom_tile(aes(x=LocationType, y=State, fill=KPI), alpha=0.50)
  })
# End Crosstab Tab ___________________________________________________________
  
# Begin Barchart Tab ------------------------------------------------------------------
  dfbc1 <- eventReactive(input$click2, {
    if(input$selectedLocations == 'All') region_list <- input$selectedLocations
    else location_list <- append(list("Skip" = "Skip"), input$selectedLocations)
    if(online2() == "SQL") {
      print("Getting from data.world")
      tdf = query(
        data.world(propsfile = "www/.data.world"),
        dataset="andyzhang/final-project", type="sql",
        query="select Race, LocationType, sum(TotalVictims) sumTotalVictims
                from MassShooting
                where ? = 'All' or LocationType in (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                group by Race, LocationType",
        queryParameters = location_list
      )
      # View(tdf)
    }
    else {
      print("Getting from csv")
      file_path = "www/MassShooting.csv"
      df <- readr::read_csv(file_path)
      tdf = df %>% dplyr::filter(Location %in% input$selectedLocations | input$selectedLocations == "All") %>%
        dplyr::group_by(Race, LocationType) %>% 
        dplyr::summarize(sumTotalVictims = sum(TotalVictims))
    }
    # The following two lines mimic what can be done with Analytic SQL. Analytic SQL does not currently work in data.world.
    tdf2 = tdf %>% group_by(Race) %>% summarize(windowAvgTotalVictims = mean(sumTotalVictims))
     View(tdf2)
    dplyr::inner_join(tdf, tdf2, by = "Race")
  })

  output$barchartData1 <- renderDataTable({DT::datatable(dfbc1(),
                        rownames = FALSE,
                        extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
  })
  output$barchartData2 <- renderDataTable({DT::datatable(fatalities,
                        rownames = FALSE,
                        extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
  })
  output$barchartData3 <- renderDataTable({DT::datatable(totalVictims,
                        rownames = FALSE,
                        extensions = list(Responsive = TRUE, FixedHeader = TRUE) )
  })
  
  output$barchartPlot1 <- renderPlot({ggplot(dfbc1(), aes(x=LocationType, y=sumTotalVictims)) +
      scale_y_continuous(labels = scales::comma) + # no scientific notation
      theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=12, hjust=0.5)) +
      geom_bar(stat = "identity") + 
      facet_wrap(~Race, ncol=1) + 
      coord_flip() + 
      geom_text(mapping=aes(x=LocationType, y=sumTotalVictims, label=round(sumTotalVictims)),colour="black", hjust=-.5) +
      geom_text(mapping=aes(x=sumTotalVictims, y=sumTotalVictims, label=round(sumTotalVictims - windowAvgTotalVictims)),colour="blue", hjust=-2) +
      # Add reference line with a label.
      geom_hline(aes(yintercept = round(windowAvgTotalVictims)), color="red") +
      geom_text(aes( -1, windowAvgTotalVictims, label = windowAvgTotalVictims, vjust = -.5, hjust = -.25), color="red")
  })
  
  output$barchartMap1 <- renderLeaflet({leaflet(data = fatalities) %>% 
    setView(lng = -98.35, lat = 39.5, zoom = 4) %>% 
    addTiles() %>% 
    addMarkers(lng = ~Longitude, lat = ~Latitude,
      options = markerOptions(draggable = TRUE, riseOnHover = TRUE),
      popup = ~as.character(paste(Case, ", ", 
                                  City, ", ", 
                                  State, ", ",
                                  " Fatalities: ", sumFatalities, ", ",
                                  " Number of Weapons: ", sumNumWeapons))
      )
  })
  
  output$barchartPlot2 <- renderPlotly({
    # The following ggplotly code doesn't work when sumProfit is negative.
    p <- ggplot(totalVictims, aes(x=Case, y=sumTotalVictims)) +
      theme(axis.text.x=element_text(angle=90, size=8, vjust=0.5)) + 
      theme(axis.text.y=element_text(size=8, hjust=0.5)) +
      geom_bar(stat = "identity")
    ggplotly(p)
  # End Barchart Tab ___________________________________________________________
  })
})
