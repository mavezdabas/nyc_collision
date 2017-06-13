#####################################
# Functions #
#####################################


##############################
# Data Frames  #
##############################
topTenAreas <- arrange(aggregate(NUMBER.OF.PERSONS.KILLED, by=list(Area=BOROUGH), FUN=sum),-x)
colnames(topTenAreas) <- c("Region","Count")


# Time Series Data Collision Count
timeSeriesCollisions <- crimeDataCSV %>%
  dplyr::group_by(DATE,Year) %>%
  summarise(number = n())

aggregateCollisionYear <- crimeDataCSV %>%
  dplyr::group_by(Year) %>%
  summarise(number = n())

yearCasualityDF <- setNames(aggregate(crimeDataCSV$NUMBER.OF.PERSONS.KILLED, by = list(Year), FUN = sum),c("Year","Death"))

yearPadestrianDF <- setNames(aggregate(crimeDataCSV$NUMBER.OF.PEDESTRIANS.KILLED, by = list(Year), FUN = sum),c("Year","Death"))



##############################
# Server #
##############################
server <- function(input,output,session) {
  rValues <- reactiveValues()

  
  observe({
    withProgress({
      year <- input$year
      if(year == "2012"){
        yearLineChartDF <- timeSeriesCollisions %>% filter(Year == "2012") %>% select(DATE,number)
        topTenFactorDF <- crimeDataCSV %>% 
          dplyr::filter(Year == "2012") %>%
          dplyr::group_by(CONTRIBUTING.FACTOR.VEHICLE.1) %>%
          summarise(Factor = n())
        topTenFactorDF <- head(topTenFactorDF[order(-topTenFactorDF$Factor),],n = 5)
        
        collisionVB <- dplyr::filter(aggregateCollisionYear,aggregateCollisionYear$Year == "2012")$number
        yearCasualityVB <- dplyr::filter(yearCasualityDF,yearCasualityDF$Year == "2012")$Death
        yearPadestrianVB <- dplyr::filter(yearPadestrianDF,yearPadestrianDF$Year == "2012")$Death
      }else if(year == "2013"){
        yearLineChartDF <- timeSeriesCollisions %>% filter(Year == "2013")
        topTenFactorDF <- crimeDataCSV %>% 
          dplyr::filter(Year == "2013") %>%
          dplyr::group_by(CONTRIBUTING.FACTOR.VEHICLE.1) %>%
          summarise(Factor = n())
        topTenFactorDF <- head(topTenFactorDF[order(-topTenFactorDF$Factor),],n = 5)
        
        collisionVB <- dplyr::filter(aggregateCollisionYear,aggregateCollisionYear$Year == "2013")$number
        yearCasualityVB <- dplyr::filter(yearCasualityDF,yearCasualityDF$Year == "2013")$Death
        yearPadestrianVB <- dplyr::filter(yearPadestrianDF,yearPadestrianDF$Year == "2013")$Death
      }else if(year == "2014"){
        yearLineChartDF <- timeSeriesCollisions %>% filter(Year == "2014")
        topTenFactorDF <- crimeDataCSV %>% 
          dplyr::filter(Year == "2014") %>%
          dplyr::group_by(CONTRIBUTING.FACTOR.VEHICLE.1) %>%
          summarise(Factor = n())
        topTenFactorDF <- head(topTenFactorDF[order(-topTenFactorDF$Factor),],n = 5)
        
        collisionVB <- dplyr::filter(aggregateCollisionYear,aggregateCollisionYear$Year == "2014")$number
        yearCasualityVB <- dplyr::filter(yearCasualityDF,yearCasualityDF$Year == "2014")$Death
        yearPadestrianVB <- dplyr::filter(yearPadestrianDF,yearPadestrianDF$Year == "2014")$Death
      }else if(year == "2015"){
        yearLineChartDF <- timeSeriesCollisions %>% filter(Year == "2015")
        topTenFactorDF <- crimeDataCSV %>% 
          dplyr::filter(Year == "2015") %>%
          dplyr::group_by(CONTRIBUTING.FACTOR.VEHICLE.1) %>%
          summarise(Factor = n())
        topTenFactorDF <- head(topTenFactorDF[order(-topTenFactorDF$Factor),],n = 5)
        
        collisionVB <- dplyr::filter(aggregateCollisionYear,aggregateCollisionYear$Year == "2015")$number
        yearCasualityVB <- dplyr::filter(yearCasualityDF,yearCasualityDF$Year == "2015")$Death
        yearPadestrianVB <- dplyr::filter(yearPadestrianDF,yearPadestrianDF$Year == "2015")$Death
      }else if(year == "2016"){
        yearLineChartDF <- timeSeriesCollisions %>% filter(Year == "2016") 
        topTenFactorDF <- crimeDataCSV %>% 
          dplyr::filter(Year == "2016") %>%
          dplyr::group_by(CONTRIBUTING.FACTOR.VEHICLE.1) %>%
          summarise(Factor = n())
        topTenFactorDF <- head(topTenFactorDF[order(-topTenFactorDF$Factor),],n = 5)
        
        collisionVB <- dplyr::filter(aggregateCollisionYear,aggregateCollisionYear$Year == "2016")$number
        yearCasualityVB <- dplyr::filter(yearCasualityDF,yearCasualityDF$Year == "2016")$Death
        yearPadestrianVB <- dplyr::filter(yearPadestrianDF,yearPadestrianDF$Year == "2016")$Death
      }else if(year == "2017"){
        yearLineChartDF <- timeSeriesCollisions %>% filter(Year == "2017")
        topTenFactorDF <- crimeDataCSV %>% 
          dplyr::filter(Year == "2017") %>%
          dplyr::group_by(CONTRIBUTING.FACTOR.VEHICLE.1) %>%
          summarise(Factor = n())
        topTenFactorDF <- head(topTenFactorDF[order(-topTenFactorDF$Factor),],n = 5)
        
        collisionVB <- dplyr::filter(aggregateCollisionYear,aggregateCollisionYear$Year == "2017")$number
        yearCasualityVB <- dplyr::filter(yearCasualityDF,yearCasualityDF$Year == "2017")$Death
        yearPadestrianVB <- dplyr::filter(yearPadestrianDF,yearPadestrianDF$Year == "2017")$Death
      }else{
        yearLineChartDF <- timeSeriesCollisions
        topTenFactorDF <- crimeDataCSV %>% 
          dplyr::group_by(CONTRIBUTING.FACTOR.VEHICLE.1) %>%
          summarise(Factor = n())
        topTenFactorDF <- head(topTenFactorDF[order(-topTenFactorDF$Factor),],n = 5)
        
        collisionVB <- nrow(crimeDataCSV)
        yearCasualityVB <- sum(yearCasualityDF$Death)
        yearPadestrianVB <- sum(yearPadestrianDF$Death)
      }
      #-----------------------------------------------------
      # Value Box To show Number of Collisions in NY area.
      output$brooklynValueBox <- renderValueBox({
        valueBox(
          value = dplyr::filter(topTenAreas,Region == "BROOKLYN") %>% dplyr::select("Count"),
          subtitle = "BROOKLYN",
          icon = icon("ambulance"),
          color = "yellow"
        )
      })
      
      output$queensValueBox <- renderValueBox({
        valueBox(
          value = dplyr::filter(topTenAreas,Region == "QUEENS") %>% dplyr::select("Count"),
          subtitle = "QUEENS",
          icon = icon("ambulance"),
          color = "aqua"
        )
      })
      
      output$manhattanValueBox <- renderValueBox({
        valueBox(
          value = dplyr::filter(topTenAreas,Region == "MANHATTAN") %>% dplyr::select("Count"),
          subtitle = "MANHATTAN",
          icon = icon("ambulance"),
          color = "blue"
        )
      })
      
      output$bronxValueBox <- renderValueBox({
        valueBox(
          value = dplyr::filter(topTenAreas,Region == "BRONX") %>% dplyr::select("Count"),
          subtitle = "BRONX",
          icon = icon("ambulance"),
          color = "green"
        )
      })
      output$statenIslandValueBox <- renderValueBox({
        valueBox(
          value = dplyr::filter(topTenAreas,Region == "STATEN ISLAND") %>% dplyr::select("Count"),
          subtitle = "STATEN ISLAND",
          icon = icon("ambulance"),
          color = "teal"
        )
      })
      output$totalCollisionValueBox <- renderValueBox({
        valueBox(
          value = collisionVB,
          subtitle = "Total Number of Collisions",
          icon = icon("ambulance"),
          color = "green"
        )
      })
      
      output$yearCasualityValueBox <- renderValueBox({
        valueBox(
          value = yearCasualityVB,
          subtitle = "Loss of Life",
          icon = icon("ambulance"),
          color = "teal"
        )
      })

      output$yearPadestrianValueBox <- renderValueBox({
        valueBox(
          value = yearPadestrianVB,
          subtitle = "Pedestrians Lost",
          icon = icon("ambulance"),
          color = "yellow"
        )
      })
      #-----------------------------------------------------
      # Time Series to View Number of Collisions over time 
      output$timeSeriesCollisionCount <- renderPlotly({
        plot_ly(yearLineChartDF,x  = ~DATE, y = ~number,type = "area",mode = 'lines') %>%
          layout(
            title = "Number of Collisions since 2012",
            xaxis = list(title = "Year"),
            yaxis = list(title = "# Collisions")
          )
        
      })
      #-----------------------------------------------------
      # Bar plot for Top 10 factors
      topTenFactorDF$CONTRIBUTING.FACTOR.VEHICLE.1 <- factor(topTenFactorDF$CONTRIBUTING.FACTOR.VEHICLE.1)
      output$barPlotFactor <- renderPlotly({
        plot_ly(topTenFactorDF, x = ~CONTRIBUTING.FACTOR.VEHICLE.1, 
                y = ~Factor, type = 'bar') %>%
          layout(
            title = "Top 10 Factors for Collision",
            xaxis = list(title = "Collision", tickangle = 0),
            yaxis = list(title = "# Collisions")
          )
      })
      
      
      # Table view block
      output$regionalChart <- renderDataTable(topTenAreas)
        
        
      
    })})}











