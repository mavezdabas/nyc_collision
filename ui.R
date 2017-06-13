library(shiny)
library(shinydashboard)
library(httr)
library(dplyr)
library(ggmap)
library(data.table)
library(scales)
library(plotly)
library(shinyjs)
library(mongolite)
library(DT)
library(data.table)


source("./scripts/loadData.R")



if (!("plotrix" %in% names(installed.packages()[,"Package"]))) {install.packages("plotrix")}
suppressMessages(library(plotrix, quietly = TRUE))

if (!("shiny" %in% names(installed.packages()[,"Package"]))) {install.packages("shiny")}
suppressMessages(library(shiny, quietly = TRUE))

if (!("openintro" %in% names(installed.packages()[,"Package"]))) {install.packages("openintro")}
suppressMessages(library(openintro, quietly = TRUE))


#####################################
# UI Header #
#####################################

header <- dashboardHeader(title = "NYC Collision Data")

sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(id = "sidebarmenu",
              p(),
              # Service center stats
              menuItem("Overall Distribution", tabName = "collisionTab",icon = icon("bar-chart"), selected = TRUE,
                       badgeLabel = "new", badgeColor = "green"),
              conditionalPanel("input.sidebarmenu == 'collisionTab'",
                               useShinyjs(),  
                               selectInput("year",label = "Select Year",
                                           c("All","2012","2013","2014","2015","2016","2017"),
                                           multiple = FALSE,selected = c("All"))
                               ),
              menuItem("Regional Distribution", tabName = "regionTab",icon = icon("bar-chart"), selected = FALSE),
              conditionalPanel("input.sidebarmenu == 'regionTab'",
                               useShinyjs(),  
                               selectInput("region",label = "Select Region",
                                           c("All","Brooklyn","Queens","Manhattan","Bronx","Staten Island"),
                                           multiple = FALSE,selected = c("All"))
              )
  )
)



body <- dashboardBody(
  includeScript("./www/scripts.js"),
  includeScript('./www/spin.min.js'),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "./www/custom.css")
  ),
  tabItems(
    tabItem(tabName = "collisionTab",
            # includeScript('www/Control.Loading.js'),
            h2("Regional Distubution"),
            fluidRow(
                valueBoxOutput("brooklynValueBox",width = 2),
                valueBoxOutput("queensValueBox",width = 2),
                valueBoxOutput("manhattanValueBox",width = 2),
                valueBoxOutput("bronxValueBox",width = 2),
                valueBoxOutput("statenIslandValueBox",width = 2)  
            ),
            fluidRow(
              box(title = "Line Plot",solidHeader = TRUE ,status = "info",plotlyOutput("timeSeriesCollisionCount"),height = 250),
              box(title = "Bar Plot",solidHeader = TRUE ,status = "info",plotlyOutput("barPlotFactor",height = 400))  
            ),
            fluidRow(
              valueBoxOutput("totalCollisionValueBox",width = 2),
              valueBoxOutput("yearCasualityValueBox",width = 2),
              valueBoxOutput("yearPadestrianValueBox",width = 2)
            ),
            fluidRow(
              DT::dataTableOutput("regionalChart")  
            )
    )
  )
)

dashboardPage(header, sidebar, body)
