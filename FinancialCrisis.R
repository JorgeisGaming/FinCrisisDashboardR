library(shiny)
library(shinydashboard)
library(tidyverse)
library(mosaic)
library(httr)
library(jsonlite)

#GDP, GDP GROWTH, INFLATION, UNEMPLOYMENT, EXPORTS, IMPORTS
#Data for East Asia and pacific; Europe and central Asia; Latin america and caribbean
#Middle east and north Africa; North america; South Asia; USA; World
#GDP GROWTH
path<-"http://api.worldbank.org/v2/country/Z7; 1W; USA; Z4; 8S; XU; ZQ; ZJ/indicator/NY.GDP.MKTP.KD.ZG?per_page=350&format=json&date=2004:2019"
GDPGrowth<-GET(url=path)
GDPGrowth <- content(GDPGrowth, as="text", encoding = "UTF-8")
GDPGrowth <- fromJSON(GDPGrowth,flatten=TRUE)
GDPGrowth <- GDPGrowth[[2]]
GDPGrowth$date<-as.numeric(GDPGrowth$date)
#GDP
path<-"http://api.worldbank.org/v2/country/Z7; 1W; USA; Z4; 8S; XU; ZQ; ZJ/indicator/NY.GDP.MKTP.CD?per_page=350&format=json&date=2004:2019"
GDP<-GET(url=path)
GDP <- content(GDP, as="text", encoding = "UTF-8")
GDP <- fromJSON(GDP,flatten=TRUE)
GDP <- GDP[[2]]
GDP$date<-as.numeric(GDP$date)
#INFLATION
path<-"http://api.worldbank.org/v2/country/Z7; 1W; USA; Z4; 8S; XU; ZQ; ZJ/indicator/FP.CPI.TOTL.ZG?per_page=350&format=json&date=2004:2019"
Inflation<-GET(url=path)
Inflation <- content(Inflation, as="text", encoding = "UTF-8")
Inflation <- fromJSON(Inflation,flatten=TRUE)
Inflation <- Inflation[[2]]
Inflation$date<-as.numeric(Inflation$date)
#UNEMPLOYMENT
path<-"http://api.worldbank.org/v2/country/Z7; 1W; USA; Z4; 8S; XU; ZQ; ZJ/indicator/SL.UEM.TOTL.ZS?per_page=350&format=json&date=2004:2019"
Unemployment<-GET(url=path)
Unemployment <- content(Unemployment, as="text", encoding = "UTF-8")
Unemployment <- fromJSON(Unemployment,flatten=TRUE)
Unemployment <- Unemployment[[2]]
Unemployment$date<-as.numeric(Unemployment$date)
#EXPORTS
path<-"http://api.worldbank.org/v2/country/Z7; 1W; USA; Z4; 8S; XU; ZQ; ZJ/indicator/NE.EXP.GNFS.ZS?per_page=350&format=json&date=2004:2019"
Exports<-GET(url=path)
Exports <- content(Exports, as="text", encoding = "UTF-8")
Exports <- fromJSON(Exports,flatten=TRUE)
Exports <- Exports[[2]]
Exports$date<-as.numeric(Exports$date)
#IMPORTS
path<-"http://api.worldbank.org/v2/country/Z7; 1W; USA; Z4; 8S; XU; ZQ; ZJ/indicator/NE.IMP.GNFS.ZS?per_page=350&format=json&date=2004:2019"
Imports<-GET(url=path)
Imports <- content(Imports, as="text", encoding = "UTF-8")
Imports <- fromJSON(Imports,flatten=TRUE)
Imports <- Imports[[2]]
Imports$date<-as.numeric(Imports$date)



#Function to turn character vector into dataframe
HELPMEGOD<-function(a){
  case_when(
    a == "GDP" ~ GDP,
    a == "GDPGrowth" ~ GDPGrowth,
    a == "Exports" ~ Exports,
    a == "Imports" ~ Imports,
    a == "Inflation" ~ Inflation,
    a == "Unemployment" ~ Unemployment
  )
}




ui <- dashboardPage(
  dashboardHeader(title = "Relevant data for 2007-2008 financial crisis"),
  dashboardSidebar(
    selectInput(inputId = "data", label = "Select which data you want to see",
                       c("GDP", "GDP Growth", "Inflation", "Unemployment", "Exports", "Imports"),
                "GDP"),
    
    checkboxGroupInput(inputId = "region", label = "Select which region data you want to see",
                       c("East Asia & the Pacific", "Europe and Central Asia",
                         "Latin America and the Caribbean", "Middle East and North Africa",
                         "South Asia", "North America", "USA", "The World"),
                       "The World")
      ),
  dashboardBody(
    fluidPage(
      fluidRow(
        column(
          width = 12,
          box(plotOutput("plot1"), title = "2007-2008 financial crisis related data", width = NULL)
        )
      )
    )
  )
)

server <- function(input,output){
 ChosenRegions<-reactive({
   GDP %>%
      filter(country.value %in% input$region)
  })
  
  output$plot1 <- renderPlot(
  ChosenRegions() %>%
  ggplot() + aes(x = date, y = value, col = country.value) + geom_line()
  )
}

shinyApp(ui=ui, server=server)

