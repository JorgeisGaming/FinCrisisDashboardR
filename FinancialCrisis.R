library(shiny)
library(shinydashboard)
library(tidyverse)
library(mosaic)
library(httr)
library(jsonlite)
library(plotly)

#GDP, GDP GROWTH, INFLATION, UNEMPLOYMENT, EXPORTS, IMPORTS
#Data for East Asia and pacific; Europe and central Asia; Latin america and caribbean
#Middle east and north Africa; North america; South Asia; USA; World; Sub-Saharan Africa
#Using USA as well since the USA was the root of the 2008 financial crisis and seeing
#Damage firsthand is really interesting; Using worldwide data to see the overall global damage.
#GDP GROWTH
path<-"http://api.worldbank.org/v2/country/Z7; 1W; USA; Z4; 8S; XU; ZQ; ZJ; ZG/indicator/NY.GDP.MKTP.KD.ZG?per_page=350&format=json&date=2004:2020"
GDPGrowth<-GET(url=path)
GDPGrowth <- content(GDPGrowth, as="text", encoding = "UTF-8")
GDPGrowth <- fromJSON(GDPGrowth,flatten=TRUE)
GDPGrowth <- GDPGrowth[[2]]
GDPGrowth$date<-as.numeric(GDPGrowth$date)
#GDP
path<-"http://api.worldbank.org/v2/country/Z7; 1W; USA; Z4; 8S; XU; ZQ; ZJ; ZG/indicator/NY.GDP.MKTP.CD?per_page=350&format=json&date=2004:2020"
GDP<-GET(url=path)
GDP <- content(GDP, as="text", encoding = "UTF-8")
GDP <- fromJSON(GDP,flatten=TRUE)
GDP <- GDP[[2]]
GDP$date<-as.numeric(GDP$date)
#Since GDP values are extremely high, I'll show the values as billions of dollars
GDP$value<-GDP$value/1000000000
#INFLATION
path<-"http://api.worldbank.org/v2/country/Z7; 1W; USA; Z4; 8S; XU; ZQ; ZJ; ZG/indicator/FP.CPI.TOTL.ZG?per_page=350&format=json&date=2004:2020"
Inflation<-GET(url=path)
Inflation <- content(Inflation, as="text", encoding = "UTF-8")
Inflation <- fromJSON(Inflation,flatten=TRUE)
Inflation <- Inflation[[2]]
Inflation$date<-as.numeric(Inflation$date)
#UNEMPLOYMENT
path<-"http://api.worldbank.org/v2/country/Z7; 1W; USA; Z4; 8S; XU; ZQ; ZJ; ZG/indicator/SL.UEM.TOTL.ZS?per_page=350&format=json&date=2004:2020"
Unemployment<-GET(url=path)
Unemployment <- content(Unemployment, as="text", encoding = "UTF-8")
Unemployment <- fromJSON(Unemployment,flatten=TRUE)
Unemployment <- Unemployment[[2]]
Unemployment$date<-as.numeric(Unemployment$date)
#EXPORTS
path<-"http://api.worldbank.org/v2/country/Z7; 1W; USA; Z4; 8S; XU; ZQ; ZJ; ZG/indicator/NE.EXP.GNFS.ZS?per_page=350&format=json&date=2004:2020"
Exports<-GET(url=path)
Exports <- content(Exports, as="text", encoding = "UTF-8")
Exports <- fromJSON(Exports,flatten=TRUE)
Exports <- Exports[[2]]
Exports$date<-as.numeric(Exports$date)
#IMPORTS
path<-"http://api.worldbank.org/v2/country/Z7; 1W; USA; Z4; 8S; XU; ZQ; ZJ; ZG/indicator/NE.IMP.GNFS.ZS?per_page=350&format=json&date=2004:2020"
Imports<-GET(url=path)
Imports <- content(Imports, as="text", encoding = "UTF-8")
Imports <- fromJSON(Imports,flatten=TRUE)
Imports <- Imports[[2]]
Imports$date<-as.numeric(Imports$date)

#Getting country names to have as options in dataframe
Regions<-distinct(GDPGrowth, country.value)
Regions<- as.vector(Regions[[1]])

#Creating dataframe of drops in different rates during crises periods, excluding worldwide values
FirstCrisis<-GDP$value[GDP$date == 2009] - GDP$value[GDP$date == 2008] #Yearly change during 2008 crisis
SecondCrisis<-GDP$value[GDP$date == 2020] - GDP$value[GDP$date == 2019] #Yearly change during pandemic
GDPDrops <- tibble(Regions, FirstCrisis, SecondCrisis) #Tibbling
GDPDrops<-GDPDrops[GDPDrops$Regions!="World",] #Making sure we don't have worldwide values
GDPDrops$FirstCrisis<-as.numeric(GDPDrops$FirstCrisis) #Turning as numeric (not sure if i needed to turn 
#these into numeric but I was troubleshooting so I left these after the bug was fixed)
GDPDrops$SecondCrisis<-as.numeric(GDPDrops$SecondCrisis) #Turning as numeric
#We do same thing of everything else so I won't copy comments

FirstCrisis<-GDPGrowth$value[GDPGrowth$date == 2009] - GDPGrowth$value[GDPGrowth$date == 2008]
SecondCrisis<-GDPGrowth$value[GDPGrowth$date == 2020] - GDPGrowth$value[GDPGrowth$date == 2019]
GDPGrowthDrops <- tibble(Regions, FirstCrisis, SecondCrisis)
GDPGrowthDrops<-GDPGrowthDrops[GDPGrowthDrops$Regions!="World",]
GDPGrowthDrops$FirstCrisis<-as.numeric(GDPGrowthDrops$FirstCrisis)
GDPGrowthDrops$SecondCrisis<-as.numeric(GDPGrowthDrops$SecondCrisis)

FirstCrisis<-Unemployment$value[Unemployment$date == 2009] - Unemployment$value[Unemployment$date == 2008]
SecondCrisis<-Unemployment$value[Unemployment$date == 2020] - Unemployment$value[Unemployment$date == 2019]
UnemploymentDrops <- tibble(Regions, FirstCrisis, SecondCrisis)
UnemploymentDrops<-UnemploymentDrops[UnemploymentDrops$Regions!="World",]
UnemploymentDrops$FirstCrisis<-as.numeric(UnemploymentDrops$FirstCrisis)
UnemploymentDrops$SecondCrisis<-as.numeric(UnemploymentDrops$SecondCrisis)

FirstCrisis<-Inflation$value[Inflation$date == 2009] - Inflation$value[Inflation$date == 2008]
SecondCrisis<-Inflation$value[Inflation$date == 2020] - Inflation$value[Inflation$date == 2019]
InflationDrops <- tibble(Regions, FirstCrisis, SecondCrisis)
InflationDrops<-InflationDrops[InflationDrops$Regions!="World",]
InflationDrops$FirstCrisis<-as.numeric(InflationDrops$FirstCrisis)
InflationDrops$SecondCrisis<-as.numeric(InflationDrops$SecondCrisis)

#For imports and exports the value of Middle East & North Africa was missing for 2020, so we 
#Got NA value in SecondCrisis column and it messed up our data and we couldn't calculate min value
#So I just replaced that NA value with mean value of all other observations to make sure we actually
#got what we wanted to see without "messing up" our data
FirstCrisis<-Imports$value[Imports$date == 2009] - Imports$value[Imports$date == 2008]
SecondCrisis<-Imports$value[Imports$date == 2020] - Imports$value[Imports$date == 2019]
ImportDrops <- tibble(Regions, FirstCrisis, SecondCrisis)
ImportDrops<-ImportDrops[ImportDrops$Regions!="World",]
ImportDrops$FirstCrisis<-as.numeric(ImportDrops$FirstCrisis)
ImportDrops$SecondCrisis<-as.numeric(ImportDrops$SecondCrisis)
ImportDrops[is.na(ImportDrops)]<-mean(ImportDrops$SecondCrisis[complete.cases(ImportDrops$SecondCrisis)])

FirstCrisis<-Exports$value[Exports$date == 2009] - Exports$value[Exports$date == 2008]
SecondCrisis<-Exports$value[Exports$date == 2020] - Exports$value[Exports$date == 2019]
ExportDrops <- tibble(Regions, FirstCrisis, SecondCrisis)
ExportDrops<-ExportDrops[ExportDrops$Regions!="World",]
ExportDrops$FirstCrisis<-as.numeric(ExportDrops$FirstCrisis)
ExportDrops$SecondCrisis<-as.numeric(ExportDrops$SecondCrisis)
ExportDrops[is.na(ExportDrops)]<-mean(ExportDrops$SecondCrisis[complete.cases(ExportDrops$SecondCrisis)])


ui <- dashboardPage(
  dashboardHeader(title = "Financial data for these regions"),
  dashboardSidebar(
    checkboxGroupInput(
      inputId = "regions",
      label = "Please select the regions you want to see",
      choices = Regions,
      selected = "United States",
      inline = FALSE,
      width = NULL)
  ),
  dashboardBody(
    fluidPage(
      fluidRow(
        column(
          width = 12,
          tabBox(
            id = "tabs", height = 600,
            title = "Relevant financial data to analyze financial crises",
            width = NULL, #Going to add tabs for every different data, with two infoboxes each
            #to see which regions were affected the most
            tabPanel("GDP", plotlyOutput(outputId="plotGDP"), infoBoxOutput("boxGDP1", width = 6),
                     infoBoxOutput("boxGDP2", width = 6)), 
            tabPanel("GDP Growth", plotlyOutput(outputId="plotGDPGrowth"), infoBoxOutput("boxGrowth1", width = 6),
                     infoBoxOutput("boxGrowth2", width = 6)),
            tabPanel("Inflation", plotlyOutput(outputId="plotInflation"), infoBoxOutput("boxInflation1", width = 6),
                     infoBoxOutput("boxInflation2", width = 6)),
            tabPanel("Unemployment", plotlyOutput(outputId="plotUnemployment"), infoBoxOutput("boxUnemployment1", width = 6),
                     infoBoxOutput("boxUnemployment2", width = 6)),
            tabPanel("Imports", plotlyOutput(outputId="plotImports"), infoBoxOutput("boxImports1", width = 6),
                     infoBoxOutput("boxImports2", width = 6)),
            tabPanel("Exports", plotlyOutput(outputId="plotExports"), infoBoxOutput("boxExports1", width = 6),
                     infoBoxOutput("boxExports2", width = 6)))
            )
        
        )
      )
    )
  )


server <- function(input,output){
  #Filtering regions we choose for each tab
  GDPRegions<-reactive({
    GDP %>%
      filter(country.value %in% input$regions)
  })
  
  GDPGrowthRegions<-reactive({
    GDPGrowth %>%
      filter(country.value %in% input$regions)
  })
  
  InflationRegions<-reactive({
    Inflation %>%
      filter(country.value %in% input$regions)
  })
  
  UnemploymentRegions<-reactive({
    Unemployment %>%
      filter(country.value %in% input$regions)
  })
  
  ImportsRegions<-reactive({
    Imports %>%
      filter(country.value %in% input$regions)
  })
  
  ExportsRegions<-reactive({
    Exports %>%
      filter(country.value %in% input$regions)
  })
  
  #Rendering plots with plotly to make them hoverable, leaving all the data when we hover,
  #Since numbers, regions and dates are all important to get when we get to certain parts
  output$plotGDP <- renderPlotly({
    GDPRegions() %>%
      ggplot() + aes(x = date, y = value, col = country.value) + geom_line(size = 1.2) +
      labs(title = "GDP of given regions from 2004 to 2020", x = "Years", 
           y = "GDP (in billions)", col = "Regions") + 
      scale_x_continuous(breaks = c(2004:2020), limits = c(2004, 2020))
  })
  
  
  output$plotGDPGrowth <- renderPlotly({
    GDPGrowthRegions() %>%
      ggplot() + aes(x = date, y = value, col = country.value) + geom_line(size = 1.2) +
      labs(title = "GDP Growth of given regions from 2004 to 2020", x = "Years", y = "GDP Growth",
           col = "Regions") + 
      scale_x_continuous(breaks = c(2004:2020), limits = c(2004, 2020)) 
  })
  
  output$plotImports <- renderPlotly({
    ImportsRegions() %>%
      ggplot() + aes(x = date, y = value, col = country.value) + geom_line(size = 1.2) +
      labs(title = "Rate of imports in given regions from 2004 to 2020", x = "Years", 
           y ="Import rates", col = "Regions") + 
      scale_x_continuous(breaks = c(2004:2020), limits = c(2004, 2020))
  })
  
  output$plotExports <- renderPlotly({
    ExportsRegions() %>%
      ggplot() + aes(x = date, y = value, col = country.value) + geom_line(size = 1.2) +
      labs(title = "Rate of exports in given regions from 2004 to 2020", x = "Years", 
           y = "Export rates", col = "Regions") +
      scale_x_continuous(breaks = c(2004:2020), limits = c(2004, 2020))
  })
  
  output$plotUnemployment <- renderPlotly({
    UnemploymentRegions() %>%
      ggplot() + aes(x = date, y = value, col = country.value) + geom_line(size = 1.2) +
      labs(title = "Unemployment rates in given regions from 2004 to 2020", x ="Years", 
           y = "Unemployment rate", col = "Regions") + 
      scale_x_continuous(breaks = c(2004:2020), limits = c(2004, 2020))
  })
  
  output$plotInflation <- renderPlotly({
    InflationRegions() %>%
      ggplot() + aes(x = date, y = value, col = country.value) + geom_line(size = 1.2) +
      labs(title = "Inflation rates in given regions from 2004 to 2020", x = "Years", 
           y = "Inflation rate", col = "Regions" ) + 
      scale_x_continuous(breaks = c(2004:2020), limits = c(2004, 2020))
  })
  
  #Adding two infoboxes per tab, showing the most drastic chances for each crisis/date and showing 
  #the most affected region for both crises
  output$boxGDP1 <-renderInfoBox({
    infoBox(
      title = paste("The Biggest change (in bln $s) during 2008 crisis"),
      value = round(min(GDPDrops$FirstCrisis),2),
      subtitle = paste(GDPDrops$Regions[GDPDrops$FirstCrisis == min(GDPDrops$FirstCrisis)],
                       "region saw the biggest drop in GDP"),
   
      color = "green",
      icon = icon("chart-bar") #Couldn't find icon selection so it's all same icons :(
    )
  })
  
  output$boxGDP2 <-renderInfoBox({
    infoBox(
      title = paste("The Biggest change (in bln $s) during pandemic"),
      value = round(min(GDPDrops$SecondCrisis),2),
      subtitle = paste(GDPDrops$Regions[GDPDrops$SecondCrisis == min(GDPDrops$SecondCrisis)],
                       "region saw the biggest drop in GDP"),
      
      color = "green",
      icon = icon("chart-bar")
    )
  })
  
  output$boxGrowth1 <-renderInfoBox({
    infoBox(
      title = "The biggest change during 2008 financial crisis (in %)",
      value = round(min(GDPGrowthDrops$FirstCrisis), 2),
      subtitle = paste("Incurred by ", 
                              GDPGrowthDrops$Regions[GDPGrowthDrops$FirstCrisis == min(GDPGrowthDrops$FirstCrisis)],
                                                     "region"),
      color = "aqua",
      icon = icon("chart-bar")
    )
  })
  
  output$boxGrowth2 <-renderInfoBox({
    infoBox(
      title = "The biggest change during COVID-19 crisis (in %)",
      value = round(min(GDPGrowthDrops$SecondCrisis), 2),
      subtitle = paste("Incurred by ", 
                       GDPGrowthDrops$Regions[GDPGrowthDrops$SecondCrisis == min(GDPGrowthDrops$SecondCrisis)],
                       "region"),
      color = "aqua",
      icon = icon("chart-bar")
    )
  })
  
  output$boxInflation1 <-renderInfoBox({
    infoBox(
      title = "The sharpest drop during the first crisis (in %)",
      value = round(min(InflationDrops$FirstCrisis), digits = 2),
      subtitle = paste("In the", InflationDrops$Regions[InflationDrops$FirstCrisis == min(InflationDrops$FirstCrisis)],
                       "region"),
      color = "orange",
      icon = icon("chart-bar")
    )
  })
  
  output$boxInflation2 <-renderInfoBox({
    infoBox(
      title = "The sharpest drop during the second crisis (in %)",
      value = round(min(InflationDrops$SecondCrisis), digits = 2),
      subtitle = paste("In the", InflationDrops$Regions[InflationDrops$SecondCrisis == min(InflationDrops$SecondCrisis)],
                       "region"),
      color = "orange",
      icon = icon("chart-bar")
    )
  })
  
  output$boxUnemployment1 <-renderInfoBox({
    infoBox(
      title = "Rise of unemployment (in %)",
      value = round(max(UnemploymentDrops$FirstCrisis), digits = 2),
      subtitle = paste(UnemploymentDrops$Regions[UnemploymentDrops$FirstCrisis == max(UnemploymentDrops$FirstCrisis)],
                       "was affected the most in 2008 crisis"),
      color = "yellow",
      icon = icon("chart-bar")
    )
  })
  
  output$boxUnemployment2 <-renderInfoBox({
    infoBox(
      title = "Rise of unemployment (in %)",
      value = round(max(UnemploymentDrops$SecondCrisis), digits = 2),
      subtitle = paste(UnemploymentDrops$Regions[UnemploymentDrops$SecondCrisis == max(UnemploymentDrops$SecondCrisis)],
                       "was affected the most during the pandemic"),
      color = "yellow",
      icon = icon("chart-bar")
    )
  })
  
  output$boxImports1 <-renderInfoBox({
    infoBox(
      title = "Drop in imports (in %)",
      value = round(min(ImportDrops$FirstCrisis), digits = 2),
      subtitle = paste("Incurred by the", 
                              ImportDrops$Regions[ImportDrops$FirstCrisis == min(ImportDrops$FirstCrisis)],
                       "region during 2008 crisis"),
      color = "maroon",
      icon = icon("chart-bar")
    )
  })
  
  output$boxImports2 <-renderInfoBox({
    infoBox(
      title = "Drop in imports (in %)",
      value = round(min(ImportDrops$SecondCrisis), digits = 2),
      subtitle = paste("Incurred by the", 
                       ImportDrops$Regions[ImportDrops$SecondCrisis == min(ImportDrops$SecondCrisis)],
                       "region during COVID-19 crisis"),
      color = "maroon",
      icon = icon("chart-bar")
    )
  })
  
  
  output$boxExports1 <-renderInfoBox({
    infoBox(
      title = "Drop in exports during 2008 financial crisis (in %)",
      value = round(min(ExportDrops$FirstCrisis), digits = 2),
      subtitle = paste("In the", 
                       ExportDrops$Regions[ExportDrops$FirstCrisis == min(ExportDrops$FirstCrisis)],
                       "region"),
      color = "teal",
      icon = icon("chart-bar")
    )
  })
  
  
  output$boxExports2 <-renderInfoBox({
    infoBox(
      title = "Drop in exports during pandemic (in %)",
      value = round(min(ExportDrops$SecondCrisis), digits = 2),
      subtitle = paste("In the", 
                       ExportDrops$Regions[ExportDrops$SecondCrisis == min(ExportDrops$SecondCrisis)],
                       "region"),
      color = "teal",
      icon = icon("chart-bar")
    )
  })
  
  
}
shinyApp(ui=ui, server=server)























#Some final thoughts which I wrote after working on this - could be interesting from the reader's POV
#but isn't part of mandatory reading


#I feel like, as is with the most creations, the things never turn out as you want them to and you
#want to keep improving and improving until you reach the point where ideas run out and everything 
#you've done starts to seem as if it's too little for the overall scheme of things.
#I hope this is normal way to feel about things you're passionate about and the final product
#has turned out acceptable, because once you conquer the obstacle, it just starts to seem so easy 
#and you feel stupid for spending so much time on it.
#Anyways, bit disappointed in myself that this is what it turned out but I feel like it's just a biased 
#opinion (can blame this on too much Business Law) - something was achieved and lot was learned.
