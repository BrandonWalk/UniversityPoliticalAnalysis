library(shiny) # this is to make interactive web apps using only R
library(leaflet) # this is use the javascript package leaflet for mapping
library(tidyverse)
library(plotly)

universityData <- read_csv("University Data.csv")

ui <- fluidPage(
  sidebarLayout( # organization for the page, other options are splitLayout and fluidRow
    sidebarPanel(
      selectInput("selectedSchools","Select Schools Below",sort(universityData$University), selected = c("Texas A&M University", "Liberty University"), multiple = TRUE, selectize = TRUE),
      plotOutput("barPlot")
    ),
    mainPanel(
      titlePanel("Political Orientation of 20 Biggest American Universities"),
      leafletOutput("map")
    )
  )
)

server <- function(input, output) {
  univ <- reactive({input$selectedSchools})
  output$map <- renderLeaflet({
    prepLeaflet <- leaflet(universityData)
    addedTiles <- addProviderTiles(prepLeaflet, providers$Stamen.Toner)
    addedCircles <- addCircleMarkers(addedTiles, 
                                     radius = sqrt(abs(universityData$`Clinton Margin`)) + 3,
                                     color = ifelse(universityData$`Clinton Margin`>0,"Blue","Red"),
                                     weight = 1,
                                     popup = paste(strsplit(universityData$University,","),"<br/>","Size:",universityData$Undergraduates)
                                     )
    addedCircles
  })
  output$barPlot <- renderPlot({
    ggplot(data=universityData[which(!is.na(match(universityData$University,univ()))),],aes(x=University,y=Undergraduates))+geom_bar(stat = "identity")
  })
}

shinyApp(ui = ui, server = server)
