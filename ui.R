library(shiny)
library(leaflet)
library(rnrfa)
library(DT)
library(sp)

shinyUI(fluidPage(
  fluidRow(
    column(2, wellPanel(
      sliderInput("n", "Stops To ride", min = 10, max = 100, value = 50,
                  step = 10),
      sliderInput("w", "Walk Distance:", min = 100, max = 5000, value = 500,
                  step = 100),
      actionButton("update", "Again!")
    )),
    column(10,
      leafletOutput("plot1"),
      DT::dataTableOutput("text")
    )
  )
))