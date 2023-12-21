library(shiny)
library(shinydashboard)
library(tidyverse)
dig.df = read.csv("DIG.csv")
# To do: tidy the data for a more readable format - later


## UI
# Header
header <- dashboardHeader(title = "DIG Data")

# Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", tabName = "widgets", icon = icon("th"))
  )
)

# Body
body <- dashboardBody(
  fluidRow(
    # Dynamic valueBoxes
    valueBoxOutput("progressBox"),
    valueBoxOutput("approvalBox")),
  
  tabItems(
    # First tab content
    tabItem(tabName = "dashboard",
            fluidRow(
              
              box(title = "Inputs", background = "aqua", solidHeader = T, collapsible = T,
                  selectInput(inputId = "TRTMT", label = "Select Treatment Group:", choices = c(0, 1), multiple = FALSE),
                  selectInput(inputId = "var",
                              label = "Select a variable:",
                              choices = names(dig.df)[c(3, 9, 24, 25)],
                              selected = names(dig.df)[3])),
              
              box(title = "Histogram", background = "aqua", solidHeader = TRUE,
                  collapsible = TRUE, plotOutput("plot1", height = 600))
              )
            ),
      
    # Second tab content
    tabItem(tabName = "widgets",
            h2("Widgets tab content")
    )
  )
)

ui <- dashboardPage(skin = "red", header, sidebar, body)


## Server

server <- function(input, output) {
  output$progressBox <- renderValueBox({
    valueBox(
      paste0(25 + 50, "%"), "Progress", icon = icon("list"),
      color = "purple"
    )
  })
  
  output$approvalBox <- renderValueBox({
    valueBox(
      "80%", "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(50)]
    hist(data)
  })
}


## Run
shinyApp(ui, server)