library(shiny)
library(shinydashboard)
library(tidyverse)
dig.df = read_csv("DIG.csv", col_select = c("ID", "TRTMT", "AGE", "RACE", "SEX", "BMI", "HEARTRTE", "DIABP", "SYSBP", "PREVMI", "ANGINA", "DIABETES", "HYPERTEN", "CVD", "WHF", "DIG", "MI", "STRK", "HOSP", "DEATH"))
dig.df$TRTMT = as.factor(dig.df$TRTMT)
dig.df$RACE = as.factor(dig.df$RACE)
dig.df$SEX = as.factor(dig.df$SEX)
# To do: tidy the data for a more readable format - later

# Continuous variables:
## AGE, BMI, HEARTRTE (?), DIABP, SYSBP

# Categorical variables:
## TRTMT, RACE, SEX, ANGINA

# History of... variables (binary variables):
## PREVMI, DIABETES, HYPERTEN

# Hospitalisation (binary variables):
## CVD, WHF, DIG, MI, UANG (unstable angina), STRK (stroke), RINF (respiratory infection), HOSP, DEATH


## UI
# Header
header <- dashboardHeader(title = "DIG Data")

# Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Boxplots", tabName = "Boxplots", icon = icon("dashboard")),
    menuItem("Barcharts", tabName = "Barcharts", icon = icon("th"))
  )
)

# Body
body <- dashboardBody(
  
  tabItems(
    # First tab content
    tabItem(tabName = "Boxplots",
            # value boxes for summary: Treatment group
            fluidRow(valueBoxOutput("treatmentNo1"), valueBoxOutput("maleProp1"), valueBoxOutput("meanAge1"),
                     valueBoxOutput("meanBMI1"), valueBoxOutput("meanDBP1"), valueBoxOutput("meanSBP1")),
            # for placebo group
            fluidRow(valueBoxOutput("treatmentNo0"), valueBoxOutput("maleProp0"), valueBoxOutput("meanAge0"),
                     valueBoxOutput("meanBMI0"), valueBoxOutput("meanDBP0"), valueBoxOutput("meanSBP0")),
            
            # plots
            fluidRow(
              
              box(title = "Inputs", background = "purple", solidHeader = T, collapsible = T,
                  selectInput("box_var", "Select Column for Boxplot:", choices = names(dig.df)[c(3, 6, 7, 8, 9)]),
                  selectInput("binary_var", "Select Binary Column for Grouping:", choices = names(dig.df)[c(2, 4, 5)])),
              
              box(title = "Boxplot", background = "purple", solidHeader = TRUE,
                  collapsible = TRUE, plotOutput("plot1", height = 600))
            )
    ),
    
    # Second tab content
    tabItem(tabName = "Barcharts",
            h2("Widgets tab content")
    )
  )
)

ui <- dashboardPage(skin = "red", header, sidebar, body)


## Server

server <- function(input, output) {
  output$treatmentNo1 <- renderValueBox({
    valueBox(
      paste0(length(dig.df$TRTMT[dig.df$TRTMT == 1])), "In Treatment Group", icon = icon("list"),
      color = "purple"
    )
  })
  
  output$treatmentNo0 <- renderValueBox({
    valueBox(
      paste0(length(dig.df$TRTMT[dig.df$TRTMT == 0])), "In Placebo Group", icon = icon("list"),
      color = "yellow"
    )
  })
  
  output$maleProp1 <- renderValueBox({
    valueBox(
      paste0(dig.df %>% filter(TRTMT == 1 & SEX == 1) %>% count()), "Males in Treatment", icon = icon("list"),
      color = "purple"
    )
  })
    
    output$maleProp0 <- renderValueBox({
      valueBox(
        paste0(dig.df %>% filter(TRTMT == 0 & SEX == 1) %>% count()), "Males in Placebo", icon = icon("list"),
               color = "yellow"
        )
    })
    
    output$meanAge1 <- renderValueBox({
      valueBox(
        paste0(dig.df %>% select(TRTMT, AGE) %>% filter(TRTMT == 1) %>% summarise(mean = round(mean(AGE), 1))), "Mean Age of Treatment", icon = icon("list"),
               color = "purple"
        )
    })
    
    output$meanAge0 <- renderValueBox({
      valueBox(
        paste0(dig.df %>% select(TRTMT, AGE) %>% filter(TRTMT == 0) %>% summarise(mean = round(mean(AGE), 1))), "Mean Age of Placebo", icon = icon("list"),
        color = "yellow"
      )
    })
    
    output$meanBMI1 <- renderValueBox({
      valueBox(
        paste0(dig.df %>% select(TRTMT, BMI) %>% filter(TRTMT == 1) %>% summarise(mean = round(mean(BMI), 1))), "Mean BMI of Treatment", icon = icon("list"),
        color = "purple"
      )
    })
    
    output$meanBMI0 <- renderValueBox({
      valueBox(
        paste0(dig.df %>% select(TRTMT, BMI) %>% filter(TRTMT == 0) %>% summarise(mean = round(mean(BMI, na.rm = T), 1))), "Mean BMI of Placebo", icon = icon("list"),
        color = "yellow"
      )
    })
    
    output$meanDBP1 <- renderValueBox({
      valueBox(
        paste0(dig.df %>% select(TRTMT, DIABP) %>% filter(TRTMT == 1) %>% summarise(mean = round(mean(DIABP, na.rm = T), 1))), "Mean Diastolic BP of Treatment", icon = icon("list"),
        color = "purple"
      )
    })
    
    output$meanDBP0 <- renderValueBox({
      valueBox(
        paste0(dig.df %>% select(TRTMT, DIABP) %>% filter(TRTMT == 0) %>% summarise(mean = round(mean(DIABP, na.rm = T), 1))), "Mean Diastolic BP of Placebo", icon = icon("list"),
        color = "yellow"
      )
    })
    
    output$meanSBP1 <- renderValueBox({
      valueBox(
        paste0(dig.df %>% select(TRTMT, SYSBP) %>% filter(TRTMT == 1) %>% summarise(mean = round(mean(SYSBP, na.rm = T), 1))), "Mean Systolic BP of Treatment", icon = icon("list"),
        color = "purple"
      )
    })
    
    output$meanSBP0 <- renderValueBox({
      valueBox(
        paste0(dig.df %>% select(TRTMT, SYSBP) %>% filter(TRTMT == 0) %>% summarise(mean = round(mean(SYSBP, na.rm = T), 1))), "Mean Systolic BP of Placebo", icon = icon("list"),
        color = "yellow"
      )
    })
  
  output$plot1 <- renderPlot({
    ggplot(dig.df, aes_string(x = input$binary_var, y = input$box_var, fill = input$binary_var)) +
      geom_boxplot() +
      labs(title = paste("Boxplot of", input$box_var, "grouped by", input$binary_var),
           x = input$binary_var, y = input$box_var) +
      theme_minimal() +
      scale_fill_manual(values = c("orange", "cornflowerblue"))
  })
}
#Testing commit

## Run
shinyApp(ui, server)