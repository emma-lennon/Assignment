library(shiny)
library(shinydashboard)
library(tidyverse)
dig.df = read_csv("DIG.csv", col_select = c("ID", "TRTMT", "AGE", "RACE", "SEX", "BMI", "HEARTRTE", "DIABP", "SYSBP", "PREVMI", 
                                            "ANGINA", "DIABETES", "HYPERTEN", "CVD", "WHF", "DIG", "MI", "STRK", "HOSP", "DEATH"))

# Convert selected character columns to factors
factor_column = names(dig.df[c(2, 4, 5, 10:20)])
dig.df = dig.df %>%
  mutate_if(names(.) %in% factor_column, as.factor)

names(dig.df) = c("ID", "Treatment", "Age", "Race", "Sex", "BMI", "Heartrate", "Diastolic_BP", "Systolic_BP", "Previous_MI",
                  "Angina", "Had_Diabetes", "Had_Hypertension", "Hospitalisation_CVD", "Hospitalisation_WHF",
                  "Hospitalisation_DIG_Toxicity", "Hospitalisation_MI", "Hospitalisation_Stroke", "Hospitalisation", "Death")

dig.df = dig.df %>%
  mutate(Treatment = factor(ifelse(Treatment == 1, "Treatment", "Placebo")),
         Sex = factor(ifelse(Sex == 1, "Male", "Female")),
         Race = factor(ifelse(Race == 1, "White", "Non-White")),
         Previous_MI = factor(ifelse(Previous_MI == 1, "Yes", "No")),
         Angina = factor(ifelse(Angina == 1, "Yes", "No")),
         Had_Diabetes = factor(ifelse(Had_Diabetes == 1, "Yes", "No")),
         Had_Hypertension = factor(ifelse(Had_Hypertension == 1, "Yes", "No")),
         Hospitalisation_CVD = factor(ifelse(Hospitalisation_CVD == 1, "Yes", "No")),
         Hospitalisation_WHF = factor(ifelse(Hospitalisation_WHF == 1, "Yes", "No")),
         Hospitalisation_DIG_Toxicity = factor(ifelse(Hospitalisation_DIG_Toxicity == 1, "Yes", "No")),
         Hospitalisation_MI = factor(ifelse(Hospitalisation_MI == 1, "Yes", "No")),
         Hospitalisation_Stroke = factor(ifelse(Hospitalisation_Stroke == 1, "Yes", "No")),
         Hospitalisation = factor(ifelse(Hospitalisation == 1, "Yes", "No")),
         Death = factor(ifelse(Death == 1, "Yes", "No")))

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