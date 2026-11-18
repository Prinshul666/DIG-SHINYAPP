library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)

dig_data <- read.csv("DIG.csv")
dig_data <- dig_data |>
  select(
    ID, TRTMT, AGE, SEX, RACE, BMI, SYSBP, DIABP, HEARTRTE, KLEVEL, CREAT, EJF_PER, HYPERTEN, DIABETES, PREVMI, DEATH, DWHF
  ) |>
  mutate(
    TRTMT_LABEL = factor(TRTMT, levels = c(0,1), labels = c("Placebo", "Digoxin")),
    SEX_LABEL = factor(SEX, levels = c(1,2), labels = c("Male", "Female")),
    RACE_LABEL = factor(RACE, levels = c(1,2), labels = c("White", "Nonwhite")),
    HYPERTEN_LABEL = factor(HYPERTEN, levels = c(0,1), labels = c("No", "Yes")),
    DIABETES_LABEL = factor(DIABETES, levels = c(0,1), labels = c("No", "Yes")),
    PREVMI_LABEL = factor(PREVMI, levels = c(0,1), labels = c("No", "Yes")),
    DEATH_LABEL = factor(DEATH, levels = c(0,1), labels = c("Alive", "Death")),
    DWHF_LABEL = factor(DWHF, levels = c(0,1), labels = c("No Event", "Death or HF Hosp"))
  )
#Creating UI for app
library(shiny)

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "DIG Trial Dashboard", titleWidth = 300
    ),
  dashboardSidebar(
    width = 300,
    selectInput("filter_trt", "Select Treatment Group:", choices = c("All", "Placebo", "Digoxin")),
    selectInput("filter_sex", "Select Patient Sex:", choices = c("All", "Male", "Female")),
    sliderInput(
      inputId = "filter_age", 
      label = "Select Age Range:",
    min = min(dig_data$AGE, na.rm = TRUE),
    max = max(dig_data$AGE, na.rm = TRUE),
    value = c(min(dig_data$AGE, na.rm = TRUE),
              max(dig_data$AGE, na.rm = TRUE))
)
),

dashboardBody(
  fluidRow(valueBoxOutput("box_total", width = 6),
           valueBoxOutput("box_deaths", width = 6)
  ),
fluidRow(box(width = 6, title = "Age Distribution", status = "primary", solidHeader = TRUE, plotOutput("plot_age")),
         box(width = 6, title = "Patient Record Table", status = "warning", solidHeader = TRUE, DTOutput("table_dig"))
)
)
)

#server
server <- function(input, output, session) {
  dig_sub <- reactive({
    data <- dig_data
    if(input$filter_trt !="All"){
      data <- data |> filter(TRTMT_LABEL == input$filter_trt)
    }
    if(input$filter_sex !="All"){
      data <- data |> filter(SEX_LABEL == input$filter_sex)
    }
    data |> filter(AGE >= input$filter_age[1] & AGE <= input$filter_age[2])
    
  })
  output$box_total <- renderValueBox({
    valueBox(
      value = format(nrow(dig_sub()), big.mark = ","), 
      "Filtered Patients", 
      icon = icon("users"), 
      color = "blue"
    )
  })
  output$box_deaths <- renderValueBox({
    count <- sum(dig_sub()$DEATH_LABEL == "Death", na.rm = TRUE)
    valueBox(
      format(count, big.mark = ", "),
      "All Cause Mortality Events",
      icon = icon("heart-pulse"),
      color = "teal"
    )
  })
  output$plot_age <- renderPlot({
    ggplot(dig_sub(), aes(x = AGE, fill = TRTMT_LABEL)) +
      geom_histogram(bins = 25, color = "#FFFFFF", alpha = 0.8, position = "identity") +
      scale_fill_manual(values = c("Placebo" = "#4C78A8", "Digoxin" = "#F58518"))+
      theme_minimal() +
      labs(x = "Patient Age (Years)", y = "Patient Count", fill = "Treatment Group")
  })
  output$table_dig <- renderDT({
    datatable(
      dig_sub(),
      options = list(pageLength = 7, scrollX = TRUE, dom = 'tp'),
      style = 'bootstrap',
      class = 'table-bordered table-striped table-hover'
    )
  })
}

shinyApp(ui = ui, server = server)


