library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(bslib)
library(thematic)

thematic_shiny(font = "auto")

dig_data <- read.csv("DIG.csv")
dig_data <- dig_data |>
  select(
    ID, TRTMT, AGE, SEX, RACE, BMI, SYSBP, DIABP, HEARTRTE, KLEVEL, CREAT, EJF_PER, HYPERTEN, DIABETES, PREVMI, DEATH, DWHF
  ) |>
  mutate(
    TRTMT_LABEL = factor(TRTMT, levels = c(0,1), labels = c("Placebo", "Digoxin")),
    SEX_LABEL = factor(SEX, levels = c(1,2), labels = c("Male", "Female")),
    RACE_LABEL = factor(RACE, levels = c(1,2), labels = c("White", "Non-white")),
    HYPERTEN_LABEL = factor(HYPERTEN, levels = c(0,1), labels = c("No", "Yes")),
    DIABETES_LABEL = factor(DIABETES, levels = c(0,1), labels = c("No", "Yes")),
    PREVMI_LABEL = factor(PREVMI, levels = c(0,1), labels = c("No", "Yes")),
    DEATH_LABEL = factor(DEATH, levels = c(0,1), labels = c("Alive", "Death")),
    DWHF_LABEL = factor(DWHF, levels = c(0,1), labels = c("No Event", "Death or HF Hosp"))
  )
#Creating UI for app
library(shiny)

ui <- page_navbar(
  theme = bs_theme(version = 5,preset = "bootstrap"),
  title = "Dig Trial Dashboard",
  
  sidebar = sidebar(
    width = 300,
    input_dark_mode(id = "dark_mode", mode = "light"),
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
#info page
nav_panel(
  title = "Info & Background",
  card(
    card_header("Digitalis Investigation Group (DIG) Trial Overview"),
    p("The Digitalis Investigation Group (DIG) trial was a randomized, double blind, multicenter clinical study designed to evaluate the safety and efficacy of digoxin in patients with heart failure in sinus rhytm."),
    tags$ul(
      tags$li(strong("Total Sample Size:"), "N = 6,800 randomized heart failure patients."),
      tags$li(strong("Primary Clinical Outcomes:"), "All cause mortality (DEATH) and Heart Failure Hospitalization (DWHF)."),
      tags$li(strong("Key Variables Evaluated:"), "Baseline patient demographics (Age, Sex , Race, BMI), vital parameters (SYSBP, DIABP, Heart Rate), and clinical comorbidities (Hypertension, Diabetes, Prior MI).")
      
    )
  )
),
#main dashboard
nav_panel(
  title = "Dashboard",
layout_column_wrap(
  width = 1/2,
  value_box(
    title = "Filtered Patients",
    value = textOutput("val_total"),
    showcase = icon("users"),
    theme = "primary"
  ),
  value_box(
    title = "All Cause Mortality Events",
    value = textOutput("val_deaths"),
    showcase = icon("heart-pulse"),
    theme = "teal"
  )
),
layout_column_wrap(
  width = 1/2,
  heights_equal = "row",
  card(
    card_header("Age Distribution"),
    plotOutput("plot_age")
  ),
  card(
    card_header("Patient Record Table"),
    DTOutput("table_dig")
  )
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
  output$val_total <- renderText({
      format(nrow(dig_sub()), big.mark = ",")
  })
  output$val_deaths <- renderText({
    count <- sum(dig_sub()$DEATH_LABEL == "Death", na.rm = TRUE)
    format(count, big.mark = ", ")
  })
  output$plot_age <- renderPlot({
    ggplot(dig_sub(), aes(x = AGE, fill = TRTMT_LABEL)) +
      geom_histogram(bins = 25, color = "#FFFFFF", alpha = 0.85, position = "identity") +
      scale_fill_manual(values = c("Placebo" = "gold", "Digoxin" = "brown"))+
      theme_minimal() +
      labs(x = "Patient Age (Years)", y = "Patient Count", fill = "Treatment Group")
  })
  output$table_dig <- renderDT({
    display_df <- dig_sub() |>
      select (
        "Patient ID" = ID,
        "Treatment" = TRTMT_LABEL,
        "Age" = AGE,
        "Sex" = SEX_LABEL,
        "Race" = RACE_LABEL,
        "BMI" = BMI,
        "Systolic BP" = SYSBP,
        "Diastolic BP" = DIABP,
        "Heart Rate" = HEARTRTE,
        "Hypertension" = HYPERTEN_LABEL,
        "Diabetes" = DIABETES_LABEL,
        "Status" = DEATH_LABEL
       )
    datatable(
      display_df,
      options = list(pageLength = 7, scrollX = TRUE, dom = 'tp'),
      class = 'table-bordered table-striped table-hover'
    )
  })
}

shinyApp(ui = ui, server = server)


