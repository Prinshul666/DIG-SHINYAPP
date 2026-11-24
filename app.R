library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(bslib)
library(thematic)

thematic_shiny(font = font_spec("Inter",scale = 1.0))

dig_data <- read.csv("DIG.csv")

dig_data <- dig_data |>
  
  select(ID, TRTMT, AGE, SEX, RACE, BMI, SYSBP, DIABP, HEARTRTE, KLEVEL, CREAT, EJF_PER, HYPERTEN, DIABETES, PREVMI, ANGINA, DIGUSE, WHF, HOSP, DEATH, DWHF) |>
  
  mutate(
    TRTMT_LABEL = factor(TRTMT, levels = c(0,1), labels = c("Placebo", "Digoxin")),
    SEX_LABEL = factor(SEX, levels = c(1,2), labels = c("Male", "Female")),
    RACE_LABEL = factor(RACE, levels = c(1,2), labels = c("White", "Non-white")),
    HYPERTEN_LABEL = factor(HYPERTEN, levels = c(0,1), labels = c("No", "Yes")),
    DIABETES_LABEL = factor(DIABETES, levels = c(0,1), labels = c("No", "Yes")),
    PREVMI_LABEL = factor(PREVMI, levels = c(0,1), labels = c("No", "Yes")),
    ANGINA_LABEL = if ("ANGINA" %in% names(dig_data)) factor (ANGINA, levels = c(0,1), labels = c("No", "Yes")) else NULL,
    DIGUSE_LABEL = if ("DIGUSE" %in% names(dig_data)) factor (DIGUSE, levels = c(0,1), labels = c("No", "Yes")) else NULL,
    DEATH_LABEL = factor(DEATH, levels = c(0,1), labels = c("Alive", "Death")),
    DWHF_LABEL = factor(DWHF, levels = c(0,1), labels = c("No Event", "Death or HF Hosp")),
    WHF_LABEL = if ("WHF" %in% names(dig_data)) factor(WHF, levels = c(0,1), labels = c("No Hosp", "HF Hosp")) else NULL,
    HOSP_LABEL = if("HOSP" %in% names(dig_data)) factor(HOSP, levels =c(0,1), labels = c("No Hosp", "Any Hosp")) else NULL
  )
col_placebo <- "brown"
col_digoxin <- "gold"
#Creating UI for app

ui <- page_navbar(
  fillable = FALSE,
  theme = bs_theme(version = 5,
                   preset = "bootstrap",
                   base_font = font_google("Inter"),
                   heading_font = font_google("Montserrat"),
                   primary = "#1E3A8A",
                   secondary = "#475569"
  ),
  
  title = "Dig Trial Dashboard",
  header = tags$head(
    tags$style(HTML("
.navbar{
position: relative !important;
min-height: 52px !important;
padding-top: 6px !important;
padding-bottom: 6px !important;
box-shadow: 0 2px 4px rgba(0,0,0,0,0.04);
}

.navbar .container-fluid {
position: static !important;
min-height: 40px;
}

.navbar-brand {
position: absolute !important;
transform: translate(-50%, -50%) !important;
left: 50% !important;
top: 50% !important;
font-family: 'Montserrat', sans-serif !important;
font-weight: 800 !important;
font-size: 1.45rem !important;
line-height: 1.2 !important;
letter-spacing: 0.02em;
margin: 0 !important;
padding: 4px 16px !important;
border-radius: 999px
}

.card{
border-radius: 10px !important;
border: 1px solid #E2E8FO !important;
box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.03), 0 2px 4px -1px rgba(0, 0 , 0, 0.02) !important;
transition: transform 0.15s ease;
}

.card{
border-radius: 10px !important;
border: 1px solid #E2E8F0 !important;
box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.03), 0 2px 4px -1px rgba(0, 0, 0, 0.02) !important;
}
.card-header{
background-color: #FFFFFF !important;
border-bottom: 1px solid #F1F5F9 !important;
font-weight: 700 !important;
font-size: 1.02rem !important;
color: #0F172A !important;
padding: 14px 18px !important;
}
.sidebar {
background-color :#F8FAFC !important;
border-right: 1px solid #E2E8F0 !important;
}
}"))
  ),
  
  sidebar = sidebar(
    width = 300,
    title = h5("Cohort Filters", class = "fw-bold text-dark mb-3"),
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
    ),
    actionButton("reset_filters", "Reset All Filters", class = "btn-outline-secondary btn-sm w-100 mt-2", icon = icon("rotate-left"))
  ),
  
  #info page
  
  nav_panel(
    title = "Info & Background",
    layout_column_wrap(
      width = 1,
      card(
        card_header("What Is This Study About? (Plain-Language Guide)"),
        p(
          "The ", strong("Digitalis Investigation Group (DIG) trial"),
          "was a large medical study testing a heart medication called ", strong("digoxin"),
          "in people living with ", strong("heart failure"), ". Heart failure does not mean the heart has stopped working-it means the heart muscle is weaker or stiffer than usual and has to work harder to pump blood around the body."
        ),
        
        p(
          "To see whether digoxin genuinely helped patients, researchers randomly divided participants into two groups: one received ",
          strong("digoxin"), " (an active medicine) and the other received a ", strong("placebo"),
          " (an inactive pill with no medicine). Neither the patients nor the doctors knew who got which pill during the study-this is called a ",
          strong("double-blind"), "trial, which helps prevent bias."
        )
      )
    ),
  
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("Key Terms Made Simple"),
        tags$ul(
          tags$li(
            strong("Placebo vs. Digoxin: "),
            "A comparison group used as a baseline so doctors can tell if changes come from the drug itself or from normal day-to-day health changes."
          ),
          tags$li(
            strong("All-Cause Mortality (DEATH): "),
            "The total number of patients who passed away from any cause during the follow-up period."
          ),
          tags$li(
            strong("Heart Failure Hospitalization (DWHF): "),
            "Whether a patient died or had to be admitted to the hospital due to worsening heart failure symptoms."
          ),
          tags$li(
            strong("Ejection Fraction (EJF_PER): "),
            "A percentage showing how much blood the heart's main pumping chamber pushes out with each beat. Lower number mean weaker pumping strength. "
          ),
          tags$li(
            strong("Blood Pressure (SYSBP / DIABP): "),
            "Systolic BP (top number) measures pressure when the heart beats; Diastolic BP (bottom number) measures pressure when the heart rests between beats."
          )
        )
      ),
      card(
        card_header("Trial Snapshot & How to Use This Dashboard"),
        tags$ul(
          tags$li(strong("Total Participants: "), "6,800 patients enrolled across multiple clinical centers."),
          tags$li(strong("Explore Dataset: "), "Select any variable below.")
          )
        )
      )
    ),
  
  # Main dashboard
  
  nav_panel(
    title = "Dashboard",
    
    # Dashboard summary boxes
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
    
    # Dashboard charts
    layout_column_wrap(
      width = 1/2,
      heights_equal = "row",
      
      card(
        min_height = "480px",
        card_header("Age Distribution"),
        plotOutput("plot_age", height = "360px")
      ),
      
      card(
        card_header("Age vs. Health Measure (Scatter Plot)"),
        
        selectInput(
          inputId = "scatter_y",
          label = "Compare Age Against:",
          choices = c(
            "Systolic Blood Pressure (mmHg)" = "SYSBP",
            "Body Mass Index (BMI)" = "BMI",
            "Ejection Fraction (%)" = "EJF_PER"
          ),
          selected = "SYSBP"
        ),
        
        plotOutput("plot_scatter"),
        
        tags$div(
          style = "padding: 8px 12px; background-color: rgba(0, 0, 0, 0.03); border-radius: 6px; font-size: 0.88rem; margin-top: 6px;",
          uiOutput("clinical_summary")
        )
      )
    ),
    
    layout_column_wrap(
      width = 1,
      card(
        card_header("Dataset Explorer (Choose the option)"),
        layout_sidebar(
          sidebar = sidebar(
            width = 310,
            selectInput(
              inputId = "smart_y",
              label = "Primary Metric (Y-Axis):",
              choices = list(
                "Demographics & Physical" = c(
                  "Age (Years)" = "AGE",
                  "Systolic BP (mmHg)" = "SYSBP",
                  "Diastolic BP (mmHg)" = "DIABP",
                  "Body Mass Index (BMI)" = "BMI",
                  "Heart Rate (bpm)" = "HEARTRTE"
                ),
                "Lab & Clinical Measurements" = c(
                  "Ejection Fraction (%)" = "EJF_PER",
                  "Serum Potassium (KLEVEL)" = "KLEVEL",
                  "Serum Creatinine (CREAT)" = "CREAT"
                ),
                "Baseline Medical Conditions" = c(
                  "Hypertension Flag" = "HYPERTEN_LABEL",
                  "Diabetes Flag" = "DIABETES_LABEL",
                  "Prior MI Flag" = "PREVMI_LABEL"
                ),
                "Trial Endpoints" = c(
                  "All-Cause Mortality" = "DEATH_LABEL",
                  "Death or HF Hospitalization" = "DWHF_LABEL"
                )
              ),
              selected = "SYSBP"
            ),
            selectInput(
              inputId = "smart_x",
              label = "Compare Against (Grouping/ X-Axis):",
              choices = c(
                "Treatment Group" = "TRTMT_LABEL",
                "Patient Sex" = "SEX_LABEL",
                "Race" = "RACE_LABEL",
                "Patient Age (Years)" = "AGE",
                "Body Mass Index (BMI)" = "BMI"
              ),
              selected = "TRTMT_LABEL"
            ),
            tags$div(
              style = "font-size: 0.82rem; color: #64748B; margin-top: 8px;",
              "Automatically routes continous variables to scatter plots/boxplots and categorical pairs to bar charts."
            )
          ),
          plotOutput("plot_smart_explorer", height = "390px")
        )
      )
    ),
    #Comorbidity and Outcome chart
    layout_column_wrap(
      width = 1,
      card(
        card_header("Clinical History & Outcomes by Treatment Group"),
        uiOutput("clinical_interpretation"),
        plotOutput("plot_comorbidities")
      )
    ),
    # Patient records
    layout_column_wrap(
      width = 1,
      
      card(
        card_header("Patient Record Table"),
        DTOutput("table_dig")
      )
    )
  )
)

#server

server <- function(input, output, session) {
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "filter_trt", selected = "All")
    updateSelectInput(session, "filter_sex", selected = "All")
    updateSliderInput(session, "filter_age", value = c(min(dig_data$AGE, na.rm = TRUE), max(dig_data$AGE, na.rm = TRUE)))
  })
  
  dig_sub <- reactive({
    req(input$filter_trt, input$filter_sex, input$filter_age)
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
      geom_histogram(bins = 25, color = "white", alpha = 0.85, position = "identity") +
      scale_fill_manual(values = c("Placebo" = "gold", "Digoxin" = "brown"))+
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "top",
        legend.title = element_blank(),
        axis.title = element_text(face = "bold", size = 11),
        axis.title.x = element_text(margin = margin(t = 6)),
        axis.title.y = element_text(margin = margin(r = 6)),
        plot.margin = margin(2, 6, 4, 6)
      ) +
      labs(x = "Patient Age (Years)", y = "Patient Count", fill = "Treatment Group")
  })
  
  output$plot_scatter <- renderPlot({
    req(input$scatter_y)
    y_col <- input$scatter_y
    y_label <- switch(
      y_col,
      "SYSBP" = "Systolic Blood Pressure (mmHg)",
      "BMI" = "Body Mass Index (BMI)",
      "EJF_PER" = "Ejection Fraction (%)"
    )
    
    plot_df <- dig_sub() |>
      filter(!is.na(AGE),!is.na(.data[[y_col]]))
    ggplot(plot_df,aes(x = AGE, y = .data[[y_col]], color = TRTMT_LABEL)) +
      geom_point(alpha = 0.45, size = 1.8) +
      geom_smooth(method = "lm",formula = y ~ x, se = FALSE, linewidth = 1) +
      scale_color_manual(values = c("Placebo" = "gold", "Digoxin" = "brown")) +
      theme_minimal(base_size = 11) +
      theme(
        legend.position = "top",
        legend.title = element_blank(),
        axis.title = element_text(face = "bold", size = 11),
        axis.title.x = element_text(margin = margin(t = 6)),
        axis.title.y = element_text(margin = margin(r = 6)),
        plot.margin = margin(2, 6, 4, 6)) +
      labs(
        x = "Patient Age (Years)",
        y = y_label,
        color = "Treatment Group" 
      )
  })
  
  output$clinical_summary <- renderUI({
    data <- dig_sub()
    total <- nrow(data)
    y_col <- input$scatter_y
    avg_val <- round(mean(data[[y_col]], na.rm = TRUE), 1)
    label_name <- switch(
      y_col,
      "SYSBP" = "Mean Systolic BP",
      "BMI" = "Mean BMI",
      "EJF_PER" = "Mean Ejection Fraction"
    )
    unit_name <- switch(
      y_col,
      "SYSBP" = "mmHg",
      "BMI" = "kg/m^2",
      "EJF_PER" = "%"
    )
    HTML(paste0("<strong>Cohort Insight:</strong> Analyzing <strong>",
                format(total, big.mark = ","), "</strong> filtered patients. ", 
                label_name, ": <strong>", avg_val, unit_name, "</strong>."))
  })
  output$plot_smart_explorer <- renderPlot({
    req(input$smart_y, input$smart_x)
    df <- dig_sub()
    req(nrow(df) > 0)
    y_var <- input$smart_y
    x_var <- input$smart_x
    plot_data <- df |> filter(!is.na(.data[[y_var]]), !is.na(.data[[x_var]]))
    req(nrow(plot_data) > 0)
    y_is_num <- is.numeric(plot_data[[y_var]])
    x_is_num <- is.numeric(plot_data[[x_var]])
    if (y_is_num && x_is_num) {
      #scatter plot for continous vs continous 
      ggplot(plot_data, aes(x = .data[[x_var]], y = .data[[y_var]], color = TRTMT_LABEL)) +
        geom_point(alpha = 0.45, size = 1.8) +
        geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth =1.2) +
        scale_color_manual(values = c("Placebo" = col_placebo, "Digoxin" = col_digoxin)) +
        theme_minimal(base_size = 12) +
        theme(
          legend.position = "top", legend.title = element_blank(),
          axis.title = element_text(face = "bold", size = 11, color = "#334155"),
          plot.margin = margin(6, 12, 6 ,12)
        ) +
        labs(x = x_var, y = y_var, color = "Treatment Group")
      
    } else if(y_is_num && !x_is_num) {
      #jitter boxplot for continous y vs categorical x
      ggplot(plot_data, aes(x = .data[[x_var]], y = .data[[y_var]], fill = .data[[x_var]])) +
        geom_boxplot(alpha = 0.70, outlier.shape = NA, width = 0.45) +
        geom_jitter(width = 0.15, alpha = 0.20, size = 1.2, color = "#1E293B") +
        scale_fill_manual(values = c("Placebo" = col_placebo, "Digoxin" = col_digoxin, "Male" = "#2563EB", "Female" = "#DB2777", "White" = "#0D9488", "Non-white" = "#D97706")) +
        theme_minimal(base_size = 12) +
        theme(
          legend.position = "none",
          axis.title = element_text(face = "bold", size = 11, color = "#334155"),
          plot.margin = margin(6, 12, 6 ,12)
        ) +
        labs(x = x_var, y = y_var)
      
    } else { #grouped bar chart for categorical y vs catergorical x
      prop_df <- plot_data |> mutate(X_FACTOR = as.factor(.data[[x_var]]))
      prop_df <- plot_data |>
        count(.data[[x_var]], .data[[y_var]]) |>
        mutate(Percent = n / sum(n) * 100, .by = .data[[x_var]])
      ggplot(prop_df, aes(x = .data[[x_var]], y = Percent, fill = .data[[y_var]])) +
        geom_col(position = "dodge", alpha = 0.88, width = 0.55) +
        theme_minimal(base_size = 12) +
        theme(
          legend.position = "top",
          axis.title = element_text(face = "bold", size = 11, color = "#334155"),
          plot.margin = margin(6, 12, 6 ,12)
        ) +
        labs(x = x_var, y = "Share of Group (%)", fill = y_var)
    }
  })
  
  output$clinical_interpretation <- renderUI({
    req(input$filter_sex, input$filter_age)
    base_df <- dig_data
    if (input$filter_sex != "All") {
      base_df <- base_df |> filter(SEX_LABEL == input$filter_sex)
    }
    base_df <- base_df |> filter(AGE >= input$filter_age[1] & AGE <= input$filter_age[2])
    
    req(nrow(base_df) > 0)
    curr_df <- dig_sub()
    
    stats <- base_df |>
      summarise(
        mort_placebo = mean(DEATH[TRTMT_LABEL == "Placebo"] == 1, na.rm = TRUE) * 100,
        mort_digoxin = mean(DEATH[TRTMT_LABEL == "Digoxin"] == 1, na.rm = TRUE) * 100,
        hosp_placebo = mean(DWHF[TRTMT_LABEL == "Placebo"] == 1, na.rm = TRUE) * 100,
        hosp_digoxin = mean(DWHF[TRTMT_LABEL == "Digoxin"] == 1, na.rm = TRUE) * 100
      )
  hosp_diff <- round(stats$hosp_placebo - stats$hosp_digoxin, 1)
  mort_diff <- round(stats$mort_digoxin - stats$mort_placebo, 1)
    
  hosp_statement <- if(is.nan(hosp_diff) || is.na(hosp_diff)) {
    "insufficient outcome data"
  } else if (hosp_diff > 0) {
    paste0("a <strong>", hosp_diff, "% reduction</strong> in heart failure hospitalizations/mortality compared to placebo")
  } else if (hosp_diff < 0 ) {
    paste0(" a <strong>", abs(hosp_diff), "% increase</strong> in heart failure hospitalizations/mortality compared to placebo")
  } else {
    "similar rates of heart failure hospitalizations comapared to placebo"
  }
  mort_statement <- if (is.nan(mort_diff) || is.na(mort_diff)) {
    "mortality outcome data is pending"
  } else if(abs(mort_diff) < 0.5) {
    "all cause mortality remain neutral between treatment groups"
  } else if (mort_diff < 0) {
    paste0("all cause mortality is <strong>", abs(mort_diff), "% lower</strong> in the digoxin arm")
  } else {
    paste0("all cause mortality is <strong>", mort_diff, "% higher</strong> in the digoxin arm")
  }
  
  HTML(paste0(
    "<div style = 'background-color: #f8f9fa; border-left: 4px solid #0d6efd; padding: 12px 16px; margin: 8px 0 16px; border-radius: 4px; font-size: 0.93rem; color: #212529;'>",
    "<strong> Clinical Interpretation (Filtered Cohort, n = ", format(nrow(curr_df), big.mark = ","), "):</strong>",
    "In this sub cohort, digoxin therapy demonstrates ", hosp_statement, ", while ", mort_statement, ".",
    "Baseline comorbidity rates above reflect pre-existing risk profiles across trial arms.",
    "</div>"
  ))
})
  
  output$plot_comorbidities <- renderPlot({
    summary_df <- dig_sub() |>
      summarise(
        Hypertension = mean(HYPERTEN == 1, na.rm = TRUE) * 100,
        Diabetes = mean(DIABETES == 1, na.rm = TRUE) * 100,
        `Prior Heart Attack` = mean(PREVMI == 1, na.rm = TRUE) * 100,
        `Mortality (DEATH)` = mean(DEATH == 1, na.rm = TRUE) * 100,
        `Death or HF Hosp` = mean(DWHF == 1, na.rm = TRUE) * 100,
        .by = TRTMT_LABEL
      ) |>
      pivot_longer(
        cols = -TRTMT_LABEL,
        names_to = "Measure",
        values_to = "Percent"
      )
    
    ggplot(summary_df, aes(x = Percent, y = reorder(Measure, Percent), fill = TRTMT_LABEL)) +
      geom_col(position = "dodge", width = 0.65, alpha = 0.9) +
      scale_fill_manual(values = c("Placebo" = "gold", "Digoxin" = "brown")) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "top",
        legend.title = element_blank(),
        axis.title = element_text(face = "bold", size = 11),
        axis.title.x = element_text(margin = margin(t = 6)),
        axis.title.y = element_text(margin = margin(r = 6)),
        plot.margin = margin(2, 6, 4, 6)
      ) +
      labs(
        x = "Share of Filtered Patients (%)",
        y = NULL
      )
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
      extensions = 'Buttons',
      options = list(pageLength = 7, scrollX = TRUE, dom = 'Bfrtip', buttons = list(
        list(extend = 'csv', className = 'btn btn-sm btn-outline-primary me-1'),
        list(extend = 'excel', className = 'btn btn-sm btn-outline-success')
      )
      ),
      class = 'table-bordered table-striped table-hover',
      rownames = FALSE
    )
  })
}

shinyApp(ui = ui, server = server)