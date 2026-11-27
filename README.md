# DIG-SHINY — DIG Trial Clinical Dashboard

A 'bslib' (Bootstrap 5) and 'tidyverse' based interactive clinical trial dashboard to analyse data from the Digitalis Investigation Group (DIG) heart failure clinical trial ('DIG.csv').

## 1.Introduction
The DIG Trial was a double blinded, randomized controlled trial investigating the impact of digoxin on patients’ mortality and admission rates in comparison to a placebo; for those with heart failure (n=6,800). The dashboard combines a plain language summary alongside interactive data displays, automated data analysis and downloadable patient record data table.

### Dashboard Functionality
### 1. Filters & Styling (Sidebar) Controls
Light/Dark Mode: A bslib::input_dark_mode() Dynamic theme switching between light and dark theme . Fonts and styling themes automatically update for each mode.
Data Filters: Patients can be filtered according to:
TRTMT (All, Placebo or Digoxin)
SEX (All, Male or Female)
AGE Range (Interactive slider representing the age range of patients at entry to the study).
Reset : A single ‘rotate-left’ button which clears all filters and re-displays the original 6,800 patient data set.

### 2. Information & Background

Lay Summary: A summary of heart failure, randomisation and double blinding.
Lay Terminology Dictionary: Lay person definitions of TRTMT (Placebo and Digoxin), DEATH, DWHF, WHF, HOSP, EJF_PER, SYSBP, DIABP and FUNCTCLS.
Study Summary and Tab Instructions: Summary statistics are shown including number of patients entered in to the study. Data analysed includes patient co-morbidities and physiological data. Colours represent Digoxin (brown) and Placebo (gold). Tab navigation instructions are also given.

### 3. Dashboard Display

### Instant Statistics:
Number of patients remaining after filtering.
All-cause mortality rate. An animated ‘heartbeat’ CSS graphic is integrated and pulses in time with the teal coloured border around the box (class: .pulse-border). Prefer reduced motion settings are respected.
Summary and Patient Data:
Age histogram for both Placebo and Digoxin patient data sets, demonstrating areas of patient age overlap.
Interactive AGE versus SYSBP, BMI or EJF_PER graph with corresponding OLS regression lines. Instantaneously displayed is the mean value for the current patient data set.

### Co-morbidities and Results:

Interpretive lay summary box highlighting the benefits of each TRTMT on patient hospital admissions and mortality rates together with the difference in all-cause mortality rates seen in each group. Applies to currently filtered data.
Grouped horizontal bar graph displaying the relative proportion of co-morbidities (HYPERTEN, DIABETES, PREVMI and ANGINA) and results (HOSP, WHF, DEATH and DWHF), by TRTMT.

### Data Analysis:

Intelligent graph type selector; automatically generating suitable graphs for the combination of data types provided:
Continuous – Continuous: Graph type is scatter-plot with regression lines. Colours differentiate between TRTMTs.
Continuous – Categorical: Box-plot with super-imposed patient data points. Horizontal orientation (via coord_flip()) is also enabled if Y-axis is categorical.
Categorical – Categorical: Grouped bar graph illustrating intra-group proportions.

### Patient Data:

Scroll-able DT table of patient data (ID, TRTMT, AGE, SEX, RACE, NYHA Class, BMI, SYSBP, DIABP, HEARTRTE, HYPERTEN, DIABETES and STATUS). Includes search functionality and buttons to download data in CSV and Excel formats.
Variables within DIG.csv
DIG.csv is imported and the following variables (as detailed in DIG_code_book.pdf) are utilised:
Identification and Demographics:
ID, TRTMT (0 = Placebo, 1 = Digoxin), AGE, SEX (1 = Male, 2 = Female), RACE (1 = White, 2 = Non-white).
Physiological Data:
BMI, SYSBP, DIABP, HEARTRTE, CHFDUR, CHESTX, NSYM (Total of responses to questions Q13 through Q20), FUNCTCLS (NYHA Functional Class I-IV).
Blood Tests:
EJF_PER (% Ejection Fraction), KLEVEL (Serum Potassium level), and CREAT (Serum Creatinine level).
Medical History:
HYPERTEN, DIABETES, PREVMI, ANGINA and DIGUSE.
Clinical Results:
WHF (Hospital admission for Worsening Heart Failure), HOSP (Any Hospital Admission), DEATH (All-cause mortality) and DWHF (Main outcome measure: death or hospital admission for heart failure).

### Folder Contents
DIG-SHINYAPP/
├── app.R               # Main Shiny UI and server application
├── DIG.csv             # Digitalis Investigation Group (DIG) clinical trial dataset
├── DIG_code_book.pdf   # Official variable definitions and coding reference
├── DIG-SHINYAPP.Rproj  # RStudio project configuration file
├── .gitignore          # Git ignore rules for R / RStudio artifacts
└── README.md           # Project documentation

### 1. Initial Setup
Install R (version 4.1+ advised for native pipe ‘|>’ functionality) and RStudio. Install required packages:
install.packages(c(
"shiny",
"shinydashboard",
"tidyverse",
"DT",
"bslib",
"thematic"
))

### 2. Launching the App

Launch RStudio and open DIG-SHINYAPP.Rproj. Alternatively change your working directory to the folder containing app.R and DIG.csv.
To launch the app either enter the following into the R console or select ‘Run App’ within RStudio:

shiny::runApp()