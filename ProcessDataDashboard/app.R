# Load necessary packages
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(readxl)
library(plotly)
library(DT)
library(scales)
library(stringr)

# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Process Measures",
                  titleWidth = 300),
  dashboardSidebar(
    width = 300,
    h4("Upload Your Data"),
    fileInput("file1", "Choose CSV or Excel File",
              multiple = FALSE,
              accept = c(".csv", ".xlsx")),
    
    uiOutput("dynamic_controls")
  ),
  dashboardBody(
    fluidRow(
      column(12,
             # The following tab is commented out for simplicity.
             # tabPanel("Overall SMM Point Prevalence",
             #          h3("SMM Cases per 10,000 Deliveries"),
             #          plotlyOutput("smm_rate_plot", height = "600px")
             # )
      ),
      box(
        width = 12,
        title = "SMM Cases per 10,000 Deliveries",
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput("smm_rate_plot", height = "600px")
      ),
      box(
        width = 12,
        title = textOutput("bar_chart_title"),
        status = "info",
        solidHeader = TRUE,
        plotlyOutput("smm_breakdown_plot", height = "600px")
      )
    )
  )
)

# Helper function to clean up measure names
clean_measure_names <- function(measure_name) {
  # Replace specific acronyms and words
  measure_name <- str_replace_all(measure_name, "pnc", "PNC")
  measure_name <- str_replace_all(measure_name, "oen", " OEN")
  measure_name <- str_replace_all(measure_name, "sud", " SUD")
  
  # Replace underscores with spaces
  measure_name <- str_replace_all(measure_name, "_", " ")
  
  # Capitalize the first letter of each word
  measure_name <- str_to_title(measure_name)
  
  # Revert specific acronyms back to uppercase
  measure_name <- str_replace_all(measure_name, "Pnc", "PNC")
  measure_name <- str_replace_all(measure_name, "Oen", "OEN")
  measure_name <- str_replace_all(measure_name, "Sud", "SUD")
  
  return(measure_name)
}

# Helper function to clean up race names
clean_race_names <- function(race_name) {
  # Replace underscores with spaces
  race_name <- str_replace_all(race_name, "_", " ")
  # Capitalize the first letter of each word
  race_name <- str_to_title(race_name)
  return(race_name)
}

# Define the server logic
server <- function(input, output, session) {
  
  # Hospital Name Mapping (hardcoded as this is a lookup table)
  hospital_mapping <- tribble(
    ~Name, ~masked_name,
    "Adventist Health Castle", "Chi Nu Psi",
    "Hilo Benioff Medical Center", "Delta Pi Omicron",
    "Kaiser Permanente Moanalua Medical Center", "Omicron Alpha Alpha",
    "Kapiolani Medical Center for Women and Children", "Beta Simga Epislon",
    "Kauai Veterans Memorial Hospital", "Iota Beta Rho",
    "Kona Community Hospital", "Gamma Xi Beta",
    "Maui Memorial Medical Center", "Psi Gamma Eta",
    "Molokai General Hospital", "Beta Alpha Epislon",
    "North Hawaii Community Hospital", "Alpha Iota Theta",
    "The Queen's Medical Center", "Simga Rho Theta",
    "Wilcox Medical Center", "Zeta Omicron Kappa"
  )
  
  # Reactive expression to read and preprocess the uploaded file
  process_data_reactive <- reactive({
    req(input$file1)
    
    ext <- tools::file_ext(input$file1$name)
    
    # Read the data based on file type
    df <- switch(ext,
                 csv = read.csv(input$file1$datapath, header = TRUE, na.strings = c("", "NA")),
                 xlsx = read_excel(input$file1$datapath),
                 stop("Unsupported file type")
    )
    
    # Data preprocessing
    smm_data <- df %>%
      mutate(
        period_start_date = mdy(period_start_date),
        year = year(period_start_date),
        quarter = quarter(period_start_date),
        quarter_label = paste0(year, " Q", quarter),
        measure = clean_measure_names(measure), # Apply the measure cleaning function
        race = clean_race_names(race) # Apply the new race cleaning function
      ) %>%
      left_join(hospital_mapping, by = "masked_name")
    
    smm_data
  })