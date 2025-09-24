# Check for and install necessary packages if they are not already installed
if (!require(shiny)) install.packages("shiny")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(lubridate)) install.packages("lubridate")
if (!require(tidyr)) install.packages("tidyr")
if (!require(readxl)) install.packages("readxl")
if (!require(plotly)) install.packages("plotly")
if (!require(scales)) install.packages("scales")
if (!require(shinydashboard)) install.packages("shinydashboard")
if (!require(DT)) install.packages("DT")
if (!require(stringr)) install.packages("stringr")

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
  dashboardHeader(title = "SMM Time Series Dashboard",
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
  measure_name <- str_replace_all(measure_name, "severe_maternal_morbidity", "SMM")
  measure_name <- str_replace_all(measure_name, "excluding", "excl.")
  measure_name <- str_replace_all(measure_name, "_oud", " OUD")
  measure_name <- str_replace_all(measure_name, "_sud", " SUD")
  
  # Replace underscores with spaces
  measure_name <- str_replace_all(measure_name, "_", " ")
  
  # Capitalize the first letter of each word
  measure_name <- str_to_title(measure_name)
  
  # Revert specific acronyms back to uppercase
  measure_name <- str_replace_all(measure_name, "Smm", "SMM")
  measure_name <- str_replace_all(measure_name, "Oud", "OUD")
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
  smm_data_reactive <- reactive({
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
  
  # Reactive UI for dynamic controls based on the uploaded data
  output$dynamic_controls <- renderUI({
    df <- smm_data_reactive()
    req(df)
    
    min_year <- min(df$year, na.rm = TRUE)
    max_year <- max(df$year, na.rm = TRUE)
    
    tagList(
      sliderInput("year_range", "Select time period:",
                  min = min_year,
                  max = max_year,
                  value = c(min_year, max_year),
                  step = 1,
                  sep = ""),
      
      radioButtons("granularity", "Display data by time granularity:",
                   choices = c("Quarter" = "quarter_label", "Year" = "year"),
                   selected = "quarter_label"),
      
      selectInput("group_by", "Group by (optional):",
                  choices = c("All" = "all", "Hospital" = "Name", "Race" = "race", "Measure" = "measure"),
                  selected = "all"),
      
      checkboxInput("show_ci", "Show Confidence Intervals", value = FALSE)
    )
  })
  
  # Reactive expression for the main filtered data
  filtered_smm_data <- reactive({
    req(input$year_range, input$granularity, input$group_by)
    
    data <- smm_data_reactive()
    
    data_filtered <- data %>%
      filter(year >= input$year_range[1] & year <= input$year_range[2]) %>%
      filter(!measure %in% c("SMM Hemorrhage", "SMM Preeclampsia"))
    
    grouping_vars <- if(input$group_by != "all") c(input$granularity, input$group_by) else input$granularity
    
    smm_summary <- data_filtered %>%
      group_by(across(all_of(grouping_vars))) %>%
      summarise(
        total_numerator = sum(numerator, na.rm = TRUE),
        total_denominator = sum(denominator, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Calculate confidence intervals
    smm_summary %>%
      mutate(
        rate = (total_numerator / total_denominator) * 10000,
        rate_lower_ci = (rate - qnorm(0.975) * 10000 * sqrt(rate/10000 * (1 - rate/10000) / total_denominator)),
        rate_upper_ci = (rate + qnorm(0.975) * 10000 * sqrt(rate/10000 * (1 - rate/10000) / total_denominator)),
        rate_lower_ci = pmax(0, rate_lower_ci), # ensure lower bound is not negative
        rate = round(rate),
        rate_lower_ci = round(rate_lower_ci),
        rate_upper_ci = round(rate_upper_ci)
      )
  })
  
  # Reactive value to store the clicked point
  clicked_point <- reactiveVal(NULL)
  
  # Observe clicks on the main plot
  observeEvent(event_data("plotly_click"), {
    click_data <- event_data("plotly_click")
    # Take only the first x-value to avoid repetition
    clicked_point(click_data$x[1])
  })
  
  # Render the main SMM rate plot with shared hover and optional CI
  output$smm_rate_plot <- renderPlotly({
    plot_data <- filtered_smm_data()
    req(plot_data)
    
    x_axis_label <- if (input$granularity == "quarter_label") "Quarter" else "Year"
    
    # Use a reactive variable for the color mapping
    plot_color <- if (input$group_by != "all") ~get(input$group_by) else I("steelblue")
    
    p <- plot_ly(
      data = plot_data,
      x = ~get(input$granularity),
      y = ~rate,
      color = plot_color,
      colors = "viridis",
      text = ~paste(
        "Time Period:", get(input$granularity),
        "<br>SMM Cases:", rate,
        if (input$group_by != "all") paste("<br>", input$group_by, ":", get(input$group_by)) else NULL,
        if (input$show_ci) paste("<br>95% CI:", paste0("[", rate_lower_ci, ", ", rate_upper_ci, "]")) else NULL
      ),
      hoverinfo = "text",
      mode = "lines+markers",
      type = "scatter"
    ) %>%
      layout(
        title = "Overall SMM Prevalence Over Time",
        xaxis = list(title = x_axis_label),
        yaxis = list(
          title = "SMM Cases per 10,000 Deliveries",
          # Fixed y-axis range and breaks
          range = c(0, 1500), 
          tickmode = "linear",
          dtick = 100 
        ),
        hovermode = "x unified",
        legend = list(
          x = 1.05,
          y = 1,
          xanchor = 'left',
          yanchor = 'top'
        )
      )
    
    if (input$show_ci) {
      p <- p %>%
        add_ribbons(
          data = plot_data,
          x = ~get(input$granularity),
          ymin = ~rate_lower_ci,
          ymax = ~rate_upper_ci,
          color = plot_color,
          opacity = 0.2,
          line = list(color = 'transparent'),
          hoverinfo = "none",
          showlegend = FALSE
        )
    }
    
    p
  })
  
  # Reactive title for the bar chart
  output$bar_chart_title <- renderText({
    if (is.null(clicked_point())) {
      return("SMM Breakdown for Selected Time Period")
    } else {
      return(paste("SMM Breakdown for", clicked_point()))
    }
  })
  
  # Render the bar chart based on the clicked point
  output$smm_breakdown_plot <- renderPlotly({
    req(clicked_point(), input$group_by != "all")
    
    clicked_x_val <- clicked_point()
    
    # Filter the data for the clicked time period
    bar_data <- filtered_smm_data() %>%
      filter(get(input$granularity) == clicked_x_val)
    
    # Create the bar chart
    plot_ly(
      data = bar_data,
      x = ~get(input$group_by),
      y = ~rate,
      type = "bar",
      color = ~get(input$group_by),
      colors = "viridis",
      text = ~paste(
        "SMM Cases:", rate,
        if (input$show_ci) paste("<br>95% CI:", paste0("[", rate_lower_ci, ", ", rate_upper_ci, "]")) else NULL
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = input$group_by),
        yaxis = list(title = "SMM Cases per 10,000 Deliveries"),
        showlegend = FALSE
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
