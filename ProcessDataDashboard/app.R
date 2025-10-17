# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(plotly)
library(readxl) 
library(lubridate) 
library(data.table)
library(tibble) 
library(RColorBrewer) # Ensure RColorBrewer is explicitly loaded

# --- DATA PROCESSING AND HELPER FUNCTIONS ---

# Function to properly format measure names (now renamed to match user's snippet)
clean_measure_names <- function(measure_name) {
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

# Function to clean race names (defined based on standard cleaning practices)
clean_race_names <- function(race_name) {
  race_name <- tolower(race_name)
  race_name <- str_replace_all(race_name, "_", " ")
  race_name <- str_to_title(race_name)
  # Example cleaning:
  race_name[grepl("other", race_name)] <- "Other / Unknown"
  return(race_name)
}

# --- UI DEFINITION ---
ui <- fluidPage(
  titlePanel("Process Measures Dashboard - Data Upload"),
  
  # Main layout with tabs for measure types
  tabsetPanel(
    id = "measure_type_tabs",
    
    # Numerator/Denominator Tab - Current Focus
    tabPanel("Numerator/Denominator",
             value = "nd_tab",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 h4("Data & Filter Controls"),
                 
                 # 1. File Input Control for Uploading Data (Renamed to input$file_upload)
                 fileInput("file_upload", "Upload Data (CSV or Excel)",
                           multiple = FALSE,
                           accept = c(".csv", ".xls", ".xlsx")),
                 
                 hr(),
                 
                 # 2. Year Filter (will be dynamically updated)
                 uiOutput("year_select_ui"),
                 
                 # 3. Measure Selector (will be dynamically updated)
                 uiOutput("measure_select_nd_ui")
               ),
               
               mainPanel(
                 width = 9,
                 # Conditional rendering of content based on file upload status
                 uiOutput("main_content") 
               )
             )
    ),
    
    # Placeholder Tabs for future development
    tabPanel("Raw Value", value = "raw_tab",
             p("This panel will display data for Raw Value measures (where Denominator = 1).")),
    tabPanel("Range", value = "range_tab",
             p("This panel will display data for Range measures (likely Compliance/Score percentages)."))
  )
)

# --- SERVER LOGIC ---
server <- function(input, output, session) {
  
  # Hospital Name Mapping (hardcoded as this is a lookup table)
  # NEW: Separating names into FullName, Name, and AbbName
  hospital_mapping <- tribble(
    ~FullName, ~Name, ~AbbName, ~masked_name,
    "Adventist Health Castle (AHC)", "Adventist Health Castle", "AHC", "Chi Nu Psi",
    "Hilo Benioff Medical Center (HBMC)", "Hilo Benioff Medical Center", "HBMC", "Delta Pi Omicron",
    "Kaiser Permanente Moanalua Medical Center (KPMMC)", "Kaiser Permanente Moanalua Medical Center", "KPMMC", "Omicron Alpha Alpha",
    "Kapiolani Medical Center for Women and Children (KMCWC)", "Kapiolani Medical Center for Women and Children", "KMCWC", "Beta Simga Epislon",
    "Kauai Veterans Memorial Hospital (KVMH)", "Kauai Veterans Memorial Hospital", "KVMH", "Iota Beta Rho",
    "Kona Community Hospital (KCH)", "Kona Community Hospital", "KCH", "Gamma Xi Beta",
    "Maui Memorial Medical Center (MMMC)", "Maui Memorial Medical Center", "MMMC", "Psi Gamma Eta",
    "Molokai General Hospital (MGH)", "Molokai General Hospital", "MGH", "Beta Alpha Epislon",
    "North Hawaii Community Hospital (NHCH)", "North Hawaii Community Hospital", "NHCH", "Alpha Iota Theta",
    "The Queen's Medical Center (QMC)", "The Queen's Medical Center", "QMC", "Simga Rho Theta",
    "Wilcox Medical Center (WMC)", "Wilcox Medical Center", "WMC", "Zeta Omicron Kappa"
  )
  
  # Reactive expression to read and preprocess the uploaded file (using input$file_upload)
  raw_data <- reactive({
    # Use req(input$file_upload) which is the actual file input ID
    req(input$file_upload) 
    
    ext <- tools::file_ext(input$file_upload$name)
    file_path <- input$file_upload$datapath
    
    # Read the data based on file type
    df <- tryCatch({
      if (ext == "csv") {
        # Using data.table::fread is more robust than read.csv for different CSV formats
        data.table::fread(file_path, data.table = FALSE, na.strings = c("", "NA")) 
      } else if (ext %in% c("xls", "xlsx")) {
        read_excel(file_path, sheet = 1)
      } else {
        validate("Unsupported file type. Please upload a CSV or Excel file.")
      }
    }, error = function(e) {
      validate(paste("Error reading file:", e$message))
    })
    
    # 1. Ensure column names are in lower case for consistent referencing
    names(df) <- tolower(names(df))
    df <- as.data.frame(df)
    
    # 2. Critical check for required columns
    required_cols <- c("hospital_unique_identifier", "masked_name", "collaborative", "measure", 
                       "race", "period_start_date", "period_end_date", "numerator", 
                       "denominator", "measure_display_type")
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
      validate(paste("Data is missing required columns:", paste(missing_cols, collapse = ", ")))
    }
    
    # 3. Data preprocessing (Based on user's provided code block)
    smm_data <- df %>%
      mutate(
        # FIX: Clean the key filter column (Crucial fix from previous iteration)
        measure_display_type = tolower(trimws(as.character(measure_display_type))),
        
        # Date and Time Derivation using mdy()
        period_start_date = mdy(period_start_date),
        year = as.factor(year(period_start_date)),
        quarter = as.factor(quarter(period_start_date)),
        quarter_label = factor(
          paste0(year, " Q", quarter),
          levels = unique(paste0(year, " Q", quarter))[order(unique(paste0(year, " Q", quarter)))]
        ),
        
        # Apply cleaning functions
        measure = clean_measure_names(measure), 
        race = clean_race_names(race),
        
        # Ensure numerators and denominators are numeric
        numerator = as.numeric(numerator),
        denominator = as.numeric(denominator)
      ) %>%
      # Join with the mapping table to get the three name types
      left_join(hospital_mapping, by = "masked_name")
    
    smm_data
  })
  
  # Reactive list of unique formatted measures for UI dropdowns
  measure_choices <- reactive({
    df <- raw_data()
    req(df, "measure")
    
    measures <- unique(df$measure)
    # The measure column is already cleaned by clean_measure_names()
    display_names <- unique(df$measure) 
    
    # Create a named vector where the name is the display name and the value is the ID
    names(display_names) <- display_names
    
    return(display_names)
  })
  
  # Reactive list of unique years for UI dropdown
  year_choices <- reactive({
    df <- raw_data()
    req(df, "year")
    
    years <- unique(df$year)
    return(sort(as.character(years), decreasing = TRUE))
  })
  
  
  # --- UI Generation based on Data ---
  
  # Render the Year Select Input (dynamic based on uploaded data)
  output$year_select_ui <- renderUI({
    req(raw_data())
    selectInput(
      "year_select",
      "Select Year:",
      choices = year_choices(),
      selected = max(year_choices())
    )
  })
  
  # Render the Measure Select Input for Numerator/Denominator tab
  output$measure_select_nd_ui <- renderUI({
    req(raw_data(), "measure_display_type")
    
    # Filter choices to only include measures of type 'numerator/denominator'
    measures_nd_ids <- unique(raw_data() %>% 
                                # Use the cleaned, lower-case value for filtering
                                filter(measure_display_type == "numerator/denominator") %>% 
                                pull(measure))
    
    # Filter the full choices list
    unique_choices_nd <- measure_choices()[measure_choices() %in% measures_nd_ids]
    
    if (length(unique_choices_nd) == 0) {
      return(p("No Numerator/Denominator measures found in data."))
    }
    
    selectInput(
      "measure_select_nd",
      "Select Measure:",
      choices = unique_choices_nd,
      selected = unique_choices_nd[1]
    )
  })
  
  # --- Conditional Main Content Render ---
  output$main_content <- renderUI({
    # Display initial message if no file uploaded
    if (is.null(input$file_upload)) {
      return(
        div(class = "mt-5 p-5 text-center bg-light border rounded",
            h3("Welcome to the Process Measures Dashboard"),
            p("Please upload a CSV or Excel file containing the process measures data using the control on the left to begin.")
        )
      )
    }
    
    # Check if data exists and measures are available
    if (is.null(input$measure_select_nd) || length(input$measure_select_nd) == 0) {
      # Message shown if data is uploaded but no N/D measures are found
      return(
        div(class = "mt-5 p-5 text-center bg-warning border rounded",
            h3("No Numerator/Denominator Measures Found"),
            p("The uploaded data does not contain any measures marked with 'numerator/denominator' 
              in the 'measure_display_type' column for the selected year and available data. 
              Please ensure the column value is correct (e.g., 'numerator/denominator').")
        )
      )
    } else {
      # Content shown after successful upload and filter selection
      tagList(
        h3(textOutput("nd_title")),
        # Line plot for time series trends by hospital
        plotlyOutput("nd_time_series_plot", height = "400px"),
        
        # Conditional Bar chart displayed after a click event
        h4(textOutput("nd_bar_chart_title")),
        # Height is maintained at 400px to accommodate the vertical labels
        plotlyOutput("nd_hospital_bar_chart", height = "400px") 
      )
    }
  })
  
  
  # --- Reactive Data Preparation for Numerator/Denominator ---
  
  # 1. Filter data based on measure type, selected year, and measure
  nd_data_filtered <- reactive({
    req(raw_data(), input$year_select, input$measure_select_nd)
    
    # Filter by measure type, year, and selected measure
    raw_data() %>%
      filter(
        measure_display_type == "numerator/denominator",
        year == input$year_select,
        measure == input$measure_select_nd
      )
  })
  
  # 2. Aggregate data to Quarter and Hospital level
  nd_data_aggregated <- reactive({
    df_agg <- nd_data_filtered() %>%
      # Group by all three name columns (FullName, Name, AbbName) and the quarter
      group_by(FullName, Name, AbbName, quarter) %>%
      summarise(
        total_numerator = sum(numerator, na.rm = TRUE),
        total_denominator = sum(denominator, na.rm = TRUE),
        rate = ifelse(total_denominator > 0, total_numerator / total_denominator, 0),
        .groups = 'drop'
      ) %>%
      # Ensure quarter is a factor for proper X-axis ordering
      mutate(quarter_string = factor(paste0("Q", quarter), levels = paste0("Q", 1:4)))
    
    return(df_agg)
  })
  
  # --- Output: Time Series Line Plot (Numerator/Denominator) ---
  output$nd_title <- renderText({
    measure_name <- input$measure_select_nd # Measure is already cleaned
    paste0("Time Series Trend: ", measure_name, " (", input$year_select, ")")
  })
  
  output$nd_time_series_plot <- renderPlotly({
    data_plot <- nd_data_aggregated()
    
    if (nrow(data_plot) == 0) {
      return(NULL)
    }
    
    # Get the number of unique hospitals to select enough colors
    n_hospitals <- length(unique(data_plot$FullName))
    # Use 'Paired' which supports up to 12 colors
    color_palette <- brewer.pal(n = max(3, n_hospitals), name = "Paired")
    
    p <- data_plot %>%
      # Use FullName for color (legend) and grouping
      ggplot(aes(x = quarter_string, y = rate, group = FullName, color = FullName,
                 # Use FullName in hover text for the main graph
                 text = paste("Hospital:", FullName, 
                              "<br>Quarter:", quarter_string,
                              "<br>Rate:", scales::percent(rate, accuracy = 0.1),
                              "<br>N (Denominator):", total_denominator))) +
      geom_line(linewidth = 1) +
      geom_point(size = 3) +
      # Y-axis limits to 0-100% with 25% breaks
      scale_y_continuous(labels = scales::percent, limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
      # Apply the new color scale
      scale_color_manual(values = color_palette) +
      labs(
        x = "Quarter",
        y = "Rate (%)",
        color = "Hospital"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    # Convert to plotly, specifying hover and click features
    ggplotly(p, tooltip = "text", source = "nd_time_series_plot_source") %>%
      config(displaylogo = FALSE, 
             modeBarButtonsToRemove = list('sendDataToCloud', 'hoverClosestCartesian', 'hoverCompareCartesian'))
  })
  
  # --- Output: Bar Chart (Hospital Comparison) via Click Event ---
  
  # Reactive value to store the quarter selected by the click
  selected_quarter_num <- reactiveVal(NULL)
  
  # Observe click event on the time series plot
  observeEvent(event_data("plotly_click", source = "nd_time_series_plot_source"), {
    click_data <- event_data("plotly_click", source = "nd_time_series_plot_source")
    
    # Extract the clicked quarter label (e.g., "Q1")
    if (!is.null(click_data) && "x" %in% names(click_data)) {
      quarter_clicked_string <- as.character(click_data$x)
      # Extract just the number (e.g., "Q1" -> 1) for filtering the aggregated data
      quarter_clicked_num <- as.numeric(gsub("Q", "", quarter_clicked_string))
      selected_quarter_num(quarter_clicked_num)
    }
  })
  
  # Generate Bar Chart Title
  output$nd_bar_chart_title <- renderText({
    if (is.null(selected_quarter_num())) {
      return("Click a point on the line chart above to compare hospitals in a specific quarter.")
    } else {
      measure_name <- input$measure_select_nd
      paste0("Hospital Comparison for ", measure_name, " in ", input$year_select, " Q", selected_quarter_num())
    }
  })
  
  # Generate Bar Chart
  output$nd_hospital_bar_chart <- renderPlotly({
    req(selected_quarter_num())
    
    # Filter the aggregated data for the selected quarter number
    data_bar <- nd_data_aggregated() %>%
      filter(quarter == selected_quarter_num())
    
    if (nrow(data_bar) == 0) {
      return(NULL)
    }
    
    # Get the number of unique hospitals to select enough colors
    n_hospitals <- length(unique(data_bar$AbbName))
    color_palette <- brewer.pal(n = max(3, n_hospitals), name = "Paired")
    
    # Create the bar chart
    p_bar <- data_bar %>%
      # X-Axis: Use AbbName (abbreviation) for axis label and sorting
      # Fill/Color: Use FullName to keep colors consistent with the line chart
      ggplot(aes(x = reorder(AbbName, rate), y = rate, fill = FullName,
                 # Hover Text: Use Name (full name without abbreviation)
                 text = paste("Hospital:", Name, 
                              "<br>Rate:", scales::percent(rate, accuracy = 0.1),
                              "<br>N (Denominator):", total_denominator))) +
      geom_bar(stat = "identity") +
      # Y-axis limits to 0-100% with 25% breaks
      scale_y_continuous(labels = scales::percent, limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
      # Apply the new color scale
      # NOTE: scale_fill_manual must use FullName as the mapping key now
      scale_fill_manual(values = color_palette) +
      labs(
        x = "Hospital Abbreviation",
        y = "Rate (%)"
      ) +
      theme_minimal() +
      theme(
        # The 90-degree rotation is kept here for safety in case ggplotly doesn't handle the short names well
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
        legend.position = "none" # Legend is redundant in bar chart
      )
    
    # Convert to plotly
    ggplotly(p_bar, tooltip = "text") %>%
      layout(title = "", xaxis = list(title = "Hospital (Abbreviation)")) %>%
      config(displaylogo = FALSE, 
             modeBarButtonsToRemove = list('sendDataToCloud', 'hoverClosestCartesian', 'hoverCompareCartesian'))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
