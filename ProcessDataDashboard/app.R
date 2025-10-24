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
library(RColorBrewer) 

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
  titlePanel("Process Measures Dashboard"), # Removed Data Upload from title since it's now in the first tab
  
  # Main layout with tabs for measure types
  tabsetPanel(
    id = "measure_type_tabs",
    
    # NEW: Measure Index Tab (Table of Contents)
    tabPanel("Measure Index",
             value = "toc_tab",
             fluidRow(
               # 1. File Upload Control - MOVED HERE
               column(12, 
                      div(style = "padding: 15px; border: 1px solid #ccc; border-radius: 5px; background-color: #f9f9f9; margin-bottom: 20px;",
                          h4("1. Upload Data File"),
                          fileInput("file_upload", "Upload Data (CSV or Excel)",
                                    multiple = FALSE,
                                    accept = c(".csv", ".xls", ".xlsx"))
                      )
               ),
               # 2. Measure Index Content
               column(12, 
                      uiOutput("toc_page_content")
               )
             )
    ),
    
    # Numerator/Denominator Tab - Current Focus
    tabPanel("Numerator/Denominator",
             value = "nd_tab",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 h4("Filter Controls: Num/Den"),
                 
                 # 2. Year Filter (will be dynamically updated)
                 uiOutput("year_select_ui_nd"), 
                 
                 # 3. Measure Selector (will be dynamically updated)
                 uiOutput("measure_select_nd_ui")
               ),
               
               mainPanel(
                 width = 9,
                 # Conditional rendering of content based on file upload status
                 uiOutput("main_content_nd") 
               )
             )
    ),
    
    # Raw Value Tab
    tabPanel("Raw Value", 
             value = "raw_tab",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 h4("Filter Controls: Raw Value"),
                 
                 # Year Filter for Raw Value
                 uiOutput("year_select_ui_raw"), 
                 
                 # Measure Selector for Raw Value measures
                 uiOutput("measure_select_raw_ui")
               ),
               
               mainPanel(
                 width = 9,
                 uiOutput("main_content_raw") 
               )
             )
    ),
    
    # Range Tab 
    tabPanel("Range",
             value = "range_tab",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 h4("Filter Controls: Range"),
                 
                 # The file input is shared, but we need the Year filter here too
                 uiOutput("year_select_ui_range"), 
                 
                 # Measure Selector for Range measures
                 uiOutput("measure_select_range_ui")
               ),
               
               mainPanel(
                 width = 9,
                 uiOutput("main_content_range") 
               )
             )
    )
  )
)

# --- SERVER LOGIC ---
server <- function(input, output, session) {
  
  # Hospital Name Mapping (hardcoded as this is a lookup table)
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
    req(input$file_upload) 
    
    ext <- tools::file_ext(input$file_upload$name)
    file_path <- input$file_upload$datapath
    
    # Read the data based on file type
    df <- tryCatch({
      if (ext == "csv") {
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
    
    # 3. Data preprocessing 
    smm_data <- df %>%
      mutate(
        # Crucial step: Clean the filter column value to ensure case-insensitive matching
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
    
    # Robustness Check: Ensure the join was successful and created the FullName column
    if (!("FullName" %in% names(smm_data)) || all(is.na(smm_data$FullName))) {
      validate("Hospital mapping failed. Ensure the 'masked_name' column in your data exactly matches the lookup table values.")
    }
    
    smm_data
  })
  
  # Reactive list of unique formatted measures for UI dropdowns
  measure_choices <- reactive({
    df <- raw_data()
    req(df, "measure")
    
    measures <- unique(df$measure)
    display_names <- unique(df$measure) 
    
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
  
  # --- TABLE OF CONTENTS LOGIC ---
  
  # Reactive list of measures organized and padded for the TOC table
  measure_list_by_type <- reactive({
    df <- raw_data()
    req(df) # Require data upload
    
    # 1. Get unique, cleaned measures and their types
    measures_unique <- df %>%
      select(measure, measure_display_type) %>%
      distinct() %>%
      # Clean up the display type column for consistent grouping
      mutate(
        clean_type = case_when(
          tolower(measure_display_type) == "numerator/denominator" ~ "Numerator/Denominator",
          tolower(measure_display_type) == "raw value" ~ "Raw Value",
          tolower(measure_display_type) == "range" ~ "Range",
          .default = "Other"
        )
      ) %>%
      filter(clean_type %in% c("Numerator/Denominator", "Raw Value", "Range"))
    
    # 2. Group, sort, and extract the lists
    list_nd <- measures_unique %>% filter(clean_type == "Numerator/Denominator") %>% pull(measure) %>% unique() %>% sort()
    list_raw <- measures_unique %>% filter(clean_type == "Raw Value") %>% pull(measure) %>% unique() %>% sort()
    list_range <- measures_unique %>% filter(clean_type == "Range") %>% pull(measure) %>% unique() %>% sort()
    
    # 3. Pad the lists to the maximum length
    max_len <- max(length(list_nd), length(list_raw), length(list_range))
    
    # Padding function
    pad_list <- function(l, max_len) {
      length(l) <- max_len
      return(l)
    }
    
    # Create the final data frame
    toc_df <- data.frame(
      `Numerator/Denominator` = pad_list(list_nd, max_len),
      `Raw Value` = pad_list(list_raw, max_len),
      `Range` = pad_list(list_range, max_len),
      check.names = FALSE # Keep column names as they are
    )
    
    return(toc_df)
  })
  
  # Render the TOC page content
  output$toc_page_content <- renderUI({
    if (is.null(input$file_upload)) {
      return(
        div(class = "mt-5 p-5 text-center",
            h3("Welcome to the Process Measures Dashboard"),
            p("Please upload a CSV or Excel file using the control above to view the measure index and begin analysis.")
        )
      )
    }
    
    # Once data is uploaded, show the TOC
    tagList(
      h2("Measure Index: Table of Contents"),
      p("Below are all the unique measures found in the uploaded file, categorized by their display type. Select a tab above to explore the data for that measure type."),
      hr(),
      tableOutput("measure_toc_table")
    )
  })
  
  # Render the actual TOC table
  output$measure_toc_table <- renderTable({
    req(raw_data())
    toc_df <- measure_list_by_type()
    
    # Convert NA to empty string for cleaner display
    toc_df[is.na(toc_df)] <- ""
    
    return(toc_df)
  }, striped = TRUE, bordered = TRUE, hover = TRUE, align = 'l')
  
  
  # --- UI Generation based on Data (Remaining Tabs) ---
  
  # Render the Year Select Input (dynamic based on uploaded data) - ND Tab
  output$year_select_ui_nd <- renderUI({
    # Only render if data is uploaded
    if (is.null(input$file_upload)) { return(p("Please upload data on the Measure Index tab.")) }
    req(raw_data())
    selectInput(
      "year_select_nd",
      "Select Year:",
      choices = year_choices(),
      selected = max(year_choices())
    )
  })
  
  # Render the Year Select Input (dynamic based on uploaded data) - Raw Tab (Adjusted)
  output$year_select_ui_raw <- renderUI({
    # Only render if data is uploaded
    if (is.null(input$file_upload)) { return(p("Please upload data on the Measure Index tab.")) }
    req(raw_data())
    
    # Filter years to only include those with raw value data and up to 2023
    raw_data_years <- raw_data() %>%
      filter(measure_display_type == "raw value", as.numeric(as.character(year)) <= 2023) %>%
      pull(year) %>%
      unique() %>%
      as.character()
    
    sorted_years <- sort(raw_data_years, decreasing = TRUE)
    
    # Set default selected year to the max year found in the filtered list
    selected_year <- if (length(sorted_years) > 0) max(sorted_years) else NULL
    
    selectInput(
      "year_select_raw",
      "Select Year:",
      choices = sorted_years,
      selected = selected_year
    )
  })
  
  # Render the Year Select Input (dynamic based on uploaded data) - Range Tab
  output$year_select_ui_range <- renderUI({
    # Only render if data is uploaded
    if (is.null(input$file_upload)) { return(p("Please upload data on the Measure Index tab.")) }
    req(raw_data())
    selectInput(
      "year_select_range",
      "Select Year:",
      choices = year_choices(),
      selected = max(year_choices())
    )
  })
  
  # Render the Measure Select Input for Numerator/Denominator tab
  output$measure_select_nd_ui <- renderUI({
    if (is.null(input$file_upload)) { return(p("Please upload data on the Measure Index tab.")) }
    req(raw_data(), "measure_display_type")
    
    # Filter choices to only include measures of type 'numerator/denominator'
    measures_nd_ids <- unique(raw_data() %>% 
                                filter(measure_display_type == "numerator/denominator") %>% 
                                pull(measure))
    
    unique_choices_nd <- measure_choices()[measure_choices() %in% measures_nd_ids]
    
    if (length(unique_choices_nd) == 0) {
      return(p("No Numerator/Denominator measures found in data for the selected year."))
    }
    
    selectInput(
      "measure_select_nd",
      "Select Measure:",
      choices = unique_choices_nd,
      selected = unique_choices_nd[1]
    )
  })
  
  # Render the Measure Select Input for Raw Value tab (UPDATED)
  output$measure_select_raw_ui <- renderUI({
    if (is.null(input$file_upload)) { return(p("Please upload data on the Measure Index tab.")) }
    req(raw_data(), "measure_display_type")
    
    # Filter choices to only include measures of type 'raw value'
    measures_raw_ids <- unique(raw_data() %>% 
                                 filter(measure_display_type == "raw value") %>% 
                                 pull(measure))
    
    unique_choices_raw <- measure_choices()[measure_choices() %in% measures_raw_ids]
    
    if (length(unique_choices_raw) == 0) {
      return(p("No Raw Value measures found in data for the selected year."))
    }
    
    selectInput(
      "measure_select_raw",
      "Select Measure:",
      choices = unique_choices_raw,
      selected = unique_choices_raw[1]
    )
  })
  
  # Render the Measure Select Input for Range tab 
  output$measure_select_range_ui <- renderUI({
    if (is.null(input$file_upload)) { return(p("Please upload data on the Measure Index tab.")) }
    req(raw_data(), "measure_display_type")
    
    # Filter choices to only include measures of type 'range'
    measures_range_ids <- unique(raw_data() %>% 
                                   filter(measure_display_type == "range") %>% 
                                   pull(measure))
    
    unique_choices_range <- measure_choices()[measure_choices() %in% measures_range_ids]
    
    if (length(unique_choices_range) == 0) {
      return(p("No Range measures found in data for the selected year."))
    }
    
    selectInput(
      "measure_select_range",
      "Select Measure:",
      choices = unique_choices_range,
      selected = unique_choices_range[1]
    )
  })
  
  
  # --- Conditional Main Content Render ---
  
  # ND Tab Content
  output$main_content_nd <- renderUI({
    if (is.null(input$file_upload)) {
      return(
        div(class = "mt-5 p-5 text-center bg-light border rounded",
            h3("Data Required"),
            p("Please upload a CSV or Excel file using the control on the Measure Index tab to begin.")
        )
      )
    }
    
    # Check if a measure is selected for ND tab
    if (is.null(input$measure_select_nd) || length(input$measure_select_nd) == 0) {
      return(
        div(class = "mt-5 p-5 text-center bg-warning border rounded",
            h3("No Numerator/Denominator Measures Available"),
            p("The uploaded data does not contain any measures marked with 'numerator/denominator' 
              in the 'measure_display_type' column for the selected year and available data. 
              Please ensure the column value is correct.")
        )
      )
    } else {
      tagList(
        h3(textOutput("nd_title")),
        plotlyOutput("nd_time_series_plot", height = "400px"),
        h4(textOutput("nd_bar_chart_title")),
        plotlyOutput("nd_hospital_bar_chart", height = "400px") 
      )
    }
  })
  
  # Raw Value Tab Content
  output$main_content_raw <- renderUI({
    if (is.null(input$file_upload)) {
      return(
        div(class = "mt-5 p-5 text-center bg-light border rounded",
            h3("Data Required"),
            p("Please upload a CSV or Excel file using the control on the Measure Index tab to begin.")
        )
      )
    }
    
    # Check if a measure is selected for Raw tab
    if (is.null(input$measure_select_raw) || length(input$measure_select_raw) == 0) {
      return(
        div(class = "mt-5 p-5 text-center bg-warning border rounded",
            h3("No Raw Value Measures Available"),
            p("The uploaded data does not contain any measures marked with 'raw value' 
              in the 'measure_display_type' column for the selected year and available data. 
              Please ensure the column value is correct.")
        )
      )
    } else {
      tagList(
        h3(textOutput("raw_title")),
        plotlyOutput("raw_time_series_plot", height = "400px"),
        h4(textOutput("raw_bar_chart_title")),
        plotlyOutput("raw_hospital_bar_chart", height = "400px") 
      )
    }
  })
  
  # Range Tab Content
  output$main_content_range <- renderUI({
    if (is.null(input$file_upload)) {
      return(
        div(class = "mt-5 p-5 text-center bg-light border rounded",
            h3("Data Required"),
            p("Please upload a CSV or Excel file using the control on the Measure Index tab to begin.")
        )
      )
    }
    
    # Check if a measure is selected for Range tab
    if (is.null(input$measure_select_range) || length(input$measure_select_range) == 0) {
      return(
        div(class = "mt-5 p-5 text-center bg-warning border rounded",
            h3("No Range Measures Available"),
            p("The uploaded data does not contain any measures marked with 'range' 
              in the 'measure_display_type' column for the selected year and available data. 
              Please ensure the column value is correct.")
        )
      )
    } else {
      tagList(
        h3(textOutput("range_title")),
        plotlyOutput("range_time_series_plot", height = "400px"),
        h4(textOutput("range_bar_chart_title")),
        plotlyOutput("range_hospital_bar_chart", height = "400px") 
      )
    }
  })
  
  
  # ----------------------------------------------------------------------------------
  # --- Numerator/Denominator Logic --------------------------------------------------
  # ----------------------------------------------------------------------------------
  
  # 1. Filter data based on measure type, selected year, and measure
  nd_data_filtered <- reactive({
    req(raw_data(), input$measure_select_nd) # Require raw data and the selected measure
    
    df <- raw_data()
    
    # Robustly determine the selected year (use max year as default/fallback if input is NULL)
    selected_year <- if (is.null(input$year_select_nd)) {
      max(as.character(df$year), na.rm = TRUE)
    } else {
      input$year_select_nd
    }
    
    # Filter by measure type, year, and selected measure
    df_filtered <- df %>%
      filter(
        measure_display_type == "numerator/denominator",
        year == selected_year, # Use the robustly determined year
        measure == input$measure_select_nd
      )
    
    # *** ADDED VALIDATION: Check if data exists after initial filtering ***
    validate(
      need(nrow(df_filtered) > 0, 
           paste0("No raw data points found for '", input$measure_select_nd, 
                  "' in year ", selected_year, 
                  ". Please verify the raw data for this measure/year combination."))
    )
    
    return(df_filtered)
  })
  
  # 2. Aggregate data to Quarter and Hospital level
  nd_data_aggregated <- reactive({
    req(nrow(nd_data_filtered()) > 0)
    df_agg <- nd_data_filtered() %>%
      # Group by all three name columns (FullName, Name, AbbName) and the quarter
      group_by(FullName, Name, AbbName, quarter) %>%
      summarise(
        # N/D logic: SUMMING N and D for traditional process measure aggregation
        total_numerator = sum(numerator, na.rm = TRUE),
        total_denominator = sum(denominator, na.rm = TRUE),
        rate = ifelse(total_denominator > 0, total_numerator / total_denominator, 0),
        .groups = 'drop'
      ) %>%
      # Ensure quarter is a factor for proper X-axis ordering
      mutate(quarter_string = factor(paste0("Q", quarter), levels = paste0("Q", 1:4)))
    
    # Filter out NaNs and infinities which can occur if numerator/denominator are bad
    df_agg <- df_agg %>% filter(!is.nan(rate) & !is.infinite(rate))
    
    # *** ADDED VALIDATION: Check if data exists after aggregation ***
    validate(
      need(nrow(df_agg) > 0, 
           "Filtered data exists but failed to aggregate by quarter (likely all denominators were zero or NA for all hospitals/quarters).")
    )
    
    return(df_agg)
  })
  
  # 3. Output: Time Series Line Plot (Numerator/Denominator)
  output$nd_title <- renderText({
    req(input$year_select_nd)
    measure_name <- input$measure_select_nd
    paste0("Time Series Trend: ", measure_name, " (", input$year_select_nd, ")")
  })
  
  output$nd_time_series_plot <- renderPlotly({
    req(nrow(nd_data_aggregated()) > 0)
    data_plot <- nd_data_aggregated()
    
    # Set levels explicitly based on the data present to avoid plotting empty factors
    present_quarters <- unique(data_plot$quarter)
    data_plot$quarter_string <- factor(
      data_plot$quarter_string, 
      levels = paste0("Q", sort(as.numeric(present_quarters)))
    )
    
    n_hospitals <- length(unique(data_plot$FullName))
    color_palette <- brewer.pal(n = max(3, n_hospitals), name = "Paired")
    
    p <- data_plot %>%
      ggplot(aes(x = quarter_string, y = rate, group = FullName, color = FullName,
                 key = quarter, # <-- ADDED KEY FOR ROBUST CLICK HANDLING
                 text = paste("Hospital:", FullName, 
                              "<br>Quarter:", quarter_string,
                              "<br>Rate:", scales::percent(rate, accuracy = 0.1),
                              "<br>N (Denominator):", total_denominator))) +
      geom_line(linewidth = 1) +
      geom_point(size = 3) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
      scale_color_manual(values = color_palette) +
      labs(x = "Quarter", y = "Rate (%)", color = "Hospital") +
      theme_minimal() + theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text", source = "nd_time_series_plot_source") %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = list('sendDataToCloud', 'hoverClosestCartesian', 'hoverCompareCartesian'))
  })
  
  # 4. Bar Chart Trigger and Logic (Numerator/Denominator)
  selected_quarter_num_nd <- reactiveVal(NULL) 
  
  observeEvent(event_data("plotly_click", source = "nd_time_series_plot_source"), {
    click_data <- event_data("plotly_click", source = "nd_time_series_plot_source")
    
    if (!is.null(click_data) && "key" %in% names(click_data)) {
      # Use the 'key' (quarter number) for robust selection
      quarter_clicked_num <- as.numeric(click_data$key)
      selected_quarter_num_nd(quarter_clicked_num)
    }
  })
  
  output$nd_bar_chart_title <- renderText({
    if (is.null(selected_quarter_num_nd())) {
      return("Click a point on the line chart above to compare hospitals in a specific quarter.")
    } else {
      measure_name <- input$measure_select_nd
      paste0("Hospital Comparison for ", measure_name, " in ", input$year_select_nd, " Q", selected_quarter_num_nd())
    }
  })
  
  output$nd_hospital_bar_chart <- renderPlotly({
    req(selected_quarter_num_nd(), nrow(nd_data_aggregated()) > 0)
    
    data_bar <- nd_data_aggregated() %>%
      filter(quarter == selected_quarter_num_nd())
    
    if (nrow(data_bar) == 0) { return(NULL) }
    
    n_hospitals <- length(unique(data_bar$AbbName))
    color_palette <- brewer.pal(n = max(3, n_hospitals), name = "Paired")
    
    p_bar <- data_bar %>%
      ggplot(aes(x = reorder(AbbName, rate), y = rate, fill = FullName,
                 text = paste("Hospital:", Name, 
                              "<br>Rate:", scales::percent(rate, accuracy = 0.1),
                              "<br>N (Denominator):", total_denominator))) +
      geom_bar(stat = "identity") +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
      scale_fill_manual(values = color_palette) +
      labs(x = "Hospital Abbreviation", y = "Rate (%)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
            legend.position = "none")
    
    ggplotly(p_bar, tooltip = "text") %>%
      layout(title = "", xaxis = list(title = "Hospital (Abbreviation)")) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = list('sendDataToCloud', 'hoverClosestCartesian', 'hoverCompareCartesian'))
  })
  
  # ----------------------------------------------------------------------------------
  # --- Raw Value Logic --------------------------------------------------------------
  # ----------------------------------------------------------------------------------
  
  # 1. Filter data based on measure type, selected year, and measure
  raw_data_filtered <- reactive({
    req(raw_data(), input$measure_select_raw)
    
    df <- raw_data()
    
    # Robustly determine the selected year
    selected_year <- if (is.null(input$year_select_raw)) {
      # Use the max year up to 2023 for raw data if input is null
      max(as.character(df$year[df$measure_display_type == "raw value" & as.numeric(as.character(df$year)) <= 2023]), na.rm = TRUE)
    } else {
      input$year_select_raw
    }
    
    # Filter by measure type, year, and selected measure
    df_filtered <- df %>%
      filter(
        measure_display_type == "raw value",
        year == selected_year,
        measure == input$measure_select_raw
      )
    
    validate(
      need(nrow(df_filtered) > 0, 
           paste0("No raw data points found for '", input$measure_select_raw, 
                  "' in year ", selected_year, 
                  ". Please verify the raw data for this measure/year combination."))
    )
    
    return(df_filtered)
  })
  
  # 2. Aggregate data to Quarter and Hospital level (Sum of raw counts)
  raw_data_aggregated <- reactive({
    req(nrow(raw_data_filtered()) > 0)
    df_agg <- raw_data_filtered() %>%
      group_by(FullName, Name, AbbName, quarter) %>%
      summarise(
        # Raw value logic: SUM the numerator (the raw count)
        total_count = sum(numerator, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      mutate(quarter_string = factor(paste0("Q", quarter), levels = paste0("Q", 1:4)))
    
    df_agg <- df_agg %>% filter(!is.nan(total_count) & !is.infinite(total_count))
    
    validate(
      need(nrow(df_agg) > 0, 
           "Filtered data exists but failed to aggregate by quarter. Check if the 'numerator' column contains valid numbers for aggregation.")
    )
    
    return(df_agg)
  })
  
  # 3. Output: Time Series Line Plot (Raw Value)
  output$raw_title <- renderText({
    req(input$year_select_raw)
    measure_name <- input$measure_select_raw
    paste0("Time Series Trend: ", measure_name, " (", input$year_select_raw, ") - Raw Count")
  })
  
  output$raw_time_series_plot <- renderPlotly({
    req(nrow(raw_data_aggregated()) > 0)
    data_plot <- raw_data_aggregated()
    
    # Set levels explicitly based on the data present to avoid plotting empty factors
    present_quarters <- unique(data_plot$quarter)
    data_plot$quarter_string <- factor(
      data_plot$quarter_string, 
      levels = paste0("Q", sort(as.numeric(present_quarters)))
    )
    
    n_hospitals <- length(unique(data_plot$FullName))
    color_palette <- brewer.pal(n = max(3, n_hospitals), name = "Paired")
    
    p <- data_plot %>%
      ggplot(aes(x = quarter_string, y = total_count, group = FullName, color = FullName,
                 key = quarter, # <-- FIX: Use quarter as key
                 text = paste("Hospital:", FullName, 
                              "<br>Quarter:", quarter_string,
                              "<br>Total Count:", total_count))) +
      geom_line(linewidth = 1) +
      geom_point(size = 3) +
      # Fixed Y-axis limit of 5, with integer breaks
      scale_y_continuous(limits = c(0, 5), breaks = 0:5) +
      scale_color_manual(values = color_palette) +
      labs(x = "Quarter", y = "Total Count", color = "Hospital") + 
      theme_minimal() + theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text", source = "raw_time_series_plot_source") %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = list('sendDataToCloud', 'hoverClosestCartesian', 'hoverCompareCartesian'))
  })
  
  # 4. Bar Chart Trigger and Logic (Raw Value)
  selected_quarter_num_raw <- reactiveVal(NULL) 
  
  observeEvent(event_data("plotly_click", source = "raw_time_series_plot_source"), {
    click_data <- event_data("plotly_click", source = "raw_time_series_plot_source")
    
    if (!is.null(click_data) && "key" %in% names(click_data)) {
      # FIX: Use the 'key' (quarter number) for robust selection
      quarter_clicked_num <- as.numeric(click_data$key)
      selected_quarter_num_raw(quarter_clicked_num)
    }
  })
  
  output$raw_bar_chart_title <- renderText({
    if (is.null(selected_quarter_num_raw())) {
      return("Click a point on the line chart above to compare hospitals in a specific quarter.")
    } else {
      measure_name <- input$measure_select_raw
      paste0("Hospital Comparison for ", measure_name, " in ", input$year_select_raw, " Q", selected_quarter_num_raw())
    }
  })
  
  output$raw_hospital_bar_chart <- renderPlotly({
    req(selected_quarter_num_raw(), nrow(raw_data_aggregated()) > 0)
    
    data_bar <- raw_data_aggregated() %>%
      filter(quarter == selected_quarter_num_raw())
    
    if (nrow(data_bar) == 0) { return(NULL) }
    
    n_hospitals <- length(unique(data_bar$AbbName))
    color_palette <- brewer.pal(n = max(3, n_hospitals), name = "Paired")
    
    p_bar <- data_bar %>%
      ggplot(aes(x = reorder(AbbName, total_count), y = total_count, fill = FullName,
                 text = paste("Hospital:", Name, 
                              "<br>Total Count:", total_count))) + 
      geom_bar(stat = "identity") +
      # Fixed Y-axis limit of 5, with integer breaks
      scale_y_continuous(limits = c(0, 5), breaks = 0:5) +
      scale_fill_manual(values = color_palette) +
      labs(x = "Hospital Abbreviation", y = "Total Count") + 
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
            legend.position = "none")
    
    ggplotly(p_bar, tooltip = "text") %>%
      layout(title = "", xaxis = list(title = "Hospital (Abbreviation)")) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = list('sendDataToCloud', 'hoverClosestCartesian', 'hoverCompareCartesian'))
  })
  
  # ----------------------------------------------------------------------------------
  # --- Range Logic ------------------------------------------------------------------
  # ----------------------------------------------------------------------------------
  
  # 1. Filter data based on measure type, selected year, and measure
  range_data_filtered <- reactive({
    req(raw_data(), input$measure_select_range) # Require raw data and the selected measure
    
    df <- raw_data()
    
    # Robustly determine the selected year (use max year as default/fallback if input is NULL)
    selected_year <- if (is.null(input$year_select_range)) {
      max(as.character(df$year), na.rm = TRUE)
    } else {
      input$year_select_range
    }
    
    # Filter by measure type (cleaned to "range"), year, and selected measure
    df_filtered <- df %>%
      filter(
        measure_display_type == "range",
        year == selected_year, # Use the robustly determined year
        measure == input$measure_select_range
      )
    
    # *** VALIDATION: Check if data exists after initial filtering ***
    validate(
      need(nrow(df_filtered) > 0, 
           paste0("No raw data points found for '", input$measure_select_range, 
                  "' in year ", selected_year, 
                  ". Please verify the raw data for this measure/year combination."))
    )
    
    return(df_filtered)
  })
  
  # 2. Aggregate data to Quarter and Hospital level
  range_data_aggregated <- reactive({
    req(nrow(range_data_filtered()) > 0)
    df_agg <- range_data_filtered() %>%
      group_by(FullName, Name, AbbName, quarter) %>%
      summarise(
        # Range logic: Calculate the rate as the mean of the numerator 
        rate = mean(numerator, na.rm = TRUE) / 100, 
        
        # For display, we can show the count of records that contributed to the average
        count = n(), 
        .groups = 'drop'
      ) %>%
      mutate(quarter_string = factor(paste0("Q", quarter), levels = paste0("Q", 1:4)))
    
    # Filter out NaNs and infinities which can occur if the numerator values were bad
    df_agg <- df_agg %>% filter(!is.nan(rate) & !is.infinite(rate))
    
    # *** VALIDATION: Check if data exists after aggregation and cleaning ***
    validate(
      need(nrow(df_agg) > 0, 
           "Filtered data exists but failed to aggregate by quarter. Check if the 'numerator' column contains valid numbers for aggregation.")
    )
    
    return(df_agg)
  })
  
  # 3. Output: Time Series Line Plot (Range)
  output$range_title <- renderText({
    req(input$year_select_range)
    measure_name <- input$measure_select_range
    paste0("Time Series Trend: ", measure_name, " (", input$year_select_range, ")")
  })
  
  output$range_time_series_plot <- renderPlotly({
    req(nrow(range_data_aggregated()) > 0)
    data_plot <- range_data_aggregated()
    
    # Set levels explicitly based on the data present to avoid plotting empty factors
    present_quarters <- unique(data_plot$quarter)
    data_plot$quarter_string <- factor(
      data_plot$quarter_string, 
      levels = paste0("Q", sort(as.numeric(present_quarters)))
    )
    
    n_hospitals <- length(unique(data_plot$FullName))
    color_palette <- brewer.pal(n = max(3, n_hospitals), name = "Paired")
    
    p <- data_plot %>%
      ggplot(aes(x = quarter_string, y = rate, group = FullName, color = FullName,
                 key = quarter, # <-- ADDED KEY FOR ROBUST CLICK HANDLING
                 text = paste("Hospital:", FullName, 
                              "<br>Quarter:", quarter_string,
                              "<br>Avg. Rate:", scales::percent(rate, accuracy = 0.1),
                              "<br>N (Records):", count))) + 
      geom_line(linewidth = 1) +
      geom_point(size = 3) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
      scale_color_manual(values = color_palette) +
      labs(x = "Quarter", y = "Average Rate (%)", color = "Hospital") + 
      theme_minimal() + theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = "text", source = "range_time_series_plot_source") %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = list('sendDataToCloud', 'hoverClosestCartesian', 'hoverCompareCartesian'))
  })
  
  # 4. Bar Chart Trigger and Logic (Range)
  selected_quarter_num_range <- reactiveVal(NULL) 
  
  observeEvent(event_data("plotly_click", source = "range_time_series_plot_source"), {
    click_data <- event_data("plotly_click", source = "range_time_series_plot_source")
    
    if (!is.null(click_data) && "key" %in% names(click_data)) {
      # Use the 'key' (quarter number) for robust selection
      quarter_clicked_num <- as.numeric(click_data$key)
      selected_quarter_num_range(quarter_clicked_num)
    }
  })
  
  output$range_bar_chart_title <- renderText({
    if (is.null(selected_quarter_num_range())) {
      return("Click a point on the line chart above to compare hospitals in a specific quarter.")
    } else {
      measure_name <- input$measure_select_range
      paste0("Hospital Comparison for ", measure_name, " in ", input$year_select_range, " Q", selected_quarter_num_range())
    }
  })
  
  output$range_hospital_bar_chart <- renderPlotly({
    req(selected_quarter_num_range(), nrow(range_data_aggregated()) > 0)
    
    data_bar <- range_data_aggregated() %>%
      filter(quarter == selected_quarter_num_range())
    
    if (nrow(data_bar) == 0) { return(NULL) }
    
    n_hospitals <- length(unique(data_bar$AbbName))
    color_palette <- brewer.pal(n = max(3, n_hospitals), name = "Paired")
    
    p_bar <- data_bar %>%
      ggplot(aes(x = reorder(AbbName, rate), y = rate, fill = FullName,
                 text = paste("Hospital:", Name, 
                              "<br>Avg. Rate:", scales::percent(rate, accuracy = 0.1),
                              "<br>N (Records):", count))) + 
      geom_bar(stat = "identity") +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
      scale_fill_manual(values = color_palette) +
      labs(x = "Hospital Abbreviation", y = "Average Rate (%)") + 
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
            legend.position = "none")
    
    ggplotly(p_bar, tooltip = "text") %>%
      layout(title = "", xaxis = list(title = "Hospital (Abbreviation)")) %>%
      config(displaylogo = FALSE, modeBarButtonsToRemove = list('sendDataToCloud', 'hoverClosestCartesian', 'hoverCompareCartesian'))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
