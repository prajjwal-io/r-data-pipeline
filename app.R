library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)
library(gamlss)
library(tseries)
library(zoo)
library(forecast)
library(shinyjs)
library(shinyWidgets)
library(data.table)
library(lubridate)
library(purrr)
library(RColorBrewer)
library(scales)

# Suppress package startup messages in the app
suppressPackageStartupMessages({
  library(FinTS)  # For ARCH test
})

# Load data and prepare it
data(oil, package = "gamlss.data")

# Create synthetic trading days (assuming 5 days per week)
trading_days <- seq(as.Date("2010-01-01"), length.out = nrow(oil), by = "day")
# Filter weekends (simplification of trading calendar)
trading_days <- trading_days[!weekdays(trading_days) %in% c("Saturday", "Sunday")]
# Ensure we have exactly nrow(oil) dates
trading_days <- trading_days[1:nrow(oil)]

# Create the dataset with date and selected columns
oil_data <- data.frame(
  Date = trading_days,
  OILPRICE = oil$OILPRICE,
  SPX = oil$SPX_log,
  GOLD = oil$GC1_log,
  USD = oil$DX1_log,
  OIL_LAG1 = oil$respLAG,
  BDIY = oil$BDIY_log,
  HEATING_OIL = oil$HO1_log,
  COMMODITY_IDX = oil$USCI_log,
  NATURAL_RESOURCES = oil$GNR_log,
  SHANGHAI_COMP = oil$SHCOMP_log,
  FTSE = oil$FTSE_log
)

# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  useShinyjs(),
  
  # Application title
  titlePanel("Data Transformation Pipeline"),
  
  navbarPage(
    "Oil Price Analysis",
    
    # Data Exploration Tab
    tabPanel(
      "Data Exploration",
      fluidRow(
        column(
          width = 12,
          h3("Oil Price Data Exploration"),
          p("This dashboard provides insights into the oil price dataset, including trends, patterns, and distributions.")
        )
      ),
      fluidRow(
        column(
          width = 6,
          box(
            width = NULL, 
            title = "Oil Price Over Time",
            plotlyOutput("timeSeriesPlot", height = "300px"),
            p("This plot shows the historical oil price movements over time, revealing long-term trends, market volatility, and pivotal market events."),
            actionLink("showOilPriceDetails", "Explore Graph Observations"),
            conditionalPanel( 
              condition = "input.showOilPriceDetails % 2 == 1", 
              div( 
                style = "margin-top: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
                p("Observable Characteristics:"),
                tags$ul(
                  tags$li("Price Range: Logarithmic scale from 3.6 to 4.6"),
                  tags$li("Initial Period (2010): Gradual, steady price increase"),
                  tags$li("Mid 2011 to Early 2012: Sustained high price plateau"),
                  tags$li("Late 2012: Abrupt and significant price decline"),
                  tags$li("Price Movements: Non-linear and discontinuous"),
                  tags$li("Distinct Phases: Long stability interrupted by sharp changes")
                ),
                p("Graph Interpretation:"),
                tags$ul(
                  tags$li("Log scale reveals nuanced price variations"),
                  tags$li("Periods of price stability clearly visible"),
                  tags$li("Dramatic price shift evident in late 2012"),
                  tags$li("Magnitude of price changes more apparent through logarithmic representation")
                )
              )
          )
         )
        ),
        # In the UI section, update the ACF box:
        column(
          width = 6,
          box(
            width = NULL,
            title = "Autocorrelation Function",
            plotOutput("acfPlot", height = "300px"),
            p("The autocorrelation function measures how oil prices correlate with their own lagged values, indicating periodicity and temporal dependencies."),
            actionLink("showAcfDetails", "Read more about this plot"),
            conditionalPanel(
              condition = "input.showAcfDetails % 2 == 1",
              div(
                style = "margin-top: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 5px;",
                p("The autocorrelation plot reveals that oil prices exhibit extremely strong serial correlation, with values close to 1.0 even at long lags. This pattern indicates:"),
                tags$ul(
                  tags$li("Strong persistence in the time series"),
                  tags$li("Non-stationarity characteristics typical of financial assets"),
                  tags$li("Evidence of a unit root or random walk process"),
                  tags$li("Need for differencing before modeling"),
                  tags$li("Long memory properties where past values significantly influence future prices")
                ),
                p("This autocorrelation structure explains why oil prices tend to trend for extended periods and why sudden price changes can have long-lasting effects on the market. For predictive modeling, transformations like differencing or using returns instead of raw prices would be necessary to address this strong autocorrelation.")
              )
            )
          )
        )
        ),
      fluidRow(
        column(
          width = 6,
          box(
            width = NULL,
            title = "Distribution Analysis",
            plotlyOutput("distributionPlot", height = "300px"),
            p("This histogram shows the distribution of oil prices, which helps assess normality and identify potential outliers.")
          )
        ),
        column(
          width = 6,
          box(
            width = NULL,
            title = "Monthly Average Prices",
            plotlyOutput("monthlyPlot", height = "300px"),
            p("Monthly averages help identify seasonal patterns and reduce noise in the time series data.")
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          box(
            width = NULL,
            title = "Correlation Matrix",
            plotlyOutput("correlationPlot", height = "500px"),
            p("This correlation matrix shows relationships between oil prices and other financial variables in the dataset.")
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          box(
            width = NULL,
            title = "Raw Data Preview",
            DTOutput("rawDataTable")
          )
        )
      )
    ),
    
    # Transformation Engine Tab
    tabPanel(
      "Transformation Pipeline",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Create New Transformation"),
          
          # Select source variable
          selectInput("sourceVar", "Source Variable:", choices = NULL),
          
          # Select transformation type
          selectInput("transformType", "Transformation Type:",
           choices = c("Rolling Standard Deviation", 
                      "Rolling Mean",
                      "Lag",
                      "Lead",
                      "Difference",
                      "Spread (requires two variables)",
                      "Ratio (requires two variables)",
                      "Product (requires two variables)",
                      "Exponential Moving Average",
                      "Log Transform",
                      "Square Root Transform",
                      "Percent Change")),
          
          # Parameters for transformations
          conditionalPanel(
            condition = "input.transformType == 'Rolling Standard Deviation' || input.transformType == 'Rolling Mean' || input.transformType == 'Exponential Moving Average'",
            numericInput("windowSize", "Window Size:", value = 7, min = 1)
          ),
          
          conditionalPanel(
            condition = "input.transformType == 'Exponential Moving Average'",
            sliderInput("alpha", "Alpha (Smoothing Factor):", min = 0.01, max = 1, value = 0.2, step = 0.01)
          ),
          
          conditionalPanel(
            condition = "input.transformType == 'Lag' || input.transformType == 'Lead'",
            numericInput("lagOrder", "Order:", value = 1, min = 1)
          ),
          
          conditionalPanel(
            condition = "input.transformType == 'Difference'",
            numericInput("diffOrder", "Order:", value = 1, min = 1)
          ),
          
          conditionalPanel(
            condition = "input.transformType.includes('requires two variables')",
            selectInput("secondVar", "Second Variable:", choices = NULL)
          ),
          
          # Field for new variable name
          textInput("newVarName", "New Variable Name (optional):", placeholder = "Auto-generated if empty"),
          
          # Button to apply transformation
          actionButton("applyTransform", "Apply Transformation", class = "btn-primary"),
          
          hr(),
          
          # Reset pipeline button
          actionButton("resetPipeline", "Reset Pipeline", class = "btn-danger")
        ),
        
        mainPanel(
          width = 9,
          
          tabsetPanel(
            tabPanel(
              "Pipeline Diagram",
              h4("Transformation Pipeline Visualization"),
              plotOutput("pipelineDiagram", height = "400px"),
              hr(),
              h4("Current Variable Set"),
              DTOutput("variableTable")
            ),
            
            tabPanel(
              "Transformed Data",
              h4("Preview of Transformed Dataset"),
              DTOutput("transformedDataTable"),
              downloadButton("downloadData", "Download Full Dataset")
            ),
            
            tabPanel(
              "Statistical Validation",
              h4("Statistical Tests and Validation"),
              DTOutput("statisticsTable"),
              downloadButton("downloadStats", "Download Statistics"),
              hr(),
              fluidRow(
                column(
                  width = 6,
                  h4("Variable Comparison"),
                  selectInput("compareVar1", "Variable 1:", choices = NULL),
                  selectInput("compareVar2", "Variable 2:", choices = NULL),
                  actionButton("compareVars", "Compare", class = "btn-info")
                ),
                column(
                  width = 6,
                  plotlyOutput("comparisonPlot", height = "300px")
                )
              )
            ),
            
            tabPanel(
              "Metadata Explorer",
              h4("Variable Metadata and Lineage"),
              selectInput("metadataVar", "Select Variable:", choices = NULL),
              verbatimTextOutput("metadataOutput"),
              plotOutput("lineageDiagram", height = "300px")
            )
          )
        )
      )
    ),
    
    # Documentation Tab
    tabPanel(
      "Documentation",
      fluidRow(
        column(
          width = 12,
          h3("Time Series Transformation Pipeline Documentation"),
          p("This application provides a comprehensive toolkit for time series data transformation, analysis, and visualization."),
          
          # Add detailed dataset information section
          h4("Oil Dataset Information", style = "margin-top: 25px; border-bottom: 1px solid #eee; padding-bottom: 10px;"),
          p("This application uses the oil dataset from the gamlss package, which contains daily prices of front month WTI (West Texas Intermediate) oil price and related financial variables."),
          
          div(
            style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
            h5("Dataset Description:"),
            p("The dataset contains 1000 observations on 25 variables related to oil prices and financial markets. It's designed to explore what affects the daily dynamics of oil prices using various financial products."),
            
            h5("Key Variables:"),
            tags$ul(
              tags$li(tags$strong("OILPRICE:"), "Log price of front month WTI oil contract traded by NYMEX (response variable)"),
              tags$li(tags$strong("SPX:"), "S&P 500 index (log)"),
              tags$li(tags$strong("GOLD:"), "Front month gold price contract (log)"),
              tags$li(tags$strong("USD:"), "US Dollar Index (log)"),
              tags$li(tags$strong("OIL_LAG1:"), "Lag 1 of OILPRICE"),
              tags$li(tags$strong("BDIY:"), "Baltic Dry Index, which is an assessment of the price of moving major raw materials by sea (log)"),
              tags$li(tags$strong("HEATING_OIL:"), "Front month heating oil contract (log)"),
              tags$li(tags$strong("COMMODITY_IDX:"), "United States Commodity Index (log)"),
              tags$li(tags$strong("NATURAL_RESOURCES:"), "S&P Global Natural Resources Index (log)"),
              tags$li(tags$strong("SHANGHAI_COMP:"), "Shanghai Stock Exchange Composite Index (log)"),
              tags$li(tags$strong("FTSE:"), "FTSE 100 Index (log)")
            ),
            
            h5("Date Information:"),
            p("The original dataset does not include explicit dates. For visualization and analysis purposes, this application adds synthetic trading dates to the data with the following approach:"),
            tags$ul(
              tags$li("A sequence of dates starting from 2010-01-01 is generated"),
              tags$li("Weekend days (Saturday and Sunday) are filtered out to simulate trading days"),
              tags$li("The resulting sequence of business days is matched to the 1000 observations in the dataset"),
              tags$li("This allows time series visualization and analysis with a realistic trading calendar")
            ),
            p("Note: The exact dates are synthetic and used for demonstration purposes. They do not represent the actual dates of the original observations.")
          ),
          
          
          h4("Available Transformations:"),
          tags$ul(
            tags$li(tags$strong("Rolling Standard Deviation:"), "Calculates the standard deviation within a moving window of specified size."),
            tags$li(tags$strong("Rolling Mean:"), "Calculates the average value within a moving window of specified size."),
            tags$li(tags$strong("Lag:"), "Shifts data backward in time by a specified number of periods."),
            tags$li(tags$strong("Lead:"), "Shifts data forward in time by a specified number of periods."),
            tags$li(tags$strong("Difference:"), "Calculates the difference between consecutive observations (or higher orders)."),
            tags$li(tags$strong("Spread:"), "Calculates the difference between two variables."),
            tags$li(tags$strong("Ratio:"), "Calculates the ratio between two variables."),
            tags$li(tags$strong("Product:"), "Calculates the product of two variables."),
            tags$li(tags$strong("Exponential Moving Average:"), "Calculates a weighted moving average with more weight to recent observations."),
            tags$li(tags$strong("Log Transform:"), "Applies natural logarithm to stabilize variance."),
            tags$li(tags$strong("Square Root Transform:"), "Applies square root transformation to stabilize variance."),
            tags$li(tags$strong("Percent Change:"), "Calculates the percentage change between consecutive observations.")
          ),
          
          h4("Using the Pipeline:"),
          tags$ol(
            tags$li("Select a source variable to transform."),
            tags$li("Choose a transformation type and set its parameters."),
            tags$li("Optionally provide a custom name for the resulting variable."),
            tags$li("Click 'Apply Transformation' to create the new variable."),
            tags$li("Continue building your pipeline by applying transformations to original or derived variables.")
          ),
          
          h4("Statistical Validation:"),
          p("For each variable, the system automatically calculates:"),
          tags$ul(
            tags$li(tags$strong("Normality Test:"), "Shapiro-Wilk test to check if the data follows a normal distribution."),
            tags$li(tags$strong("Stationarity Test:"), "Augmented Dickey-Fuller test to check if the time series is stationary."),
            tags$li(tags$strong("ARCH Test:"), "Test for autoregressive conditional heteroskedasticity, common in financial time series."),
            tags$li(tags$strong("Correlation:"), "Pearson correlation coefficient with the target variable (OILPRICE).")
          ),
          
          h4("Metadata System:"),
          p("The metadata system tracks:"),
          tags$ul(
            tags$li("Complete transformation history for each variable"),
            tags$li("Parameter settings for all transformations"),
            tags$li("Lineage information showing parent-child relationships between variables")
          ),
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Initialize reactive values
  rv <- reactiveValues(
    data = oil_data,
    metadata = list(),
    pipeline = list(),
    current_variables = names(oil_data)[-1]  # All columns except Date
  )
  
  # Initialize metadata for original variables
  observe({
    # This ensures the code only runs once by tracking if it's been executed
    if (is.null(rv$metadata_initialized)) {
        for (var in rv$current_variables) {
        if (!(var %in% names(rv$metadata))) {
            rv$metadata[[var]] <- list(
            source = "original",
            transformations = list(),
            parents = character(0)
            )
        }
        }
        # Set flag to indicate this has been run
        rv$metadata_initialized <- TRUE
    }
  })
  observeEvent(input$toggleAcfInfo, {
    shinyjs::toggle("acfInfoDetails")
  })
  # Update UI variable choices when available variables change
  observe({
    # Update choices for source variables
    updateSelectInput(session, "sourceVar", choices = rv$current_variables)
    updateSelectInput(session, "secondVar", choices = rv$current_variables)
    
    # Update comparison variable choices
    updateSelectInput(session, "compareVar1", choices = rv$current_variables)
    updateSelectInput(session, "compareVar2", choices = rv$current_variables)
    
    # Update metadata variable choices
    updateSelectInput(session, "metadataVar", choices = rv$current_variables)
  })
  
  # Define time series plot
  output$timeSeriesPlot <- renderPlotly({
    p <- ggplot(rv$data, aes(x = Date, y = OILPRICE)) +
      geom_line(color = "#3498db") +
      theme_minimal() +
      labs(y = "Oil Price (log)", x = "Date") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>% layout(autosize = TRUE)
  })
  
  # Define ACF plot
  output$acfPlot <- renderPlot({
    acf(rv$data$OILPRICE, main = "Autocorrelation of Oil Prices")
  })
  
  # Define distribution plot
  output$distributionPlot <- renderPlotly({
    p <- ggplot(rv$data, aes(x = OILPRICE)) +
      geom_histogram(bins = 30, fill = "#3498db", color = "white") +
      theme_minimal() +
      labs(x = "Oil Price (log)", y = "Frequency")
    
    ggplotly(p) %>% layout(autosize = TRUE)
  })
  
  # Define monthly average plot
  output$monthlyPlot <- renderPlotly({
    # Create month column
    monthly_data <- rv$data %>%
      mutate(Month = format(Date, "%Y-%m")) %>%
      group_by(Month) %>%
      summarize(AvgPrice = mean(OILPRICE, na.rm = TRUE)) %>%
      mutate(MonthDate = as.Date(paste0(Month, "-01")))
    
    p <- ggplot(monthly_data, aes(x = MonthDate, y = AvgPrice)) +
      geom_line(color = "#2ecc71") +
      geom_point(color = "#2ecc71") +
      theme_minimal() +
      labs(y = "Average Oil Price (log)", x = "Month") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>% layout(autosize = TRUE)
  })
  
  # Define correlation matrix plot
  output$correlationPlot <- renderPlotly({
    # Select numeric columns only for correlation
    numeric_cols <- sapply(rv$data, is.numeric)
    numeric_data <- rv$data[, numeric_cols]
    
    # Calculate correlation matrix
    cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
    
    # Convert to long format for plotting
    cor_data <- as.data.frame(as.table(cor_matrix))
    names(cor_data) <- c("Var1", "Var2", "Correlation")
    
    # Create heatmap
    plot_ly(
      x = rownames(cor_matrix),
      y = colnames(cor_matrix),
      z = cor_matrix,
      type = "heatmap",
      colorscale = list(
        c(0, "#B31B1B"),    # Strong negative (red)
        c(0.5, "#FFFFFF"),  # No correlation (white)
        c(1, "#1B7FB3")     # Strong positive (blue)
      ),
      zmin = -1,
      zmax = 1
    ) %>%
      layout(
        title = "Correlation Matrix of Financial Variables",
        xaxis = list(title = ""),
        yaxis = list(title = "")
      )
  })
  
  # Define raw data table
  output$rawDataTable <- renderDT({
    datatable(
      head(rv$data, 100),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  })
  
  # Apply transformation
  observeEvent(input$applyTransform, {
    # Get source variable
    source_var <- input$sourceVar
    transform_type <- input$transformType
    
    # Generate variable name if not provided
    new_var_name <- input$newVarName
    if (new_var_name == "") {
      # Auto-generate name based on transformation
      if (transform_type == "Rolling Standard Deviation") {
        new_var_name <- paste0(source_var, "_rollsd_", input$windowSize)
      } else if (transform_type == "Rolling Mean") {
        new_var_name <- paste0(source_var, "_rollmean_", input$windowSize)
      } else if (transform_type == "Lag") {
        new_var_name <- paste0(source_var, "_lag_", input$lagOrder)
      } else if (transform_type == "Lead") {
        new_var_name <- paste0(source_var, "_lead_", input$lagOrder)
      } else if (transform_type == "Difference") {
        new_var_name <- paste0(source_var, "_diff_", input$diffOrder)
      } else if (transform_type == "Spread (requires two variables)") {
        new_var_name <- paste0(source_var, "_minus_", input$secondVar)
      } else if (transform_type == "Ratio (requires two variables)") {
        new_var_name <- paste0(source_var, "_div_", input$secondVar)
      } else if (transform_type == "Product (requires two variables)") {
        new_var_name <- paste0(source_var, "_times_", input$secondVar)
      } else if (transform_type == "Exponential Moving Average") {
        new_var_name <- paste0(source_var, "_ema_", input$windowSize)
      } else if (transform_type == "Log Transform") {
        new_var_name <- paste0(source_var, "_log")
      } else if (transform_type == "Square Root Transform") {
        new_var_name <- paste0(source_var, "_sqrt")
      } else if (transform_type == "Percent Change") {
        new_var_name <- paste0(source_var, "_pct_change")
      }
    }
    
    # Check if new variable name already exists
    if (new_var_name %in% names(rv$data)) {
      showNotification(
        paste("Variable name", new_var_name, "already exists. Please choose a different name."),
        type = "error"
      )
      return()
    }
    
    # Create new column based on selected transformation
    tryCatch({
      if (transform_type == "Rolling Standard Deviation") {
        window_size <- input$windowSize
        rv$data[[new_var_name]] <- zoo::rollapply(rv$data[[source_var]], 
                                                width = window_size, 
                                                FUN = sd, 
                                                fill = NA, 
                                                align = "right")
        
        # Add metadata
        rv$metadata[[new_var_name]] <- list(
          source = "derived",
          transformations = list(
            list(
              type = "rolling_sd",
              parameters = list(window_size = window_size),
              applied_to = source_var
            )
          ),
          parents = source_var
        )
        
        # Update pipeline
        rv$pipeline[[length(rv$pipeline) + 1]] <- list(
          from = source_var,
          to = new_var_name,
          type = "rolling_sd",
          params = list(window_size = window_size)
        )
      } else if (transform_type == "Rolling Mean") {
        window_size <- input$windowSize
        rv$data[[new_var_name]] <- zoo::rollmean(rv$data[[source_var]], 
                                               k = window_size, 
                                               fill = NA, 
                                               align = "right")
        
        # Add metadata
        rv$metadata[[new_var_name]] <- list(
          source = "derived",
          transformations = list(
            list(
              type = "rolling_mean",
              parameters = list(window_size = window_size),
              applied_to = source_var
            )
          ),
          parents = source_var
        )
        
        # Update pipeline
        rv$pipeline[[length(rv$pipeline) + 1]] <- list(
          from = source_var,
          to = new_var_name,
          type = "rolling_mean",
          params = list(window_size = window_size)
        )
      } else if (transform_type == "Lag") {
        lag_order <- input$lagOrder
        rv$data[[new_var_name]] <- lag(rv$data[[source_var]], lag_order)
        
        # Add metadata
        rv$metadata[[new_var_name]] <- list(
          source = "derived",
          transformations = list(
            list(
              type = "lag",
              parameters = list(order = lag_order),
              applied_to = source_var
            )
          ),
          parents = source_var
        )
        
        # Update pipeline
        rv$pipeline[[length(rv$pipeline) + 1]] <- list(
          from = source_var,
          to = new_var_name,
          type = "lag",
          params = list(order = lag_order)
        )
      } else if (transform_type == "Lead") {
        lead_order <- input$lagOrder
        rv$data[[new_var_name]] <- lead(rv$data[[source_var]], lead_order)
        
        # Add metadata
        rv$metadata[[new_var_name]] <- list(
          source = "derived",
          transformations = list(
            list(
              type = "lead",
              parameters = list(order = lead_order),
              applied_to = source_var
            )
          ),
          parents = source_var
        )
        
        # Update pipeline
        rv$pipeline[[length(rv$pipeline) + 1]] <- list(
          from = source_var,
          to = new_var_name,
          type = "lead",
          params = list(order = lead_order)
        )
      } else if (transform_type == "Difference") {
        diff_order <- input$diffOrder
        rv$data[[new_var_name]] <- c(rep(NA, diff_order), 
                                   diff(rv$data[[source_var]], 
                                        differences = diff_order))
        
        # Add metadata
        rv$metadata[[new_var_name]] <- list(
          source = "derived",
          transformations = list(
            list(
              type = "difference",
              parameters = list(order = diff_order),
              applied_to = source_var
            )
          ),
          parents = source_var
        )
        
        # Update pipeline
        rv$pipeline[[length(rv$pipeline) + 1]] <- list(
          from = source_var,
          to = new_var_name,
          type = "difference",
          params = list(order = diff_order)
        )
      } else if (transform_type == "Spread (requires two variables)") {
        second_var <- input$secondVar
        rv$data[[new_var_name]] <- rv$data[[source_var]] - rv$data[[second_var]]
        
        # Add metadata
        rv$metadata[[new_var_name]] <- list(
          source = "derived",
          transformations = list(
            list(
              type = "spread",
              parameters = list(var1 = source_var, var2 = second_var),
              applied_to = c(source_var, second_var)
            )
          ),
          parents = c(source_var, second_var)
        )
        
        # Update pipeline
        rv$pipeline[[length(rv$pipeline) + 1]] <- list(
          from = c(source_var, second_var),
          to = new_var_name,
          type = "spread",
          params = list()
        )
      } else if (transform_type == "Ratio (requires two variables)") {
        second_var <- input$secondVar
        rv$data[[new_var_name]] <- rv$data[[source_var]] / rv$data[[second_var]]
        
        # Add metadata
        rv$metadata[[new_var_name]] <- list(
          source = "derived",
          transformations = list(
            list(
              type = "ratio",
              parameters = list(var1 = source_var, var2 = second_var),
              applied_to = c(source_var, second_var)
            )
          ),
          parents = c(source_var, second_var)
        )
        
        # Update pipeline
        rv$pipeline[[length(rv$pipeline) + 1]] <- list(
          from = c(source_var, second_var),
          to = new_var_name,
          type = "ratio",
          params = list()
        )
      } else if (transform_type == "Product (requires two variables)") {
        second_var <- input$secondVar
        rv$data[[new_var_name]] <- rv$data[[source_var]] * rv$data[[second_var]]
        
        # Add metadata
        rv$metadata[[new_var_name]] <- list(
          source = "derived",
          transformations = list(
            list(
              type = "product",
              parameters = list(var1 = source_var, var2 = second_var),
              applied_to = c(source_var, second_var)
            )
          ),
          parents = c(source_var, second_var)
        )
        
        # Update pipeline
        rv$pipeline[[length(rv$pipeline) + 1]] <- list(
          from = c(source_var, second_var),
          to = new_var_name,
          type = "product",
          params = list()
        )
      } else if (transform_type == "Exponential Moving Average") {
        window_size <- input$windowSize
        alpha <- input$alpha
        
        # Calculate EMA
        values <- rv$data[[source_var]]
        n <- length(values)
        ema <- numeric(n)
        ema[1] <- values[1]  # Initialize with first value
        
        for (i in 2:n) {
          ema[i] <- alpha * values[i] + (1 - alpha) * ema[i-1]
        }
        
        rv$data[[new_var_name]] <- ema
        
        # Add metadata
        rv$metadata[[new_var_name]] <- list(
          source = "derived",
          transformations = list(
            list(
              type = "exponential_moving_average",
              parameters = list(window_size = window_size, alpha = alpha),
              applied_to = source_var
            )
          ),
          parents = source_var
        )
        
        # Update pipeline
        rv$pipeline[[length(rv$pipeline) + 1]] <- list(
          from = source_var,
          to = new_var_name,
          type = "exponential_moving_average",
          params = list(window_size = window_size, alpha = alpha)
        )
      } else if (transform_type == "Log Transform") {
        # Check if data is positive
        if (min(rv$data[[source_var]], na.rm = TRUE) <= 0) {
          # Add small constant to ensure positive values
          constant <- abs(min(rv$data[[source_var]], na.rm = TRUE)) + 0.01
          rv$data[[new_var_name]] <- log(rv$data[[source_var]] + constant)
        } else {
          rv$data[[new_var_name]] <- log(rv$data[[source_var]])
        }
        
        # Add metadata
        rv$metadata[[new_var_name]] <- list(
          source = "derived",
          transformations = list(
            list(
              type = "log_transform",
              parameters = list(),
              applied_to = source_var
            )
          ),
          parents = source_var
        )
        
        # Update pipeline
        rv$pipeline[[length(rv$pipeline) + 1]] <- list(
          from = source_var,
          to = new_var_name,
          type = "log_transform",
          params = list()
        )
      } else if (transform_type == "Square Root Transform") {
        # Check if data is non-negative
        if (min(rv$data[[source_var]], na.rm = TRUE) < 0) {
          # Add constant to ensure non-negative values
          constant <- abs(min(rv$data[[source_var]], na.rm = TRUE)) + 0.01
          rv$data[[new_var_name]] <- sqrt(rv$data[[source_var]] + constant)
        } else {
          rv$data[[new_var_name]] <- sqrt(rv$data[[source_var]])
        }
        
        # Add metadata
        rv$metadata[[new_var_name]] <- list(
          source = "derived",
          transformations = list(
            list(
              type = "sqrt_transform",
              parameters = list(),
              applied_to = source_var
            )
          ),
          parents = source_var
        )
        
        # Update pipeline
        rv$pipeline[[length(rv$pipeline) + 1]] <- list(
          from = source_var,
          to = new_var_name,
          type = "sqrt_transform",
          params = list()
        )
      } else if (transform_type == "Percent Change") {
        # Calculate percent change
        rv$data[[new_var_name]] <- c(NA, diff(rv$data[[source_var]]) / lag(rv$data[[source_var]], 1) * 100)
        
# Add metadata
        rv$metadata[[new_var_name]] <- list(
          source = "derived",
          transformations = list(
            list(
              type = "percent_change",
              parameters = list(),
              applied_to = source_var
            )
          ),
          parents = source_var
        )
        
        # Update pipeline
        rv$pipeline[[length(rv$pipeline) + 1]] <- list(
          from = source_var,
          to = new_var_name,
          type = "percent_change",
          params = list()
        )
      }
      
      # Update current variables list
      rv$current_variables <- c(rv$current_variables, new_var_name)
      
      # Show success message
      showNotification(
        paste("Created new variable:", new_var_name),
        type = "message"
      )
      
      # Reset new variable name input
      updateTextInput(session, "newVarName", value = "")
      
    }, error = function(e) {
      showNotification(
        paste("Error applying transformation:", e$message),
        type = "error"
      )
    })
  })
  
  # Reset the transformation pipeline
  observeEvent(input$resetPipeline, {
    # Confirm reset
    showModal(modalDialog(
      title = "Reset Pipeline",
      "Are you sure you want to reset the entire transformation pipeline?",
      footer = tagList(
        actionButton("confirmReset", "Yes, Reset Pipeline", class = "btn-danger"),
        modalButton("Cancel")
      )
    ))
  })
  
  # Handle confirmed reset
  observeEvent(input$confirmReset, {
    # Reset data to original
    rv$data <- oil_data
    
    # Reset metadata to original variables only
    rv$metadata <- list()
    for (var in names(oil_data)[-1]) {  # Skip Date column
      rv$metadata[[var]] <- list(
        source = "original",
        transformations = list(),
        parents = character(0)
      )
    }
    
    # Reset pipeline
    rv$pipeline <- list()
    
    # Reset current variables
    rv$current_variables <- names(oil_data)[-1]  # All columns except Date
    
    # Close modal
    removeModal()
    
    # Show success message
    showNotification("Pipeline reset successfully.", type = "message")
  })
  
  # Variable table
  output$variableTable <- renderDT({
    # Create table of all variables
    var_table <- data.frame(
      Variable = rv$current_variables,
      Type = sapply(rv$current_variables, function(var) {
        if (rv$metadata[[var]]$source == "original") {
          "Original"
        } else {
          "Derived"
        }
      }),
      Description = sapply(rv$current_variables, function(var) {
        if (rv$metadata[[var]]$source == "original") {
          "Original variable"
        } else {
          transforms <- rv$metadata[[var]]$transformations
          last_transform <- transforms[[length(transforms)]]
          
          if (last_transform$type == "rolling_sd") {
            paste0("Rolling SD of '", last_transform$applied_to, 
                   "' with window size ", last_transform$parameters$window_size)
          } else if (last_transform$type == "rolling_mean") {
            paste0("Rolling mean of '", last_transform$applied_to, 
                   "' with window size ", last_transform$parameters$window_size)
          } else if (last_transform$type == "lag") {
            paste0("Lag order ", last_transform$parameters$order, " of '", last_transform$applied_to, "'")
          } else if (last_transform$type == "lead") {
            paste0("Lead order ", last_transform$parameters$order, " of '", last_transform$applied_to, "'")
          } else if (last_transform$type == "difference") {
            paste0(last_transform$parameters$order, "-order difference of '", last_transform$applied_to, "'")
          } else if (last_transform$type == "spread") {
            paste0("Spread between '", last_transform$parameters$var1, 
                   "' and '", last_transform$parameters$var2, "'")
          } else if (last_transform$type == "ratio") {
            paste0("Ratio of '", last_transform$parameters$var1, 
                   "' to '", last_transform$parameters$var2, "'")
          } else if (last_transform$type == "product") {
            paste0("Product of '", last_transform$parameters$var1, 
                   "' and '", last_transform$parameters$var2, "'")
          } else if (last_transform$type == "exponential_moving_average") {
            paste0("EMA of '", last_transform$applied_to, 
                   "' with alpha ", last_transform$parameters$alpha)
          } else if (last_transform$type == "log_transform") {
            paste0("Log transform of '", last_transform$applied_to, "'")
          } else if (last_transform$type == "sqrt_transform") {
            paste0("Square root transform of '", last_transform$applied_to, "'")
          } else if (last_transform$type == "percent_change") {
            paste0("Percent change of '", last_transform$applied_to, "'")
          } else {
            "Unknown transformation"
          }
        }
      }),
      NAs = sapply(rv$current_variables, function(var) {
        sum(is.na(rv$data[[var]]))
      })
    )
    
    datatable(
      var_table,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  })
  
  # Transformed data table
  output$transformedDataTable <- renderDT({
    datatable(
      head(rv$data, 100),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  })
  
  # Download handler for data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("transformed_oil_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rv$data, file, row.names = FALSE)
    }
  )
  
  calculate_statistics <- reactive({
    # First create a basic table with just the variable names
    stat_table <- data.frame(
      Variable = rv$current_variables,
      stringsAsFactors = FALSE
    )
    
    # Add basic statistics columns one at a time with error handling
    stat_table$Mean <- sapply(rv$current_variables, function(var) {
      if(is.numeric(rv$data[[var]])) {
        tryCatch({
          round(mean(rv$data[[var]], na.rm = TRUE), 4)
        }, error = function(e) {
          "Error"
        })
      } else {
        "N/A"
      }
    })
    
    stat_table$SD <- sapply(rv$current_variables, function(var) {
      if(is.numeric(rv$data[[var]])) {
        tryCatch({
          round(sd(rv$data[[var]], na.rm = TRUE), 4)
        }, error = function(e) {
          "Error"
        })
      } else {
        "N/A"
      }
    })
    
    stat_table$NAs <- sapply(rv$current_variables, function(var) {
      sum(is.na(rv$data[[var]]))
    })
    
    # Add Shapiro-Wilk test results
    shapiro_results <- sapply(rv$current_variables, function(var) {
      if(!is.numeric(rv$data[[var]])) return("N/A")
      
      data_no_na <- na.omit(rv$data[[var]])
      if(length(data_no_na) < 4 || length(data_no_na) > 5000) return("N/A")
      
      tryCatch({
        p_value <- shapiro.test(data_no_na)$p.value
        return(format(round(p_value, 4), nsmall = 4))
      }, error = function(e) {
        return("Error")
      })
    })
    stat_table$"Shapiro-Wilk (p-value)" <- shapiro_results
    
    # Add ADF test results
    adf_results <- sapply(rv$current_variables, function(var) {
      if(!is.numeric(rv$data[[var]])) return("N/A")
      
      data_no_na <- na.omit(rv$data[[var]])
      if(length(data_no_na) <= 10) return("N/A")
      
      tryCatch({
        # Suppress warnings about small p-values
        suppressWarnings({
          p_value <- adf.test(data_no_na)$p.value
        })
        return(format(round(p_value, 4), nsmall = 4))
      }, error = function(e) {
        return("Error")
      })
    })
    stat_table$"ADF Test (p-value)" <- adf_results
    
    # Add ARCH test results
    arch_results <- sapply(rv$current_variables, function(var) {
      if(!is.numeric(rv$data[[var]])) return("N/A")
      
      data_no_na <- na.omit(rv$data[[var]])
      if(length(data_no_na) <= 10) return("N/A")
      
      tryCatch({
        # Set a minimum lags value safely
        lags <- min(5, floor(length(data_no_na)/5))
        if(lags < 1) return("N/A")
        
        arch_test <- suppressWarnings(FinTS::ArchTest(data_no_na, lags = lags))
        return(format(round(arch_test$p.value, 4), nsmall = 4))
      }, error = function(e) {
        return("Error")
      })
    })
    stat_table$"ARCH Test (p-value)" <- arch_results
    
    # Add correlation with OILPRICE
    corr_results <- sapply(rv$current_variables, function(var) {
      if(var == "OILPRICE") return("1.0000")
      if(!is.numeric(rv$data[[var]])) return("N/A")
      
      tryCatch({
        corr <- cor(rv$data[[var]], rv$data$OILPRICE, use = "pairwise.complete.obs")
        return(format(round(corr, 4), nsmall = 4))
      }, error = function(e) {
        return("Error")
      })
    })
    stat_table$"Correlation with OILPRICE" <- corr_results
    
    # Add interpretation columns - being careful to handle all possible input types
    stat_table$Normality <- sapply(stat_table$"Shapiro-Wilk (p-value)", function(p) {
      if(is.na(p) || p == "N/A" || p == "Error") return("N/A")
      p_num <- as.numeric(p)
      if(is.na(p_num)) return("N/A")
      if(p_num < 0.05) return("Non-normal") else return("Normal")
    })
    
    stat_table$Stationarity <- sapply(stat_table$"ADF Test (p-value)", function(p) {
      if(is.na(p) || p == "N/A" || p == "Error") return("N/A")
      p_num <- as.numeric(p)
      if(is.na(p_num)) return("N/A")
      if(p_num < 0.05) return("Stationary") else return("Non-stationary")
    })
    
    stat_table$"ARCH Effect" <- sapply(stat_table$"ARCH Test (p-value)", function(p) {
      if(is.na(p) || p == "N/A" || p == "Error") return("N/A")
      p_num <- as.numeric(p)
      if(is.na(p_num)) return("N/A")
      if(p_num < 0.05) return("Present") else return("Not present")
    })
    
    return(stat_table)
  })
  
  # Statistics table
  output$statisticsTable <- renderDT({
    stat_table <- calculate_statistics()
    
    datatable(
      stat_table,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE
      )
    ) %>%
      formatStyle(
        'Normality',
        backgroundColor = styleEqual(
          c('Normal', 'Non-normal'),
          c('#d4edda', '#f8d7da')
        )
      ) %>%
      formatStyle(
        'Stationarity',
        backgroundColor = styleEqual(
          c('Stationary', 'Non-stationary'),
          c('#d4edda', '#f8d7da')
        )
      ) %>%
      formatStyle(
        'ARCH Effect',
        backgroundColor = styleEqual(
          c('Present', 'Not present'),
          c('#f8d7da', '#d4edda')
        )
      )
  })
  
  # Download handler for statistics
  output$downloadStats <- downloadHandler(
    filename = function() {
      paste("oil_data_statistics_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(calculate_statistics(), file, row.names = FALSE)
    }
  )
  
  # Variable comparison plot
  observeEvent(input$compareVars, {
    var1 <- input$compareVar1
    var2 <- input$compareVar2
    
    output$comparisonPlot <- renderPlotly({
      # Create data for plotting
      plot_data <- data.frame(
        Date = rv$data$Date,
        Var1 = rv$data[[var1]],
        Var2 = rv$data[[var2]]
      )
      
      # Scale data for better visualization
      if (sd(plot_data$Var1, na.rm = TRUE) > 0 && sd(plot_data$Var2, na.rm = TRUE) > 0) {
        plot_data$Var1_scaled <- scale(plot_data$Var1)
        plot_data$Var2_scaled <- scale(plot_data$Var2)
        
        p <- ggplot(plot_data, aes(x = Date)) +
          geom_line(aes(y = Var1_scaled, color = var1)) +
          geom_line(aes(y = Var2_scaled, color = var2)) +
          theme_minimal() +
          labs(title = "Variable Comparison (Scaled)",
               y = "Standardized Value",
               color = "Variable") +
          scale_color_manual(values = c("#3498db", "#e74c3c")) +
          theme(legend.position = "bottom")
        
        ggplotly(p) %>% layout(autosize = TRUE)
      } else {
        # If scaling not possible, just plot raw values
        p <- ggplot(plot_data, aes(x = Date)) +
          geom_line(aes(y = Var1, color = var1)) +
          geom_line(aes(y = Var2, color = var2)) +
          theme_minimal() +
          labs(title = "Variable Comparison",
               y = "Value",
               color = "Variable") +
          scale_color_manual(values = c("#3498db", "#e74c3c")) +
          theme(legend.position = "bottom")
        
        ggplotly(p) %>% layout(autosize = TRUE)
      }
    })
  })
  
  # Metadata output
  output$metadataOutput <- renderPrint({
    var <- input$metadataVar
    
    if (!is.null(var) && var != "") {
      cat("Metadata for:", var, "\n\n")
      
      if (var %in% names(rv$metadata)) {
        meta <- rv$metadata[[var]]
        
        cat("Source:", meta$source, "\n")
        
        if (meta$source == "derived") {
          cat("\nParent Variables:\n")
          cat(paste("- ", meta$parents, collapse = "\n"), "\n")
          
          cat("\nTransformation History:\n")
          for (i in seq_along(meta$transformations)) {
            transform <- meta$transformations[[i]]
            cat("Transformation #", i, ":\n", sep = "")
            cat("  Type:", transform$type, "\n")
            
            if (transform$type %in% c("rolling_sd", "rolling_mean")) {
              cat("  Window Size:", transform$parameters$window_size, "\n")
            } else if (transform$type == "exponential_moving_average") {
              cat("  Window Size:", transform$parameters$window_size, "\n")
              cat("  Alpha:", transform$parameters$alpha, "\n")
            } else if (transform$type %in% c("lag", "lead")) {
              cat("  Order:", transform$parameters$order, "\n")
            } else if (transform$type == "difference") {
              cat("  Order:", transform$parameters$order, "\n")
            } else if (transform$type %in% c("spread", "ratio", "product")) {
              cat("  Variable 1:", transform$parameters$var1, "\n")
              cat("  Variable 2:", transform$parameters$var2, "\n")
            }
            
            cat("  Applied to:", paste(transform$applied_to, collapse = ", "), "\n\n")
          }
        } else {
          cat("\nOriginal variable with no transformations.\n")
        }
      } else {
        cat("No metadata available for this variable.")
      }
    } else {
      cat("Select a variable to view its metadata.")
    }
  })
  
  output$pipelineDiagram <- renderPlot({
    # Initialize empty plot
    plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
         xaxt = "n", yaxt = "n", xlab = "", ylab = "",
         main = "Transformation Pipeline")
    
    # Check if there are any transformations to display
    if (length(rv$pipeline) == 0) {
      text(0.5, 0.5, "Apply transformations to visualize the pipeline",
           cex = 1.2)
      return()
    }
    
    # Get all unique nodes - with safe error handling
    all_nodes <- unique(c(
      unlist(lapply(rv$pipeline, function(p) {
        if (is.null(p$from)) return(NULL)
        return(p$from)
      })),
      unlist(lapply(rv$pipeline, function(p) {
        if (is.null(p$to)) return(NULL)
        return(p$to)
      }))
    ))
    
    # Just to be safe, remove any NULL values
    all_nodes <- all_nodes[!is.null(all_nodes)]
    
    # Find original and derived variables with error checking
    original_vars <- c()
    for (var in all_nodes) {
      if (var %in% names(rv$metadata) && 
          !is.null(rv$metadata[[var]]) && 
          !is.null(rv$metadata[[var]]$source) && 
          rv$metadata[[var]]$source == "original") {
        original_vars <- c(original_vars, var)
      }
    }
    
    # If no original variables found, use all variables that don't have parents
    if (length(original_vars) == 0) {
      original_vars <- all_nodes[!all_nodes %in% sapply(rv$pipeline, function(p) p$to)]
    }
    
    derived_vars <- setdiff(all_nodes, original_vars)
    
    # Simple layout - original variables on top row, derived on bottom
    node_coords <- list()
    
    # Place original variables in top row
    num_original <- length(original_vars)
    if (num_original > 0) {
      for (i in 1:num_original) {
        var <- original_vars[i]
        x <- i / (num_original + 1)
        y <- 0.8
        node_coords[[var]] <- c(x, y)
      }
    }
    
    # Place derived variables in bottom row
    num_derived <- length(derived_vars)
    if (num_derived > 0) {
      for (i in 1:num_derived) {
        var <- derived_vars[i]
        x <- i / (num_derived + 1)
        y <- 0.3
        node_coords[[var]] <- c(x, y)
      }
    }
    
    # Draw connections
    for (p in rv$pipeline) {
      if (is.null(p$from) || is.null(p$to)) next
      
      from_nodes <- p$from
      to_node <- p$to
      
      for (from_node in from_nodes) {
        if (from_node %in% names(node_coords) && to_node %in% names(node_coords)) {
          from_x <- node_coords[[from_node]][1]
          from_y <- node_coords[[from_node]][2]
          to_x <- node_coords[[to_node]][1]
          to_y <- node_coords[[to_node]][2]
          
          # Draw arrow
          arrows(from_x, from_y, to_x, to_y, length = 0.1, col = "darkgrey")
          
          # Add transformation label with safety checks
          transform_type <- if (!is.null(p$type)) p$type else "Unknown"
          mid_x <- (from_x + to_x) / 2
          mid_y <- (from_y + to_y) / 2
          
          # Simple label without trying to access potentially missing elements
          text(mid_x, mid_y, transform_type, cex = 0.7, col = "darkblue", 
               pos = 3, offset = 0.2)
        }
      }
    }
    
    # Draw nodes
    for (node in names(node_coords)) {
      x <- node_coords[[node]][1]
      y <- node_coords[[node]][2]
      
      # Determine node color based on whether it's original or derived
      is_original <- node %in% original_vars
      node_col <- if (is_original) "#3498db" else "#2ecc71"  # Blue or green
      
      # Draw node
      symbols(x, y, circles = 0.03, inches = FALSE, add = TRUE, 
              bg = node_col, fg = "black")
      
      # Add node label
      text(x, y, node, cex = 0.8)
    }
    
    # Add a simple legend
    legend("bottomright", 
           legend = c("Original Variables", "Derived Variables"),
           fill = c("#3498db", "#2ecc71"),
           cex = 0.8)
  })
}

# Run the application
shinyApp(ui = ui, server = server)        # Add metadata