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

# Load data and prepare it
data(oil)
oil_data <- data.frame(
  Date = as.Date(oil$DATE),
  OILPRICE = oil$OILPRICE
)

# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  useShinyjs(),
  
  # Application title
  titlePanel("Time Series Data Transformation Pipeline"),
  
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
            p("This plot shows the historical oil prices over time, revealing long-term trends and major market events.")
          )
        ),
        column(
          width = 6,
          box(
            width = NULL,
            title = "Autocorrelation Function",
            plotOutput("acfPlot", height = "300px"),
            p("The autocorrelation function measures how oil prices correlate with their own lagged values, indicating periodicity and temporal dependencies.")
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
                                "Product (requires two variables)")),
          
          # Parameters for transformations
          conditionalPanel(
            condition = "input.transformType == 'Rolling Standard Deviation' || input.transformType == 'Rolling Mean'",
            numericInput("windowSize", "Window Size:", value = 7, min = 1)
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
          
          h4("Available Transformations:"),
          tags$ul(
            tags$li(tags$strong("Rolling Standard Deviation:"), "Calculates the standard deviation within a moving window of specified size."),
            tags$li(tags$strong("Rolling Mean:"), "Calculates the average value within a moving window of specified size."),
            tags$li(tags$strong("Lag:"), "Shifts data backward in time by a specified number of periods."),
            tags$li(tags$strong("Lead:"), "Shifts data forward in time by a specified number of periods."),
            tags$li(tags$strong("Difference:"), "Calculates the difference between consecutive observations (or higher orders)."),
            tags$li(tags$strong("Spread:"), "Calculates the difference between two variables."),
            tags$li(tags$strong("Ratio:"), "Calculates the ratio between two variables."),
            tags$li(tags$strong("Product:"), "Calculates the product of two variables.")
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
            tags$li(tags$strong("Correlation:"), "Pearson correlation coefficient with the target variable (OILPRICE).")
          ),
          
          h4("Metadata System:"),
          p("The metadata system tracks:"),
          tags$ul(
            tags$li("Complete transformation history for each variable"),
            tags$li("Parameter settings for all transformations"),
            tags$li("Lineage information showing parent-child relationships between variables")
          )
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
    metadata = list(
      OILPRICE = list(
        source = "original",
        transformations = list(),
        parents = character(0)
      )
    ),
    pipeline = list(),
    current_variables = c("OILPRICE")
  )
  
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
      labs(y = "Oil Price", x = "Date") +
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
      labs(x = "Oil Price", y = "Frequency")
    
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
      labs(y = "Average Oil Price", x = "Month") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>% layout(autosize = TRUE)
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
    
    # Reset metadata
    rv$metadata <- list(
      OILPRICE = list(
        source = "original",
        transformations = list(),
        parents = character(0)
      )
    )
    
    # Reset pipeline
    rv$pipeline <- list()
    
    # Reset current variables
    rv$current_variables <- c("OILPRICE")
    
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
  
  # Calculate statistics for variables
  calculate_statistics <- reactive({
    # Create table for all variables
    stat_table <- data.frame(
      Variable = rv$current_variables,
      Mean = sapply(rv$current_variables, function(var) {
        round(mean(rv$data[[var]], na.rm = TRUE), 4)
      }),
      SD = sapply(rv$current_variables, function(var) {
        round(sd(rv$data[[var]], na.rm = TRUE), 4)
      }),
      Min = sapply(rv$current_variables, function(var) {
        round(min(rv$data[[var]], na.rm = TRUE), 4)
      }),
      Max = sapply(rv$current_variables, function(var) {
        round(max(rv$data[[var]], na.rm = TRUE), 4)
      }),
      NAs = sapply(rv$current_variables, function(var) {
        sum(is.na(rv$data[[var]]))
      }),
      "Shapiro-Wilk (p-value)" = sapply(rv$current_variables, function(var) {
        # Skip Date column and handle NAs
        if (is.numeric(rv$data[[var]])) {
          data_no_na <- na.omit(rv$data[[var]])
          if (length(data_no_na) > 3 && length(data_no_na) <= 5000) {
            round(shapiro.test(data_no_na)$p.value, 4)
          } else {
            "N/A"
          }
        } else {
          "N/A"
        }
      }),
      "ADF Test (p-value)" = sapply(rv$current_variables, function(var) {
        # Skip Date column and handle NAs
        if (is.numeric(rv$data[[var]])) {
          data_no_na <- na.omit(rv$data[[var]])
          if (length(data_no_na) > 10) {
            tryCatch({
              round(adf.test(data_no_na)$p.value, 4)
            }, error = function(e) {
              "Error"
            })
          } else {
            "N/A"
          }
        } else {
          "N/A"
        }
      }),
      "Correlation with OILPRICE" = sapply(rv$current_variables, function(var) {
        # Skip calculating correlation with itself
        if (var == "OILPRICE") {
          return("1.0000")
        }
        
        # Skip Date column and handle NAs
        if (is.numeric(rv$data[[var]])) {
          # Create data frame without NAs
          complete_data <- na.omit(data.frame(
            x = rv$data[[var]],
            y = rv$data$OILPRICE
          ))
          
          if (nrow(complete_data) > 3) {
            round(cor(complete_data$x, complete_data$y), 4)
          } else {
            "N/A"
          }
        } else {
          "N/A"
        }
      })
    )
    
    # Add interpretation columns
    stat_table$"Normality" <- sapply(stat_table$"Shapiro-Wilk (p-value)", function(p) {
      if (is.character(p) || is.na(p)) {
        return("N/A")
      } else if (as.numeric(p) < 0.05) {
        return("Non-normal")
      } else {
        return("Normal")
      }
    })
    
    stat_table$"Stationarity" <- sapply(stat_table$"ADF Test (p-value)", function(p) {
      if (is.character(p) || is.na(p)) {
        return("N/A")
      } else if (as.numeric(p) < 0.05) {
        return("Stationary")
      } else {
        return("Non-stationary")
      }
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
      )
  })
  