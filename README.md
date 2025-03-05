# Time Series Data Transformation Pipeline - https://prajjwaldash.shinyapps.io/r-data-pipeline/

## Overview

This RShiny application implements a comprehensive data preparation pipeline for time series analysis, focusing on transformations, statistical validation, and visualization. The system is designed to be robust, extensible, and allow complex transformation compositions using financial time series data from the oil dataset.

## Features

### Data Exploration Dashboard

- Interactive time series plots showing oil price trends over time
- Autocorrelation function plot to analyze temporal dependencies
- Distribution analysis with histogram visualization
- Monthly average price visualization for seasonal pattern detection
- Correlation matrix of financial variables to explore relationships
- Raw data preview with pagination and searching capabilities

### Transformation Engine

The application supports a flexible system for applying and composing the following transformations:

- **Rolling Standard Deviation**: Calculate standard deviation within a moving window of configurable size
- **Rolling Mean**: Calculate average within a moving window of configurable size
- **Lagging**: Shift data backward in time by an arbitrary order
- **Leading**: Shift data forward in time by an arbitrary order
- **Differencing**: Calculate first or higher-order differences between consecutive observations
- **Spread**: Calculate difference between two variables
- **Ratio**: Calculate the ratio between two variables
- **Product**: Calculate the product of two variables
- **Exponential Moving Average**: Calculate weighted average with more weight to recent observations
- **Log Transform**: Apply natural logarithm to stabilize variance
- **Square Root Transform**: Apply square root transformation to stabilize variance
- **Percent Change**: Calculate percentage change between consecutive observations

### User Interface for Transformation Pipeline

The interface allows users to:

- Select source variables (raw or previously transformed)
- Choose transformation types with appropriate parameters
- Specify the sequence of transformations (composition)
- Name resulting variables or use auto-naming conventions
- Visualize the transformation pipeline/workflow through interactive diagrams

### Metadata System

The metadata tracking mechanism:

- Records the complete transformation history for each derived variable
- Stores the sequence and parameters of applied transformations
- Maintains lineage information (parent variables)
- Provides metadata visualization and inspection through textual and graphical representations

### Statistical Validation

For each variable (original and derived), the application calculates and displays:

- Descriptive statistics (mean, standard deviation, min, max)
- Normality tests (Shapiro-Wilk)
- Stationarity tests (Augmented Dickey-Fuller)
- ARCH tests (for volatility clustering common in financial time series)
- Correlation coefficients with the target variable (OILPRICE)
- Visual interpretation of test results

### Results Presentation

- Interactive tables showing variable names, transformation histories, and statistics
- Visualizations comparing original and transformed variables
- Sorting/filtering of results based on statistical measures
- Color-coded indicators for test results

### Export Functionality

- Download the complete dataset (original + transformed variables) as CSV
- Export statistical summary tables as CSV
- Transformation history and lineage information available for reference

## Oil Dataset Information

The application uses the oil dataset from the `gamlss` package, which contains:

- 1000 observations of oil prices and related financial variables
- Log price of front month WTI oil contract traded by NYMEX (OILPRICE) as the response variable
- Various financial predictors such as:
  - SPX: S&P 500 index (log)
  - GOLD: Front month gold price contract (log)
  - USD: US Dollar Index (log)
  - OIL_LAG1: Lag 1 of OILPRICE
  - BDIY: Baltic Dry Index (log)
  - HEATING_OIL: Front month heating oil contract (log)
  - COMMODITY_IDX: United States Commodity Index (log)
  - NATURAL_RESOURCES: S&P Global Natural Resources Index (log)
  - SHANGHAI_COMP: Shanghai Stock Exchange Composite Index (log)
  - FTSE: FTSE 100 Index (log)

## Technical Implementation

### Key Components

1. **Reactive Data Store**: Maintains the current state of the dataset and transformations
2. **Metadata Repository**: Tracks transformation history and lineage
3. **Transformation Pipeline**: Records the sequence and parameters of transformations
4. **Statistical Calculation Engine**: Computes relevant metrics for validation, including financial-specific tests
5. **Visualization Modules**: Renders interactive plots and diagrams

### Code Architecture

- **UI Components**: Organized in tabs for different functionalities
- **Server Logic**: Handles transformations, statistical calculations, and rendering
- **Metadata Management**: Maintains comprehensive information about variable derivations
- **Pipeline Visualization**: Creates graphical representations of transformation flows

## Usage Guide

### Data Exploration

1. Navigate to the "Data Exploration" tab to view visualizations of the oil price dataset
2. Explore time series plots, autocorrelation function, and distribution analysis
3. Examine the correlation matrix to understand relationships between financial variables
4. Review the raw data in the data table at the bottom of the page

### Creating Transformations

1. Go to the "Transformation Pipeline" tab
2. Select a source variable from the dropdown menu
3. Choose a transformation type (Rolling SD, Rolling Mean, Lag, etc.)
4. Set appropriate parameters (window size, lag order, etc.)
5. Optionally provide a custom name for the new variable
6. Click "Apply Transformation" to create the transformed variable
7. View the updated pipeline diagram and variable table

### Statistical Validation

1. Navigate to the "Statistical Validation" tab within the "Transformation Pipeline" section
2. Review the computed statistics for all variables
3. Check normality, stationarity, and ARCH effect test results (color-coded for easy interpretation)
4. Use the comparison tool to visualize relationships between variables

### Metadata Exploration

1. Go to the "Metadata Explorer" tab
2. Select a variable to view its detailed transformation history
3. Review the lineage diagram showing parent-child relationships
4. Examine transformation parameters and sequencing

### Exporting Results

1. Use the "Download Full Dataset" button to export all variables as CSV
2. Use the "Download Statistics" button to export the statistical summary

## Technical Requirements

- R (version 4.0.0 or higher)
- Required packages:
  - shiny, shinydashboard, shinythemes
  - dplyr, tidyr
  - ggplot2, plotly
  - DT
  - gamlss (for the oil dataset)
  - tseries
  - zoo
  - forecast
  - shinyjs
  - shinyWidgets
  - data.table
  - lubridate
  - purrr
  - RColorBrewer
  - scales
  - FinTS (for ARCH testing)

## Installation and Running

1. Install the required R packages:
```r
install.packages(c("shiny", "shinydashboard", "shinythemes", "dplyr", "tidyr", 
                   "ggplot2", "plotly", "DT", "gamlss", "tseries", "zoo", 
                   "forecast", "shinyjs", "shinyWidgets", "data.table", 
                   "lubridate", "purrr", "RColorBrewer", "scales", "FinTS"))
```

2. Save the application code to a file named `app.R`

3. Run the application:
```r
shiny::runApp("path/to/app.R")
```

## Implementation Notes

### Date Handling
Since the original oil dataset doesn't include explicit dates, the application creates synthetic trading dates starting from 2010-01-01. This allows proper time series visualization and analysis.

### Financial-Specific Testing
The application includes ARCH (Autoregressive Conditional Heteroskedasticity) testing which is particularly relevant for financial time series that often exhibit volatility clustering.

### Correlation Analysis
The correlation matrix in the data exploration tab helps identify relationships between oil prices and other financial variables, providing insights for variable selection before transformation.

## Extension Possibilities

This pipeline can be extended to:

1. Support additional transformation types (wavelet transforms, Fourier transforms, etc.)
2. Include more financial-specific tests (Ljung-Box, Jarque-Bera, etc.)
3. Add forecasting capabilities based on transformed variables
4. Support upload of custom datasets beyond the oil price example
5. Implement machine learning model integration for predictive analytics
6. Add export of transformation recipes as executable R code
7. Implement batch processing for multiple transformations

## Conclusion

This Time Series Data Transformation Pipeline provides a powerful, interactive framework for preparing, exploring, and validating financial time series data. Its extensible design allows for complex transformation compositions while maintaining clear lineage information and statistical validation.
