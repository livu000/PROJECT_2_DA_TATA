# Data manipulation and CSV loading
library(dplyr)        # Filter, select, mutate, group_by, summarise
library(tidyr)        # Pivot, separate, unite, and reshape data
library(readr)        # Read and write CSV and delimited files

# Numerical computing
library(matrixStats)  # Fast row-wise and column-wise matrix statistics

# Data visualization
library(ggplot2)      # Create charts and visualizations

# Scientific computing and statistics
library(MASS)         # Statistical methods, distributions, and regression
library(pracma)       # Numerical methods and mathematical functions

# Machine learning
library(caret)        # Train models, tune parameters, and evaluate performance
library(tidymodels)   # Modeling, preprocessing, and validation workflows
# Prefer tidymodels functions where conflicts exist
tidymodels::tidymodels_prefer()

# Scientific & statistical computing / modeling (Equivalent to scipy & statsmodels)
library(stats)
# stats (Built-in): Statistical tests, probability distributions,
# optimization, regression models, and scientific calculations.

# File and directory paths (Equivalent to pathlib.Path)
library(fs)
# fs (or base file.path): Manage file paths safely across operating systems.


# ------------------------------------------------------------------------------
# 2. VERIFY INSTALLED LIBRARY VERSIONS
# ------------------------------------------------------------------------------
cat("R Base version:    ", R.version.string, "\n")
cat("dplyr version:     ", as.character(packageVersion("dplyr")), "\n")
cat("readr version:     ", as.character(packageVersion("readr")), "\n")
cat("lubridate version: ", as.character(packageVersion("lubridate")), "\n")
cat("ggplot2 version:   ", as.character(packageVersion("ggplot2")), "\n")
cat("stats version:     ", as.character(packageVersion("stats")), "\n")
cat("caret version:     ", as.character(packageVersion("caret")), "\n")
cat("fs version:        ", as.character(packageVersion("fs")), "\n")


# ------------------------------------------------------------------------------
# 3. LOAD DATASET INTO A DATA FRAME
# ------------------------------------------------------------------------------
# Note: Use forward slashes (/) in R for file paths.
# na.strings converts empty cells and common null strings into R's NA automatically.
file_path <- "C:/Users/vasav/OneDrive/Desktop/DA_PROJECTS/Project_2/Online Retail DATA ANALYTICS - Copy.csv"

df <- read.csv(
  file = file_path,
  na.strings = c("", "NA", "N/A", "null", "NULL"),
  stringsAsFactors = FALSE,
  check.names = FALSE # Preserves exact column names from the CSV file
)


# ------------------------------------------------------------------------------
# 4. CHECK DATASET SIZE (ROWS AND COLUMNS) & BAR CHART
# ------------------------------------------------------------------------------
# To check how many rows and columns (Equivalent to df.shape)
total_rows <- nrow(df)
total_columns <- ncol(df)
cat("\nNumber of rows and columns: (", total_rows, ", ", total_columns, ")\n", sep = "")

# Create a bar chart of Dataset Size
par(mar = c(5, 5, 4, 2)) # Equivalent to plt.tight_layout()
barplot(
  height = c(total_rows, total_columns),
  names.arg = c("Rows", "Columns"),
  main = "Dataset Size",
  xlab = "Dataset Components",
  ylab = "Count",
  col = c("steelblue", "darkorange")
)


# ------------------------------------------------------------------------------
# 5. PRINT COLUMN NAMES, DATA TYPES, AND NON-NULL COUNTS
# ------------------------------------------------------------------------------
# To print column names (Equivalent to df.columns.tolist())
cat("\nThe columns names are :-\n")
print(colnames(df))

# To check DataTypes and structure (Equivalent to df.info())
cat("\nInfo about the df:\n")
str(df)

# Detailed tabular summary matching pandas df.info() output
df_info_summary <- data.frame(
  Column_Name = colnames(df),
  Data_Type = sapply(df, function(x) paste(class(x), collapse = "/")),
  Non_Null_Count = sapply(df, function(x) sum(!is.na(x))),
  Null_Count = sapply(df, function(x) sum(is.na(x))),
  row.names = NULL
)
print(df_info_summary)


# ------------------------------------------------------------------------------
# 6. TYPE CONVERSION: InvoiceDate STRING TO DATETIME
# ------------------------------------------------------------------------------
# Equivalent to pd.to_datetime(df["InvoiceDate"], format="%d-%m-%Y %H:%M")
df$InvoiceDate <- as.POSIXct(
  df$InvoiceDate,
  format = "%d-%m-%Y %H:%M",
  tz = "UTC"
)

# Check the data type (Equivalent to print(df["InvoiceDate"].dtype))
cat("\nInvoiceDate data type:", class(df$InvoiceDate), "\n")

# Display the first 5 rows of InvoiceDate (Equivalent to display(df[["InvoiceDate"]].head()))
cat("\nFirst 5 rows of InvoiceDate:\n")
print(head(df["InvoiceDate"], 5))


# ------------------------------------------------------------------------------
# 7. NULL COUNT FOR A PARTICULAR COLUMN (CustomerID)
# ------------------------------------------------------------------------------
# Equivalent to df["CustomerID"].isna().sum()
Count_null <- sum(is.na(df$CustomerID))
cat("\nTotal null count (CustomerID):", Count_null, "\n")


# ------------------------------------------------------------------------------
# 8. FUNCTION: NULL AND NOT-NULL SUMMARY PER COLUMN
# ------------------------------------------------------------------------------
# Count the null and its percentage using a custom function
null_summary <- function(series) {
  
  n_len <- length(series)
  
  # Count the nulls in series
  count_null <- sum(is.na(series))
  
  # Calculate percentage of null values in series
  null_percentage <- if (n_len > 0) (count_null / n_len) * 100 else 0
  
  # Calculate not-null count
  count_notnull <- sum(!is.na(series))
  
  # Calculate percentage of not-null values in series
  notnull_percentage <- if (n_len > 0) (count_notnull / n_len) * 100 else 0
  
  # Return as a named numeric vector (Equivalent to pd.Series)
  return(c(
    "Total null count is" = count_null,
    "Percentage of null is" = round(null_percentage, 2),
    "Total not null count is" = count_notnull,
    "Percentage of not null is" = round(notnull_percentage, 2)
  ))
}

# Apply null_summary to every column and transpose (.T) so each column is a row
null_apply <- as.data.frame(t(sapply(df, null_summary)))

# Display the null summary
cat("\nNull Summary Table Across All Columns:\n")
print(null_apply)
# View(null_apply) # Uncomment to open in RStudio's interactive data viewer tab


# ------------------------------------------------------------------------------
# 9. TOTAL NULL VALUES AND PERCENTAGE ACROSS ENTIRE DATASET
# ------------------------------------------------------------------------------
total_cells <- nrow(df) * ncol(df) # Equivalent to df.size
total_nulls <- sum(is.na(df))
total_nulls_percentage <- (total_nulls / total_cells) * 100

cat("\ntotal null values in cells -----> ", total_nulls, "\n")
cat("total null values in cells percentage -----> ", total_nulls_percentage, "%\n")


# ------------------------------------------------------------------------------
# 10. BAR AND PIE CHARTS FOR NULL VS NON-NULL PER COLUMN
# ------------------------------------------------------------------------------
# Apply the null analysis function to every column and transpose
null_apply <- as.data.frame(t(sapply(df, null_summary)))

# Rename columns to standard names
colnames(null_apply) <- c(
  "Null_Count",
  "Null_Percentage",
  "Non_Null_Count",
  "Non_Null_Percentage"
)

# Loop through every column and create its Bar and Pie charts
for (column in rownames(null_apply)) {
  
  # Get null and non-null counts for this column
  null_count <- null_apply[column, "Null_Count"]
  non_null_count <- null_apply[column, "Non_Null_Count"]
  
  # Get null and non-null percentages for this column
  null_percentage <- null_apply[column, "Null_Percentage"]
  non_null_percentage <- null_apply[column, "Non_Null_Percentage"]
  
  # CHART 1: BAR CHART - COUNTS
  par(mar = c(5, 5, 4, 2))
  barplot(
    height = c(null_count, non_null_count),
    names.arg = c("Null", "Non-Null"),
    main = paste0("Null vs Non-Null Count: ", column),
    ylab = "Count",
    col = c("tomato", "steelblue")
  )
  
  # CHART 2: PIE CHART - PERCENTAGES
  par(mar = c(4, 4, 4, 4))
  if ((null_count + non_null_count) == 0) {
    # Handle an empty Series without dividing by zero (Equivalent to plt.axis("off"))
    plot.new()
    title(main = paste0("No Data Available: ", column))
  } else {
    # Format labels with 2 decimal places (Equivalent to autopct="%1.2f%%")
    pie_vals <- c(null_percentage, non_null_percentage)
    pie_labels <- sprintf("%s\n(%.2f%%)", c("Null", "Non-Null"), pie_vals)
    
    # Base R pie() requires strictly positive values > 0 to render slices
    safe_pie_vals <- ifelse(pie_vals <= 0, 0.0001, pie_vals)
    
    pie(
      x = safe_pie_vals,
      labels = pie_labels,
      init.angle = 90, # Equivalent to startangle=90 in matplotlib
      main = paste0("Null vs Non-Null Percentage: ", column),
      col = c("tomato", "steelblue")
    )
  }
}


# ------------------------------------------------------------------------------
# 11. PARTICULAR COLUMN DATA VIEW (Country TOP 100 & BOTTOM 100)
# ------------------------------------------------------------------------------
# For particular column data view of top 100
cat("\nTop 100 rows of Country:\n")
print(head(df$Country, 100))

# For particular column data view of bottom 100
cat("\nBottom 100 rows of Country:\n")
print(tail(df$Country, 100))


# ------------------------------------------------------------------------------
# 12. FUNCTION: VIEW TOP 500, MIDDLE 500, AND BOTTOM 500 ROWS
# ------------------------------------------------------------------------------
# Ensure R console allows printing 500 values at once without early truncation
options(max.print = 2000)

view_500 <- function(series, col_name = NULL) {
  
  if (is.null(col_name)) {
    col_name <- deparse(substitute(series))
  }
  
  cat("\n", strrep("=", 80), "\n", sep = "")
  cat("Column name is : ", col_name, "\n", sep = "")
  cat(strrep("=", 80), "\n", sep = "")
  
  # View top 500 rows
  cat("\n--- TOP 500 ROWS ---\n")
  print(head(series, 500))
  
  # View middle 500 rows (Adjusted for R's 1-based indexing)
  # Matches Python: middle_position = max(0, (len(series) // 2) - 500)
  n_len <- length(series)
  middle_start <- max(1, floor(n_len / 2) - 499)
  middle_end <- min(n_len, middle_start + 499)
  
  cat("\n--- MIDDLE 500 ROWS ---\n")
  if (n_len > 0) {
    print(series[middle_start:middle_end])
  } else {
    cat("Series is empty.\n")
  }
  
  # View bottom 500 values
  cat("\n--- BOTTOM 500 ROWS ---\n")
  print(tail(series, 500))
}

# Applying view_500 function to all columns
for (columns in colnames(df)) {
  view_500(df[[columns]], col_name = columns)
}


# ------------------------------------------------------------------------------
# 13. UNIQUE VALUES AND VALUE COUNTS ANALYSIS
# ------------------------------------------------------------------------------
# Function to get the count of unique non-null values (Equivalent to series.nunique())
unique_value <- function(series) {
  print(length(unique(na.omit(series))))
}

cat("\nNumber of unique values in Country:\n")
unique_value(df[["Country"]])

# To view the unique values array for one series (Equivalent to df.Country.unique())
cat("\nUnique values in Country:\n")
print(unique(df$Country))

# Function to view unique values with their counts and total unique count
unique_value_count <- function(series, col_name = NULL) {
  if (!is.null(col_name)) {
    cat("\n--------------------------------------------------\n")
    cat("Column:", col_name, "\n")
  }
  
  # Sort in descending order to match pandas series.value_counts()
  counts_table <- sort(table(series, useNA = "no"), decreasing = TRUE)
  
  cat("The unique_values and its counts ----->\n")
  print(counts_table)
  
  cat("No count of unique_values -----> ", length(unique(na.omit(series))), "\n")
}

# Loop through all columns and display unique value counts
for (Column in colnames(df)) {
  unique_value_count(df[[Column]], col_name = Column)
}


# ------------------------------------------------------------------------------
# 14. DUPLICATED COUNT AND PERCENTAGE ANALYSIS
# ------------------------------------------------------------------------------
duplicated_count <- function(data_obj) {
  
  # sum(duplicated(...)) works on both full DataFrames (rows) and single columns
  dup_sum <- sum(duplicated(data_obj))
  
  # NROW() returns number of rows for a DataFrame or length for a vector
  total_len <- NROW(data_obj)
  dup_pct <- if (total_len > 0) (dup_sum / total_len) * 100 else 0
  
  cat("\nThe exact duplicated count:    ", dup_sum, "\n")
  cat("The percentage of duplicated: ", dup_pct, "%\n")
}

# Uncomment below to check duplicates for each column individually:
# for (Columns_1 in colnames(df)) {
#   cat("\nColumn:", Columns_1)
#   duplicated_count(df[[Columns_1]])
# }

# Check duplicate rows across the entire DataFrame
duplicated_count(df)