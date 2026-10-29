# 1. DATA ANALYTICS - LIBRARY IMPORTS

# If any package is missing, run this line once in the RStudio Console:
# install.packages(c(
#   "dplyr", "tidyr", "readr", "matrixStats", "ggplot2",
#   "MASS", "pracma", "caret", "tidymodels", "fs",
#   "lubridate", "writexl"
# ))

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
tidymodels::tidymodels_prefer()

# Date/time helpers
library(lubridate)

# Scientific & statistical computing / modeling
library(stats)        # Included with R; statistical tests and distributions

# File and directory paths
library(fs)           # Safe path and directory management

# 2. VERIFY INSTALLED LIBRARY VERSIONS
cat("R Base version:    ", R.version.string, "\n")
cat("dplyr version:     ", as.character(packageVersion("dplyr")), "\n")
cat("readr version:     ", as.character(packageVersion("readr")), "\n")
cat("lubridate version: ", as.character(packageVersion("lubridate")), "\n")
cat("ggplot2 version:   ", as.character(packageVersion("ggplot2")), "\n")
cat("stats version:     ", as.character(packageVersion("stats")), "\n")
cat("caret version:     ", as.character(packageVersion("caret")), "\n")
cat("fs version:        ", as.character(packageVersion("fs")), "\n")

# 3. LOAD DATASET INTO A DATA FRAME
# Use forward slashes (/) in R for Windows file paths.
# na.strings converts empty cells and common null strings into NA.
file_path <- "C:/Users/vasav/OneDrive/Desktop/DA_PROJECTS/Project_2/Online Retail DATA ANALYTICS - Copy.csv"

if (!file.exists(file_path)) {
  stop(
    "Dataset file was not found. Update file_path near the top of this script:\n",
    file_path
  )
}

df <- read.csv(
  file = file_path,
  na.strings = c("", "NA", "N/A", "null", "NULL"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

cat("\nDataset loaded successfully.\n")

# 4. CHECK DATASET SIZE (ROWS AND COLUMNS) & BAR CHART
# Equivalent to Python: df.shape
total_rows <- nrow(df)
total_columns <- ncol(df)

cat(
  "\nNumber of rows and columns: (",
  total_rows, ", ", total_columns, ")\n",
  sep = ""
)

# Create a bar chart of dataset size
par(mar = c(5, 5, 4, 2))
barplot(
  height = c(total_rows, total_columns),
  names.arg = c("Rows", "Columns"),
  main = "Dataset Size",
  xlab = "Dataset Components",
  ylab = "Count",
  col = c("steelblue", "darkorange")
)

# 5. PRINT COLUMN NAMES, DATA TYPES, AND NON-NULL COUNTS
# Equivalent to df.columns.tolist()
cat("\nThe column names are:\n")
print(colnames(df))

# Equivalent to df.info()
cat("\nInfo about df:\n")
str(df)

# Detailed tabular summary matching pandas df.info()
df_info_summary <- data.frame(
  Column_Name = colnames(df),
  Data_Type = vapply(df, function(x) paste(class(x), collapse = "/"),
                     character(1)),
  Non_Null_Count = vapply(df, function(x) sum(!is.na(x)), integer(1)),
  Null_Count = vapply(df, function(x) sum(is.na(x)), integer(1)),
  row.names = NULL,
  check.names = FALSE
)

cat("\nDetailed DataFrame Information:\n")
print(df_info_summary)

# 6. TYPE CONVERSION: InvoiceDate STRING TO DATETIME
# Equivalent to pandas:
# pd.to_datetime(df["InvoiceDate"], format="%d-%m-%Y %H:%M", errors="coerce")
if ("InvoiceDate" %in% names(df)) {
  df$InvoiceDate <- suppressWarnings(
    as.POSIXct(
      as.character(df$InvoiceDate),
      format = "%d-%m-%Y %H:%M",
      tz = "UTC"
    )
  )
  
  cat("\nInvoiceDate data type:\n")
  print(class(df$InvoiceDate))
  
  cat("\nFirst 5 rows of InvoiceDate:\n")
  print(head(df["InvoiceDate"], 5))
  
  cat("\nMissing dates:", sum(is.na(df$InvoiceDate)), "\n")
} else {
  warning("Column 'InvoiceDate' was not found; date conversion was skipped.")
}

# 7. NULL COUNT FOR A PARTICULAR COLUMN (CustomerID)
# Equivalent to df["CustomerID"].isna().sum()
if ("CustomerID" %in% names(df)) {
  Count_null <- sum(is.na(df$CustomerID))
  cat("\nTotal null count (CustomerID):", Count_null, "\n")
} else {
  warning("Column 'CustomerID' was not found.")
}

# 8. FUNCTION: NULL AND NOT-NULL SUMMARY PER COLUMN
null_summary <- function(series) {
  n_len <- length(series)
  
  # Count the nulls in the series
  count_null <- sum(is.na(series))
  
  # Calculate percentage of null values
  null_percentage <- if (n_len > 0) {
    (count_null / n_len) * 100
  } else {
    0
  }
  
  # Calculate not-null count
  count_notnull <- sum(!is.na(series))
  
  # Calculate percentage of not-null values
  notnull_percentage <- if (n_len > 0) {
    (count_notnull / n_len) * 100
  } else {
    0
  }
  
  return(c(
    "Total null count is" = count_null,
    "Percentage of null is" = round(null_percentage, 2),
    "Total not null count is" = count_notnull,
    "Percentage of not null is" = round(notnull_percentage, 2)
  ))
}

# Apply null_summary to every column and transpose so each column is one row
null_apply <- as.data.frame(t(sapply(df, null_summary)))

cat("\nNull Summary Table Across All Columns:\n")
print(null_apply)
# View(null_apply) # Uncomment to open RStudio's interactive data viewer

# 9. TOTAL NULL VALUES AND PERCENTAGE ACROSS ENTIRE DATASET
total_cells <- nrow(df) * ncol(df)
total_nulls <- sum(is.na(df))
total_nulls_percentage <- if (total_cells > 0) {
  (total_nulls / total_cells) * 100
} else {
  0
}

cat("\nTotal null values in cells -----> ", total_nulls, "\n")
cat(
  "Total null values in cells percentage -----> ",
  round(total_nulls_percentage, 5), "%\n"
)

# 10. BAR AND PIE CHARTS FOR NULL VS NON-NULL PER COLUMN
null_apply <- as.data.frame(t(sapply(df, null_summary)))
colnames(null_apply) <- c(
  "Null_Count",
  "Null_Percentage",
  "Non_Null_Count",
  "Non_Null_Percentage"
)

# Loop through each column and create a base R bar chart and pie chart
for (column in rownames(null_apply)) {
  null_count <- as.numeric(null_apply[column, "Null_Count"])
  non_null_count <- as.numeric(null_apply[column, "Non_Null_Count"])
  null_percentage <- as.numeric(null_apply[column, "Null_Percentage"])
  non_null_percentage <- as.numeric(null_apply[column, "Non_Null_Percentage"])
  
  # CHART 1: BAR CHART - COUNTS
  par(mar = c(5, 5, 4, 2))
  barplot(
    height = c(null_count, non_null_count),
    names.arg = c("Null", "Non-Null"),
    main = paste0("Null vs Non-Null Count: ", column),
    xlab = "Value Type",
    ylab = "Count",
    col = c("tomato", "steelblue")
  )
  
  # CHART 2: PIE CHART - PERCENTAGES
  par(mar = c(4, 4, 4, 4))
  if ((null_count + non_null_count) == 0) {
    plot.new()
    title(main = paste0("No Data Available: ", column))
  } else {
    pie_vals <- c(null_percentage, non_null_percentage)
    pie_labels <- sprintf(
      "%s\n(%.2f%%)",
      c("Null", "Non-Null"),
      pie_vals
    )
    
    # Base R pie() requires positive values to render slices.
    safe_pie_vals <- ifelse(pie_vals <= 0, 0.0001, pie_vals)
    
    pie(
      x = safe_pie_vals,
      labels = pie_labels,
      init.angle = 90,
      main = paste0("Null vs Non-Null Percentage: ", column),
      col = c("tomato", "steelblue")
    )
  }
}


# 11. REPEAT NULL CHARTS WITH GGPLOT2 (SEABORN-STYLE EQUIVALENT)
# The original Python notebook makes a second, theme-based set of null charts
# using Seaborn. In R, ggplot2 is used for the equivalent presentation.
# This section intentionally repeats these charts, matching the notebook.

plot_null_charts_ggplot <- function(summary_table, plot_theme = theme_bw()) {
  for (column in rownames(summary_table)) {
    null_count <- as.numeric(summary_table[column, "Null_Count"])
    non_null_count <- as.numeric(summary_table[column, "Non_Null_Count"])
    null_percentage <- as.numeric(summary_table[column, "Null_Percentage"])
    non_null_percentage <- as.numeric(summary_table[column, "Non_Null_Percentage"])
    
    chart_data <- data.frame(
      Value_Type = factor(
        c("Null", "Non-Null"),
        levels = c("Null", "Non-Null")
      ),
      Count = c(null_count, non_null_count),
      Percentage = c(null_percentage, non_null_percentage)
    )
    
    # Chart 1: bar chart of null and non-null counts
    p_bar <- ggplot(chart_data, aes(x = Value_Type, y = Count, fill = Value_Type)) +
      geom_col() +
      labs(
        title = paste0("Null vs Non-Null Count: ", column),
        x = "Value Type",
        y = "Count"
      ) +
      plot_theme +
      guides(fill = "none")
    
    print(p_bar)
    
    # Chart 2: pie chart of percentages
    if ((null_count + non_null_count) > 0) {
      p_pie <- ggplot(
        chart_data,
        aes(x = "", y = Percentage, fill = Value_Type)
      ) +
        geom_col(width = 1) +
        coord_polar(theta = "y") +
        geom_text(
          aes(label = sprintf("%s\n%.2f%%", Value_Type, Percentage)),
          position = position_stack(vjust = 0.5),
          size = 3
        ) +
        labs(
          title = paste0("Null vs Non-Null Percentage: ", column),
          x = NULL,
          y = NULL
        ) +
        plot_theme +
        theme(
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank()
        )
      
      print(p_pie)
    } else {
      cat("\nNo data available for null chart:", column, "\n")
    }
  }
}

# Prepare the table in the standard format expected by the chart function.
null_apply_gg <- as.data.frame(t(sapply(df, null_summary)))
colnames(null_apply_gg) <- c(
  "Null_Count",
  "Null_Percentage",
  "Non_Null_Count",
  "Non_Null_Percentage"
)

# Equivalent to a white-grid chart theme
plot_null_charts_ggplot(null_apply_gg, theme_bw())

# 12. PARTICULAR COLUMN DATA VIEW (Country TOP 100 & BOTTOM 100)
if ("Country" %in% names(df)) {
  cat("\nTop 100 rows of Country:\n")
  print(head(df$Country, 100))
  
  cat("\nBottom 100 rows of Country:\n")
  print(tail(df$Country, 100))
} else {
  warning("Column 'Country' was not found; Country view was skipped.")
}

# 13. FUNCTION: VIEW TOP 500, MIDDLE 500, AND BOTTOM 500 VALUES
# Allow the console to print more values before truncating output.
options(max.print = 2000)

view_500 <- function(series, col_name = NULL) {
  if (is.null(col_name)) {
    col_name <- deparse(substitute(series))
  }
  
  cat("\n", strrep("=", 80), "\n", sep = "")
  cat("Column name is: ", col_name, "\n", sep = "")
  cat("Total values: ", length(series), "\n", sep = "")
  cat(strrep("=", 80), "\n", sep = "")
  
  cat("\n--- TOP 500 ROWS ---\n")
  print(head(series, 500))
  
  # Middle 500 values; R uses 1-based indexing.
  n_len <- length(series)
  middle_start <- max(1, floor(n_len / 2) - 499)
  middle_end <- min(n_len, middle_start + 499)
  
  cat("\n--- MIDDLE 500 ROWS ---\n")
  if (n_len > 0) {
    print(series[middle_start:middle_end])
  } else {
    cat("Series is empty.\n")
  }
  
  cat("\n--- BOTTOM 500 ROWS ---\n")
  print(tail(series, 500))
}

# Apply view_500 to all columns
for (column in colnames(df)) {
  view_500(df[[column]], col_name = column)
}

# 14. UNIQUE VALUES AND VALUE COUNTS ANALYSIS
# Get count of unique non-null values (equivalent to Series.nunique())
unique_value <- function(series) {
  print(length(unique(na.omit(series))))
}

if ("Country" %in% names(df)) {
  cat("\nNumber of unique values in Country:\n")
  unique_value(df[["Country"]])
  
  cat("\nUnique values in Country:\n")
  print(unique(df$Country))
}

# View unique values with their counts and total unique count
unique_value_count <- function(series, col_name = NULL) {
  if (!is.null(col_name)) {
    cat("\n--------------------------------------------------\n")
    cat("Column:", col_name, "\n")
  }
  
  cat("Unique values and their counts ----->\n")
  print(sort(table(series, useNA = "no"), decreasing = TRUE))
  
  cat(
    "Number of unique non-null values -----> ",
    length(unique(na.omit(series))), "\n"
  )
}

for (column in colnames(df)) {
  unique_value_count(df[[column]], col_name = column)
}

# 15. DUPLICATED COUNT AND PERCENTAGE ANALYSIS
# Works for a complete data frame (duplicate rows) or a vector (duplicate values)
duplicated_count_basic <- function(data_obj) {
  dup_sum <- sum(duplicated(data_obj))
  total_len <- NROW(data_obj)
  dup_pct <- if (total_len > 0) (dup_sum / total_len) * 100 else 0
  
  cat("\nExact duplicated count: ", dup_sum, "\n")
  cat("Percentage duplicated: ", round(dup_pct, 2), "%\n")
}

# Uncomment to check duplicates for each column individually:
# for (column in colnames(df)) {
#   cat("\nColumn:", column, "\n")
#   duplicated_count_basic(df[[column]])
# }

cat("\nDuplicate-row analysis for the original dataset:\n")
duplicated_count_basic(df)

# 16. DISPLAY ALL DUPLICATE RECORDS
duplicated_records <- function(data_obj) {
  duplicate_rows <- data_obj[duplicated(data_obj), , drop = FALSE]
  cat("\n--- DUPLICATE RECORDS (EXTRA COPIES) ---\n")
  
  if (nrow(duplicate_rows) == 0) {
    cat("No duplicate records found.\n")
  } else {
    print(duplicate_rows)
  }
}

duplicated_records(df)

# 17. VIEW EXACT DUPLICATE PAIRS SIDE-BY-SIDE
duplicated_count <- function(data_obj) {
  # Extra duplicate copies: occurrences that would be removed by drop_duplicates.
  extra_dup_count <- sum(duplicated(data_obj))
  total_len <- NROW(data_obj)
  extra_dup_pct <- if (total_len > 0) {
    (extra_dup_count / total_len) * 100
  } else {
    0
  }
  
  # Count all rows belonging to duplicate groups: original plus repeat copies.
  all_involved_count <- sum(
    duplicated(data_obj) | duplicated(data_obj, fromLast = TRUE)
  )
  all_involved_pct <- if (total_len > 0) {
    (all_involved_count / total_len) * 100
  } else {
    0
  }
  
  cat("\nExtra duplicate copies count (keep='first') -----> ",
      extra_dup_count, "\n")
  cat("Percentage of extra duplicate copies -----> ",
      round(extra_dup_pct, 2), "%\n")
  cat("Total rows in duplicate groups (keep=FALSE) -----> ",
      all_involved_count, "\n")
  cat("Percentage of rows in duplicate groups -----> ",
      round(all_involved_pct, 2), "%\n")
  
  duplicate_pairs <- if (is.data.frame(data_obj)) {
    data_obj[
      duplicated(data_obj) | duplicated(data_obj, fromLast = TRUE),
      ,
      drop = FALSE
    ]
  } else {
    data_obj[
      duplicated(data_obj) | duplicated(data_obj, fromLast = TRUE)
    ]
  }
  
  # Sort duplicate rows so exact copies appear together.
  if (is.data.frame(duplicate_pairs) && nrow(duplicate_pairs) > 0) {
    sort_key <- do.call(
      paste,
      c(
        lapply(duplicate_pairs, function(x) {
          out <- as.character(x)
          out[is.na(x)] <- "<NULL>"
          out
        }),
        sep = "\r"
      )
    )
    duplicate_pairs <- duplicate_pairs[order(sort_key), , drop = FALSE]
  } else if (!is.data.frame(duplicate_pairs) && length(duplicate_pairs) > 0) {
    duplicate_pairs <- sort(duplicate_pairs)
  }
  
  cat("\n--- EXACT DUPLICATE RECORDS (ORIGINAL + COPY) ---\n")
  if (NROW(duplicate_pairs) == 0) {
    cat("No duplicate records found.\n")
  } else {
    cat("\nFirst 10 duplicate records:\n")
    print(head(duplicate_pairs, 10))
    cat("\nLast 10 duplicate records:\n")
    print(tail(duplicate_pairs, 10))
  }
  
  invisible(duplicate_pairs)
}

# Run on the entire original DataFrame
duplicate_pairs_original <- duplicated_count(df)

# 18. SIMPLE STATISTICAL DESCRIPTION OF THE ORIGINAL DATASET
# Equivalent to pandas df.describe() for numeric columns. No rows are dropped
# from df itself; the statistics omit missing values per numeric column.
safe_describe_numeric <- function(dataframe) {
  numeric_data <- dataframe[
    vapply(dataframe, is.numeric, logical(1))
  ]
  
  if (ncol(numeric_data) == 0) {
    cat("\nNo numeric columns are available for describe().\n")
    return(invisible(NULL))
  }
  
  describe_one <- function(x) {
    x <- x[!is.na(x) & is.finite(x)]
    
    if (length(x) == 0) {
      return(c(
        count = 0, mean = NA_real_, sd = NA_real_,
        min = NA_real_, `25%` = NA_real_, `50%` = NA_real_,
        `75%` = NA_real_, max = NA_real_
      ))
    }
    
    c(
      count = length(x),
      mean = mean(x),
      sd = if (length(x) > 1) sd(x) else NA_real_,
      min = min(x),
      `25%` = unname(quantile(x, 0.25, type = 7)),
      `50%` = unname(quantile(x, 0.50, type = 7)),
      `75%` = unname(quantile(x, 0.75, type = 7)),
      max = max(x)
    )
  }
  
  result <- as.data.frame(t(vapply(
    numeric_data,
    describe_one,
    FUN.VALUE = c(
      count = 0, mean = 0, sd = 0, min = 0,
      `25%` = 0, `50%` = 0, `75%` = 0, max = 0
    )
  )))
  cat("\nDescriptive statistics for the original df:\n")
  print(result)
  invisible(result)
}

describe_original <- safe_describe_numeric(df)

# 19. CREATE A COPY OF THE DATAFRAME FOR CLEANING (df_1)
df_1 <- df

# 20. FUNCTION TO TRIM AND CONVERT TEXT TO PROPER CASE
# Python used .strip().title() on every string value.
# tools::toTitleCase() is the base R equivalent used here for text columns.
trim_proper <- function(dataframe) {
  for (column in names(dataframe)) {
    if (is.character(dataframe[[column]])) {
      values <- dataframe[[column]]
      not_missing <- !is.na(values)
      
      values[not_missing] <- tools::toTitleCase(
        trimws(values[not_missing])
      )
      dataframe[[column]] <- values
    }
  }
  dataframe
}

df_1 <- trim_proper(df_1)

cat("\nFirst 6 rows after trimming text and applying proper case:\n")
print(head(df_1))
cat("\nStructure of df_1 after text standardization:\n")
str(df_1)

# 21. DROP DUPLICATES AND HANDLE NULL CUSTOMERID / DESCRIPTION
# Step 1: Drop exact duplicate rows
rows_before_deduplication <- nrow(df_1)
df_1 <- df_1[!duplicated(df_1), , drop = FALSE]
rows_after_deduplication <- nrow(df_1)

cat("\nDuplicate rows removed: ",
    rows_before_deduplication - rows_after_deduplication, "\n")

# Step 2: Replace missing CustomerID with "X".
# Convert to character first, matching pandas' behavior after filling a numeric
# CustomerID column with the text value "X".
if ("CustomerID" %in% names(df_1)) {
  df_1$CustomerID <- as.character(df_1$CustomerID)
  df_1$CustomerID[is.na(df_1$CustomerID)] <- "X"
}

# Step 2.1: Replace missing Description with "X"
if ("Description" %in% names(df_1)) {
  df_1$Description <- as.character(df_1$Description)
  df_1$Description[is.na(df_1$Description)] <- "X"
}

# Step 3: Verify results
cat("\nDuplicate rows remaining:", sum(duplicated(df_1)), "\n")

if ("CustomerID" %in% names(df_1)) {
  cat("\nNull values in CustomerID:\n")
  print(sum(is.na(df_1$CustomerID)))
}

cat("\nTotal null values in each column:\n")
print(colSums(is.na(df_1)))

cat("\nFinal DataFrame shape:\n")
print(dim(df_1))

cat("\nFinal structure of df_1:\n")
str(df_1)

# 22. SEPARATE COLUMNS BY DATA TYPE
numeric_columns <- names(df_1)[
  vapply(df_1, is.numeric, logical(1))
]

categorical_columns <- names(df_1)[
  vapply(
    df_1,
    function(x) is.character(x) || is.factor(x) || is.logical(x),
    logical(1)
  )
]

datetime_columns <- names(df_1)[
  vapply(
    df_1,
    function(x) inherits(x, c("POSIXct", "POSIXlt", "Date")),
    logical(1)
  )
]

timedelta_columns <- names(df_1)[
  vapply(df_1, function(x) inherits(x, "difftime"), logical(1))
]

classified_columns <- c(
  numeric_columns,
  categorical_columns,
  datetime_columns,
  timedelta_columns
)

other_columns <- names(df_1)[!names(df_1) %in% classified_columns]

cat("\nNumeric Columns:\n")
print(numeric_columns)
cat("\nCategorical Columns:\n")
print(categorical_columns)
cat("\nDate/Time Columns:\n")
print(datetime_columns)
cat("\nTime Duration Columns:\n")
print(timedelta_columns)
cat("\nOther Columns:\n")
print(other_columns)

cat("\nTotal DataFrame Columns:", length(names(df_1)), "\n")
cat("Total Classified Columns:",
    length(unique(classified_columns)) + length(other_columns), "\n")
cat(
  "All Columns Accounted For:",
  setequal(names(df_1), c(classified_columns, other_columns)),
  "\n"
)


# 23. DETAILED DESCRIPTIVE STATISTICS FOR NUMERIC COLUMNS
# Helper: return all modes. Unlike base R's mode(), this calculates the
# statistical mode of the data values, similar to pandas Series.mode().
statistical_mode <- function(series) {
  series <- series[!is.na(series) & is.finite(series)]
  if (length(series) == 0) {
    return(NA_real_)
  }
  
  counts <- table(series)
  modes_as_text <- names(counts)[counts == max(counts)]
  
  if (is.numeric(series)) {
    return(as.numeric(modes_as_text))
  }
  modes_as_text
}

# 1. Function to find mean, median, and mode
mean_median_mode <- function(series) {
  mean_value <- round(mean(series), 5)
  median_value <- round(median(series), 5)
  mode_value <- round(statistical_mode(series), 5)
  list(
    mean_value = mean_value,
    median_value = median_value,
    mode_value = mode_value
  )
}

# 2. Function to find minimum, maximum, range, and coefficient of range
min_max_range <- function(series) {
  min_value <- round(min(series), 5)
  max_value <- round(max(series), 5)
  range_value <- round(max_value - min_value, 5)
  denominator <- max_value + min_value
  
  coefficient_range <- if (denominator != 0) {
    round(range_value / denominator, 5)
  } else {
    NA_real_
  }
  
  c(
    min_value = min_value,
    max_value = max_value,
    range_value = range_value,
    coefficient_range = coefficient_range
  )
}

# 3. Function to find percentiles
percentiles <- function(series) {
  probs <- c(
    0.01, 0.05, 0.10, 0.20, 0.25,
    0.30, 0.40, 0.50, 0.60, 0.70,
    0.75, 0.80, 0.90, 0.95, 0.99
  )
  values <- quantile(series, probs = probs, na.rm = TRUE, names = FALSE, type = 7)
  round(values, 5)
}

# 4. Function to find quartiles, IQR, QD, and CQD
quartiles_iqr_qd_cqd <- function(series) {
  quartile_values <- quantile(
    series, probs = c(0.25, 0.50, 0.75),
    na.rm = TRUE, names = FALSE, type = 7
  )
  q1 <- round(quartile_values[1], 5)
  q2 <- round(quartile_values[2], 5)
  q3 <- round(quartile_values[3], 5)
  
  iqr <- round(q3 - q1, 5)
  qd <- round(iqr / 2, 5)
  denominator <- q3 + q1
  cqd <- if (denominator != 0) {
    round(iqr / denominator, 5)
  } else {
    NA_real_
  }
  
  c(q1 = q1, q2 = q2, q3 = q3, iqr = iqr, qd = qd, cqd = cqd)
}

# 5. Function to find absolute deviations
absolute_deviations <- function(series) {
  mean_value <- mean(series)
  median_value <- median(series)
  
  mean_absolute_deviation <- round(
    mean(abs(series - mean_value)), 5
  )
  median_absolute_deviation <- round(
    median(abs(series - median_value)), 5
  )
  scaled_mad <- round(median_absolute_deviation * 1.4826, 5)
  
  c(
    mean_absolute_deviation = mean_absolute_deviation,
    median_absolute_deviation = median_absolute_deviation,
    scaled_mad = scaled_mad
  )
}

# 6. Function to find variance, standard deviation, and standard error
variance_std_error <- function(series) {
  n <- length(series)
  
  sample_variance <- if (n > 1) round(var(series), 5) else NA_real_
  population_variance <- round(mean((series - mean(series))^2), 5)
  sample_std <- if (n > 1) round(sd(series), 5) else NA_real_
  population_std <- round(sqrt(population_variance), 5)
  standard_error <- if (n > 1) {
    round(sd(series) / sqrt(n), 5)
  } else {
    NA_real_
  }
  
  c(
    sample_variance = sample_variance,
    population_variance = population_variance,
    sample_std = sample_std,
    population_std = population_std,
    standard_error = standard_error
  )
}

# 7. Function to find coefficient of variation
coefficient_variation <- function(series) {
  mean_value <- mean(series)
  std_value <- if (length(series) > 1) sd(series) else NA_real_
  
  if (length(series) > 1 && mean_value != 0) {
    round((std_value / mean_value) * 100, 5)
  } else {
    NA_real_
  }
}

# 8. Function to find bias-corrected skewness and kurtosis.
# These formulas correspond to SciPy's bias = FALSE, Fisher excess kurtosis.
skewness_kurtosis <- function(series) {
  n <- length(series)
  if (n < 3 || length(unique(series)) <= 1) {
    return(c(
      skewness_value = NA_real_,
      excess_kurtosis = NA_real_,
      pearson_kurtosis = NA_real_
    ))
  }
  
  centered <- series - mean(series)
  m2 <- mean(centered^2)
  m3 <- mean(centered^3)
  m4 <- mean(centered^4)
  
  if (m2 == 0) {
    return(c(
      skewness_value = NA_real_,
      excess_kurtosis = NA_real_,
      pearson_kurtosis = NA_real_
    ))
  }
  
  g1 <- m3 / (m2^(3 / 2))
  skewness_value <- sqrt(n * (n - 1)) / (n - 2) * g1
  
  if (n >= 4) {
    g2 <- (m4 / (m2^2)) - 3
    excess_kurtosis <- (
      (n - 1) / ((n - 2) * (n - 3))
    ) * ((n + 1) * g2 + 6)
    pearson_kurtosis <- excess_kurtosis + 3
  } else {
    excess_kurtosis <- NA_real_
    pearson_kurtosis <- NA_real_
  }
  
  c(
    skewness_value = round(skewness_value, 5),
    excess_kurtosis = round(excess_kurtosis, 5),
    pearson_kurtosis = round(pearson_kurtosis, 5)
  )
}

# 9. Function to find root-mean-square statistics
Root_Mean_Square <- function(series) {
  valid_count <- length(series)
  unique_count <- length(unique(series))
  total_sum <- round(sum(series), 5)
  rms_value <- round(sqrt(mean(series^2)), 5)
  
  c(
    valid_count = valid_count,
    unique_count = unique_count,
    total_sum = total_sum,
    rms_value = rms_value
  )
}

# 10. Function to find outliers using the IQR method
outlier_statistics <- function(series) {
  q <- quantile(
    series, probs = c(0.25, 0.75),
    na.rm = TRUE, names = FALSE, type = 7
  )
  q1 <- q[1]
  q3 <- q[2]
  iqr <- q3 - q1
  
  lower_fence <- round(q1 - (1.5 * iqr), 5)
  upper_fence <- round(q3 + (1.5 * iqr), 5)
  outlier_count <- sum(
    series < lower_fence | series > upper_fence,
    na.rm = TRUE
  )
  
  c(
    lower_fence = lower_fence,
    upper_fence = upper_fence,
    outlier_count = outlier_count
  )
}

# Apply all functions to numeric columns
for (column in numeric_columns) {
  # Convert values to numeric and remove NA/NaN/infinite values.
  series <- suppressWarnings(as.numeric(as.character(df_1[[column]])))
  series <- series[!is.na(series) & is.finite(series)]
  
  if (length(series) == 0) {
    cat("\n", strrep("=", 60), "\n", sep = "")
    cat("Column Name: ", column, "\n", sep = "")
    cat("No valid numeric values available.\n")
    next
  }
  
  central <- mean_median_mode(series)
  min_max <- min_max_range(series)
  percentile_values <- percentiles(series)
  quartile_values <- quartiles_iqr_qd_cqd(series)
  deviation_values <- absolute_deviations(series)
  variance_values <- variance_std_error(series)
  cv <- coefficient_variation(series)
  shape_values <- skewness_kurtosis(series)
  rms_values <- Root_Mean_Square(series)
  outlier_values <- outlier_statistics(series)
  
  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("Column Name: ", column, "\n", sep = "")
  cat(strrep("=", 60), "\n", sep = "")
  
  cat("\n--- DATA SUMMARY ---\n")
  cat("Valid Count: ", rms_values["valid_count"], "\n", sep = "")
  cat(
    "Missing/Invalid Count: ",
    length(df_1[[column]]) - rms_values["valid_count"], "\n",
    sep = ""
  )
  cat("Unique Values: ", rms_values["unique_count"], "\n", sep = "")
  cat("Sum: ", rms_values["total_sum"], "\n", sep = "")
  cat("Root Mean Square: ", rms_values["rms_value"], "\n", sep = "")
  
  cat("\n--- CENTRAL TENDENCY ---\n")
  cat("Mean: ", central$mean_value, "\n", sep = "")
  cat("Median: ", central$median_value, "\n", sep = "")
  cat("Mode: ")
  print(central$mode_value)
  
  cat("\n--- MINIMUM, MAXIMUM AND RANGE ---\n")
  cat("Minimum: ", min_max["min_value"], "\n", sep = "")
  cat("Maximum: ", min_max["max_value"], "\n", sep = "")
  cat("Range: ", min_max["range_value"], "\n", sep = "")
  cat("Coefficient of Range: ", min_max["coefficient_range"], "\n", sep = "")
  
  cat("\n--- PERCENTILES ---\n")
  names(percentile_values) <- c(
    "P1", "P5", "P10", "P20", "P25", "P30", "P40", "P50",
    "P60", "P70", "P75", "P80", "P90", "P95", "P99"
  )
  print(percentile_values)
  
  cat("\n--- QUARTILES AND QUARTILE DISPERSION ---\n")
  cat("Q1: ", quartile_values["q1"], "\n", sep = "")
  cat("Q2: ", quartile_values["q2"], "\n", sep = "")
  cat("Q3: ", quartile_values["q3"], "\n", sep = "")
  cat("IQR: ", quartile_values["iqr"], "\n", sep = "")
  cat("Quartile Deviation (QD): ", quartile_values["qd"], "\n", sep = "")
  cat(
    "Coefficient of Quartile Deviation (CQD): ",
    quartile_values["cqd"], "\n", sep = ""
  )
  
  cat("\n--- ABSOLUTE DEVIATIONS ---\n")
  cat(
    "Mean Absolute Deviation: ",
    deviation_values["mean_absolute_deviation"], "\n", sep = ""
  )
  cat(
    "Median Absolute Deviation (MAD): ",
    deviation_values["median_absolute_deviation"], "\n", sep = ""
  )
  cat("Scaled MAD: ", deviation_values["scaled_mad"], "\n", sep = "")
  
  cat("\n--- VARIANCE, STANDARD DEVIATION AND ERROR ---\n")
  cat("Sample Variance: ", variance_values["sample_variance"], "\n", sep = "")
  cat(
    "Population Variance: ",
    variance_values["population_variance"], "\n", sep = ""
  )
  cat(
    "Sample Standard Deviation: ",
    variance_values["sample_std"], "\n", sep = ""
  )
  cat(
    "Population Standard Deviation: ",
    variance_values["population_std"], "\n", sep = ""
  )
  cat("Standard Error: ", variance_values["standard_error"], "\n", sep = "")
  
  cat("\n--- RELATIVE DISPERSION ---\n")
  cat("Coefficient of Variation (%): ", cv, "\n", sep = "")
  
  cat("\n--- DISTRIBUTION SHAPE ---\n")
  cat("Skewness: ", shape_values["skewness_value"], "\n", sep = "")
  cat(
    "Excess Kurtosis (Fisher): ",
    shape_values["excess_kurtosis"], "\n", sep = ""
  )
  cat(
    "Pearson Kurtosis: ",
    shape_values["pearson_kurtosis"], "\n", sep = ""
  )
  
  cat("\n--- OUTLIER DETECTION (IQR METHOD) ---\n")
  cat("Lower Fence: ", outlier_values["lower_fence"], "\n", sep = "")
  cat("Upper Fence: ", outlier_values["upper_fence"], "\n", sep = "")
  cat("Outlier Count: ", outlier_values["outlier_count"], "\n", sep = "")
}


# 24. RE-DO EDA ON df_1
cat("\n\n")
cat("RE-DO EDA ON df_1\n")

# 1. Check rows and columns
cat("\nNumber of rows and columns: ")
print(dim(df_1))
cat("Number of rows: ", nrow(df_1), "\n", sep = "")
cat("Number of columns: ", ncol(df_1), "\n", sep = "")

# 2. Check data types and non-null counts
cat("\nInformation about df_1:\n")
str(df_1)

df_1_info <- data.frame(
  Column_Name = names(df_1),
  Data_Type = vapply(df_1, function(x) paste(class(x), collapse = "/"),
                     character(1)),
  Non_Null_Count = vapply(df_1, function(x) sum(!is.na(x)), integer(1)),
  Null_Count = vapply(df_1, function(x) sum(is.na(x)), integer(1)),
  row.names = NULL,
  check.names = FALSE
)
print(df_1_info)

# 3. Null summary for every column
null_apply_df1 <- as.data.frame(t(sapply(df_1, null_summary)))
colnames(null_apply_df1) <- c(
  "Total Null Count",
  "Percentage of Null (%)",
  "Total Not Null Count",
  "Percentage of Not Null (%)"
)

cat("\nNull Summary of Every Column in df_1:\n")
print(null_apply_df1)

# 4. Total null values and percentage
total_nulls_df1 <- sum(is.na(df_1))
total_cells_df1 <- nrow(df_1) * ncol(df_1)
total_nulls_percentage_df1 <- if (total_cells_df1 > 0) {
  (total_nulls_df1 / total_cells_df1) * 100
} else {
  0
}

cat("Total null values in df_1:", total_nulls_df1, "\n")
cat(
  "Total null values percentage in df_1:",
  round(total_nulls_percentage_df1, 2), "%\n"
)

# 5. View top, middle, and bottom 500 values for every column
view_500_df1 <- function(series, col_name = NULL) {
  if (is.null(col_name)) {
    col_name <- deparse(substitute(series))
  }
  
  cat("\nColumn Name:", col_name, "\n")
  cat("Total Values:", length(series), "\n")
  
  cat("\n--- TOP 500 ROWS ---\n")
  print(head(series, 500))
  
  cat("\n--- MIDDLE 500 ROWS ---\n")
  n_len <- length(series)
  middle_start <- max(1, floor((n_len - 500) / 2) + 1)
  middle_end <- min(n_len, middle_start + 499)
  
  if (n_len > 0) {
    print(series[middle_start:middle_end])
  } else {
    cat("Series is empty.\n")
  }
  
  cat("\n--- BOTTOM 500 ROWS ---\n")
  print(tail(series, 500))
}

for (column in names(df_1)) {
  view_500_df1(df_1[[column]], col_name = column)
}

# 6. Unique values and counts for every column
unique_value_count_df1 <- function(series, col_name = NULL) {
  if (is.null(col_name)) {
    col_name <- deparse(substitute(series))
  }
  
  cat("\nColumn Name:", col_name, "\n")
  cat("\nUnique Values and Their Counts:\n")
  print(sort(table(series, useNA = "ifany"), decreasing = TRUE))
  cat(
    "\nNumber of unique non-null values:",
    length(unique(na.omit(series))), "\n"
  )
}

for (column in names(df_1)) {
  unique_value_count_df1(df_1[[column]], col_name = column)
}

# 7. Display all rows involved in duplicate groups
duplicated_records_df1 <- function(data_obj) {
  duplicate_rows <- data_obj[
    duplicated(data_obj) | duplicated(data_obj, fromLast = TRUE),
    ,
    drop = FALSE
  ]
  
  cat("\n--- DUPLICATE RECORDS IN df_1 ---\n")
  if (nrow(duplicate_rows) == 0) {
    cat("No duplicate records found.\n")
  } else {
    print(duplicate_rows)
  }
  invisible(duplicate_rows)
}

duplicated_records_df1(df_1)

# 8. Count duplicates and percentages in df_1
duplicated_count_df1 <- function(data_obj) {
  extra_dup_count <- sum(duplicated(data_obj))
  extra_dup_pct <- if (nrow(data_obj) > 0) {
    (extra_dup_count / nrow(data_obj)) * 100
  } else {
    0
  }
  
  all_involved_count <- sum(
    duplicated(data_obj) | duplicated(data_obj, fromLast = TRUE)
  )
  all_involved_pct <- if (nrow(data_obj) > 0) {
    (all_involved_count / nrow(data_obj)) * 100
  } else {
    0
  }
  
  cat("\nExtra duplicate copies count:", extra_dup_count, "\n")
  cat("Percentage of extra duplicate copies:",
      round(extra_dup_pct, 2), "%\n")
  cat("Total rows involved in duplicate groups:",
      all_involved_count, "\n")
  cat("Percentage of rows involved in duplicate groups:",
      round(all_involved_pct, 2), "%\n")
  
  duplicate_rows <- data_obj[
    duplicated(data_obj) | duplicated(data_obj, fromLast = TRUE),
    ,
    drop = FALSE
  ]
  
  if (nrow(duplicate_rows) > 0) {
    sort_key <- do.call(
      paste,
      c(
        lapply(duplicate_rows, function(x) {
          out <- as.character(x)
          out[is.na(x)] <- "<NULL>"
          out
        }),
        sep = "\r"
      )
    )
    duplicate_rows <- duplicate_rows[order(sort_key), , drop = FALSE]
    
    cat("\nFirst 10 duplicate rows:\n")
    print(head(duplicate_rows, 10))
    cat("\nLast 10 duplicate rows:\n")
    print(tail(duplicate_rows, 10))
  } else {
    cat("\nNo duplicate records found.\n")
  }
  
  invisible(duplicate_rows)
}

duplicate_rows_df1 <- duplicated_count_df1(df_1)

# 25. EXPORT CSV AND EXCEL FILES FOR df_1
# Output directory from the supplied Python notebook
output_dir <- "C:/Users/vasav/OneDrive/Desktop/DA_PROJECTS/Project_2/Excel_Files_Folder"

if (!dir.exists(output_dir)) {
  fs::dir_create(output_dir)
}

csv_output_path <- file.path(output_dir, "Retail_Cleaned_Data_RSTUDIO.csv")
xlsx_output_path <- file.path(output_dir, "Retail_Cleaned_Data_RSTUDIO.xlsx")

# Export to CSV
write.csv(
  df_1,
  file = csv_output_path,
  row.names = FALSE,
  na = ""
)
cat("\nCleaned CSV exported to:\n", csv_output_path, "\n", sep = "")

# Export to Excel when the writexl package is installed
if (requireNamespace("writexl", quietly = TRUE)) {
  writexl::write_xlsx(df_1, path = xlsx_output_path)
  cat("\nCleaned Excel file exported to:\n", xlsx_output_path, "\n", sep = "")
} else {
  warning(
    "The CSV was exported, but Excel export was skipped because 'writexl' ",
    "is not installed. Run install.packages('writexl') and run the script again."
  )
}

# 26. BOX PLOTS FOR NUMERIC COLUMNS IN df_1
box_plot <- function(numeric_cols, data_frame = df_1) {
  for (column in numeric_cols) {
    values <- suppressWarnings(as.numeric(as.character(data_frame[[column]])))
    chart_data <- data.frame(Value = values)
    chart_data <- chart_data[
      !is.na(chart_data$Value) & is.finite(chart_data$Value),
      ,
      drop = FALSE
    ]
    
    if (nrow(chart_data) == 0) {
      cat("\nSkipping box plot for", column, "- no valid numeric values.\n")
      next
    }
    
    p <- ggplot(chart_data, aes(x = "", y = Value)) +
      geom_boxplot(na.rm = TRUE) +
      labs(
        title = paste("Box Plot of", column),
        x = NULL,
        y = column
      ) +
      theme_minimal()
    
    print(p)
  }
}

# Call the function
box_plot(numeric_columns)

# 27. VIOLIN PLOTS FOR NUMERIC COLUMNS IN df_1
violin_chart <- function(numeric_cols, data_frame = df_1) {
  for (column in numeric_cols) {
    values <- suppressWarnings(as.numeric(as.character(data_frame[[column]])))
    chart_data <- data.frame(Value = values)
    chart_data <- chart_data[
      !is.na(chart_data$Value) & is.finite(chart_data$Value),
      ,
      drop = FALSE
    ]
    
    if (nrow(chart_data) < 2 || length(unique(chart_data$Value)) < 2) {
      cat("\nSkipping violin plot for", column,
          "- at least two distinct numeric values are required.\n")
      next
    }
    
    p <- ggplot(chart_data, aes(x = "", y = Value)) +
      geom_violin(trim = TRUE, na.rm = TRUE) +
      labs(
        title = paste("Violin Plot of", column),
        x = NULL,
        y = column
      ) +
      theme_minimal()
    
    print(p)
  }
}

# Call the function
violin_chart(numeric_columns)
