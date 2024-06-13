library(data.table)
library(openxlsx)

# Read the original data
df <- fread("cognitive_pl_2018.csv")

# Read the Excel file containing variable names
variable_names <- read.xlsx("kolumny_do_agregacji_2018.xlsx", colNames = TRUE)

# Initialize an empty data frame with the same number of rows as df
aggregated_df <- data.frame(matrix(NA, nrow = nrow(df), ncol = ncol(variable_names)))
colnames(aggregated_df) <- names(variable_names)

# Define function to extract the first digit
get_first_digit <- function(x) {
  as.numeric(substr(as.character(x), 1, 1))
}

# Loop over each column name in the variable names
for (col_name in names(variable_names)) {
  # Get the variables listed under the current column name
  variables <- variable_names[[col_name]]
  
  # Filter out variables that are not in the dataframe's columns
  valid_variables <- variables[variables %in% colnames(df)]
  
  # Proceed if there are any valid variables
  if (length(valid_variables) > 0) {
    # Aggregate the columns, replace NaNs with 0, and sum row-wise
    column_data <- df[, .SD, .SDcols = valid_variables]
    column_data[is.na(column_data)] <- 0
    
    # Apply get_first_digit to each element of the dataframe and sum row-wise
    first_digits_df <- column_data[, lapply(.SD, function(x) sapply(x, get_first_digit)), .SDcols = valid_variables]
    aggregated_df[[col_name]] <- rowSums(first_digits_df, na.rm = TRUE)
  }
}

aggregated_df$CNTSTUID <- df$CNTSTUID
aggregated_df$CNTSCHID <- df$CNTSCHID
non_zero_values <- aggregated_df[, c("math_score", "read_score")] != 0
all_non_zero <- apply(non_zero_values, 1, all)
number_of_non_zero_observations <- sum(all_non_zero)
print(number_of_non_zero_observations)
limited_df <- aggregated_df[all_non_zero, ]
limited_df <- limited_df[, c("CNTSTUID", "CNTSCHID", setdiff(names(limited_df), c("CNTSTUID", "CNTSCHID")))]

# Save the aggregated data frame to a CSV file
write.csv(limited_df, "cognitive_2018_aggregated.csv", row.names = FALSE)