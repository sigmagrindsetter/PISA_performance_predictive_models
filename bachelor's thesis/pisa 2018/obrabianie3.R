student_questionaire_2018 <- read.csv("student_questionaire_pl_2018.csv")
schools_pl_2018 <- read.csv("schools_pl_2018.csv")

total_cols <- ncol(student_questionaire_2018)

# Create a vector of column indices to keep
cols_to_keep <- c(2, 6:(683))

# Subset the dataframe to keep only the desired columns
student_questionaire_2018 <- student_questionaire_2018[, cols_to_keep, drop = FALSE]


cognitive_2018_aggregated <- read.csv("cognitive_2018_aggregated.csv")

# Merge the two tables on school id
merged_data <- merge(cognitive_2018_aggregated, schools_pl_2018, by = "CNTSCHID", all.x = TRUE)

# all.x = TRUE keeps all rows from the first (left) table (cognitive_aggregated)

# Check the structure of the merged data
str(merged_data)

merged_data <- merge(merged_data, student_questionaire_2018, by = "CNTSTUID", all.x = TRUE)

write.csv(merged_data, file = "pisa_dataset_pl_2018.csv")