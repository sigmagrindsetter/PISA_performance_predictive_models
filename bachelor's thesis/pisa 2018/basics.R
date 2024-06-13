# Calculate column-wise averages for math_score, read_score, and science_score, excluding zeros
math_avg <- mean(limited_df$math_score[limited_df$math_score != 0], na.rm = TRUE)
read_avg <- mean(limited_df$read_score[limited_df$read_score != 0], na.rm = TRUE)
science_avg <- mean(aggregated_df$science_score[aggregated_df$science_score != 0], na.rm = TRUE)

# Print the column-wise averages
print(math_avg)
print(read_avg)
print(science_avg)

math_max <- max(aggregated_df$math_score, na.rm = TRUE)
read_max <- max(aggregated_df$read_score, na.rm = TRUE)
science_max <- max(aggregated_df$science_score, na.rm = TRUE)

# Print the maximum values
print(math_max)
print(read_max)
print(science_max)

par(mfrow=c(1,3))

# Filter out zero values from math_score
filtered_math_score <- aggregated_df$math_score[aggregated_df$math_score != 0]

# Create a histogram for math_score excluding zero values
hist(filtered_math_score, main="Math Score Histogram (excluding zeros)", xlab="Math Score")
# Filter out zero values from read_score
filtered_read_score <- aggregated_df$read_score[aggregated_df$read_score != 0]

# Create a histogram for read_score excluding zero values
hist(filtered_read_score, main="Read Score Histogram", xlab="Read Score")

# Filter out zero values from science_score
filtered_science_score <- aggregated_df$science_score[aggregated_df$science_score != 0]

# Create a histogram for science_score excluding zero values
hist(filtered_science_score, main="Science Score Histogram", xlab="Science Score")