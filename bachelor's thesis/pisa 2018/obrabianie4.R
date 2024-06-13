df <- read.csv("pisa_dataset_pl_2018.csv")

df <- df[,-1]

nan_percentages <- sapply(df, function(x) sum(is.na(x)) / length(x))
columns_to_remove <- nan_percentages > 0.2
df <- df[, !columns_to_remove]


non_zero_values <- df[, c("math_score", "read_score")] != 0
all_non_zero <- apply(non_zero_values, 1, all)
number_of_non_zero_observations <- sum(all_non_zero)
print(number_of_non_zero_observations)
limited_df <- df[all_non_zero, ]

math_avg <- mean(df$math_score[df$math_score != 0], na.rm = TRUE)
read_avg <- mean(df$read_score[df$read_score != 0], na.rm = TRUE)

df$binary_math_score <- ifelse(df$math_score > math_avg, 1, 0)
df$binary_read_score <- ifelse(df$read_score > read_avg, 1, 0)

specified_columns <- c("binary_math_score", "binary_read_score", "ST003D02T", "ST004D01T", "MISCED",
                       "FISCED", "ST013Q01TA", "ST011Q02TA", "ST011Q04TA", "ST012Q09NA", "ST012Q02TA",
                       "ST211Q02HA", "ST211Q03HA", "ST016Q01NA",
                       "ST034Q01TA", "ST034Q05TA",
                       "ST062Q03TA", "ST038Q05NA", "BMMJ1", "BFMJ2", "SC001Q01TA", "SC013Q01TA",
                       "SC061Q04TA", "SCHSIZE", "CLSIZE", 
                       "SC048Q03NA", "SC017Q01NA", "REPEAT")
columns_exist <- specified_columns %in% names(df)

# Print which columns exist and which do not
cat("Existing Columns:\n")
print(specified_columns[columns_exist])

cat("\nMissing Columns:\n")
print(specified_columns[!columns_exist])

model_df <- na.omit(df[, specified_columns])

new_names <- c("MATHSCORE", "READSCORE", "BIRTHMONTH", "GENDER", "MISCED", "FISCED", "BOOKSHOME",
               "OWNROOM", "OWNCOMPUTER", "INSTRUMENTHOME", "CARSHOME", "TCHLISTEN",
               "TCHUNDERSTAND", "LIFESATISF", "ALIENATION", "LIKEME", "LATE", "BULLIED",
               "MISEI", "FISEI", "CITY", "PRIVATE", "DRUGS", "SCHSIZE", "CLSIZE",
               "POORSTU", "TCHLACK", "REPEAT")
names(model_df) <- new_names

model_df$Q1BIRTH <- ifelse(model_df$BIRTHMONTH %in% 1:3, 1, 0)
model_df$Q2BIRTH <- ifelse(model_df$BIRTHMONTH %in% 4:6, 1, 0)
model_df$Q3BIRTH <- ifelse(model_df$BIRTHMONTH %in% 7:9, 1, 0)
model_df$Q4BIRTH <- ifelse(model_df$BIRTHMONTH %in% 10:12, 1, 0)

model_df$GENDER <- model_df$GENDER - 1
model_df$OWNROOM <- model_df$OWNROOM - 1
model_df$OWNCOMPUTER <- model_df$OWNCOMPUTER - 1
model_df$PRIVATE <- model_df$PRIVATE - 1

model_df$MISCED <- ifelse(model_df$MISCED >= 5, 1, 0)
model_df$FISCED <- ifelse(model_df$FISCED >= 5, 1, 0)

model_df$BOOKSHOME <- ifelse(model_df$BOOKSHOME >= 4, 1, 0)

model_df$CARSHOME <- ifelse(model_df$CARSHOME >= 3, 1, 0)
model_df$TCHLISTEN <- ifelse(model_df$TCHLISTEN >= 3, 1, 0)
model_df$TCHUNDERSTAND <- ifelse(model_df$TCHUNDERSTAND >= 3, 1, 0)
model_df$BULLIED <- ifelse(model_df$BULLIED >= 3, 1, 0)
model_df$LATE <- ifelse(model_df$LATE >= 3, 1, 0)
model_df$TCHLACK <- ifelse(model_df$TCHLACK >= 3, 1, 0)

model_df$DRUGS <- ifelse(model_df$DRUGS >= 2, 1, 0)
model_df$INSTRUMENTHOME <- ifelse(model_df$INSTRUMENTHOME >= 2, 1, 0)

model_df$LIKEME <- ifelse(model_df$LIKEME <= 2, 1, 0)
model_df$ALIENATION <- ifelse(model_df$ALIENATION <= 2, 1, 0)

model_df$LIFESATISF <- ifelse(model_df$LIFESATISF >= 7, 1, 0)

model_df$VILLAGE <- ifelse(model_df$CITY == 1, 1, 0)
model_df$TOWN <- ifelse(model_df$CITY %in% 2:3, 1, 0)
model_df$CITY <- ifelse(model_df$CITY %in% 4:5, 1, 0)
model_df <- model_df[,-3]

write.csv(model_df, file = "train_data_2018.csv", row.names = FALSE)

df2018 <- read_csv("cognitive_2018_aggregated.csv")
df2022 <- read_csv("cognitive_2022_aggregated.csv")
math_hist_2018 <- hist(df2018$math_score, main="Math Score 2018 Histogram", xlab="Math Score", breaks = 15,col = "blue")
math_hist_2022 <- hist(df2022$math_score, main="Math Score 2022 Histogram", xlab="Math Score", breaks = 15,col = "blue")
read_hist_2018 <- hist(df2018$read_score, main="Read Score 2018 Histogram", xlab="Read Score", breaks = 15,col = "blue")
read_hist_2022 <- hist(df2022$read_score, main="Read Score 2022 Histogram", xlab="Read Score", breaks = 15,col = "blue")

reg2018 <- plot(df2018$read_score, df2018$math_score, main = "2018 Scoring",
                xlab = "READSCORE", ylab = "MATHSCORE", pch = 16,
                col = rgb(0, 0, 1, 0.1),  # Blue with alpha for opacity
                cex = 0.5)
reg2022 <- plot(df2022$read_score, df2022$math_score, main = "2022 Scoring",
                xlab = "READSCORE", ylab = "MATHSCORE", pch = 16,
                col = rgb(0, 0, 1, 0.1),  # Blue with alpha for opacity
                cex = 0.5)
column_names_df <- data.frame(ColumnName = names(test_df), stringsAsFactors = FALSE)

# Print each column name without quotes using cat() within a loop
cat("Column Names:\n")
sapply(column_names_df$ColumnName, function(x) cat(x, "\n"))