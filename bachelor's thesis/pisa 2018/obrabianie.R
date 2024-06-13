library(data.table)
library(haven)

sas_file <- "cy08msp_stu_qqq.sas7bdat"
data <- read_sas(sas_file)
csv_file = "cy08msp_stu_qqq.csv"
write.csv(data, file = csv_file, row.names = FALSE)
df = fread(csv_file)
pl_df <- df[df$CNT == "POL",]


# Display the distinct values
# distinct_values <- unique(df$CNT)
# print(distinct_values)


varying_columns <- vapply(pl_df, function(x) length(unique(x)) > 1, logical(1L))
pol_df <- pl_df[, varying_columns, with = FALSE]

final = "student_questionaire_pl_2022.csv"
data.table::fwrite(pol_df, file = final, row.names = FALSE)

