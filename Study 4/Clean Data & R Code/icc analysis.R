# Uncomment lines 3-21 to create "SAQ_random_sample_[timestamp].csv" ###########

# # Import data
# df = read.csv("SAQ_participant_answers.csv", stringsAsFactors = FALSE)
# 
# # Set a seed for reproducibility
# set.seed(123)
# 
# # Randomly sample 10 rows
# sample_df = df[sample(nrow(df), 10), ]
# 
# # View the result
# sample_df # rows 159, 179, 14, 195, 170, 50, 118, 43, 200, 196
# 
# # Create a timestamp for the filename
# timestamp = format(Sys.time(), "%Y-%m-%d_%H%M")
# 
# # Save to a new CSV with the timestamp in the filename
# output_file = paste0("SAQ_random_sample_", timestamp, ".csv")
# write.csv(sample_df, output_file, row.names = FALSE)
# cat("Saved to:", output_file, "\n")

# Load in libraries
library(dplyr)
library(tidyr)
library(psych)

# Import data
GPT_scores = read.csv("SAQ_participant_scores.csv", stringsAsFactors = FALSE)
AMG_scores = read.csv("SAQ_random_sample_AMG_marking.csv", stringsAsFactors = FALSE)
DYHL_scores = read.csv("SAQ_random_sample_DYHL_marking.csv", stringsAsFactors = FALSE)

# Only select the GPT scores for the random sample
GPT_scores = GPT_scores[c(159, 179, 14, 195, 170, 50, 118, 43, 200, 196),]

# Only select the scores from the AMG and DYHL dataframes
AMG_scores = AMG_scores[seq(2, nrow(AMG_scores), by = 2), , drop = FALSE]
AMG_scores = AMG_scores %>% mutate(across(everything(), as.integer))
str(AMG_scores)

DYHL_scores = DYHL_scores[seq(2, nrow(DYHL_scores), by = 2), , drop = FALSE]
DYHL_scores = DYHL_scores %>% mutate(across(everything(), as.integer))
str(DYHL_scores)

# Convert each scoring dataframe into a vector (participant × item)
icc_data = data.frame(
  ChatGPT = as.vector(as.matrix(GPT_scores)),
  AMG      = as.vector(as.matrix(AMG_scores)),
  DYHL     = as.vector(as.matrix(DYHL_scores))
)

# Calculate all ICCs
icc_results = ICC(icc_data)
icc_results

# Select ICC(3,1) and ICC(2,1)
icc3_1 = icc_results$results[icc_results$results$type == "ICC3", ]
icc2_1 = icc_results$results[icc_results$results$type == "ICC2", ]
cat("ICC(3,1) two-way mixed, consistency:\n")
print(icc3_1)
cat("\nICC(2,1) two-way random, absolute agreement:\n")
print(icc2_1)

