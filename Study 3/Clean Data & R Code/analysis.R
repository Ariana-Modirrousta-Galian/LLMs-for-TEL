# Clear R environment
rm(list = ls(all = TRUE)) 

# Load in libraries
library(dplyr)
library(car)
library(stringr)
library(lsr) 
library(BayesFactor) 
library(ggplot2)
library(ggpattern)
library(effectsize)


# Data cleaning ################################################################


# Import data
part1 = read.csv("Part 1.csv")
part2 = read.csv("Part 2.csv")
part3 = read.csv("Part 3.csv")

# Remove header rows
part1 = part1[c(-1,-2),] 
part2 = part2[c(-1,-2),] 
part3 = part3[c(-1,-2),] 

# Only keep participants that completed all three parts
common_ids = Reduce(intersect, list(
  part1$PROLIFIC_PID,
  part2$PROLIFIC_PID,
  part3$PROLIFIC_PID
))
common_ids
part1 = part1[part1$PROLIFIC_PID %in% common_ids,]
part2 = part2[part2$PROLIFIC_PID %in% common_ids,]
part3 = part3[part3$PROLIFIC_PID %in% common_ids,]

# Look for potential bots
part1_bots = part1[part1$Q_RecaptchaScore <= 0.5,]$PROLIFIC_PID
part2_bots = part2[part2$Q_RecaptchaScore <= 0.5,]$PROLIFIC_PID
part3_bots = part3[part3$Q_RecaptchaScore <= 0.5,]$PROLIFIC_PID
all_part_bots = Reduce(intersect, list(part1_bots, part2_bots, part3_bots))
all_part_bots

# Look at data from potential bots
parts = list(part1, part2, part3)
results = lapply(parts, function(df) {
  df2 = df[df$PROLIFIC_PID %in% all_part_bots, ] 
  df2$Duration_min = as.numeric(df2$Duration..in.seconds.) / 60
  df2
}) 
results 

# Look for low effort at practice test
SAQs = part2[part2$Group == "SAQ", ] %>%
  rowwise() %>%
  mutate(
    blank_count = sum(is.na(c_across(TeaQ01:TeaQ10)) | trimws(c_across(TeaQ01:TeaQ10)) == ""),
    all_blank = blank_count == 10
  ) %>%
  ungroup()
SAQs$blank_count 
SAQs$all_blank 
print(SAQs %>% select(TeaQ01:TeaQ10), n = Inf, width = Inf) 

# Look for unusually high or low practice test scores
SAQ_scores = part2[part2$Group == "SAQ", grepl("^Tea_F", names(part2))] |>
  lapply(function(col) {
    str_extract(col, "[0-9]+/[0-9]+$") |>
      sapply(function(x) {
        parts = strsplit(x, "/")[[1]]
        as.numeric(parts[1]) / as.numeric(parts[2])
      }) |>
      unname()
  })
SAQ_scores = do.call(cbind, SAQ_scores)
SAQ_scores 
rowMeans(SAQ_scores) # participants 5 and 50 have an average SAQ score of .13
mean(rowMeans(SAQ_scores)) 
sd(rowMeans(SAQ_scores))
min(rowMeans(SAQ_scores))
max(rowMeans(SAQ_scores))

# 2 SDs below the mean is .136, so the two participants are showing worse performance than 95% of the sample
mean_SAQ = mean(rowMeans(SAQ_scores))
sd_SAQ = sd(rowMeans(SAQ_scores))
mean(rowMeans(SAQ_scores)) - 2 * sd(rowMeans(SAQ_scores))

# Remove these two participants 
exclude_ids = part2[part2$Group == "SAQ", "PROLIFIC_PID"]
exclude_ids = exclude_ids[c(5,50)]
part1 = part1[!part1$PROLIFIC_PID %in% exclude_ids, ]
part2 = part2[!part2$PROLIFIC_PID %in% exclude_ids, ]
part3 = part3[!part3$PROLIFIC_PID %in% exclude_ids, ]


# Demographics #################################################################


# Age
mean(as.numeric(part1$age))
sd(as.numeric(part1$age))

# Gender
table(part1$gender) # 1 = male; 2 = female


# Completion times #############################################################


# Part one
median(as.numeric(part1$Duration..in.seconds.)/60)

# Part two
median(as.numeric(part2$Duration..in.seconds.)/60)
median(as.numeric(part2[part2$Group == "SAQ",]$Duration..in.seconds.)/60)
median(as.numeric(part2[part2$Group == "Restudy",]$Duration..in.seconds.)/60)

# Part three
median(as.numeric(part3$Duration..in.seconds.)/60)


# Testing effect ###############################################################


# Sample sizes
nrow(part1)
nrow(part2)
nrow(part3)

# Get PROLIFIC_PID and Group from part 2 
IDs = part2[,c("PROLIFIC_PID","Group")]

# Add Group to the part 3 data
part3 = part3 %>%
  left_join(IDs, by = "PROLIFIC_PID")

# Calculate the proportion correct in the final test
part3$SC0 = as.numeric(part3$SC0)
part3$prop_correct = part3$SC0/14

# Export the key variables into a .csv file
# saq_part2 = part2[part2$Group == "SAQ", ]
# saq_items = sapply(saq_part2[, grepl("^Tea_F", names(saq_part2))], function(col) {
#   s = str_extract(col, "[0-9]+/[0-9]+$")
#   as.numeric(sub("/.*", "", s)) / as.numeric(sub(".*/", "", s))
# })
# practice = data.frame(PROLIFIC_PID = saq_part2$PROLIFIC_PID,
#                       practice_prop = rowMeans(saq_items))
# export_data = part3[, c("PROLIFIC_PID", "Group", "prop_correct")]
# export_data$practice_prop = practice$practice_prop[match(export_data$PROLIFIC_PID, practice$PROLIFIC_PID)]
# write.csv(export_data, "data_AMG.csv", row.names = FALSE)

# T-test
var.test(part3[part3$Group == "SAQ",]$prop_correct, part3[part3$Group == "Restudy",]$prop_correct)
t.test(part3[part3$Group == "SAQ",]$prop_correct, part3[part3$Group == "Restudy",]$prop_correct, alternative = "greater")
cohensD(part3[part3$Group == "SAQ",]$prop_correct, part3[part3$Group == "Restudy",]$prop_correct)
ttestBF(part3[part3$Group == "SAQ",]$prop_correct, part3[part3$Group == "Restudy",]$prop_correct, nullInterval = c(0, Inf) )

# Standard deviations
sd(part3[part3$Group == "SAQ",]$prop_correct)
sd(part3[part3$Group == "Restudy",]$prop_correct)

# Group sizes
nrow(part3[part3$Group == "SAQ",])
nrow(part3[part3$Group == "Restudy",])

# Violin plot
ggplot(part3, aes(Group, prop_correct, fill = Group)) +
  geom_violin(alpha = 0.8) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.4, fill = "white") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1, size = 0.75, color = "black") +
  stat_summary(fun = mean, geom = "point", size = 4, fill = "black") +
  theme_classic() +
  guides(fill = "none") +
  labs(y= "Proportion Correct", x = "Group") +
  scale_x_discrete(labels= c("Restudy", "Retrieval Practice")) +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 16)) +
  ylim(0,1)

# Correlation betwen practice and final test performance
saq_part2 = part2 %>% filter(Group == "SAQ")
saq_items = sapply(saq_part2[, grepl("^Tea_F", names(saq_part2))], function(col) {
  s = str_extract(col, "[0-9]+/[0-9]+$")
  as.numeric(sub("/.*", "", s)) / as.numeric(sub(".*/", "", s))
})
practice = data.frame(PROLIFIC_PID = saq_part2$PROLIFIC_PID,
                      practice = rowMeans(saq_items))
final = part3 %>%
  select(PROLIFIC_PID, final = prop_correct)
pt_ft = inner_join(practice, final, by = "PROLIFIC_PID")
cor.test(pt_ft$practice, pt_ft$final)


# Retention intervals and completion times #####################################


# Change the start date variable into date format
part1$StartDate = as.POSIXct(part1$StartDate, format="%d/%m/%Y %H:%M")
part2$StartDate = as.POSIXct(part2$StartDate, format="%d/%m/%Y %H:%M")
part3$StartDate = as.POSIXct(part3$StartDate, format="%d/%m/%Y %H:%M")

# Get each participant's start date for each part
all_parts = merge(part1[, c("PROLIFIC_PID", "StartDate")],
                  part2[, c("PROLIFIC_PID", "StartDate")],
                  by = "PROLIFIC_PID", suffixes = c("_1", "_2"))
all_parts = merge(all_parts,
                  part3[, c("PROLIFIC_PID", "StartDate")],
                  by = "PROLIFIC_PID")
names(all_parts)[names(all_parts) == "StartDate"] = "StartDate_3"

# Calculate the retention interval between parts 1 & 2 and parts 2 & 3 for each participant
all_parts$Interval_1_2 = as.numeric(difftime(all_parts$StartDate_2, all_parts$StartDate_1, units="days"))
all_parts$Interval_2_3 = as.numeric(difftime(all_parts$StartDate_3, all_parts$StartDate_2, units="days"))
intervals = all_parts[, c("PROLIFIC_PID", "Interval_1_2", "Interval_2_3")]

# Range of retention intervals between parts 1 & 2 and parts 2 & 3
range(intervals$Interval_1_2) * 24
range(intervals$Interval_2_3) * 24

# Get each participant's completion time from part 2
part2_completion_time = part2[, c("PROLIFIC_PID", "Duration..in.seconds.")]

# Add retention intervals and part 2 completion times to the part 3 data
part3 = merge(part3, intervals, by = "PROLIFIC_PID", all.x = TRUE)
part3 = merge(part3, part2_completion_time, by = "PROLIFIC_PID", all.x = TRUE)

# Change the structure of the part 3 data
str(part3)
part3$Group = as.factor(part3$Group)
part3$Duration..in.seconds..y = as.numeric(part3$Duration..in.seconds..y)

# Range of completion times from part 2
range(part3$Duration..in.seconds..y) / 60

# ANCOVA
summary(aov(prop_correct ~ Group + Interval_1_2 + Interval_2_3 + Duration..in.seconds..y, data = part3))
eta_squared(aov(prop_correct ~ Group + Interval_1_2 + Interval_2_3 + Duration..in.seconds..y, data = part3))


# Prior knowledge ##############################################################


# Get each participant's prior knowledge 
prior_knowledge = merge(part1[, c("PROLIFIC_PID", "prior_knowledge_1")],
                        part2[, c("PROLIFIC_PID", "Group")],
                        by = "PROLIFIC_PID", suffixes = c("_1", "_2"))

# Add prior knowledge to the part 3 data
part3 = merge(part3, prior_knowledge[, c("PROLIFIC_PID", "prior_knowledge_1")], by = "PROLIFIC_PID", all.x = TRUE)

# Change the structure of the part 3 data
str(part3)
part3$prior_knowledge_1 = as.numeric(part3$prior_knowledge_1)

# Compare prior knowledge between the two groups
var.test(part3[part3$Group == "SAQ",]$prior_knowledge_1, part3[part3$Group == "Restudy",]$prior_knowledge_1)
t.test(part3[part3$Group == "SAQ",]$prior_knowledge_1, part3[part3$Group == "Restudy",]$prior_knowledge_1, var.equal = T)
cohensD(part3[part3$Group == "SAQ",]$prior_knowledge_1, part3[part3$Group == "Restudy",]$prior_knowledge_1)
ttestBF(part3[part3$Group == "SAQ",]$prior_knowledge_1, part3[part3$Group == "Restudy",]$prior_knowledge_1)

# Standard deviations
sd(part3[part3$Group == "SAQ",]$prior_knowledge_1)
sd(part3[part3$Group == "Restudy",]$prior_knowledge_1)


# Item indices #################################################################
## Practice test ###############################################################


# Get SAQ data
SAQ_scores = part2[part2$Group == "SAQ", grepl("^Tea_F", names(part2))] |>
  lapply(function(col) {
    str_extract(col, "[0-9]+/[0-9]+$") |>
      sapply(function(x) {
        parts = strsplit(x, "/")[[1]]
        as.numeric(parts[1]) / as.numeric(parts[2])
      }) |>
      unname()
  })
SAQ_scores = do.call(cbind, SAQ_scores)
SAQ_scores = data.frame(SAQ_scores)

# Calculate item difficulty index for each SAQ and across all SAQs
SAQ_difficulty = SAQ_scores %>%
  summarise(across(starts_with("Tea"), ~ mean(., na.rm = TRUE)))

# Calculate discriminatory power for each SAQ and across all SAQs
SAQ_discrimination = SAQ_scores %>%
  summarise(
    across(
      everything(),
      ~ {
        this_item = cur_column()
        others = setdiff(names(SAQ_scores), this_item)
        
        # total score excluding this SAQ
        total_excluding = rowMeans(SAQ_scores[others], na.rm = TRUE)
        
        # Pearson correlation between SAQ and total score
        cor(.x, total_excluding, use = "complete.obs")
      }
    )
  )


## Final test ##################################################################


# Change the structure of the part 3 data
str(part3)
part3 = part3 %>% 
  mutate(across(starts_with("MCQ"), as.numeric))

# If participants selected anything other than option 1 (correct) for each MCQ, code it as 0 (incorrect)
part3 = part3 %>%
  mutate(across(starts_with("MCQ"),
                ~ ifelse(. == 1, 1, 0)))

# Calculate item difficulty index for each MCQ and across all MCQs
MCQ_difficulty = part3 %>%
  summarise(across(starts_with("MCQ"), ~ mean(., na.rm = TRUE)))

# Get MCQ data
MCQ_data = part3 %>% select(starts_with("MCQ"))

# Calculate discriminatory power for each MCQ and across all MCQs
MCQ_discrimination = MCQ_data %>%
  summarise(
    across(
      everything(),   # apply to all MCQ columns
      ~ {
        this_item = cur_column()                  # name of current MCQ
        others = setdiff(names(MCQ_data), this_item)  # all other MCQs
        
        # compute total score excluding current item
        total_excluding = rowMeans(MCQ_data[others], na.rm = TRUE)
        
        # point-biserial correlation
        cor(.x, total_excluding, use = "complete.obs")
      }
    )
  )


## Comparisons #################################################################
### Difficulty #################################################################


# Bind the practice test item difficulties 
SAQ_difficulty = data.frame(
  test = as.factor("Practice"),
  difficulty = as.numeric(SAQ_difficulty[1, ])
)

# Bind the final test item difficulties 
MCQ_difficulty = data.frame(
  test = as.factor("Final"),
  difficulty = as.numeric(MCQ_difficulty[1, ])
)

# Bind the practice and final test item difficulties
difficulty = rbind(SAQ_difficulty, MCQ_difficulty)

# T-test
var.test(difficulty[difficulty$test == "Practice",]$difficulty, difficulty[difficulty$test == "Final",]$difficulty)
t.test(difficulty[difficulty$test == "Practice",]$difficulty, difficulty[difficulty$test == "Final",]$difficulty, var.equal = T)
cohensD(difficulty[difficulty$test == "Practice",]$difficulty, difficulty[difficulty$test == "Final",]$difficulty)
ttestBF(difficulty[difficulty$test == "Practice",]$difficulty, difficulty[difficulty$test == "Final",]$difficulty)

# Standard deviations
sd(difficulty[difficulty$test == "Practice",]$difficulty)
sd(difficulty[difficulty$test == "Final",]$difficulty)


### Discriminatory power #######################################################


# Bind the practice test item discriminatory power
SAQ_discrimination = data.frame(
  test = as.factor("Practice"),
  discrimination = as.numeric(SAQ_discrimination[1, ])
)

# Bind the final test item discriminatory power 
MCQ_discrimination = data.frame(
  test = as.factor("Final"),
  discrimination = as.numeric(MCQ_discrimination[1, ])
)

# Bind the practice and final test item discriminatory power
discrimination = rbind(SAQ_discrimination, MCQ_discrimination)

# T-test
var.test(discrimination[discrimination$test == "Practice",]$discrimination, discrimination[discrimination$test == "Final",]$discrimination)
t.test(discrimination[discrimination$test == "Practice",]$discrimination, discrimination[discrimination$test == "Final",]$discrimination, var.equal = T)
cohensD(discrimination[discrimination$test == "Practice",]$discrimination, discrimination[discrimination$test == "Final",]$discrimination)
ttestBF(discrimination[discrimination$test == "Practice",]$discrimination, discrimination[discrimination$test == "Final",]$discrimination)

# Standard deviations
sd(discrimination[discrimination$test == "Practice",]$discrimination)
sd(discrimination[discrimination$test == "Final",]$discrimination)

