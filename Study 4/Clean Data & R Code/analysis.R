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
library(tidyr)
library(emmeans)


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
  part1$part1_id,
  part2$part2_id,
  part3$part3_id
))
common_ids
part1 = part1[part1$part1_id %in% common_ids,]
part2 = part2[part2$part2_id %in% common_ids,]
part3 = part3[part3$part3_id %in% common_ids,]

# Look for potential bots
part1_bots = part1[part1$Q_RecaptchaScore <= 0.5,]$part1_id
part2_bots = part2[part2$Q_RecaptchaScore <= 0.5,]$part2_id
part3_bots = part3[part3$Q_RecaptchaScore <= 0.5,]$part3_id
all_part_bots = Reduce(intersect, list(part1_bots, part2_bots, part3_bots))
all_part_bots

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
rowMeans(SAQ_scores) # participants 35 and 101 have an average SAQ score of .15 and .14, respectively
mean(rowMeans(SAQ_scores)) 
sd(rowMeans(SAQ_scores))
min(rowMeans(SAQ_scores))
max(rowMeans(SAQ_scores))

# 2 SDs below the mean is .258, so the two participants are showing worse performance than 95% of the sample
mean_SAQ = mean(rowMeans(SAQ_scores))
sd_SAQ = sd(rowMeans(SAQ_scores))
mean(rowMeans(SAQ_scores)) - 2 * sd(rowMeans(SAQ_scores))

# Remove these two participants 
exclude_ids = part2[part2$Group == "SAQ", "part2_id"]
exclude_ids = exclude_ids[c(35,101)]
part1 = part1[!part1$part1_id %in% exclude_ids, ]
part2 = part2[!part2$part2_id %in% exclude_ids, ]
part3 = part3[!part3$part3_id %in% exclude_ids, ]


# Demographics #################################################################


# Age
mean(as.numeric(part1$age))
sd(as.numeric(part1$age))

# Gender
table(part1$gender) 


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
IDs = part2[,c("part2_id","Group")]
names(IDs) = c("part3_id", "Group")

# Add Group to the part 3 data
part3 = part3 %>%
  left_join(IDs, by = "part3_id")

# Make MCQs 0 = incorrect and 10 = correct for parity with SAQs
part3$SC0 = as.numeric(part3$SC0)
part3$SC0 = part3$SC0*10

# Calculate proportion correct
part3$prop_correct_mcq = part3$SC0/70

# # Export SAQs into a .csv file
# write.csv(part3 %>% select(starts_with("SAQ")),"~/Desktop/SAQ_participant_answers.csv", row.names = FALSE)

# Import GPT-5.4 scores for the final test SAQs
part3_SAQ_scores = read.csv("SAQ_participant_scores.csv")

# Replace SAQ answers with GPT-5.4 scores
part3[, paste0("SAQ0", 1:7)] <- part3_SAQ_scores[, paste0("SAQ0", 1:7)]

# Calculate total SAQ score for each participant
part3 <- part3 %>%
  mutate(SC1 = rowSums(select(., starts_with("SAQ")), na.rm = TRUE))

# Calculate proportion correct
part3$prop_correct_saq = part3$SC1/70

# Combine MCQ and SAQ score
part3$SC2 = part3$SC0 + part3$SC1

# Calculate proportion correct
part3$prop_correct = part3$SC2/140

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

# Export the key variables into a .csv file
# write.csv(part3[,c("part3_id", "Group", "prop_correct")],"~/Downloads/data_AMG_2.csv", row.names = FALSE)

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

# Reshape to long format
part3_long = part3 %>%
  select(part3_id, Group, prop_correct_mcq, prop_correct_saq) %>%
  pivot_longer(cols = c(prop_correct_mcq, prop_correct_saq),
               names_to = "Format",
               values_to = "prop_correct_format") %>%
  mutate(Format = dplyr::recode(Format, prop_correct_mcq = "MCQ", prop_correct_saq = "SAQ"))

# Change structure of long data
part3_long$Group = as.factor(part3_long$Group)
part3_long$Format = as.factor(part3_long$Format)
part3_long$part3_id = as.factor(part3_long$part3_id)

# ANOVA (not preregistered)
TE_aov = aov(prop_correct_format ~ Group * Format + Error(part3_id/Format), data = part3_long)
summary(TE_aov)
eta_squared(TE_aov)
emmeans(TE_aov, ~ Group | Format)
pairs(emmeans(TE_aov, ~ Group | Format))


# Retention intervals and completion times #####################################


# Change the start date variable into date format
part1$StartDate = as.POSIXct(part1$StartDate, format="%d/%m/%Y %H:%M")
part2$StartDate = as.POSIXct(part2$StartDate, format="%d/%m/%Y %H:%M")
part3$StartDate = as.POSIXct(part3$StartDate, format="%d/%m/%Y %H:%M")

# Get each participant's start date for each part
all_parts = merge(part1[, c("part1_id", "StartDate")],
                  part2[, c("part2_id", "StartDate")],
                  by.x = "part1_id", by.y = "part2_id", suffixes = c("_1", "_2"))
all_parts = merge(all_parts,
                  part3[, c("part3_id", "StartDate")],
                  by.x = "part1_id", by.y = "part3_id")
names(all_parts)[names(all_parts) == "part1_id"] = "part3_id"
names(all_parts)[names(all_parts) == "StartDate"] = "StartDate_3"

# Calculate the retention interval between parts 1 & 2 and parts 2 & 3 for each participant
all_parts$Interval_1_2 = as.numeric(difftime(all_parts$StartDate_2, all_parts$StartDate_1, units="days"))
all_parts$Interval_2_3 = as.numeric(difftime(all_parts$StartDate_3, all_parts$StartDate_2, units="days"))
intervals = all_parts[, c("part3_id", "Interval_1_2", "Interval_2_3")]

# Range of retention intervals between parts 1 & 2 and parts 2 & 3
range(intervals$Interval_1_2) * 24
range(intervals$Interval_2_3) * 24

# Get each participant's completion time from part 2
part2_completion_time = part2[, c("part2_id", "Duration..in.seconds.")]
names(part2_completion_time)[names(part2_completion_time) == "part2_id"] = "part3_id"

# Add retention intervals and part 2 completion times to the part 3 data
part3 = merge(part3, intervals, by = "part3_id", all.x = TRUE)
part3 = merge(part3, part2_completion_time, by = "part3_id", all.x = TRUE)

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
prior_knowledge = merge(part1[, c("part1_id", "prior_knowledge_1")],
                       part2[, c("part2_id", "Group")],
                       by.x = "part1_id", by.y = "part2_id", suffixes = c("_1", "_2"))
names(prior_knowledge)[names(prior_knowledge) == "part1_id"] = "part3_id"

# Add prior knowledge to the part 3 data
part3 = merge(part3, prior_knowledge[, c("part3_id", "prior_knowledge_1")], by = "part3_id", all.x = TRUE)

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
practice_SAQ_scores = part2[part2$Group == "SAQ", grepl("^Tea_F", names(part2))] |>
  lapply(function(col) {
    str_extract(col, "[0-9]+/[0-9]+$") |>
      sapply(function(x) {
        parts = strsplit(x, "/")[[1]]
        as.numeric(parts[1]) / as.numeric(parts[2])
      }) |>
      unname()
  })
practice_SAQ_scores = do.call(cbind, practice_SAQ_scores)
practice_SAQ_scores = data.frame(practice_SAQ_scores)

# Calculate item difficulty index for each SAQ and across all SAQs
practice_SAQ_difficulty = practice_SAQ_scores %>%
  summarise(across(starts_with("Tea"), ~ mean(., na.rm = TRUE)))

# Calculate discriminatory power for each SAQ and across all SAQs
practice_SAQ_discrimination = practice_SAQ_scores %>%
  summarise(
    across(
      everything(),
      ~ {
        this_item = cur_column()
        others = setdiff(names(practice_SAQ_scores), this_item)
        
        # total score excluding this SAQ
        total_excluding = rowMeans(practice_SAQ_scores[others], na.rm = TRUE)
        
        # Pearson correlation between SAQ and total score
        cor(.x, total_excluding, use = "complete.obs")
      }
    )
  )


## Final test ##################################################################
### SAQs #######################################################################


# Get SAQ data
final_SAQ_data = part3 %>%
  select(all_of(paste0("SAQ0", 1:7))) %>%
  mutate(across(everything(), ~ . / 10))

# Calculate item difficulty index for each SAQ and across all SAQs
final_SAQ_difficulty = final_SAQ_data %>%
  summarise(across(everything(), ~ mean(., na.rm = TRUE)))

# Calculate discriminatory power for each SAQ and across all SAQs
final_SAQ_discrimination = final_SAQ_data %>%
  summarise(
    across(
      everything(),
      ~ {
        this_item = cur_column()
        others = setdiff(names(final_SAQ_data), this_item)
        
        # total score excluding this SAQ
        total_excluding = rowMeans(final_SAQ_data[others], na.rm = TRUE)
        
        # Pearson correlation between SAQ and total score
        cor(.x, total_excluding, use = "complete.obs")
      }
    )
  )


### MCQs #######################################################################


# Fix MCQ naming inconsistency
names(part3)[names(part3) == "MQ03"] = "MCQ03"

# Answer key: the correct option text for each MCQ item
mcq_key = c(
  MCQ01 = "As a vegetable",
  MCQ02 = "shifted the standard from tea pressed into cakes to loose leaf tea",
  MCQ03 = "silver, opium",
  MCQ04 = "To grow tea themselves and control the market",
  MCQ05 = "second",
  MCQ06 = "Dutch traders brought tea to Europe",
  MCQ07 = "10 times more than"
)

# Recode each MCQ as 1 (correct) or 0 (incorrect) against the answer key
part3 = part3 %>%
  mutate(across(all_of(names(mcq_key)), ~ ifelse(. == mcq_key[cur_column()], 1, 0)))

# Calculate item difficulty index for each MCQ and across all MCQs
MCQ_difficulty = part3 %>%
  summarise(across(all_of(names(mcq_key)), ~ mean(., na.rm = TRUE)))

# Get MCQ data
MCQ_data = part3 %>% select(all_of(names(mcq_key)))

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
practice_difficulty = data.frame(
  test = as.factor("Practice"),
  difficulty = as.numeric(practice_SAQ_difficulty[1, ])
)

# Bind the final test SAQ item difficulties 
final_SAQ_difficulty = data.frame(
  test = factor("Final SAQ"),
  difficulty = as.numeric(final_SAQ_difficulty)
)

# Bind the final test MCQ item difficulties 
final_MCQ_difficulty = data.frame(
  test = factor("Final MCQ"),
  difficulty = as.numeric(MCQ_difficulty)
)

# Bind the practice and final test item difficulties
difficulty = rbind(
  practice_difficulty,
  final_SAQ_difficulty,
  final_MCQ_difficulty
)

# ANOVA
difficulty_aov = aov(difficulty ~ test, data = difficulty)
summary(difficulty_aov)
eta_squared(difficulty_aov)
TukeyHSD(difficulty_aov)

# Descriptives
difficulty %>%
  group_by(test) %>%
  summarise(
    mean = mean(difficulty),
    sd = sd(difficulty),
    n = n()
  )


### Discriminatory power #######################################################


# Bind the practice test item discriminatory power
practice_discrimination = data.frame(
  test = as.factor("Practice"),
  discrimination = as.numeric(practice_SAQ_discrimination[1, ])
)

# Bind the final test SAQ item discriminatory power 
final_SAQ_discrimination = data.frame(
  test = factor("Final SAQ"),
  discrimination = as.numeric(final_SAQ_discrimination)
)

# Bind the final test SAQ item discriminatory power 
final_MCQ_discrimination = data.frame(
  test = factor("Final MCQ"),
  discrimination = as.numeric(MCQ_discrimination)
)

# Bind the practice and final test item discriminatory power
discrimination = rbind(
  practice_discrimination,
  final_SAQ_discrimination,
  final_MCQ_discrimination
)

# ANOVA
discrimination_aov = aov(discrimination ~ test, data = discrimination)
summary(discrimination_aov)
eta_squared(discrimination_aov)
TukeyHSD(discrimination_aov)

# Descriptives
discrimination %>%
  group_by(test) %>%
  summarise(
    mean = mean(discrimination),
    sd = sd(discrimination),
    n = n()
  )

