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
names(part1)[names(part1) == "Prolific.ID"] = "Participant.ID"
part1[part1$Participant.ID == "5e5913ae7a584411eb9ab586@email.prolific.com",]$Participant.ID = "5e5913ae7a584411eb9ab586"
part1[part1$Participant.ID == "677723f5cb416c3806aaf069@auth.prolific.com",]$Participant.ID = "677723f5cb416c3806aaf069"
common_ids = Reduce(intersect, list(
  part1$Participant.ID,
  part2$Participant.ID,
  part3$Participant.ID
))
common_ids
part1 = part1[part1$Participant.ID %in% common_ids,]
part2 = part2[part2$Participant.ID %in% common_ids,]
part3 = part3[part3$Participant.ID %in% common_ids,]

# Look for potential bots
part1_bots = part1[part1$Q_RecaptchaScore <= 0.5,]$Participant.ID
part2_bots = part2[part2$Q_RecaptchaScore <= 0.5,]$Participant.ID
part3_bots = part3[part3$Q_RecaptchaScore <= 0.5,]$Participant.ID
all_part_bots = Reduce(intersect, list(part1_bots, part2_bots, part3_bots))
all_part_bots

# Look at data from potential bots
parts = list(part1, part2, part3)
results = lapply(parts, function(df) {
  df2 = df[df$Participant.ID %in% all_part_bots, ] 
  df2$Duration_min = as.numeric(df2$Duration..in.seconds.) / 60
  df2
}) 
results 

# Look for low effort at practice test
part2[part2$QuestionCount == 20,]$SC0
df = part2 %>%
  filter(QuestionCount == 20) %>%
  mutate(across(Q1:Q20, ~ sub("^\\(([a-d])\\).*", "\\1", .))) %>%
  select(Q1:Q20)
df
straightliners = apply(df, 1, function(row) length(unique(row)) == 1)
df[straightliners, ]

# Look for low effort at final test
part3$SC0
df = part3 %>%
  mutate(across(Q1.:Q20, ~ sub("^\\(([a-d])\\).*", "\\1", .))) %>%
  select(Q1.:Q20)
df
straightliners = apply(df, 1, function(row) length(unique(row)) == 1)
df[straightliners, ]


# Demographics ##############################################################


# Age
mean(as.numeric(part1$Age))
sd(as.numeric(part1$Age))

# Gender
table(part1$Gender) 


# Completion times #############################################################


# Part one
median(as.numeric(part1$Duration..in.seconds.)/60)

# Part two
median(as.numeric(part2$Duration..in.seconds.)/60)
median(as.numeric(part2[part2$QuestionCount == 20,]$Duration..in.seconds.)/60)
median(as.numeric(part2[part2$QuestionCount == 0,]$Duration..in.seconds.)/60)

# Part three
median(as.numeric(part3$Duration..in.seconds.)/60)


# Testing effect ###############################################################


# Sample sizes
nrow(part1)
nrow(part2)
nrow(part3)

# Get PROLIFIC_PID and Group from part 2 
part2$Group = "Restudy"
part2[part2$QuestionCount == 20,]$Group = "Retrieval"
IDs = part2[,c("Participant.ID","Group")]

# Add Group to the part 3 data
part3 = part3 %>%
  left_join(IDs, by = "Participant.ID")

# Calculate the proportion correct in the final test
part3$SC0 = as.numeric(part3$SC0)
part3$prop_correct = part3$SC0/20

# Export the key variables into a .csv file
# write.csv(part3[,c("Participant.ID", "Group", "prop_correct")],"~/Downloads/data_JZ.csv", row.names = FALSE)

# T-test
var.test(part3[part3$Group == "Retrieval",]$prop_correct, part3[part3$Group == "Restudy",]$prop_correct)
t.test(part3[part3$Group == "Retrieval",]$prop_correct, part3[part3$Group == "Restudy",]$prop_correct, alternative = "greater", var.equal = T)
cohensD(part3[part3$Group == "Retrieval",]$prop_correct, part3[part3$Group == "Restudy",]$prop_correct)
ttestBF(part3[part3$Group == "Retrieval",]$prop_correct, part3[part3$Group == "Restudy",]$prop_correct, nullInterval = c(0, Inf) )

# Standard deviations
sd(part3[part3$Group == "Retrieval",]$prop_correct)
sd(part3[part3$Group == "Restudy",]$prop_correct)

# Group sizes
nrow(part3[part3$Group == "Retrieval",])
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


# Retention intervals and completion times #####################################


# Change the start date variable into date format
part1$StartDate = as.POSIXct(part1$StartDate, format="%d/%m/%Y %H:%M")
part2$StartDate = as.POSIXct(part2$StartDate, format="%d/%m/%Y %H:%M")
part3$StartDate = as.POSIXct(part3$StartDate, format = "%Y-%m-%d %H:%M:%S")

# Get each participant's start date for each part
all_parts = merge(part1[, c("Participant.ID", "StartDate")],
                  part2[, c("Participant.ID", "StartDate")],
                  by = "Participant.ID", suffixes = c("_1", "_2"))
all_parts = merge(all_parts,
                  part3[, c("Participant.ID", "StartDate")],
                  by = "Participant.ID")
names(all_parts)[names(all_parts) == "StartDate"] = "StartDate_3"

# Calculate the retention interval between parts 1 & 2 and parts 2 & 3 for each participant
all_parts$Interval_1_2 = as.numeric(difftime(all_parts$StartDate_2, all_parts$StartDate_1, units="days"))
all_parts$Interval_2_3 = as.numeric(difftime(all_parts$StartDate_3, all_parts$StartDate_2, units="days"))
intervals = all_parts[, c("Participant.ID", "Interval_1_2", "Interval_2_3")]

# Range of retention intervals between parts 1 & 2 and parts 2 & 3
range(intervals$Interval_1_2) * 24
range(intervals$Interval_2_3) * 24

# Get each participant's completion time from part 2
part2_completion_time = part2[, c("Participant.ID", "Duration..in.seconds.")]

# Add retention intervals and part 2 completion times to the part 3 data
part3 = merge(part3, intervals, by = "Participant.ID", all.x = TRUE)
part3 = merge(part3, part2_completion_time, by = "Participant.ID", all.x = TRUE)

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
prior_knowledge = merge(part1[, c("Participant.ID", "Prior.knowledge_1")],
                        part2[, c("Participant.ID", "Group")],
                        by = "Participant.ID", suffixes = c("_1", "_2"))

# Add prior knowledge to the part 3 data
part3 = merge(part3, prior_knowledge[, c("Participant.ID", "Prior.knowledge_1")], by = "Participant.ID", all.x = TRUE)

# Change the structure of the part 3 data
str(part3)
part3$Prior.knowledge_1 = as.numeric(part3$Prior.knowledge_1)

# Compare prior knowledge between the two groups
var.test(part3[part3$Group == "Retrieval",]$Prior.knowledge_1, part3[part3$Group == "Restudy",]$Prior.knowledge_1)
t.test(part3[part3$Group == "Retrieval",]$Prior.knowledge_1, part3[part3$Group == "Restudy",]$Prior.knowledge_1)
cohensD(part3[part3$Group == "Retrieval",]$Prior.knowledge_1, part3[part3$Group == "Restudy",]$Prior.knowledge_1)
ttestBF(part3[part3$Group == "Retrieval",]$Prior.knowledge_1, part3[part3$Group == "Restudy",]$Prior.knowledge_1)

# Standard deviations
sd(part3[part3$Group == "Retrieval",]$Prior.knowledge_1)
sd(part3[part3$Group == "Restudy",]$Prior.knowledge_1)

# ANCOVA
summary(aov(prop_correct ~ Group + Prior.knowledge_1, data = part3))
eta_squared(aov(prop_correct ~ Group + Prior.knowledge_1, data = part3))


# Item indices #################################################################
## Practice test ###############################################################


# Manually core each MCQ
part2$Q1 = ifelse(grepl("^\\(b\\)", part2$Q1), 1, 0)
part2$Q2 = ifelse(grepl("^\\(a\\)", part2$Q2), 1, 0)
part2$Q3 = ifelse(grepl("^\\(b\\)", part2$Q3), 1, 0)
part2$Q4 = ifelse(grepl("^\\(b\\)", part2$Q4), 1, 0)
part2$Q5 = ifelse(grepl("^\\(b\\)", part2$Q5), 1, 0)
part2$Q6 = ifelse(grepl("^\\(b\\)", part2$Q6), 1, 0)
part2$Q7 = ifelse(grepl("^\\(b\\)", part2$Q7), 1, 0)
part2$Q8 = ifelse(grepl("^\\(a\\)", part2$Q8), 1, 0)
part2$Q9 = ifelse(grepl("^\\(a\\)", part2$Q9), 1, 0)
part2$Q10 = ifelse(grepl("^\\(b\\)", part2$Q10), 1, 0)
part2$Q11 = ifelse(grepl("^\\(b\\)", part2$Q11), 1, 0)
part2$Q12 = ifelse(grepl("^\\(c\\)", part2$Q12), 1, 0)
part2$Q13 = ifelse(grepl("^\\(a\\)", part2$Q13), 1, 0)
part2$Q14 = ifelse(grepl("^\\(c\\)", part2$Q14), 1, 0)
part2$Q15 = ifelse(grepl("^\\(a\\)", part2$Q15), 1, 0)
part2$Q16 = ifelse(grepl("^\\(b\\)", part2$Q16), 1, 0)
part2$Q17 = ifelse(grepl("^\\(a\\)", part2$Q17), 1, 0)
part2$Q18 = ifelse(grepl("^\\(c\\)", part2$Q18), 1, 0)
part2$Q19 = ifelse(grepl("^\\(a\\)", part2$Q19), 1, 0)
part2$Q20 = ifelse(grepl("^\\(b\\)", part2$Q20), 1, 0)

# Calculate item difficulty index for each MCQ and across all MCQs
pt_difficulty = part2 %>%
  filter(Group == "Retrieval") %>%
  summarise(across(matches("^Q[0-9]+$"), ~ mean(., na.rm = TRUE)))

# Get MCQ data
MCQ_data = part2 %>% 
  filter(Group == "Retrieval") %>%
  select(matches("^Q[0-9]+$"))

# Calculate discriminatory power for each MCQ and across all MCQs
pt_discrimination = MCQ_data %>%
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


## Final test ########################################################################


# Manually core each MCQ
part3$Q1. = ifelse(grepl("^\\(a\\)", part3$Q1.), 1, 0)
part3$Q2 = ifelse(grepl("^\\(b\\)", part3$Q2), 1, 0)
part3$Q3 = ifelse(grepl("^\\(a\\)", part3$Q3), 1, 0)
part3$Q4 = ifelse(grepl("^\\(d\\)", part3$Q4), 1, 0)
part3$Q5. = ifelse(grepl("^\\(d\\)", part3$Q5.), 1, 0)
part3$Q6. = ifelse(grepl("^\\(b\\)", part3$Q6.), 1, 0)
part3$Q7 = ifelse(grepl("^\\(d\\)", part3$Q7), 1, 0)
part3$Q8 = ifelse(grepl("^\\(a\\)", part3$Q8), 1, 0)
part3$Q9 = ifelse(grepl("^\\(c\\)", part3$Q9), 1, 0)
part3$Q10 = ifelse(grepl("^\\(c\\)", part3$Q10), 1, 0)
part3$Q11 = ifelse(grepl("^\\(a\\)", part3$Q11), 1, 0)
part3$Q12 = ifelse(grepl("^\\(b\\)", part3$Q12), 1, 0)
part3$Q13. = ifelse(grepl("^\\(a\\)", part3$Q13.), 1, 0)
part3$Q14. = ifelse(grepl("^\\(b\\)", part3$Q14.), 1, 0)
part3$Q15 = ifelse(grepl("^\\(b\\)", part3$Q15), 1, 0)
part3$Q16 = ifelse(grepl("^\\(d\\)", part3$Q16), 1, 0)
part3$Q17. = ifelse(grepl("^\\(c\\)", part3$Q17.), 1, 0)
part3$Q18. = ifelse(grepl("^\\(b\\)", part3$Q18.), 1, 0)
part3$Q19. = ifelse(grepl("^\\(d\\)", part3$Q19.), 1, 0)
part3$Q20 = ifelse(grepl("^\\(c\\)", part3$Q20), 1, 0)

# Calculate item difficulty index for each MCQ and across all MCQs
ft_difficulty = part3 %>%
  summarise(across(matches("^Q[0-9]+\\.?$"), ~ mean(., na.rm = TRUE)))

# Get MCQ data
MCQ_data = part3 %>% select(matches("^Q[0-9]+\\.?$"))

# Calculate discriminatory power for each MCQ and across all MCQs
ft_discrimination = MCQ_data %>%
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
pt_difficulty = data.frame(
  test = as.factor("Practice"),
  difficulty = as.numeric(pt_difficulty[1, ])
)

# Bind the final test item difficulties 
ft_difficulty = data.frame(
  test = as.factor("Final"),
  difficulty = as.numeric(ft_difficulty[1, ])
)

# Bind the practice and final test item difficulties
difficulty = rbind(pt_difficulty, ft_difficulty)

# T-test
var.test(difficulty[difficulty$test == "Practice",]$difficulty, difficulty[difficulty$test == "Final",]$difficulty)
t.test(difficulty[difficulty$test == "Practice",]$difficulty, difficulty[difficulty$test == "Final",]$difficulty)
cohensD(difficulty[difficulty$test == "Practice",]$difficulty, difficulty[difficulty$test == "Final",]$difficulty)
ttestBF(difficulty[difficulty$test == "Practice",]$difficulty, difficulty[difficulty$test == "Final",]$difficulty)

# Standard deviations
sd(difficulty[difficulty$test == "Practice",]$difficulty)
sd(difficulty[difficulty$test == "Final",]$difficulty)


### Discriminatory power #######################################################


# Bind the practice test item discriminatory power
pt_discrimination = data.frame(
  test = as.factor("Practice"),
  discrimination = as.numeric(pt_discrimination[1, ])
)

# Bind the final test item discriminatory power 
ft_discrimination = data.frame(
  test = as.factor("Final"),
  discrimination = as.numeric(ft_discrimination[1, ])
)

# Bind the practice and final test item discriminatory power
discrimination = rbind(pt_discrimination, ft_discrimination)

# T-test
var.test(discrimination[discrimination$test == "Practice",]$discrimination, discrimination[discrimination$test == "Final",]$discrimination)
t.test(discrimination[discrimination$test == "Practice",]$discrimination, discrimination[discrimination$test == "Final",]$discrimination, var.equal = T)
cohensD(discrimination[discrimination$test == "Practice",]$discrimination, discrimination[discrimination$test == "Final",]$discrimination)
ttestBF(discrimination[discrimination$test == "Practice",]$discrimination, discrimination[discrimination$test == "Final",]$discrimination)

# Standard deviations
sd(discrimination[discrimination$test == "Practice",]$discrimination)
sd(discrimination[discrimination$test == "Final",]$discrimination)

