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
part2_psychedelics = read.csv("Part 2 (Psychedelics).csv")
part2_stemcells = read.csv("Part 2 (Stem cells).csv")

# Remove header rows
part1 = part1[c(-1,-2),] 
part2_psychedelics = part2_psychedelics[c(-1,-2),] 
part2_stemcells = part2_stemcells[c(-1,-2),] 

# Only keep participants that completed both parts
common_ids = intersect(
  part1$PROLIFIC_PID,
  union(part2_psychedelics$PROLIFIC_PID, part2_stemcells$PROLIFIC_PID)
)
common_ids
part1 = part1[part1$PROLIFIC_PID %in% common_ids,]
part2_psychedelics = part2_psychedelics[part2_psychedelics$PROLIFIC_PID %in% common_ids,]
part2_stemcells = part2_stemcells[part2_stemcells$PROLIFIC_PID %in% common_ids,]

# Look for potential bots
part1_bots = part1[part1$Q_RecaptchaScore <= 0.5,]$PROLIFIC_PID
part2_psychedelics_bots = part2_psychedelics[part2_psychedelics$Q_RecaptchaScore <= 0.5,]$PROLIFIC_PID
part2_stemcells_bots = part2_stemcells[part2_stemcells$Q_RecaptchaScore <= 0.5,]$PROLIFIC_PID
all_part_bots = part1_bots[part1_bots %in% union(part2_psychedelics_bots, part2_stemcells_bots)]
all_part_bots

# Look at data from potential bots
parts = list(part1, part2_psychedelics, part2_stemcells)
results = lapply(parts, function(df) {
  df2 = df[df$PROLIFIC_PID %in% all_part_bots, ] 
  df2$Duration_min = as.numeric(df2$Duration..in.seconds.) / 60
  df2
}) 
results

# Exclude likely bot
exclude_ids = c("65bbf83dbe0f27e1a871bf48") # low recaptcha score across both parts and completed the final test in < 2 mins
part1 = part1[!part1$PROLIFIC_PID %in% exclude_ids, ]
part2_psychedelics = part2_psychedelics[!part2_psychedelics$PROLIFIC_PID %in% exclude_ids, ]
part2_stemcells = part2_stemcells[!part2_stemcells$PROLIFIC_PID %in% exclude_ids, ]

# Look for low effort at practice test
part1[part1$QuestionCount == 20,]$SC0
df = part1 %>%
  filter(QuestionCount == 20) %>%
  mutate(across(Q1.:Q20, ~ sub("^\\(([a-d])\\).*", "\\1", .))) %>%
  select(Q1.:Q20)
straightliners = apply(df, 1, function(row) length(unique(row)) == 1)
df[straightliners, ]

# Look for low effort at final test
part2_psychedelics$SC0
df = part2_psychedelics %>%
  mutate(across(Q1:Q20, ~ sub("^\\(([a-d])\\).*", "\\1", .))) %>%
  select(Q1:Q20)
straightliners = apply(df, 1, function(row) length(unique(row)) == 1)
df[straightliners, ]

part2_stemcells$SC0
df = part2_stemcells %>%
  mutate(across(Q1.:Q20, ~ sub("^\\(([a-d])\\).*", "\\1", .))) %>%
  select(Q1.:Q20)
straightliners = apply(df, 1, function(row) length(unique(row)) == 1)
df[straightliners, ]


# Demographics #################################################################


# Age
mean(as.numeric(part1$Age))
sd(as.numeric(part1$Age))

# Gender
table(part1$Gender)


# Completion times #############################################################


# Part one
median(as.numeric(part1$Duration..in.seconds.)/60)
median(as.numeric(part1[part1$QuestionCount == 20,]$Duration..in.seconds.)/60)
median(as.numeric(part1[part1$QuestionCount == 0,]$Duration..in.seconds.)/60)

# Part two
median(c(as.numeric(part2_psychedelics$Duration..in.seconds.)/60,
     as.numeric(part2_stemcells$Duration..in.seconds.)/60))


# Testing effect ###############################################################


# Sample sizes
nrow(part1)
nrow(part2_psychedelics)
nrow(part2_stemcells)

# Add Group to the part 1 data
part1 = part1 %>%
  mutate(Group = case_when(
    startsWith(FL_14_DO, "E") ~ "elaboratedexplanation",
    startsWith(FL_14_DO, "R") ~ "retrievalpractice"
  ))

# Get PROLIFIC_PID and Group from part 1
IDs = part1[,c("PROLIFIC_PID", "Group")]

# Add Group to the part 2 data
part2_psychedelics = part2_psychedelics %>%
  left_join(IDs, by = "PROLIFIC_PID")
part2_stemcells = part2_stemcells %>%
  left_join(IDs, by = "PROLIFIC_PID")

# Add Video to the part 2 data
part2_psychedelics$Video = "psychedelics"
part2_stemcells$Video = "stemcells"

# Combine the two part 2 data sets
part2_psychedelics_selected = part2_psychedelics[, c("SC0", "PROLIFIC_PID", "Group", "Video")]
part2_stemcells_selected = part2_stemcells[, c("SC0", "PROLIFIC_PID", "Group", "Video")]
part2 = rbind(part2_psychedelics_selected, part2_stemcells_selected)

# Change the structure of the data frame
str(part2)
part2$PROLIFIC_PID = as.factor(part2$PROLIFIC_PID)
part2$Group = as.factor(part2$Group)
part2$Video = as.factor(part2$Video)
part2$SC0 = as.numeric(part2$SC0)

# Add proportion correct to the part 2 data
part2$prop_correct = part2$SC0/20

# Export the key variables into a .csv file
# export_data = part2[, c("PROLIFIC_PID", "Group", "prop_correct")]
# practice = part1[part1$QuestionCount == 20, ]
# export_data$practice_prop = as.numeric(practice$SC0[match(export_data$PROLIFIC_PID, practice$PROLIFIC_PID)]) / 20
# write.csv(export_data, "data_JY.csv", row.names = FALSE)

# ANOVA
leveneTest(prop_correct ~ Group * Video, data = part2)
summary(aov(prop_correct ~ Group * Video, data = part2))
eta_squared(aov(prop_correct ~ Group * Video, data = part2))

# Main effect of Group
part2 %>%
  group_by(Group) %>%
  summarise(
    n = n(),
    mean = mean(prop_correct, na.rm = TRUE),
    sd = sd(prop_correct, na.rm = TRUE)
  )

# Violin plot
ggplot(part2, aes(Group, prop_correct, fill = Group)) +
  geom_violin(alpha = 0.8) +
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.4, fill = "white") +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.1, size = 0.75, color = "black") +
  stat_summary(fun = mean, geom = "point", size = 4, fill = "black") +
  theme_classic() +
  guides(fill = "none") +
  labs(y= "Proportion Correct", x = "Group") +
  scale_x_discrete(labels= c("Elaborated Explanation", "Retrieval Practice")) +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 16)) +
  ylim(0,1)


# Retention intervals and completion times #####################################


# Add start date to the part 2 data
part2$StartDate = part2_psychedelics$StartDate[match(part2$PROLIFIC_PID, part2_psychedelics$PROLIFIC_PID)]
missing_idx = is.na(part2$StartDate)
part2$StartDate[missing_idx] = part2_stemcells$StartDate[match(part2$PROLIFIC_PID[missing_idx], part2_stemcells$PROLIFIC_PID)]

# Change the start date variable into date format
part1$StartDate = as.POSIXct(part1$StartDate, format="%d/%m/%Y %H:%M")
part2$StartDate = as.POSIXct(part2$StartDate, format="%d/%m/%Y %H:%M")

# Get each participant's start date for each part
all_parts = merge(part1[, c("PROLIFIC_PID", "StartDate")],
                  part2[, c("PROLIFIC_PID", "StartDate")],
                  by = "PROLIFIC_PID", suffixes = c("_1", "_2"))

# Calculate the retention interval between parts 1 & 2 for each participant
all_parts$Interval_1_2 = as.numeric(difftime(all_parts$StartDate_2, all_parts$StartDate_1, units="days"))
intervals = all_parts[, c("PROLIFIC_PID", "Interval_1_2")]

# Range of retention intervals between parts 1 & 2
range(intervals$Interval_1_2) * 24

# Get each participant's completion time from part 1
part1_completion_time = part1[, c("PROLIFIC_PID", "Duration..in.seconds.")]

# Add the retention interval and part 1 completion time to the part 2 data
part2 = merge(part2, intervals, by = "PROLIFIC_PID", all.x = TRUE)
part2 = merge(part2, part1_completion_time, by = "PROLIFIC_PID", all.x = TRUE)

# Change the structure of the part 2 data
str(part2)
part2$Duration..in.seconds. = as.numeric(part2$Duration..in.seconds.)

# Range of completion times from part 1
range(part2$Duration..in.seconds.) / 60

# ANCOVA
summary(aov(prop_correct ~ Group + Interval_1_2 + Duration..in.seconds., data = part2))
eta_squared(aov(prop_correct ~ Group + Interval_1_2 + Duration..in.seconds., data = part2))


# Prior knowledge ##############################################################


# Create one column for prior knowledge
part1$Prior.knowledge_1[part1$Prior.knowledge_1 == ""] = NA
part1$Prior.knowledge_1.1[part1$Prior.knowledge_1.1 == ""] = NA
part1$Prior.knowledge_1.2[part1$Prior.knowledge_1.2 == ""] = NA
part1$Prior.knowledge_1.3[part1$Prior.knowledge_1.3 == ""] = NA
part1$prior_knowledge = coalesce(part1$Prior.knowledge_1, 
                                 part1$Prior.knowledge_1.1, 
                                 part1$Prior.knowledge_1.2, 
                                 part1$Prior.knowledge_1.3)

# Get each participant's prior knowledge 
prior_knowledge = merge(part1[, c("PROLIFIC_PID", "prior_knowledge")],
                        part2[, c("PROLIFIC_PID", "Group")],
                        by = "PROLIFIC_PID")

# Add prior knowledge to the part 2 data
part2 = merge(part2, prior_knowledge[, c("PROLIFIC_PID", "prior_knowledge")], by = "PROLIFIC_PID", all.x = TRUE)

# Change the structure of the part 2 data
str(part2)
part2$prior_knowledge = as.numeric(part2$prior_knowledge)

# Compare prior knowledge between the two groups
var.test(part2[part2$Group == "retrievalpractice",]$prior_knowledge, part2[part2$Group == "elaboratedexplanation",]$prior_knowledge)
t.test(part2[part2$Group == "retrievalpractice",]$prior_knowledge, part2[part2$Group == "elaboratedexplanation",]$prior_knowledge, var.equal = T)
cohensD(part2[part2$Group == "retrievalpractice",]$prior_knowledge, part2[part2$Group == "elaboratedexplanation",]$prior_knowledge)
ttestBF(part2[part2$Group == "retrievalpractice",]$prior_knowledge, part2[part2$Group == "elaboratedexplanation",]$prior_knowledge)

# Standard deviations
sd(part2[part2$Group == "retrievalpractice",]$prior_knowledge)
sd(part2[part2$Group == "elaboratedexplanation",]$prior_knowledge)


# Judgment of learning #########################################################


# Create one column for JOL
part1$JOL_1[part1$JOL_1 == ""] = NA
part1$JOL._1[part1$JOL._1 == ""] = NA
part1$JOL_1.1[part1$JOL_1.1 == ""] = NA
part1$JOL_1.2[part1$JOL_1.2 == ""] = NA
part1$JOL = coalesce(part1$JOL_1, 
                     part1$JOL._1, 
                     part1$JOL_1.1, 
                     part1$JOL_1.2)

# Get each participant's JOL
JOL = merge(part1[, c("PROLIFIC_PID", "JOL")],
                        part2[, c("PROLIFIC_PID", "Group")],
                        by = "PROLIFIC_PID")

# Add JOL to the part 2 data
part2 = merge(part2, JOL[, c("PROLIFIC_PID", "JOL")], by = "PROLIFIC_PID", all.x = TRUE)

# Change the structure of the part 2 data
str(part2)
part2$JOL = as.numeric(part2$JOL)

# Compare prior knowledge between the two groups
var.test(part2[part2$Group == "retrievalpractice",]$JOL, part2[part2$Group == "elaboratedexplanation",]$JOL)
t.test(part2[part2$Group == "retrievalpractice",]$JOL, part2[part2$Group == "elaboratedexplanation",]$JOL, var.equal = T)
cohensD(part2[part2$Group == "retrievalpractice",]$JOL, part2[part2$Group == "elaboratedexplanation",]$JOL)
ttestBF(part2[part2$Group == "retrievalpractice",]$JOL, part2[part2$Group == "elaboratedexplanation",]$JOL)

# Standard deviations
sd(part2[part2$Group == "retrievalpractice",]$JOL)
sd(part2[part2$Group == "elaboratedexplanation",]$JOL)


# Item indices #################################################################
## Practice test ###############################################################
### Stem cells #################################################################


# Manually core each MCQ
part1$Q1. = ifelse(grepl("^\\(b\\)", part1$Q1.), 1, 0)
part1$Q2 = ifelse(grepl("^\\(a\\)", part1$Q2), 1, 0)
part1$Q3 = ifelse(grepl("^\\(b\\)", part1$Q3), 1, 0)
part1$Q4 = ifelse(grepl("^\\(b\\)", part1$Q4), 1, 0)
part1$Q5 = ifelse(grepl("^\\(b\\)", part1$Q5), 1, 0)
part1$Q6 = ifelse(grepl("^\\(b\\)", part1$Q6), 1, 0)
part1$Q7 = ifelse(grepl("^\\(b\\)", part1$Q7), 1, 0)
part1$Q8 = ifelse(grepl("^\\(a\\)", part1$Q8), 1, 0)
part1$Q9 = ifelse(grepl("^\\(a\\)", part1$Q9), 1, 0)
part1$Q10 = ifelse(grepl("^\\(b\\)", part1$Q10), 1, 0)
part1$Q11 = ifelse(grepl("^\\(b\\)", part1$Q11), 1, 0)
part1$Q12 = ifelse(grepl("^\\(c\\)", part1$Q12), 1, 0)
part1$Q13 = ifelse(grepl("^\\(a\\)", part1$Q13), 1, 0)
part1$Q14 = ifelse(grepl("^\\(c\\)", part1$Q14), 1, 0)
part1$Q15 = ifelse(grepl("^\\(a\\)", part1$Q15), 1, 0)
part1$Q16 = ifelse(grepl("^\\(b\\)", part1$Q16), 1, 0)
part1$Q17 = ifelse(grepl("^\\(a\\)", part1$Q17), 1, 0)
part1$Q18 = ifelse(grepl("^\\(c\\)", part1$Q18), 1, 0)
part1$Q19 = ifelse(grepl("^\\(a\\)", part1$Q19), 1, 0)
part1$Q20 = ifelse(grepl("^\\(b\\)", part1$Q20), 1, 0)

# Calculate item difficulty index for each MCQ and across all MCQs
sc_pt_difficulty = part1 %>%
  filter(FL_14_DO == "RetrievalPractice,StemCells") %>%
  summarise(across(matches("^Q1\\.$|^Q[2-9]$|^Q1[0-9]$|^Q20$"), ~ mean(., na.rm = TRUE)))

# Get MCQ data
MCQ_data = part1 %>% 
  filter(FL_14_DO == "RetrievalPractice,StemCells") %>%
  select(matches("^Q1\\.$|^Q[2-9]$|^Q1[0-9]$|^Q20$"))

# Calculate discriminatory power for each MCQ and across all MCQs
sc_pt_discrimination = MCQ_data %>%
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


### Psychedelics ###############################################################


# Manually core each MCQ
part1$Q1 = ifelse(grepl("^\\(a\\)", part1$Q1), 1, 0)
part1$Q2.1 = ifelse(grepl("^\\(d\\)", part1$Q2.1), 1, 0)
part1$Q3.1 = ifelse(grepl("^\\(c\\)", part1$Q3.1), 1, 0)
part1$Q4.1 = ifelse(grepl("^\\(b\\)", part1$Q4.1), 1, 0)
part1$Q5.1 = ifelse(grepl("^\\(d\\)", part1$Q5.1), 1, 0)
part1$Q6.1 = ifelse(grepl("^\\(c\\)", part1$Q6.1), 1, 0)
part1$Q7.1 = ifelse(grepl("^\\(a\\)", part1$Q7.1), 1, 0)
part1$Q8.1 = ifelse(grepl("^\\(b\\)", part1$Q8.1), 1, 0)
part1$Q9.1 = ifelse(grepl("^\\(b\\)", part1$Q9.1), 1, 0)
part1$Q10.1 = ifelse(grepl("^\\(c\\)", part1$Q10.1), 1, 0)
part1$Q11.1 = ifelse(grepl("^\\(a\\)", part1$Q11.1), 1, 0)
part1$Q12.1 = ifelse(grepl("^\\(b\\)", part1$Q12.1), 1, 0)
part1$Q13.1 = ifelse(grepl("^\\(d\\)", part1$Q13.1), 1, 0)
part1$Q14.1 = ifelse(grepl("^\\(d\\)", part1$Q14.1), 1, 0)
part1$Q15.1 = ifelse(grepl("^\\(a\\)", part1$Q15.1), 1, 0)
part1$Q16.1 = ifelse(grepl("^\\(c\\)", part1$Q16.1), 1, 0)
part1$Q17.1 = ifelse(grepl("^\\(a\\)", part1$Q17.1), 1, 0)
part1$Q18.1 = ifelse(grepl("^\\(b\\)", part1$Q18.1), 1, 0)
part1$Q19.1 = ifelse(grepl("^\\(c\\)", part1$Q19.1), 1, 0)
part1$Q20.1 = ifelse(grepl("^\\(a\\)", part1$Q20.1), 1, 0)

# Calculate item difficulty index for each MCQ and across all MCQs
p_pt_difficulty = part1 %>%
  filter(FL_14_DO == "RetrievalPractice,Psychedelics") %>%
  summarise(across(matches("^Q1$|^Q[0-9]+\\.1$"), ~ mean(., na.rm = TRUE)))

# Get MCQ data
MCQ_data = part1 %>% 
  filter(FL_14_DO == "RetrievalPractice,Psychedelics") %>%
  select(matches("^Q1$|^Q[0-9]+\\.1$"))

# Calculate discriminatory power for each MCQ and across all MCQs
p_pt_discrimination = MCQ_data %>%
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


## Final test ##################################################################
### Stem cells #################################################################


# Manually core each MCQ
part2_stemcells$Q1. = ifelse(grepl("^\\(a\\)", part2_stemcells$Q1.), 1, 0)
part2_stemcells$Q2 = ifelse(grepl("^\\(b\\)", part2_stemcells$Q2), 1, 0)
part2_stemcells$Q3 = ifelse(grepl("^\\(a\\)", part2_stemcells$Q3), 1, 0)
part2_stemcells$Q4 = ifelse(grepl("^\\(d\\)", part2_stemcells$Q4), 1, 0)
part2_stemcells$Q5 = ifelse(grepl("^\\(d\\)", part2_stemcells$Q5), 1, 0)
part2_stemcells$Q6 = ifelse(grepl("^\\(b\\)", part2_stemcells$Q6), 1, 0)
part2_stemcells$Q7 = ifelse(grepl("^\\(d\\)", part2_stemcells$Q7), 1, 0)
part2_stemcells$Q8 = ifelse(grepl("^\\(a\\)", part2_stemcells$Q8), 1, 0)
part2_stemcells$Q9 = ifelse(grepl("^\\(c\\)", part2_stemcells$Q9), 1, 0)
part2_stemcells$Q10 = ifelse(grepl("^\\(c\\)", part2_stemcells$Q10), 1, 0)
part2_stemcells$Q11 = ifelse(grepl("^\\(a\\)", part2_stemcells$Q11), 1, 0)
part2_stemcells$Q12 = ifelse(grepl("^\\(b\\)", part2_stemcells$Q12), 1, 0)
part2_stemcells$Q13 = ifelse(grepl("^\\(a\\)", part2_stemcells$Q13), 1, 0)
part2_stemcells$Q14 = ifelse(grepl("^\\(b\\)", part2_stemcells$Q14), 1, 0)
part2_stemcells$Q15 = ifelse(grepl("^\\(b\\)", part2_stemcells$Q15), 1, 0)
part2_stemcells$Q16 = ifelse(grepl("^\\(d\\)", part2_stemcells$Q16), 1, 0)
part2_stemcells$Q17 = ifelse(grepl("^\\(c\\)", part2_stemcells$Q17), 1, 0)
part2_stemcells$Q18 = ifelse(grepl("^\\(b\\)", part2_stemcells$Q18), 1, 0)
part2_stemcells$Q19 = ifelse(grepl("^\\(d\\)", part2_stemcells$Q19), 1, 0)
part2_stemcells$Q20 = ifelse(grepl("^\\(c\\)", part2_stemcells$Q20), 1, 0)

# Calculate item difficulty index for each MCQ and across all MCQs
sc_ft_difficulty = part2_stemcells %>%
  summarise(across(matches("^Q[0-9]+\\.?$"), ~ mean(., na.rm = TRUE)))

# Get MCQ data
MCQ_data = part2_stemcells %>% select(matches("^Q[0-9]+\\.?$"))

# Calculate discriminatory power for each MCQ and across all MCQs
sc_ft_discrimination = MCQ_data %>%
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


### Psychedelics ###############################################################


# Manually core each MCQ
part2_psychedelics$Q1 = ifelse(grepl("^\\(c\\)", part2_psychedelics$Q1), 1, 0)
part2_psychedelics$Q2 = ifelse(grepl("^\\(d\\)", part2_psychedelics$Q2), 1, 0)
part2_psychedelics$Q3 = ifelse(grepl("^\\(c\\)", part2_psychedelics$Q3), 1, 0)
part2_psychedelics$Q4 = ifelse(grepl("^\\(c\\)", part2_psychedelics$Q4), 1, 0)
part2_psychedelics$Q5 = ifelse(grepl("^\\(d\\)", part2_psychedelics$Q5), 1, 0)
part2_psychedelics$Q6 = ifelse(grepl("^\\(d\\)", part2_psychedelics$Q6), 1, 0)
part2_psychedelics$Q7 = ifelse(grepl("^\\(b\\)", part2_psychedelics$Q7), 1, 0)
part2_psychedelics$Q8 = ifelse(grepl("^\\(c\\)", part2_psychedelics$Q8), 1, 0)
part2_psychedelics$Q9 = ifelse(grepl("^\\(b\\)", part2_psychedelics$Q9), 1, 0)
part2_psychedelics$Q10 = ifelse(grepl("^\\(a\\)", part2_psychedelics$Q10), 1, 0)
part2_psychedelics$Q11 = ifelse(grepl("^\\(c\\)", part2_psychedelics$Q11), 1, 0)
part2_psychedelics$Q12 = ifelse(grepl("^\\(d\\)", part2_psychedelics$Q12), 1, 0)
part2_psychedelics$Q13 = ifelse(grepl("^\\(c\\)", part2_psychedelics$Q13), 1, 0)
part2_psychedelics$Q14 = ifelse(grepl("^\\(b\\)", part2_psychedelics$Q14), 1, 0)
part2_psychedelics$Q15 = ifelse(grepl("^\\(c\\)", part2_psychedelics$Q15), 1, 0)
part2_psychedelics$Q16 = ifelse(grepl("^\\(c\\)", part2_psychedelics$Q16), 1, 0)
part2_psychedelics$Q17 = ifelse(grepl("^\\(c\\)", part2_psychedelics$Q17), 1, 0)
part2_psychedelics$Q18 = ifelse(grepl("^\\(b\\)", part2_psychedelics$Q18), 1, 0)
part2_psychedelics$Q19 = ifelse(grepl("^\\(b\\)", part2_psychedelics$Q19), 1, 0)
part2_psychedelics$Q20 = ifelse(grepl("^\\(a\\)", part2_psychedelics$Q20), 1, 0)

# Calculate item difficulty index for each MCQ and across all MCQs
p_ft_difficulty = part2_psychedelics %>%
  summarise(across(matches("^Q[0-9]+\\.?$"), ~ mean(., na.rm = TRUE)))

# Get MCQ data
MCQ_data = part2_psychedelics %>% select(matches("^Q[0-9]+\\.?$"))

# Calculate discriminatory power for each MCQ and across all MCQs
p_ft_discrimination = MCQ_data %>%
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
sc_pt_difficulty = data.frame(
  type = as.factor("StemCells"),
  test = as.factor("Practice"),
  difficulty = as.numeric(sc_pt_difficulty[1, ])
)
p_pt_difficulty = data.frame(
  type = as.factor("Psychedelics"),
  test = as.factor("Practice"),
  difficulty = as.numeric(p_pt_difficulty[1, ])
)
pt_difficulty = rbind(sc_pt_difficulty, p_pt_difficulty)

# Bind the final test item difficulties 
sc_ft_difficulty = data.frame(
  type = as.factor("StemCells"),
  test = as.factor("Final"),
  difficulty = as.numeric(sc_ft_difficulty[1, ])
)
p_ft_difficulty = data.frame(
  type = as.factor("Psychedelics"),
  test = as.factor("Final"),
  difficulty = as.numeric(p_ft_difficulty[1, ])
)
ft_difficulty = rbind(sc_ft_difficulty, p_ft_difficulty)

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
sc_pt_discrimination = data.frame(
  type = as.factor("StemCells"),
  test = as.factor("Practice"),
  discrimination = as.numeric(sc_pt_discrimination[1, ])
)
p_pt_discrimination = data.frame(
  type = as.factor("Psychedelics"),
  test = as.factor("Practice"),
  discrimination = as.numeric(p_pt_discrimination[1, ])
)
pt_discrimination = rbind(sc_pt_discrimination, p_pt_discrimination)

# Bind the final test item discriminatory power 
sc_ft_discrimination = data.frame(
  type = as.factor("StemCells"),
  test = as.factor("Final"),
  discrimination = as.numeric(sc_ft_discrimination[1, ])
)
p_ft_discrimination = data.frame(
  type = as.factor("Psychedelics"),
  test = as.factor("Final"),
  discrimination = as.numeric(p_ft_discrimination[1, ])
)
ft_discrimination = rbind(sc_ft_discrimination, p_ft_discrimination)

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

