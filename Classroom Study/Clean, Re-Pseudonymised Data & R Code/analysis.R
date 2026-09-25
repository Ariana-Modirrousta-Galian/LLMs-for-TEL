# Clear R environment
rm(list = ls(all = TRUE)) 

# Load in libraries
library(dplyr)
library(readxl)
library(ggplot2)
library(lsr) 
library(BayesFactor) 


# Data cleaning ################################################################


# Import data
week1 = read.csv("Week 1.csv")
week3 = read.csv("Week 3.csv")
week5 = read.csv("Week 5.csv")
week7 = read.csv("Week 7.csv")
week9 = read.csv("Week 9.csv")
finalsurvey = read.csv("Final Survey.csv")
class_exams = read_excel("biweeklyquizzes.xlsx")
final_exam = read_excel("finalexam.xlsx", na = "NA", col_names = F)

# Remove header rows
week1 = week1[c(-1,-2),] 
week3 = week3[c(-1,-2),] 
week5 = week5[c(-1,-2),] 
week7 = week7[c(-1,-2),] 
week9 = week9[c(-1,-2),] 
finalsurvey = finalsurvey[c(-1,-2),]

# Trim white space from candidate numbers
week1$candidate_number = trimws(week1$candidate_number)
week3$candidate_number = trimws(week3$candidate_number)
week5$candidate_number = trimws(week5$candidate_number)
week7$candidate_number = trimws(week7$candidate_number)
week9$candidate_number = trimws(week9$candidate_number)
finalsurvey$candidate_number = trimws(finalsurvey$candidate_number)

# Remove rows with missing candidate numbers
week1 = week1 |>
  filter(candidate_number != "")
week3 = week3 |>
  filter(candidate_number != "")
week5 = week5 |>
  filter(candidate_number != "")
week7 = week7 |>
  filter(candidate_number != "")
week9 = week9 |>
  filter(candidate_number != "")
finalsurvey = finalsurvey |>
  filter(candidate_number != "")

# In week 1, P03 took part twice (their first attempt was submitted under a
# different identifier, corrected during pseudonymisation). Delete their second attempt
week1 <- week1 |>
  filter(!(candidate_number == "P03" & Progress == 95))

# Delete the second attempt from all other duplicates
week1 = week1 |>
  distinct(candidate_number, .keep_all = TRUE)
week3 = week3 |>
  distinct(candidate_number, .keep_all = TRUE)
week5 = week5 |>
  distinct(candidate_number, .keep_all = TRUE)
week7 = week7 |>
  distinct(candidate_number, .keep_all = TRUE)
week9 = week9 |>
  distinct(candidate_number, .keep_all = TRUE)

# Progress in week 1:
# 100 = finished 
# 95,89 = finished but did not click through to the end
# 74,63 = finished initial survey but not SAQs
# 11 = provided informed consent

# Progress in week 3:
# 100 = finished
# 48,4 = provided their candidate number (progress varies depending on whether students took part in week 1 and therefore have to do the initial survey)

# Progress in week 5:
# 100 = finished
# 96 = finished but did not click through to the end
# 80,72 = did not finish SAQs

# Progress in week 7
# 100 = finished
# 56 = did not finish SAQs
# 48,44 = provided their candidate number (progress varies depending on what screen they clicked to)

# Progress in week 9
# 100 = finished
# 96 = finished but did not click through to the end
# 73 = finished SAQs but not final survey
# 42 = did not finish SAQs  
# 27,4 = provided their candidate number (progress varies depending on whether students took part in week 1 and therefore have to do the initial survey)

# Convert progress to numeric
week1$Progress = as.numeric(week1$Progress)
week3$Progress = as.numeric(week3$Progress)
week5$Progress = as.numeric(week5$Progress)
week7$Progress = as.numeric(week7$Progress)
week9$Progress = as.numeric(week9$Progress)

# Only keep candidate_number and progress
w1 = week1 |> select(candidate_number, progress_week1 = Progress)
w3 = week3 |> select(candidate_number, progress_week3 = Progress)
w5 = week5 |> select(candidate_number, progress_week5 = Progress)
w7 = week7 |> select(candidate_number, progress_week7 = Progress)
w9 = week9 |> select(candidate_number, progress_week9 = Progress)

# Merge all weeks
progress_summary_all = w1 |>
  full_join(w3, by = "candidate_number") |>
  full_join(w5, by = "candidate_number") |>
  full_join(w7, by = "candidate_number") |>
  full_join(w9, by = "candidate_number")

# Only keep participants that finished the practice test in at least one week 
progress_summary_subset = progress_summary_all |>
  filter(
    (progress_week1 >= 89 & !is.na(progress_week1)) |
    (progress_week3 == 100 & !is.na(progress_week3)) |
    (progress_week5 >= 96 & !is.na(progress_week5)) |
    (progress_week7 == 100 & !is.na(progress_week7)) |
    (progress_week9 >= 73 & !is.na(progress_week9))
  )


# Demographics #################################################################


# Get demographics across all weeks
demographics = bind_rows(
  week1 |> select(candidate_number, age, gender),
  week3 |> select(candidate_number, age, gender),
  week5 |> select(candidate_number, age, gender),
  week7 |> select(candidate_number, age, gender),
  week9 |> select(candidate_number, age, gender)
) |>
  distinct(candidate_number, .keep_all = TRUE)

# Get demographics from subset
subset_demographics = progress_summary_subset |>
  left_join(demographics, by = "candidate_number")

# Age
subset_demographics$age = as.numeric(subset_demographics$age)
subset_demographics |>
  summarise(
    n = sum(!is.na(age)),
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE)
  )

# Gender
table(subset_demographics$gender)


# Participation rates ##########################################################


# Participation each week 
week_completion_counts = progress_summary_subset |>
  summarise(
    progress_week1 = sum(progress_week1 >= 89, na.rm = TRUE),
    progress_week3 = sum(progress_week3 == 100, na.rm = TRUE),
    progress_week5 = sum(progress_week5 >= 96, na.rm = TRUE),
    progress_week7 = sum(progress_week7 == 100, na.rm = TRUE),
    progress_week9 = sum(progress_week9 >= 73, na.rm = TRUE)
  )
week_completion_counts

# Number of practice tests completed
progress_summary_subset = progress_summary_subset |>
  mutate(n_tests_completed = 
           ifelse(is.na(progress_week1), 0, progress_week1 >= 89) +
           ifelse(is.na(progress_week3), 0, progress_week3 == 100) +
           ifelse(is.na(progress_week5), 0, progress_week5 >= 96) +
           ifelse(is.na(progress_week7), 0, progress_week7 == 100) +
           ifelse(is.na(progress_week9), 0, progress_week9 >= 73))
table(progress_summary_subset$n_tests_completed)


# Completion times ##########################################################


# Median completion time across all weeks
all_durations = c(
  week1 |> filter(Progress >= 89) |> pull(`Duration..in.seconds.`) |> as.numeric(),
  week3 |> filter(Progress == 100) |> pull(`Duration..in.seconds.`) |> as.numeric(),
  week5 |> filter(Progress >= 96) |> pull(`Duration..in.seconds.`) |> as.numeric(),
  week7 |> filter(Progress == 100) |> pull(`Duration..in.seconds.`) |> as.numeric(),
  week9 |> filter(Progress >= 73) |> pull(`Duration..in.seconds.`) |> as.numeric()
)
median_duration_mins = median(all_durations, na.rm = TRUE) / 60
median_duration_mins

# Median completion time for each week
median_durations_by_week = tibble(
  week = c("week1", "week3", "week5", "week7", "week9"),
  median_duration_mins = c(
    week1 |> filter(Progress >= 89) |> pull(`Duration..in.seconds.`) |> as.numeric() |> median(na.rm = TRUE) / 60,
    week3 |> filter(Progress == 100) |> pull(`Duration..in.seconds.`) |> as.numeric() |> median(na.rm = TRUE) / 60,
    week5 |> filter(Progress >= 96) |> pull(`Duration..in.seconds.`) |> as.numeric() |> median(na.rm = TRUE) / 60,
    week7 |> filter(Progress == 100) |> pull(`Duration..in.seconds.`) |> as.numeric() |> median(na.rm = TRUE) / 60,
    week9 |> filter(Progress >= 73) |> pull(`Duration..in.seconds.`) |> as.numeric() |> median(na.rm = TRUE) / 60
  )
)
median_durations_by_week


# H1 ###########################################################################
# Practice quiz participation will be positively associated with summative assessment performance


## Class exam grade ############################################################


# Get candidate numbers and proportion of practice tests completed
h1_class_exams = progress_summary_subset |>
  mutate(proportion_tests_completed = n_tests_completed / 5) |>
  select(candidate_number, proportion_tests_completed)

# Add average class exam grade
h1_class_exams = h1_class_exams |>
  left_join(
    class_exams |>
      select(Candidate, AVERAGE),
    by = c("candidate_number" = "Candidate")
  )
# P04 and P10 did not have an average class exam grade

# Linear regression 
h1_class_exams_model = lm(AVERAGE ~ proportion_tests_completed, data = h1_class_exams)
summary(h1_class_exams_model)
# Effect is in the predicted direction, so one-tailed p-value = .015

# Scatter plot
ggplot(h1_class_exams, aes(x = proportion_tests_completed, y = AVERAGE)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, colour = "#FF0000", fill = "#619CFF") +
  theme_classic() +
  labs(x = "Number of Practice Tests Completed", y = "Average Class Exam Grade (%)") +
  scale_x_continuous(breaks = seq(0, 1, 0.2), labels = 0:5) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 16))

# Plot Cook's distance for each observation 
plot(h1_class_exams_model, which = 4)

# Calculate Cook's distance for each observation 
h1_complete = model.frame(h1_class_exams_model)
cd = cooks.distance(h1_class_exams_model)

# Remove observations with Cook's distance greater than the 4/n threshold
h1_no_influential = h1_complete |>
  filter(cd <= 4 / nrow(h1_complete))

# Linear regression after excluding influential observations (sensitivity analysis)
summary(lm(AVERAGE ~ proportion_tests_completed, data = h1_no_influential)) 
# Effect is in the predicted direction, so one-tailed p-value = .001

# Convert prior knowledge to numeric
week1$prior_knowledge_1 = as.numeric(week1$prior_knowledge_1)
week3$prior_knowledge_1 = as.numeric(week3$prior_knowledge_1)
week5$prior_knowledge_1 = as.numeric(week5$prior_knowledge_1)
week7$prior_knowledge_1 = as.numeric(week7$prior_knowledge_1)

# Combine prior knowledge across weeks, keeping each participant's first non-missing response
prior_knowledge = bind_rows(
  week1 |> select(candidate_number, prior_knowledge_1),
  week3 |> select(candidate_number, prior_knowledge_1),
  week5 |> select(candidate_number, prior_knowledge_1),
  week7 |> select(candidate_number, prior_knowledge_1)
) |>
  filter(!is.na(prior_knowledge_1)) |>
  distinct(candidate_number, .keep_all = TRUE)

# Add prior knowledge to the H1 dataset
h1_class_exams_pk = h1_class_exams |>
  left_join(prior_knowledge, by = "candidate_number")

# Linear regression controlling for prior knowledge (sensitivity analysis)
summary(lm(AVERAGE ~ proportion_tests_completed + prior_knowledge_1, data = h1_class_exams_pk))
# Effect is in the predicted direction, so one-tailed p-value = .032


## Final exam grade ############################################################


# Get candidate numbers and proportion of practice tests completed
h1_final_exam = progress_summary_subset |>
  mutate(proportion_tests_completed = n_tests_completed / 5) |>
  select(candidate_number, proportion_tests_completed)

# Rename final exam columns
names(final_exam) = c("Candidate", "FINAL")

# Add final exam grade
h1_final_exam = h1_final_exam |>
  left_join(
    final_exam |>
      select(Candidate, FINAL),
    by = c("candidate_number" = "Candidate")
  )
# P04, P10, P35, and P07 did not have a final exam grade

# Linear regression
h1_final_exam_model = lm(FINAL ~ proportion_tests_completed, data = h1_final_exam)
summary(h1_final_exam_model)
# Effect is in the predicted direction, so one-tailed p-value = .010

# Scatter plot
ggplot(h1_final_exam, aes(x = proportion_tests_completed, y = FINAL)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, colour = "#FF0000", fill = "#619CFF") +
  theme_classic() +
  labs(x = "Number of Practice Tests Completed", y = "Final Exam Grade (%)") +
  scale_x_continuous(breaks = seq(0, 1, 0.2), labels = 0:5) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 16))

# Plot Cook's distance for each observation
plot(h1_final_exam_model, which = 4)

# Calculate Cook's distance for each observation
h1_final_complete = model.frame(h1_final_exam_model)
cd_final = cooks.distance(h1_final_exam_model)

# Remove observations with Cook's distance greater than the 4/n threshold
h1_final_no_influential = h1_final_complete |>
  filter(cd_final <= 4 / nrow(h1_final_complete))

# Linear regression after excluding influential observations (sensitivity analysis)
summary(lm(FINAL ~ proportion_tests_completed, data = h1_final_no_influential))
# Effect is in the predicted direction, so one-tailed p-value < .001

# Add prior knowledge to the final exam dataset
h1_final_exam_pk = h1_final_exam |>
  left_join(prior_knowledge, by = "candidate_number")

# Linear regression controlling for prior knowledge (sensitivity analysis)
summary(lm(FINAL ~ proportion_tests_completed + prior_knowledge_1, data = h1_final_exam_pk))
# Effect is in the predicted direction, so one-tailed p-value = .018


# H2 ###########################################################################
# Test anxiety will be lower at the end of the study than at the start (H2a), and this difference will be greater with higher levels of practice quiz participation (H2b)


# Convert test anxiety to numeric
week1 = week1 |>
  mutate(across(c(test_anxiety_1_1, test_anxiety_2_1), as.numeric))
week3 = week3 |>
  mutate(across(c(test_anxiety_1_1, test_anxiety_2_1), as.numeric))
week5 = week5 |>
  mutate(across(c(test_anxiety_1_1, test_anxiety_2_1), as.numeric))
week7 = week7 |>
  mutate(across(c(test_anxiety_1_1, test_anxiety_2_1), as.numeric))
week9 = week9 |>
  mutate(across(c(test_anxiety_1_1, test_anxiety_2_1), as.numeric))
finalsurvey = finalsurvey |>
  mutate(across(c(test_anxiety_1_1, test_anxiety_2_1), as.numeric))

# Initial survey
h2_initial = bind_rows(
  week1 |> mutate(week = 1) |> select(candidate_number, week, test_anxiety_1_1, test_anxiety_2_1),
  week3 |> mutate(week = 3) |> select(candidate_number, week, test_anxiety_1_1, test_anxiety_2_1),
  week5 |> mutate(week = 5) |> select(candidate_number, week, test_anxiety_1_1, test_anxiety_2_1),
  week7 |> mutate(week = 7) |> select(candidate_number, week, test_anxiety_1_1, test_anxiety_2_1)
)

# Final survey
h2_end_survey = week9 |>
  select(candidate_number, test_anxiety_1_1, test_anxiety_2_1) |>
  full_join(
    finalsurvey |>
      select(candidate_number, test_anxiety_1_1, test_anxiety_2_1),
    by = "candidate_number",
    suffix = c("", "_fs")
  ) |>
  mutate(
    test_anxiety_1_1 = coalesce(test_anxiety_1_1, test_anxiety_1_1_fs),
    test_anxiety_2_1 = coalesce(test_anxiety_2_1, test_anxiety_2_1_fs)
  ) |>
  select(candidate_number, test_anxiety_1_1, test_anxiety_2_1)

# Create an initial test anxiety score by averaging the two test anxiety items from the initial survey for each participant
h2_start = h2_initial |>
  filter(!is.na(test_anxiety_1_1), !is.na(test_anxiety_2_1)) |>
  group_by(candidate_number) |>
  slice_min(week, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(test_anxiety_start = rowMeans(across(c(test_anxiety_1_1, test_anxiety_2_1)))) |>
  select(candidate_number, test_anxiety_start)

# Create a final test anxiety score in the same way
h2_end = h2_end_survey |>
  filter(!is.na(test_anxiety_1_1), !is.na(test_anxiety_2_1)) |>
  mutate(test_anxiety_end = rowMeans(across(c(test_anxiety_1_1, test_anxiety_2_1)))) |>
  select(candidate_number, test_anxiety_end)

# Only keep participants with initial and final test anxiety scores, add practice test completion, and calculate test anxiety change
h2_data = progress_summary_subset |>
  select(candidate_number, n_tests_completed) |>
  inner_join(h2_start, by = "candidate_number") |>
  inner_join(h2_end, by = "candidate_number") |>
  mutate(
    proportion_tests_completed = n_tests_completed / 5,
    test_anxiety_change = test_anxiety_start - test_anxiety_end
  )

# T-test
t.test(h2_data$test_anxiety_start, h2_data$test_anxiety_end, paired = TRUE, alternative = "greater")
cohensD(h2_data$test_anxiety_start, h2_data$test_anxiety_end, method = "paired")
ttestBF(h2_data$test_anxiety_start, h2_data$test_anxiety_end, paired = T, nullInterval = c(0, Inf) )

# Means and sds
h2_data |>
  summarise(
    start_mean = mean(test_anxiety_start, na.rm = TRUE),
    start_sd = sd(test_anxiety_start, na.rm = TRUE),
    end_mean = mean(test_anxiety_end, na.rm = TRUE),
    end_sd = sd(test_anxiety_end, na.rm = TRUE)
  )

# Linear regression
h2_test_anxiety_model = lm(test_anxiety_change ~ proportion_tests_completed, data = h2_data)
summary(h2_test_anxiety_model)
# Effect is not in the predicted direction, so one-tailed p-value = .565

# Plot Cook's distance 
plot(h2_test_anxiety_model, which = 4)

# Calculate Cook's distance 
h2_complete = model.frame(h2_test_anxiety_model)
cd = cooks.distance(h2_test_anxiety_model)

# Remove values greater than the 4/n threshold
h2_no_influential = h2_complete |>
  filter(cd <= 4 / nrow(h2_complete))

# Linear regression after excluding influential observations (sensitivity analysis)
summary(lm(test_anxiety_change ~ proportion_tests_completed, data = h2_no_influential)) 
# Effect is not in the predicted direction, so one-tailed p-value = .945

# Add prior knowledge to the H2 dataset
h2_data_pk = h2_data |>
  left_join(prior_knowledge, by = "candidate_number")

# Linear regression controlling for prior knowledge (sensitivity analysis)
summary(lm(test_anxiety_change ~ proportion_tests_completed + prior_knowledge_1, data = h2_data_pk))
# Effect is not in the predicted direction, so one-tailed p-value = .571


# H3 ###########################################################################
# Large language model (LLM) perception will be more favourable at the end of the study than at the start (H3a), and this difference will be greater with higher levels of practice quiz participation (H3b).


# Convert LLM perception to numeric
week1 = week1 |>
  mutate(across(c(ai_1_1, ai_2_1), as.numeric))
week3 = week3 |>
  mutate(across(c(ai_1_1, ai_2_1), as.numeric))
week5 = week5 |>
  mutate(across(c(ai_1_1, ai_2_1), as.numeric))
week7 = week7 |>
  mutate(across(c(ai_1_1, ai_2_1), as.numeric))
week9 = week9 |>
  mutate(across(c(ai_1_1, ai_2_1), as.numeric))
finalsurvey = finalsurvey |>
  mutate(across(c(ai_1_1, ai_2_1), as.numeric))

# Initial survey
h3_initial = bind_rows(
  week1 |> mutate(week = 1) |> select(candidate_number, week, ai_1_1, ai_2_1),
  week3 |> mutate(week = 3) |> select(candidate_number, week, ai_1_1, ai_2_1),
  week5 |> mutate(week = 5) |> select(candidate_number, week, ai_1_1, ai_2_1),
  week7 |> mutate(week = 7) |> select(candidate_number, week, ai_1_1, ai_2_1)
)

# Final survey
h3_end_survey = week9 |>
  select(candidate_number, ai_1_1, ai_2_1) |>
  full_join(
    finalsurvey |>
      select(candidate_number, ai_1_1, ai_2_1),
    by = "candidate_number",
    suffix = c("", "_fs")
  ) |>
  mutate(
    ai_1_1 = coalesce(ai_1_1, ai_1_1_fs),
    ai_2_1 = coalesce(ai_2_1, ai_2_1_fs)
  ) |>
  select(candidate_number, ai_1_1, ai_2_1)

# Create an initial LLM perception score by averaging the two LLM perception items from the initial survey for each participant
h3_start = h3_initial |>
  filter(!is.na(ai_1_1), !is.na(ai_2_1)) |>
  group_by(candidate_number) |>
  slice_min(week, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(ai_perception_start = rowMeans(across(c(ai_1_1, ai_2_1)))) |>
  select(candidate_number, ai_perception_start)

# Create a final LLM perception score in the same way
h3_end = h3_end_survey |>
  filter(!is.na(ai_1_1), !is.na(ai_2_1)) |>
  mutate(ai_perception_end = rowMeans(across(c(ai_1_1, ai_2_1)))) |>
  select(candidate_number, ai_perception_end)

# Only keep participants with initial and final LLM perception scores, add practice test completion, and calculate LLM perception change
h3_data = progress_summary_subset |>
  select(candidate_number, n_tests_completed) |>
  inner_join(h3_start, by = "candidate_number") |>
  inner_join(h3_end, by = "candidate_number") |>
  mutate(
    proportion_tests_completed = n_tests_completed / 5,
    ai_perception_change = ai_perception_start - ai_perception_end
  )

# T-test
t.test(h3_data$ai_perception_start, h3_data$ai_perception_end, paired = TRUE, alternative = "less")
cohensD(h3_data$ai_perception_start, h3_data$ai_perception_end, method = "paired")
ttestBF(h3_data$ai_perception_start, h3_data$ai_perception_end, paired = T, nullInterval = c(-Inf, 0) )

# Mean and sds
h3_data |>
  summarise(
    start_mean = mean(ai_perception_start, na.rm = TRUE),
    start_sd = sd(ai_perception_start, na.rm = TRUE),
    end_mean = mean(ai_perception_end, na.rm = TRUE),
    end_sd = sd(ai_perception_end, na.rm = TRUE)
  )

# Linear regression
h3_ai_perception_model = lm(ai_perception_change ~ proportion_tests_completed, data = h3_data)
summary(h3_ai_perception_model)
# Effect is not in the predicted direction, so one-tailed p-value = .514

# Plot Cook's distance 
plot(h3_ai_perception_model, which = 4)

# Calculate Cook's distance 
h3_complete = model.frame(h3_ai_perception_model)
cd = cooks.distance(h3_ai_perception_model)

# Remove values greater than the 4/n threshold
h3_no_influential = h3_complete |>
  filter(cd <= 4 / nrow(h3_complete))

# Linear regression after excluding influential observations (sensitivity analysis)
summary(lm(ai_perception_change ~ proportion_tests_completed, data = h3_no_influential)) 
# Effect is in the predicted direction, so one-tailed p-value = .297

# Add prior knowledge to the H3 dataset
h3_data_pk = h3_data |>
  left_join(prior_knowledge, by = "candidate_number")

# Linear regression controlling for prior knowledge (sensitivity analysis)
summary(lm(ai_perception_change ~ proportion_tests_completed + prior_knowledge_1, data = h3_data_pk))
# Effect is in the predicted direction, so one-tailed p-value = .482

# Combine the two LLM perception questions into a single long-format data frame
h3_items_end_long = bind_rows(
  h3_end_survey |> select(candidate_number, rating = ai_1_1) |> mutate(item = "Educator Use"),
  h3_end_survey |> select(candidate_number, rating = ai_2_1) |> mutate(item = "Learning Gain")
) |>
  filter(!is.na(rating)) |>
  mutate(item = factor(item, levels = c("Educator Use", "Learning Gain")))

# Histograms
ggplot(h3_items_end_long, aes(x = rating)) +
  geom_bar(fill = "#619CFF", colour = "black") +
  geom_vline(xintercept = 4, linetype = "dashed", colour = "#FF0000", linewidth = 1) +
  facet_wrap(~ item, strip.position = "bottom") +
  theme_classic() +
  labs(x = NULL, y = "Number of Students") +
  scale_x_continuous(breaks = 1:7, limits = c(0.5, 7.5)) +
  scale_y_continuous(breaks = 1:10, limits = c(0, 10), expand = expansion(mult = c(0, 0.05))) +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        strip.text = element_text(size = 16),
        strip.background = element_blank(),
        strip.placement = "outside",
        panel.spacing = unit(1.5, "lines"))

# T-test vs. mid-point
t.test(h3_data$ai_perception_end, mu = 4, alternative = "greater")
cohensD(h3_data$ai_perception_end, mu = 4)
ttestBF(h3_data$ai_perception_end, mu = 4, nullInterval = c(0, Inf))


# Time between initial and final survey ########################################


# Match recorded date format across files
week1 = week1 |> mutate(RecordedDate = as.POSIXct(RecordedDate, format = "%Y-%m-%d %H:%M:%S"))
week3 = week3 |> mutate(RecordedDate = as.POSIXct(RecordedDate, format = "%d/%m/%Y %H:%M"))
week5 = week5 |> mutate(RecordedDate = as.POSIXct(RecordedDate, format = "%d/%m/%Y %H:%M"))
week7 = week7 |> mutate(RecordedDate = as.POSIXct(RecordedDate, format = "%d/%m/%Y %H:%M"))
week9 = week9 |> mutate(RecordedDate = as.POSIXct(RecordedDate, format = "%d/%m/%Y %H:%M"))
finalsurvey = finalsurvey |> mutate(RecordedDate = as.POSIXct(RecordedDate, format = "%d/%m/%Y %H:%M"))

# Get the date of each participant's initial survey 
initial_dates = bind_rows(
  week1 |> mutate(week = 1) |> select(candidate_number, week, RecordedDate),
  week3 |> mutate(week = 3) |> select(candidate_number, week, RecordedDate),
  week5 |> mutate(week = 5) |> select(candidate_number, week, RecordedDate),
  week7 |> mutate(week = 7) |> select(candidate_number, week, RecordedDate)
) |>
  group_by(candidate_number) |>
  slice_min(week, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(candidate_number, initial_date = RecordedDate)

# Get the date of each participant's final survey 
final_dates = week9 |>
  select(candidate_number, RecordedDate) |>
  full_join(
    finalsurvey |> select(candidate_number, RecordedDate),
    by = "candidate_number",
    suffix = c("", "_fs")
  ) |>
  mutate(RecordedDate = coalesce(RecordedDate, RecordedDate_fs)) |>
  select(candidate_number, final_date = RecordedDate)

# Calculate the time difference in days for each participant
survey_time_diff = initial_dates |>
  inner_join(final_dates, by = "candidate_number") |>
  mutate(days_between = as.numeric(difftime(final_date, initial_date, units = "days")))
survey_time_diff

# Mean and sd
survey_time_diff |>
  summarise(
    mean_days = mean(days_between, na.rm = TRUE),
    sd_days = sd(days_between, na.rm = TRUE)
  )


# Free-text comments ###########################################################


# Combine free-text responses from week 9 and the final survey 
open_comments = week9 |>
  select(candidate_number, ai_open) |>
  full_join(
    finalsurvey |>
      select(candidate_number, ai_open),
    by = "candidate_number",
    suffix = c("", "_fs")
  ) |>
  mutate(ai_open = coalesce(ai_open, ai_open_fs)) |>
  select(candidate_number, ai_open) |>
  filter(!is.na(ai_open), ai_open != "")
View(data.frame(Comment = open_comments$ai_open))

