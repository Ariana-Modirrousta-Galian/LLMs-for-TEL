# Clear R environment
rm(list = ls(all = TRUE)) 

# Load in libraries
library(effectsize)
library(emmeans)

# Import data
exp1 = read.csv("data_JY.csv")
exp2 = read.csv("data_JZ.csv")
exp3 = read.csv("data_AMG.csv")
exp4 = read.csv("data_AMG_2.csv")

# Match column names 
exp1$Group[exp1$Group == "retrievalpractice"] = "treatment"
exp1$Group[exp1$Group == "elaboratedexplanation"] = "control"

exp2$Group[exp2$Group == "Retrieval"] = "treatment"
exp2$Group[exp2$Group == "Restudy"] = "control"

exp3$Group[exp3$Group == "SAQ"] = "treatment"
exp3$Group[exp3$Group == "Restudy"] = "control"

exp4$Group[exp4$Group == "SAQ"] = "treatment"
exp4$Group[exp4$Group == "Restudy"] = "control"

names(exp2)[names(exp2) == "Participant.ID"] = "PROLIFIC_PID"

names(exp4)[names(exp4) == "part3_id"] = "PROLIFIC_PID"

# Add experiment column
exp1$experiment = as.factor("1")
exp2$experiment = as.factor("2")
exp3$experiment = as.factor("3")
exp4$experiment = as.factor("4")

# Change structure of group column
exp1$Group = as.factor(exp1$Group)
exp2$Group = as.factor(exp2$Group)
exp3$Group = as.factor(exp3$Group)
exp4$Group = as.factor(exp4$Group)

# Combine experiments
exps = rbind(exp1, exp2, exp3, exp4)

# Add practice test format column
exps$format = as.factor(ifelse(exps$experiment %in% c("1", "2"), "MCQ", "SAQ"))

# Run 2 x 4 ANOVA
aov = aov(prop_correct ~ Group * experiment, data = exps)
summary(aov)
eta_squared(aov)
emmeans(aov, ~ Group)
pairs(emmeans(aov, ~ Group))

# Run 2 x 2 ANOVA
format_aov = aov(prop_correct ~ Group * format, data = exps)
summary(format_aov)
eta_squared(format_aov)
emmeans(format_aov, ~ Group | format)
pairs(emmeans(format_aov, ~ Group | format))

