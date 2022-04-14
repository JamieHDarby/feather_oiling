
# Load in packages and data -----------------------------------------------

if(!require(DescTools)) install.packages("DescTools")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(stats)) install.packages("stats")
if(!require(broom)) install.packages("broom")

# Read in data
perc.data <- read.csv(file = "data/perc_data.csv") %>%
  mutate(treatment = as.factor(treatment))

# Read in the AI data
ai.data <- read.csv(file = "data/ai_calc.csv") %>%
  mutate(Treatment = as.factor(Treatment))

# Weight change analysis --------------------------------------------------

# Check normality of feather pre- and post-treatment weights
# to establish what type of test is suitable
shapiro.test(perc.data$pre_weight)
shapiro.test(perc.data$post_weight)

# Do the same for the log of values to see if transformation sorts it
shapiro.test(log(perc.data$pre_weight))
shapiro.test(log(perc.data$post_weight))

# Subset dataframes by treatment type
# Ignore treatment 0, non-oiled
T1.weight <- subset(perc.data, treatment == "1", !is.na(post_weight))
T2.weight <- subset(perc.data, treatment == "2", !is.na(post_weight))
T3.weight <- subset(perc.data, treatment == "3", !is.na(post_weight))
T4.weight <- subset(perc.data, treatment == "4", !is.na(post_weight))

# Wilcoxon signed rank test for each subset testing weight increase
wilcox.test(x = T1.weight$pre_weight,
            y = T1.weight$post_weight,
            paired = T, alternative = "two.sided")

wilcox.test(x = T2.weight$pre_weight,
            y = T2.weight$post_weight,
            paired = T, alternative = "two.sided")

wilcox.test(x = T3.weight$pre_weight,
            y = T3.weight$post_weight,
            paired = T, alternative = "two.sided")

wilcox.test(x = T4.weight$pre_weight,
            y = T4.weight$post_weight,
            paired = T, alternative = "two.sided")

# Linear model to test the weight change before and after each treatment
weight_change_lm <- lm(perc_weight_change ~ treatment, data = perc.data)

# Summarise this
summary(weight_change_lm)

# Amalgamation Index (AI) analysis ----------------------------------------

# Get mean and sd of AI by treatment type
with(ai.data, tapply(Mean.AI, list(Treatment), mean, na.rm=T))
with(ai.data, tapply(Mean.AI, list(Treatment), sd, na.rm=T))

# Run an ANOVA, including an interaction term
# between treatment and pre-preparation type
AI_type_anova <- aov(Mean.AI ~ 
                      Treatment +
                      Preparation.Type + 
                      Treatment * Preparation.Type,
                    data = ai.data)

# Summarise this ANOVA
summary(AI_type_anova)

# Tukey test of this ANOVA.
# Is a mess, so a simplified ANOVA is run without pre-preparation type
TukeyHSD(AI_type_anova)

# Simplified ANOVA to compare treatment types irrespective of pre-preparation
AI_simple_anova <- aov(Mean.AI ~ 
                       Treatment,
                     data = ai.data)

# Tukey test for this ANOVA
TukeyHSD(AI_simple_anova)

# Percolation test time analysis ------------------------------------------

# Mean time per treatment (all treatments)
with(perc.data, tapply(time, list(perc.data$treatment), mean, na.rm=T))
with(perc.data, tapply(time, list(perc.data$treatment), sd, na.rm=T))

#Test normality of log and raw time
shapiro.test(perc.data$time)
shapiro.test(log(perc.data$time))

# Calculate feather size, just weight over length
perc.data$feather.size<- (perc.data$pre_weight * 1000) / (perc.data$length)

# 
mod.data <- perc.data %>%
  filter(!is.na(time),
         !is.na(feather.size))

# Create models with pre and post weights and size
glm1 <- glm(time ~ treatment + pre_weight + post_weight + feather.size,
           data = mod.data)

# VIFs are an issue between post_weight and treatment in this one
VIF(glm1)

# This is the model without post_weight, removed because of high VIF
glm2 <- glm(time ~ treatment + pre_weight + feather.size,
           data = mod.data)

# Here you can see high VIF values (>2) for pre.weight and feather size
VIF(glm2)

# Run 2 separate models
# glm3 is glm 2 without feather size
glm3 <- glm(time ~ treatment + pre_weight,
            data = mod.data)

# VIFs are fine
VIF(glm3)

# glm3 is glm 2 without pre weight
glm4 <- glm(time ~ treatment + feather.size,
            data = mod.data)

# VIFs are fine
VIF(glm4)

# glm4 fits better according to AIC
AIC(glm3, glm4)

# glm5 is glm4 but without feather size
glm5 <- glm(time ~ treatment,
            data = mod.data)

# glm5 fits better according to AIC
AIC(glm4, glm5)

# glm6 is a null model
glm6 <- glm(time ~ 1,
            data = mod.data)

# glm5 fits better according to AIC
AIC(glm5, glm6)


# Rerun glm5 without filtering out NAs for other variables
glm_final <- glm(time ~ treatment,
            data = perc.data, family = gaussian(link = "identity"))

# Summarise
summary(glm_final)

# Test residuals for normailty
shapiro.test(residuals(glm_final))

# Quick look at model residuals
# Residuals have a slight positive skew, but appear relatively normal
ggplot() +
  geom_density(aes(x = residuals(glm_final)), fill = "grey")
