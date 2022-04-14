
# Load in packages --------------------------------------------------------

if(!require(DescTools)) install.packages("DescTools")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(stats)) install.packages("stats")
if(!require(broom)) install.packages("broom")

# Percolation test graph --------------------------------------------------

# Read in data
perc.data <- read.csv(file = "data/perc_data.csv") %>%
  mutate(treatment = as.factor(treatment))

# Quick examination
head(perc.data)
  
# Vector of correct treatment names
treatment_names <- c("Control","Trace\nsheen","Dark\nsheen",
  "Standard\nslick", "Heavy\nslick")

# Boxplot of percolation data
perc.plot <- ggplot(perc.data[which(!is.na(perc.data$treatment)),]) +
  geom_boxplot(aes(x = treatment, y = time, fill = treatment),
               size = 0.6, alpha = 0.6) + 
  scale_x_discrete(name = NULL,
                   labels = treatment_names) +
  labs(y = "Time (seconds)") +
  scale_fill_viridis_d(option = "A") +
  theme(legend.position = "none")

# Look at the plot
perc.plot

# Save it off
ggsave(perc.plot, filename = "plots/perc_plot.png",
       width = 6, height = 4, dpi = 500)

# Weight change plots -----------------------------------------------------

# Boxplot of weight changes
weight.plot <-
  ggplot(perc.data[which(!is.na(perc.data$treatment)),]) +
  geom_boxplot(aes(x = treatment, y = perc_weight_change, fill = treatment),
               size = 0.6, alpha = 0.6) +
  # Again correct labels for treat
  scale_x_discrete(name = NULL,
                   labels = treatment_names) +
  labs(y = "Percentage Weight Change") +
  scale_y_continuous(breaks = c(0, 250, 500, 750, 1000, 1250)) +
  scale_fill_viridis_d(option = "A") +
  theme(legend.position = "none")

# Take a look
weight.plot

# Save it off
ggsave(weight.plot, filename = "plots/weight_plot.png",
       width = 6, height = 4, dpi = 500)

# Graph out the amalgation index (AI) data --------------------------------

# Read in the AI data
ai.data <- read.csv(file = "data/ai_calc.csv") %>%
  mutate(Treatment = as.factor(Treatment))

# Take a look
head(ai.data)

# Boxplot of AI results, fill by pre-treatment
ai.plot <-
  ggplot(ai.data[which(!is.na(ai.data$Treatment)),]) +
  geom_boxplot(aes(x = Treatment, y = Mean.AI, fill = Preparation.Type),
               size = 0.6, alpha = 0.8) + 
  scale_x_discrete(name = NULL,
                   labels = c("Control","Trace\nsheen","Dark\nsheen",
                              "Standard\nslick", "Heavy\nslick")) +
  labs(y = "Amalgamation Index", fill = "Preparation\ntype") +
  scale_fill_brewer(palette = "Pastel1", labels = c("Fresh", "Percolation\ntested"))

# Take a look
ai.plot

# Save it off
ggsave(ai.plot, filename = "plots/ai_plot.png",
       width = 6, height = 4, dpi = 500)
