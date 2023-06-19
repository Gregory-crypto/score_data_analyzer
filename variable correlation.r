

# Libraries

library(tidyverse)
library(ggpubr)
library(patchwork)

# Data

scores <- read_csv("http://roycekimmons.com/system/generate_data.php?dataset=exams&n=1000")

# Preparation

scores <- rename(scores, "prep_level" = "test preparation course")
scores$prep_level <- fct_infreq(as.factor(scores$prep_level))
scores |>
    ggplot(aes(x = prep_level, fill = prep_level)) +
    geom_bar(alpha = 0.9) +
  labs(title = "Test preparation course",
  y = "Number of people", x = "Preparation type") +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "none",
  plot.title = element_text(size = rel(1.2), hjust = 0.5))

# Numeracal variables renaming

scores <- rename(scores, "math" = "math score")
scores <- rename(scores, "reading" = "reading score")
scores <- rename(scores, "writing" = "writing score")

# Math ~ Reading

cor1 <- scores |>
  ggplot(aes(x = math, y = reading, color = prep_level)) +
  geom_point(size = 2) +
  labs(title = "Correlation between results",
  subtitle = "math ~ reading",
  y = "Number of people", x = "Score", color = "Preparation") +
  theme(plot.title = element_text(size = rel(1.1), hjust = 0.5),
  plot.subtitle = element_text(hjust = 0.5)) +
  stat_cor(aes(color = NULL), show.legend = FALSE, method = "pearson") +
  geom_smooth(aes(group = 1), method = "lm", color = "#676b75") +
  scale_color_brewer(palette = "Set3")

# Reading ~ Writing

cor2 <- scores |>
  ggplot(aes(x = reading, y = writing, color = prep_level)) +
  geom_point(size = 2) +
  labs(title = "Correlation between results",
  subtitle = "reading ~ writing",
  y = "Number of people", x = "Score", color = "Preparation") +
  theme(plot.title = element_text(size = rel(1.1), hjust = 0.5),
  plot.subtitle = element_text(hjust = 0.5)) +
  stat_cor(aes(color = NULL), show.legend = FALSE, method = "pearson") +
  geom_smooth(aes(group = 1), method = "lm", color = "#676b75") +
  scale_color_brewer(palette = "Set3")

# Writing ~ Math

cor3 <- scores |>
  ggplot(aes(x = writing, y = math, color = prep_level)) +
  geom_point(size = 2) +
  labs(title = "Correlation between results",
  subtitle = "writing ~ math",
  y = "Number of people", x = "Score", color = "Preparation") +
  theme(plot.title = element_text(size = rel(1.1), hjust = 0.5),
  plot.subtitle = element_text(hjust = 0.5)) +
  stat_cor(aes(color = NULL), show.legend = FALSE, method = "pearson") +
  geom_smooth(aes(group = 1), method = "lm", color = "#676b75") +
  scale_color_brewer(palette = "Set3")

# Creation correlation patch

(cor1 + cor2 + cor3) +
  plot_annotation(title = "Relationship between numerical variables",
  tag_levels = "I",
  theme = theme(plot.title = element_text(size = rel(1.3), hjust = 0.5))) +
  plot_layout(ncol = 3)
