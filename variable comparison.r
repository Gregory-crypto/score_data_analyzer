

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

# Math

scores <- rename(scores, "math" = "math score")
math <- scores |>
    ggplot(aes(x = math)) +
    geom_histogram(aes(alpha = 0.9, fill =  ..x..)) +
    geom_density(aes(y = ..density.. * 3000),
    color = "#676b75", fill = "#676b75", linewidth = 1, alpha = 0.1) +
  labs(title = "Math test score distribution",
  y = "Number of people", x = "Score") +
  scale_fill_gradient(low = "#824141", high = "#418252") +
  scale_y_continuous(
    name = "Number of people",
    sec.axis = sec_axis(~ . / 3000, name = "Density")) +
  theme(axis.text = element_text(size = 12)) +
  theme(legend.position = "none",
  plot.title = element_text(size = rel(1.2), hjust = 0.5))
  scores <- rename(scores, "reading" = "reading score")

# Reading

reading <- scores |>
    ggplot(aes(x = reading)) +
    geom_histogram(aes(alpha = 0.9, fill =  ..x..)) +
    geom_density(aes(y = ..density.. * 3000),
    color = "#676b75", fill = "#676b75", linewidth = 1, alpha = 0.1) +
  labs(title = "Reading test score distribution",
  y = "Number of people", x = "Score") +
  scale_fill_gradient(low = "#824141", high = "#418252") +
  scale_y_continuous(
    name = "Number of people",
    sec.axis = sec_axis(~ . / 3000, name = "Density")) +
  theme(axis.text = element_text(size = 12)) +
  theme(legend.position = "none",
  plot.title = element_text(size = rel(1.2), hjust = 0.5))

# Writing

scores <- rename(scores, "writing" = "writing score")
writing <- scores |>
    ggplot(aes(x = writing)) +
    geom_histogram(aes(alpha = 0.9, fill =  ..x..)) +
    geom_density(aes(y = ..density.. * 3000),
    color = "#676b75", fill = "#676b75", linewidth = 1, alpha = 0.1) +
  labs(title = "Writing test score distribution",
  y = "Number of people", x = "Score") +
  scale_fill_gradient(low = "#824141", high = "#418252") +
  scale_y_continuous(
    name = "Number of people",
    sec.axis = sec_axis(~ . / 3000, name = "Density")) +
  theme(axis.text = element_text(size = 12)) +
  theme(legend.position = "none",
  plot.title = element_text(size = rel(1.2), hjust = 0.5))

# Creating summary patch

(math + reading + writing) +
  plot_annotation(title = "Comparison of results in three subjects",
  tag_levels = "I",
  theme = theme(plot.title = element_text(size = rel(1.3), hjust = 0.5))) +
  plot_layout(ncol = 3)