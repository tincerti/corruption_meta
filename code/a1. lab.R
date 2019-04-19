################################################################################
# Libraries and Import
################################################################################
rm(list=ls())

# Libraries
library(tidyverse)
library(metafor)

# Data import
load(file="data/meta_results.RData")

################################################################################
# Plot lab results
################################################################################

ggplot(lab, aes(ate_vote, author_reduced)) +
  geom_point(color = "steelblue2", size = 1.5) + 
  geom_errorbarh(aes(y = author_reduced, 
                     xmin = ate_vote - 1.96*se_vote, 
                     xmax = ate_vote + 1.96*se_vote),
                color="grey30", size=0.5, alpha = 0.5, height = 0.05) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_text(aes(label = country, x = 0.25, y = author_reduced), size = 3) +
  xlab("Change in vote share (percentage points)") + 
  scale_x_continuous(limits = c(-.9, 0.3), breaks=seq(-.9,0.3, .1)) +
  theme_classic() +
  theme(axis.title.y=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size = 8)) +
  theme(axis.text.x = element_text(size = 8)) +
  theme(legend.position = "none")

ggsave("figs/lab.pdf", height = 4, width = 6)