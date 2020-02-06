################################################################################
# Libraries and Import
################################################################################
rm(list=ls())

# Libraries
library(tidyverse)
library(metafor)
library(stargazer)
library(meta)
library(dmetar)

# Do not display numeric values in scientific notation
options(scipen = 999)

# Data import
load(file="data/meta_results.RData")

################################################################################
# P-curve: survey experiments
################################################################################
# Restimate meta-analysis with metagen: survey experiments
re_survey_pcurve <- metagen(ate_vote,
                       se_vote,
                       data = survey,
                       studlab = paste(author_reduced),
                       comb.fixed = FALSE,
                       comb.random = TRUE,
                       method.tau = "REML",
                       hakn = TRUE,
                       prediction = TRUE,
                       sm = "SMD")

# Create p-curve 
pcurve(re_survey_pcurve, N = N)

################################################################################
# P-curve: field experiments
################################################################################
# Restimate meta-analysis with metagen: field experiments
re_field_pcurve <- metagen(ate_vote,
                       se_vote,
                       data = field,
                       studlab = paste(author_reduced),
                       comb.fixed = FALSE,
                       comb.random = TRUE,
                       method.tau = "REML",
                       hakn = TRUE,
                       prediction = TRUE,
                       sm = "SMD")

# Create p-curve 
pcurve(re_field_pcurve, N = N)


################################################################################
# P-curve: survey experiments
################################################################################
library(dmetar)
pcurve(re_survey)

# Keep survey experiments with p values below 0.05 only
p_curve = meta %>% filter(p_reported < 0.05 & field == 0)

# Bin p-values by 0.01
p_curve$p_curve = with(p_curve, ifelse(p_reported < 0.01, 0.01, NA))

# Calculate percentages of p-values under values
p_curve = p_curve %>% group_by(p_curve) %>% summarize(count=n())
p_curve$p = as.factor(p_curve$p_curve)
p_curve$percent = p_curve$count / sum(p_curve$count)

# Plot p values
pcurve_survey = 
  ggplot(p_curve) +
  geom_point(aes(p, percent, group = 1), 
             color = "steelblue2", size = 1.5) + 
  geom_line(aes(p, percent, group = 1), color = "grey70") +
  xlab("P-values") + 
  ylab("Share of studies below p-value (%)") + 
  scale_y_continuous(labels = scales::percent, limits = c(0, 1), breaks=seq(0,1,.1)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size = 8)) +
  theme(axis.text.x = element_text(size = 8)) +
  theme(legend.position = "none")

################################################################################
# P-curve: field experiments
################################################################################
# Keep survey experiments with p values below 0.05 only
p_curve = meta %>% filter(p_reported < 0.05 & field == 1)

# Bin p-values by 0.01
p_curve$p_curve = with(p_curve, ifelse(p_reported < 0.05, 0.05, NA))
p_curve$p_curve = with(p_curve, ifelse(p_reported < 0.04, 0.04, p_curve))
p_curve$p_curve = with(p_curve, ifelse(p_reported < 0.03, 0.03, p_curve))
p_curve$p_curve = with(p_curve, ifelse(p_reported < 0.02, 0.02, p_curve))
p_curve$p_curve = with(p_curve, ifelse(p_reported < 0.01, 0.01, p_curve))

# Calculate percentages of p-values under values
p_curve = p_curve %>% group_by(p_curve) %>% summarize(count=n())
p_curve$p = as.numeric(p_curve$p_curve)
p_curve$percent = p_curve$count / sum(p_curve$count)

# Plot p values
pcurve_field = 
  ggplot(p_curve) +
  geom_point(aes(p, percent, group = 1), 
             color = "seagreen3", size = 1.5) + 
  geom_line(aes(p, percent, group = 1), color = "grey70") +
  xlab("P-values") + 
  ylab("Share of studies below p-value (%)") + 
  scale_y_continuous(labels = scales::percent, limits = c(0, 1), breaks=seq(0,1,.1)) +
  scale_x_continuous(breaks = seq(from = 0, to = 0.05, by = 0.01)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size = 8)) +
  theme(axis.text.x = element_text(size = 8)) +
  theme(legend.position = "none")
