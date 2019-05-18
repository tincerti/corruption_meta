################################################################################
# Libraries and Import
################################################################################
rm(list=ls())

# Libraries
library(tidyverse)
library(metafor)
library(stargazer)
library(gridExtra)
library(ggplotify)

# Data import
load(file="data/meta_results.RData")

################################################################################
# Funnel Plot Analysis
################################################################################
# Examine number of published articles by field or survey
# with  group
sum = meta %>% 
  group_by(published, type) %>% 
  summarise (number = n())

# Funnel plot for all experiments
re_funnel = funnel(re, 
            back = "grey95", col = "steelblue2",
            digits = c(1,2))

dev.copy(pdf,'figs/funnel_re_all.pdf')
dev.off()

# Funnel plot for field experiments
funnel(re_field, 
       back = "grey95", col = "steelblue2",
       digits = c(1,2))

dev.copy(pdf,'figs/funnel_re_field.pdf')
dev.off()

# Funnel plot for survey experiments
funnel(fe_survey, 
       back = "grey95", col = "steelblue2",
       digits = c(1,2))

funnel(re_survey, 
       back = "grey95", col = "steelblue2",
       digits = c(1,2))

dev.copy(pdf,'figs/funnel_re_survey.pdf')
dev.off()

# Funnel plot with moderator for field experiments
funnel(me_mod, 
          back = "grey95", col = "steelblue2",
          digits = c(1,2))

dev.copy(pdf,'figs/funnel_all_mod.pdf')
dev.off()

# Regression test for funnel plot asymmetry
regtest_re = regtest(re, model = "lm", predictor = "sei")
regtest_mod = regtest(me_mod, model = "lm", predictor = "sei")

regtest_re_survey = regtest(re_survey, model = "lm", predictor = "sei")
regtest_re_field = regtest(re_field, model = "lm", predictor = "sei")

################################################################################
# P-curve: field experiments
################################################################################
# Calculate p-values from point estimates and standard errors
meta$p = with(meta, pnorm(ate_vote/se_vote))

# Calculate percentage of p-values below value
meta$p_threshold = with(meta, ifelse(p < 0.05, .05, NA))
meta$p_threshold = with(meta, ifelse(p < 0.04, .04, p_threshold))
meta$p_threshold = with(meta, ifelse(p < 0.03, .03, p_threshold))
meta$p_threshold = with(meta, ifelse(p < 0.02, .02, p_threshold))
meta$p_threshold = with(meta, ifelse(p < 0.01, .01, p_threshold))

# Keep below 0.05 only
p_curve = meta %>% filter(!is.na(p_threshold) & field == 1)

# Calculate percentages of p-values under values
p_curve = p_curve %>% group_by(p_threshold) %>% summarize(count=n())
p_curve$p_threshold = as.factor(p_curve$p_threshold)
p_curve$percent = p_curve$count / sum(p_curve$count)

# Plot p values
ggplot(p_curve) +
  geom_point(aes(p_threshold, percent, group = 1), 
             color = "steelblue2", size = 1.5) + 
  geom_line(aes(p_threshold, percent, group = 1), color = "grey70") +
  xlab("P-values") + 
  ylab("Share of studies below p-value (%)") + 
  scale_y_continuous(labels = scales::percent, limits = c(0, 1), breaks=seq(0,1,.1)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size = 8)) +
  theme(axis.text.x = element_text(size = 8)) +
  theme(legend.position = "none")

ggsave("figs/pcurve.pdf", height = 4, width = 6)

