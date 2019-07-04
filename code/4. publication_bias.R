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
library(glmnet)

# Do not display numeric values in scientific notation
options(scipen = 999)

# Data import
load(file="data/meta_results.RData")

################################################################################
# Test if publication predicts reported p-values
################################################################################
# Converted reported p_values to numeric
meta$p = gsub("<", '', meta$p_reported)
meta$p = gsub(">", '', meta$p)
meta$p = as.character(meta$p)

# OLS
p_ols = lm(published ~ p_reported, data = meta)

# Logit
p_logit = glm(published ~ p_reported, data = meta, family = "binomial")

# Output regression table
stargazer(p_ols, p_logit,
          out = "figs/published.tex", 
          title= "Do p-values predict publication status?",
          label = "p_publication",
          align=TRUE, 
          dep.var.caption = NULL,
          model.names = FALSE,
          model.numbers = FALSE, 
          multicolumn = TRUE,
          intercept.bottom = FALSE,
          #intercept.top = TRUE,
          column.labels = c("OLS", "Logit"),
          covariate.labels = c("Reference: P less than 0.01", "P less than 0.05", 
                               "P less than 0.1", "P greater than 0.1"),
          dep.var.labels = c("Published", "Published"), 
          digits = 2,
          no.space=TRUE,
          keep.stat = c("n"))

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

# Regression tests for funnel plot asymmetry
regtest_re = regtest(re, model = "lm", predictor = "sei")
regtest_mod = regtest(me_mod, model = "lm", predictor = "sei")
regtest_re_survey = regtest(re_survey, model = "lm", predictor = "sei")
regtest_re_field = regtest(re_field, model = "lm", predictor = "sei")

# Regression tests for funnel plot asymmetry: export table
studies = c("All", "All with moderator", "Field", "Survey")
pvalue = c(regtest_re$pval, regtest_mod$pval, regtest_re_field$pval,
                       regtest_re_survey$pval)

regtest = data.frame(studies, pvalue)
regtest = regtest %>% rename("Studies included" = studies, "p value" = pvalue)

stargazer(regtest, summary = FALSE, rownames = FALSE,
          out = "figs/regtest_funnel.tex", 
          title= "Regression tests for funnel plot asymmetry",
          label = "tab: funnel",
          column.sep.width = "5cm")

################################################################################
# P-curve: survey experiments
################################################################################
# Keep p values below 0.05 only
meta$p_curve = with(meta, ifelse(grepl(">", meta$p_reported) == TRUE, NA, p))
p_curve = meta %>% filter(!is.na(p_curve) & field == 0)

# Calculate percentages of p-values under values
p_curve = p_curve %>% group_by(p_curve) %>% summarize(count=n())
p_curve$p = as.factor(p_curve$p_curve)
p_curve$percent = p_curve$count / sum(p_curve$count)

# Plot p values
ggplot(p_curve) +
  geom_point(aes(p_curve, percent, group = 1), 
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
p_curve = meta %>% filter(!is.na(p_curve) & field == 1)

# Calculate percentages of p-values under values
p_curve = p_curve %>% group_by(p_curve) %>% summarize(count=n())
p_curve$p = as.factor(p_curve$p_curve)
p_curve$percent = p_curve$count / sum(p_curve$count)

# Plot p values
ggplot(p_curve) +
  geom_point(aes(p_curve, percent, group = 1), 
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
