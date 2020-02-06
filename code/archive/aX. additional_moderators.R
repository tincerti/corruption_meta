################################################################################
# Libraries and Import
################################################################################
rm(list=ls())

# Libraries
library(tidyverse)
library(readxl)
library(metafor)
library(stargazer)

# Data import - Gallup
meta = read_excel("data/study_results.xlsx")
load(file="data/meta.RData")

################################################################################
# Calculate meta-analytic results
################################################################################
# Calculate confidence intervals for studies with SEs only
meta$ci_lower = with(meta, ifelse(is.na(ci_lower), ate_vote - 1.96*se_vote, ci_lower))
meta$ci_upper = with(meta, ifelse(is.na(ci_upper), ate_vote + 1.96*se_vote, ci_upper))

################################################################################
# Conduct moderator analysis with country as moderator
################################################################################
meta$field = with(meta, ifelse(type == "Field" | type == "Natural", 1, 0))
meta$survey = with(meta, ifelse(type == "Survey", 1, 0))

# Remove lab experiments
meta = meta %>% 
  filter(type == "Field" | type == "Natural" | type == "Survey") %>%
  arrange(ate_vote) %>%
  mutate(author_reduced = reorder(author_reduced, -ate_vote))

# Fixed effects model without moderators
fe = rma.uni(yi = ate_vote, sei = se_vote, data = meta, method = "FE")

# Random effects model without moderators
re = rma(yi = ate_vote, sei = se_vote, data = meta)
het_total = re$tau2 # Estimate of total amount of heterogeneity

# Mixed effects model with survey moderator
meta$country_binary = as.numeric(as.factor(meta$country))
me_mod = rma(yi = ate_vote, sei = se_vote, mods = country_binary, data = meta)
res_het = me_mod$tau2 # Estimate of residual heterogeneity with moderator

# Calculate total heterogeneity accounted for by survey moderator
het_accounted = (het_total - res_het)/het_total
