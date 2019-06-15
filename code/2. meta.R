################################################################################
# Libraries and Import
################################################################################
rm(list=ls())

# Libraries
library(tidyverse)
library(readxl)
library(metafor)

# Data import - Gallup
meta = read_excel("data/study_results.xlsx")
load(file="data/meta.RData")

################################################################################
# Calculate meta-analytic results
################################################################################
# Calculate confidence intervals for studies with SEs only
meta$ci_lower = with(meta, ifelse(is.na(ci_lower), ate_vote - 1.96*se_vote, ci_lower))
meta$ci_upper = with(meta, ifelse(is.na(ci_upper), ate_vote + 1.96*se_vote, ci_upper))

# Keep field experiments only
field = meta %>% 
  filter(type == "Field" | type == "Natural") %>%
  arrange(ate_vote) %>%
  mutate(author_reduced = reorder(author_reduced, -ate_vote))

# Remove Banarjee papers
#field = field %>% filter(author_reduced != "Banerjee et al. (2011)" & 
#                         author_reduced != "Banerjee et al. (2010)")

# Keep survey experiments only
survey = meta %>% 
  filter(type == "Survey") %>%
  arrange(ate_vote) %>%
  mutate(author_reduced = reorder(author_reduced, -ate_vote))

# Random effects model
re_field = rma.uni(yi = ate_vote, sei = se_vote, data = field)
re_survey = rma.uni(yi = ate_vote, sei = se_vote, data = survey)

# Fixed effects model: unweighted
fe_field = rma.uni(yi = ate_vote, sei = se_vote, weighted = "FALSE",
                   method = "FE", data = field)

fe_survey = rma.uni(yi = ate_vote, sei = se_vote, weighted = "FALSE",
                   method = "FE", data = survey)

# Fixed effects model: weighted
fe_field = rma.uni(yi = ate_vote, sei = se_vote, weights = N,
                   method = "FE", data = field)

fe_survey = rma.uni(yi = ate_vote, sei = se_vote, weights = N,
                   method = "FE", data = survey)

# Normality test
shapiro.test(field$ate_vote) 
mean(field$ate_vote)

################################################################################
# Conduct moderator analysis with type of study as moderator
################################################################################
meta$field = with(meta, ifelse(type == "Field" | type == "Natural", 1, 0))
meta$survey = with(meta, ifelse(type == "Survey", 1, 0))

# Save lab experiments
lab = meta %>% 
  filter(type == "Lab") %>%
  arrange(ate_vote) %>%
  mutate(author_reduced = reorder(author_reduced, -ate_vote))

# Remove lab experiments
meta = meta %>% 
  filter(type == "Field" | type == "Natural" | type == "Survey") %>%
  arrange(ate_vote) %>%
  mutate(author_reduced = reorder(author_reduced, -ate_vote))

# Fixed effects model without moderators
fe = rma.uni(yi = ate_vote, sei = se_vote, data = meta, method = "FE")

# Random effects model without moderators
re = rma(yi = ate_vote, sei = se_vote, data = meta)
het_all = re$tau2

# Mixed effects model with survey moderator
me_mod = rma(yi = ate_vote, sei = se_vote, mods = survey, data = meta)
het_mod = me_mod$tau2

# Calculate residual heterogeneity
res_het = (het_all - het_mod)/het_all
pred = predict(me_mod, newmods = cbind(seq(from = 0, to = 1, by = 1), 0), addx = TRUE)

################################################################################
# Save information for plotting
################################################################################
save(meta, field, survey, lab, 
     fe, re, me_mod, fe_field, fe_survey, re_field, re_survey, 
     file = "data/meta_results.RData")