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

# Keep field experiments only
field = meta %>% 
  filter(type == "Field" | type == "Natural") %>%
  arrange(ate_vote) %>%
  mutate(author_reduced = reorder(author_reduced, -ate_vote))

# Keep survey experiments only
survey = meta %>% 
  filter(type == "Survey") %>%
  arrange(ate_vote) %>%
  mutate(author_reduced = reorder(author_reduced, -ate_vote))

# Random effects model - field experiments with different tao estimators
re_field_reml = rma.uni(yi = ate_vote, sei = se_vote, method = "REML" ,data = field)
re_field_hs = rma.uni(yi = ate_vote, sei = se_vote, method = "HS" ,data = field)
re_field_dl = rma.uni(yi = ate_vote, sei = se_vote, method = "DL" ,data = field)
re_field_sj = rma.uni(yi = ate_vote, sei = se_vote, method = "SJ" ,data = field)
re_field_ml = rma.uni(yi = ate_vote, sei = se_vote, method = "ML" ,data = field)
re_field_eb = rma.uni(yi = ate_vote, sei = se_vote, method = "EB" ,data = field)

# Random effects model - survey experiments with different tao estimators
re_survey_reml = rma.uni(yi = ate_vote, sei = se_vote, method = "REML" ,data = survey)
re_survey_hs = rma.uni(yi = ate_vote, sei = se_vote, method = "HS" ,data = survey)
re_survey_dl = rma.uni(yi = ate_vote, sei = se_vote, method = "DL" ,data = survey)
re_survey_sj = rma.uni(yi = ate_vote, sei = se_vote, method = "SJ" ,data = survey)
re_survey_ml = rma.uni(yi = ate_vote, sei = se_vote, method = "ML" ,data = survey)
re_survey_eb = rma.uni(yi = ate_vote, sei = se_vote, method = "EB" ,data = survey)

