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

# Random effects model
re_field = rma.uni(yi = ate_vote, sei = se_vote, data = field)
re_survey = rma.uni(yi = ate_vote, sei = se_vote, data = survey)

# Fixed effects model
fe_field = rma.uni(yi = ate_vote, sei = se_vote, weighted = TRUE,
                   method = "FE", data = field)

fe_survey = rma.uni(yi = ate_vote, sei = se_vote, weighted = TRUE,
                   method = "FE", data = survey)

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
het_total = re$tau2 # Estimate of total amount of heterogeneity

# Mixed effects model with survey moderator
me_mod = rma(yi = ate_vote, sei = se_vote, mods = survey, data = meta)
res_het = me_mod$tau2 # Estimate of residual heterogeneity with moderator

# Calculate total heterogeneity accounted for by survey moderator
het_accounted = (het_total - res_het)/het_total
het_accounted = sapply(het_accounted, 
                       function(x)paste0(round(x*100, 3),"%",collapse="%"))

################################################################################
# Export models: primary results
################################################################################
# Create row labels
Value = 
  c("Field: weighted fixed effects ", 
    "" ,
    "Field: random effects", 
    "" ,
    "Survey: weighted fixed effects ", 
    "" ,
    "Survey: random effects", "")

# Populate rows with point estimates and standard errors
Estimate = 
  c(round(fe_field$beta[1], 3), 
    paste0("(", format(unlist(round(fe_field$se, 3))),")"), 
    round(re_field$beta[1], 3), 
    paste0("(", format(unlist(round(re_field$se, 3))),")"),
    round(fe_survey$beta[1], 3), 
    paste0("(", format(unlist(round(fe_survey$se, 3))),")"), 
    round(re_survey$beta[1], 3), 
    paste0("(", format(unlist(round(re_survey$se, 3))),")"))

# Create confidence intervals
`95% CI` = 
  c(paste0(round(fe_field$ci.lb, 3), " to ", round(fe_field$ci.ub, 3)),
    "", 
    paste0(round(re_field$ci.lb, 3), " to ", round(re_field$ci.ub, 3)),
    "",
    paste0(round(fe_survey$ci.lb, 3), " to ", round(fe_survey$ci.ub, 3)),
    "", 
    paste0(round(re_survey$ci.lb, 3), " to ", round(re_survey$ci.ub, 3)),
    "")

# Combine into dataframe
meta_type = data.frame(Value, Estimate, `95% CI`, check.names = FALSE)

# Export using stargazer
stargazer(meta_type,
          out = "figs/meta_estimates.tex",
          title= "Meta-analysis by type of experiment",
          label = "meta_type",
          digits = 3,
          column.sep.width = "30pt",
          rownames = FALSE, 
          summary = FALSE,
          notes = "\\parbox[t]{\\textwidth}{\\footnotesize \\textit{Note:} Standard errors in parenthesis. Figures rounded to nearest thousandth decimal place.}"
          )

################################################################################
# Export models: random effects model without moderator
################################################################################
# Create row labels
Value = c("Estimate", "" , "Estimated total heterogeneity", "")

# Populate rows with point estimates and standard errors
se = paste0("(", format(unlist(round(re$se, 3))),")")
se.tau2 = paste0("(", format(unlist(round(re$se.tau2, 3))),")")

Estimate = c(round(re$beta[1], 3), 
             se[1], 
             round(re$tau2[1], 3), 
             se.tau2)

# Create confidence intervals
`95% CI` = 
  c(paste0(round(re$ci.lb, 3), " to ", round(re$ci.ub, 3)),
    "",
    paste0(round(re$tau2 - (1.96 * re$se.tau2), 3), " to ", 
           round(re$tau2 + (1.96 * re$se.tau2), 3)),
    "")

# Combine into dataframe
re_out = data.frame(Value, Estimate, `95% CI`, check.names = FALSE)

# Export using stargazer
stargazer(re_out,
          out = "figs/re_out.tex",
          title= "Random effects meta-analysis (all studies)",
          label = "re_model",
          digits = 3,
          column.sep.width = "30pt",
          rownames = FALSE, 
          summary = FALSE,
          notes = "\\parbox[t]{\\textwidth}{\\footnotesize \\textit{Note:} Standard errors in parenthesis. Figures rounded to nearest thousandth decimal place.}"
          )

################################################################################
# Export models: moderated model
################################################################################
# Create row labels
Value = 
  c("Constant", 
    "" ,
    "Survey experiment moderator", 
    "",
    "Residual heterogenity with moderator", 
    "",
    "Heterogenity accounted for", "")

# Populate rows with point estimates and standard errors
se = paste0("(", format(unlist(round(me_mod$se, 3))),")")
se.tau2 = paste0("(", format(unlist(round(me_mod$se.tau2, 3))),")")

Estimate = c(round(me_mod$beta[1], 3), se[1], 
             round(me_mod$beta[2], 3), se[2],
             round(me_mod$tau2[1], 3), se.tau2,
             het_accounted, "")

# Create confidence intervals
`95% CI` = 
  c(paste0(round(me_mod$ci.lb[1], 3), " to ", round(me_mod$ci.ub[1], 3)),
    "",
    paste0(round(me_mod$ci.lb[2], 3), " to ", round(me_mod$ci.ub[2], 3)),
    "",
    paste0(round(me_mod$tau2 - (1.96 * me_mod$se.tau2), 3), " to ", 
           round(me_mod$tau2 + (1.96 * me_mod$se.tau2), 3)),
    "",
    "",
    "")

# Combine into dataframe
me_mod_out = data.frame(Value, Estimate, `95% CI`, check.names = FALSE)

# Export using stargazer
stargazer(me_mod_out,
          out = "figs/me_mod_out.tex",
          title= "Mixed effects meta-analysis with survey experiment moderator",
          label = "me_mod",
          digits = 3,
          rownames = FALSE, 
          summary = FALSE,
          notes = "\\parbox[t]{\\textwidth}{\\footnotesize \\textit{Note:} Standard errors in parenthesis. Figures rounded to nearest thousandth decimal place.}"
          )

################################################################################
# Save information for plotting
################################################################################
save(meta, field, survey, lab, 
     fe, re, me_mod, fe_field, fe_survey, re_field, re_survey, 
     file = "data/meta_results.RData")