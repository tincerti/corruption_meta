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
library(magrittr)
library(DeclareDesign)

# Do not display numeric values in scientific notation
options(scipen = 999)

# Data import
load(file="data/meta_results.RData")

################################################################################
# Add reported p-values for remaining studies
################################################################################
# Arias et al. 
meta$p_reported = with(meta, ifelse(author_reduced == "Arias et al.",
                       2*pt(-abs(ate_vote/se_vote), df = N-1),
                       p_reported))

# Banerjee, Green, Green, and Pande
meta$p_reported = with(meta, ifelse(author_reduced == "Banerjee et al. (2010)",
                       2*pt(-abs(ate_vote/se_vote), df = N-1),
                       p_reported))

# Banerjee, Green, McManus, Pande
meta$p_reported = with(meta, ifelse(author_reduced == "Banerjee et al.",
                       2*pt(-abs(ate_vote/se_vote), df = N-1),
                       p_reported))

# Banerjee, Kumar, Pande & Su
meta$p_reported = with(meta, ifelse(author_reduced == "Banerjee et al. (2011)",
                       0.268,
                       p_reported))

# Boas, Hidalgo, & Melo
meta$p_reported = with(meta, ifelse(author_reduced == "Boas et al.",
                       2*pt(-abs(ate_vote/se_vote), df = N-1),
                       p_reported))

# Buntaine, Jablonski, Nielson & Pickering (Councillor): 
# Note that paper reports 1-tailed test, so recalculating as two-tailed
meta$p_reported = with(meta, ifelse(author_reduced == "Buntain et al. (Councillor)",
                       2*pt(-abs(ate_vote/se_vote), df = N-1),
                       p_reported))

# Buntaine, Jablonski, Nielson & Pickering (Chair)
meta$p_reported = with(meta, ifelse(author_reduced == "Buntain et al. (Chair)",
                       0.754,
                       p_reported))

# Chong, De La O, Karlan & Leonard Wantchekon
meta$p_reported = with(meta, ifelse(author_reduced == "Chong et al.",
                       2*pt(-abs(ate_vote/se_vote), df = N-1),
                       p_reported))

# De Figueiredo, Hidalgo, & Kasahara (DEM/PFL)
meta$p_reported = with(meta, ifelse(author_reduced == "De Figueiredo et al. (DEM/PFL)",
                       2*pt(-abs(ate_vote/se_vote), df = N-1),
                       p_reported))

# De Figueiredo, Hidalgo, & Kasahara (PT)
meta$p_reported = with(meta, ifelse(author_reduced == "De Figueiredo et al. (PT)",
                       2*pt(-abs(ate_vote/se_vote), df = N-1),
                       p_reported))

# Ferraz and Finan
meta$p_reported = with(meta, ifelse(author_reduced == "Ferraz and Finan",
                       2*pt(-abs(ate_vote/se_vote), df = N-1),
                       p_reported))

################################################################################
# Test if publication predicts reported p-values
################################################################################
# Group by major significance level
meta$p_siglevel = with(meta, ifelse(p_reported > 0.10, ">0.10", NA))
meta$p_siglevel = with(meta, ifelse(p_reported < 0.10, "<0.10", p_siglevel))
meta$p_siglevel = with(meta, ifelse(p_reported < 0.05, "<0.05", p_siglevel))
meta$p_siglevel = with(meta, ifelse(p_reported < 0.01, "<0.01", p_siglevel))

# OLS
p_ols = lm(published ~ p_siglevel, data = meta)

# Logit
p_logit = glm(published ~ p_siglevel, data = meta, family = "binomial")

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
          column.labels = c("OLS", "Logit"),
          covariate.labels = c("Reference: P less than 0.01", "P less than 0.05", 
                               "P less than 0.1", "P greater than 0.1"),
          dep.var.labels = c("Published", "Published"), 
          digits = 2,
          no.space=TRUE,
          keep.stat = c("n"))

################################################################################
# P-curve
################################################################################
#### All experiments ####
# Restimate meta-analysis with metagen: all experiments
re_all_pcurve <- metagen(ate_vote,
                         se_vote,
                         data = meta,
                         studlab = paste(author_reduced),
                         comb.fixed = FALSE,
                         comb.random = TRUE,
                         method.tau = "REML",
                         hakn = TRUE,
                         prediction = TRUE,
                         sm = "SMD")

# Create visual p-curve
pcurve(re_all_pcurve)

# Save p-curve
dev.copy(pdf,'figs/p_curve_all.pdf', width=12, height=7)
dev.off()

#### Survey experiments ####
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

# Save p-curve
dev.copy(pdf,'figs/p_curve_survey.pdf', width=12, height=7)
dev.off()

#### Field experiments ####
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

# Save p-curve
dev.copy(pdf,'figs/p_curve_field.pdf', width=12, height=7)
dev.off()

################################################################################
# Funnel Plot Analysis
################################################################################
# Funnel plot for all experiments
re_funnel = funnel(re, 
            back = "grey95", col = "steelblue2",
            digits = c(1,2))

dev.copy(pdf,'figs/funnel_re_all.pdf', width=10, height=7)
dev.off()

# Funnel plot for field experiments
funnel(re_field, 
       back = "grey95", col = "steelblue2",
       digits = c(1,2))

dev.copy(pdf,'figs/funnel_re_field.pdf', width=10, height=7)
dev.off()

# Funnel plot for survey experiments
funnel(re_survey, 
       back = "grey95", col = "steelblue2",
       digits = c(1,2))

dev.copy(pdf,'figs/funnel_re_survey.pdf', width=10, height=7)
dev.off()

# Funnel plot with moderator for field experiments
funnel(me_mod, 
          back = "grey95", col = "steelblue2",
          digits = c(1,2))

dev.copy(pdf,'figs/funnel_all_mod.pdf', width=10, height=7)
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
# Trim and fill
################################################################################
# Perform trim and fill analysis
trimfill_all = trimfill(re) # Estimates increase compared to random effects
trimfill_field = trimfill(re_field) # Estimates unchanged
trimfill_survey = trimfill(re_survey) # Estimates unchanged

# Export funnel plot with trimfill method
funnel(trimfill_all, 
          back = "grey95", col = "steelblue2",
          digits = c(1,2))

dev.copy(pdf,'figs/funnel_trimfill.pdf', width=10, height=7)
dev.off()

# Export tables of trim and fill estimates 
# Create row labels
Value = 
  c("All experiments: random effects ", 
    "",
    "Field: random effects", 
    "",
    "Survey: random effects", 
    "")

# Populate rows with point estimates and standard errors
Estimate = 
  c(round(trimfill_all$beta[1], 3), 
    paste0("(", format(unlist(round(trimfill_all$se, 3))),")"), 
    round(trimfill_field$beta[1], 3), 
    paste0("(", format(unlist(round(trimfill_field$se, 3))),")"),
    round(trimfill_survey$beta[1], 3), 
    paste0("(", format(unlist(round(trimfill_survey$se, 3))),")"))

# Create confidence intervals
`95% CI` = 
  c(paste0(round(trimfill_all$ci.lb, 3), " to ", round(trimfill_all$ci.ub, 3)),
    "", 
    paste0(round(trimfill_field$ci.lb, 3), " to ", round(trimfill_field$ci.ub, 3)),
    "",
    paste0(round(trimfill_survey$ci.lb, 3), " to ", round(trimfill_survey$ci.ub, 3)),
    "")

# Combine into dataframe
trimfill_df = data.frame(Value, Estimate, `95% CI`, check.names = FALSE)

# Export using stargazer
stargazer(trimfill_df,
          out = "figs/trimfill.tex",
          title= "Trim and fill estimates by subgroup",
          label = "trimfill",
          digits = 3,
          column.sep.width = "30pt",
          rownames = FALSE, 
          summary = FALSE,
          notes = "\\parbox[t]{\\textwidth}{\\footnotesize \\textit{Note:} Standard errors in parenthesis. Figures rounded to nearest thousandth decimal place.}"
          )

################################################################################
# PET-PEESE
################################################################################
# Create variance variables
meta$var_vote = meta$se_vote^2
field$var_vote = field$se_vote^2
survey$var_vote = survey$se_vote^2

# Create PET-PEESE function
petpeese <- function(dataset) {
  pet = lm(ate_vote ~ se_vote, weight = 1/var_vote, data = dataset)
  peese = lm(ate_vote ~ var_vote, weight = 1/var_vote, data = dataset) 
  int_pet = pet$coefficients[1]
  se_pet = summary(pet)$coefficients[1,2]
  int_peese = peese$coefficients[1]
  se_peese = summary(peese)$coefficients[1,2]
  p_pet = summary(pet)$coefficients[1,4]
  petpeese_int = ifelse(p_pet > .05, int_pet, int_peese)
  petpeese_se = ifelse(p_pet > .05, se_pet, se_peese)
  return(c(petpeese_int, petpeese_se))
}

# Calculate PET-PEESE estimates and standard errors
petpeese_all_int = petpeese(meta)[1]
petpeese_all_se = petpeese(meta)[2]
petpeese_field_int = petpeese(field)[1]
petpeese_field_se = petpeese(field)[2]
petpeese_survey_int = petpeese(survey)[1]
petpeese_survey_se = petpeese(survey)[2]

# Define categories
Value = 
  c("All experiments", 
    "",
    "Field experiments", 
    "",
    "Survey experiments", 
    "")

# Populate rows with point estimates and standard errors
Estimate = 
  c(round(petpeese_all_int, 3), 
    paste0("(", round(petpeese_all_se, 3),")"), 
    round(petpeese_field_int, 3), 
    paste0("(", round(petpeese_field_se, 3),")"),
    round(petpeese_survey_int, 3), 
    paste0("(", round(petpeese_survey_se, 3),")"))

# Create confidence intervals
`95% CI` = 
  c(paste0(round(petpeese_all_int - 1.96 * petpeese_all_se, 3), " to ", 
           round(petpeese_all_int + 1.96 * petpeese_all_se, 3)), 
    "", 
    paste0(round(petpeese_field_int - 1.96 * petpeese_field_se, 3), " to ", 
           round(petpeese_field_int + 1.96 * petpeese_field_se, 3)),
    "",
    paste0(round(petpeese_survey_int - 1.96 * petpeese_survey_se, 3), " to ", 
           round(petpeese_survey_int + 1.96 * petpeese_survey_se, 3)),
    "")

# Combine into dataframe
petpeese_df = data.frame(Value, Estimate, `95% CI`, check.names = FALSE)

# Export using stargazer
stargazer(petpeese_df,
          out = "figs/petpeese.tex",
          title= "PET-PEESE estimates by subgroup",
          label = "petpeese",
          digits = 3,
          column.sep.width = "30pt",
          rownames = FALSE, 
          summary = FALSE,
          notes = "\\parbox[t]{\\textwidth}{\\footnotesize \\textit{Note:} Standard errors in parenthesis. Figures rounded to nearest thousandth decimal place.}"
          )
  
################################################################################
# Create table of all p-values
################################################################################
# Clean and compile data frame
meta$p_reported = as.numeric(meta$p_reported)
meta$p_reported = sprintf("%.3f", round(meta$p_reported, 3))
meta$p_replicated = as.numeric(meta$p_replicated)
meta$p_replicated = with(meta, ifelse(!is.na(p_replicated),
                         sprintf("%.3f", round(p_replicated, 3)),
                         ""))
                         
# Clean and compile data frame
p_table = meta %>% 
  select(author_reduced, type, published, p_reported, p_replicated) %>%
  arrange(p_reported) %>%
  mutate(author_reduced = as.character(author_reduced)) %>%
  rename(Study = author_reduced, 
         Published = published,
         `Experiment Type` = type,
         `Reported p-value` = p_reported,
         `Replicated p-value` = p_replicated)

# Clean strings for export
p_table$Study = gsub("&", "and", p_table$Study)
  
# Output table using stargazer
stargazer(p_table,
          out = "figs/p_values_all.tex",
          title= "P-values by study",
          label = "p_study",
          digits = 3,
          rownames = FALSE, 
          summary = FALSE
          )