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
# Calculate field meta-analytic results without Banerjee studies
################################################################################
# Calculate confidence intervals for studies with SEs only
meta$ci_lower = with(meta, ifelse(is.na(ci_lower), ate_vote - 1.96*se_vote, ci_lower))
meta$ci_upper = with(meta, ifelse(is.na(ci_upper), ate_vote + 1.96*se_vote, ci_upper))

# Keep field experiments and remove Banarjee papers
field = meta %>% 
  filter(type == "Field" | type == "Natural") %>%
  arrange(ate_vote) %>%
  mutate(author_reduced = reorder(author_reduced, -ate_vote)) %>%
  filter(author_reduced != "Banerjee et al. (2011)" & 
         author_reduced != "Banerjee et al. (2010)")

# Random effects model
re_field = rma.uni(yi = ate_vote, sei = se_vote, data = field)

# Fixed effects model
fe_field = rma.uni(yi = ate_vote, sei = se_vote, weighted = TRUE,
                   method = "FE", data = field)

################################################################################
# Export table: field without Banerjee studies
################################################################################
# Create row labels
Value = c("Field: weighted fixed effects ", "" ,
          "Field: random effects", "")

# Populate rows with point estimates and standard errors
Estimate = c(round(fe_field$beta[1], 3), 
             paste0("(", format(unlist(round(fe_field$se, 3))),")"), 
             round(re_field$beta[1], 3), 
             paste0("(", format(unlist(round(re_field$se, 3))),")"))

# Create confidence intervals
`95% CI` = 
  c(paste0(round(fe_field$ci.lb, 3), " to ", round(fe_field$ci.ub, 3)),
    "", 
    paste0(round(re_field$ci.lb, 3), " to ", round(re_field$ci.ub, 3)),
    "")

# Combine into dataframe
meta_type = data.frame(Value, Estimate, `95% CI`, check.names = FALSE)

# Export using stargazer
stargazer(meta_type,
          out = "figs/meta_estimates_no_banerjee.tex",
          title= "Meta-analysis (all field experiments excluding \\citet{banerjee2010can} and \\citet{banerjee2011informed})",
          label = "meta_no_banerjee",
          digits = 3,
          column.sep.width = "30pt",
          rownames = FALSE, 
          summary = FALSE,
          notes = "\\parbox[t]{\\textwidth}{\\footnotesize \\textit{Note:} Standard errors in parenthesis. Figures rounded to nearest thousandth decimal place.}"
          )

################################################################################
# Plot field results
################################################################################
# Add meta-anlaysis parameters
ate_vote_fe = coef(fe_field)
se_vote_fe = summary(fe_field)$se
ate_vote_re = coef(re_field)
se_vote_re = summary(re_field)$se

meta_fe = data.frame(type="Field", year=NA, author = "Fixed effects model", 
                     author_reduced = "Fixed effects model", country = NA, 
                     ate_vote = ate_vote_fe, se_vote = se_vote_fe, ci_upper = NA, 
                     ci_lower = NA, p_reported = NA, p_replicated = NA,
                     published = NA, N = NA, Notes = NA)

meta_re = data.frame(type="Field", year=NA, author = "Random effects model", 
                     author_reduced = "Random effects model", country = NA, 
                     ate_vote = ate_vote_re, se_vote = se_vote_re, ci_upper = NA, 
                     ci_lower = NA, p_reported = NA, p_replicated = NA,
                     published = NA, N = NA, Notes = NA)

field = rbind(field, meta_fe, meta_re)

# Re-order factor levels
field$author_reduced = fct_relevel(field$author_reduced, "Fixed effects model", after = 0)
field$author_reduced = fct_relevel(field$author_reduced, "Random effects model", after = 0)

# Mutliply effect size by 100
field$ate_vote = field$ate_vote*100
field$se_vote = field$se_vote*100

# Plot field results
ggplot(field, aes(ate_vote, author_reduced)) +
  geom_point(color = "seagreen3", size = 1.5) + 
  geom_point(data = subset(field, 
             author_reduced == "Fixed effects model" | 
             author_reduced == "Random effects model"), 
           size = 1.5, color = "black", fill = "black") +
  geom_errorbarh(aes(y = author_reduced, 
                     xmin = ate_vote - 1.96*se_vote, 
                     xmax = ate_vote + 1.96*se_vote),
                color="grey30", size=0.5, alpha = 0.5, height = 0.2) +
  geom_text(aes(label = country, x = 25, y = author_reduced), size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 2.5, linetype = "solid") +
  xlab("Change in vote share (percentage points)") + 
  scale_x_continuous(limits = c(-70, 30), breaks=seq(-70,30, 10)) +
  theme_classic() +
  theme(axis.title.y=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size = 8)) +
  theme(axis.text.x = element_text(size = 8)) +
  theme(legend.position = "none")

ggsave("figs/field_no_banerjee.pdf", height = 3.5, width = 6)

################################################################################
# Calculate survey meta-analytic results with De Figueiredo study
################################################################################
# Keep survey experiments only
survey = meta %>% filter(type == "Survey")

# Add De Figueiredo et al. to survey experiments
defig = data.frame(type="Survey", year = 2011, 
                   author = "De Figueiredo, Hidalgo, & Kasahara", 
                   author_reduced = "De Figueiredo et al.", country = "Brazil", 
                   ate_vote = 0.01, se_vote = NA, ci_upper = 0.07, 
                   ci_lower = -0.07, p_reported = 0.42, published = 0, N = 133, 
                   Notes = NA)

survey = rbind(survey, defig)

# Rearrange data set 
survey = survey %>% arrange(ate_vote) %>%
  mutate(author_reduced = reorder(author_reduced, -ate_vote))

# Calculate SEs for De Figueiredo et al. 
survey$se_vote = with(survey, ifelse(is.na(se_vote), 
                                     (ate_vote - ci_lower)/1.96, se_vote))

# Random effects
re_survey = rma.uni(yi = ate_vote, sei = se_vote, data = survey)

# Fixed effects model: weighted
fe_survey = rma.uni(yi = ate_vote, sei = se_vote, weighted = TRUE,
                   method = "FE", data = survey)

################################################################################
# Export table with De Figueiredo study
################################################################################
# Create row labels
Value = c("Survey: weighted fixed effects ", "" ,
          "Survey: random effects", "")

# Populate rows with point estimates and standard errors
Estimate = c(round(fe_survey$beta[1], 3), 
             paste0("(", format(unlist(round(fe_survey$se, 3))),")"), 
             round(re_survey$beta[1], 3), 
             paste0("(", format(unlist(round(re_survey$se, 3))),")"))

# Create confidence intervals
`95% CI` = 
  c(paste0(round(fe_survey$ci.lb, 3), " to ", round(fe_survey$ci.ub, 3)),
    "", 
    paste0(round(re_survey$ci.lb, 3), " to ", round(re_survey$ci.ub, 3)),
    "")

# Create dataframe
meta_type = data.frame(Value, Estimate, `95% CI`, check.names = FALSE)

# Export using stargazer
stargazer(meta_type,
          out = "figs/meta_estimates_defig.tex",
          title= "Meta-analysis (all survey experiments including \\citet{de2011voters}",
          label = "meta_survey_defig",
          digits = 3,
          column.sep.width = "30pt",
          rownames = FALSE, 
          summary = FALSE,
          notes = "\\parbox[t]{\\textwidth}{\\footnotesize \\textit{Note:} Standard errors in parenthesis. Figures rounded to nearest thousandth decimal place.}"
          )

################################################################################
# Plot survey results with De Figeureido
################################################################################
# Add meta-anlaysis parameters
ate_vote_fe = coef(fe_survey)
se_vote_fe = summary(fe_survey)$se
ate_vote_re = coef(re_survey)
se_vote_re = summary(re_survey)$se

meta_fe = data.frame(type="Survey", year=NA, author = "Fixed effects model", 
                     author_reduced = "Fixed effects model", country = NA, 
                     ate_vote = ate_vote_fe, se_vote = se_vote_fe, ci_upper = NA, 
                     ci_lower = NA, p_reported = NA, p_replicated = NA,
                     published = NA, N = NA, Notes = NA)

meta_re = data.frame(type="Survey", year=NA, author = "Random effects model", 
                  author_reduced = "Random effects model", country = NA, 
                  ate_vote = ate_vote_re, se_vote = se_vote_re, ci_upper = NA, 
                  ci_lower = NA, p_reported = NA, p_replicated = NA,
                  published = NA, N = NA,  Notes = NA)

survey = rbind(survey, meta_fe, meta_re)

# Re-order factor levels
survey$author_reduced = fct_relevel(survey$author_reduced, "Fixed effects model", after = 0)
survey$author_reduced = fct_relevel(survey$author_reduced, "Random effects model", after = 0)

# Mutliply effect size by 100
survey$ate_vote = survey$ate_vote*100
survey$se_vote = survey$se_vote*100

# Plot survey results - with De Figueriedo
ggplot(survey, aes(ate_vote, author_reduced)) +
  geom_point(color = "steelblue2", size = 1.5) + 
  geom_point(data = subset(survey, 
             author_reduced == "Fixed effects model" | 
             author_reduced == "Random effects model"), 
           size = 1.5, color = "black", fill = "black") +
  geom_errorbarh(aes(y = author_reduced, 
                     xmin = ate_vote - 1.96*se_vote, 
                     xmax = ate_vote + 1.96*se_vote),
                color="grey30", size=0.5, alpha = 0.5, height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 2.5, linetype = "solid") +
  geom_text(aes(label = country, x = 25, y = author_reduced), size = 3) +
  xlab("Change in vote share (percentage points)") + 
  scale_x_continuous(limits = c(-70, 30), breaks=seq(-70,30, 10)) +
  theme_classic() +
  theme(axis.title.y=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size = 8)) +
  theme(axis.text.x = element_text(size = 8)) +
  theme(legend.position = "none")

ggsave("figs/survey_defig.pdf", height = 4, width = 6)

################################################################################
# Conduct moderator analysis excluding Banerjee et. al studies
################################################################################
meta$field = with(meta, ifelse(type == "Field" | type == "Natural", 1, 0))
meta$survey = with(meta, ifelse(type == "Survey", 1, 0))

# Remove lab experiments and Banarjee studies
meta = meta %>% 
  filter(type == "Field" | type == "Natural" | type == "Survey") %>%
  filter(author_reduced != "Banerjee et al. (2011)" & 
         author_reduced != "Banerjee et al. (2010)") %>%
  arrange(ate_vote) %>%
  mutate(author_reduced = reorder(author_reduced, -ate_vote))

# Fixed effects model without moderators
fe = rma.uni(yi = ate_vote, sei = se_vote, weighted = TRUE, 
             data = meta, method = "FE")

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
# Export models: moderator analysis excluding Banerjee et. al studies
################################################################################
# Create row labels
Value = c("Estimate",
          "",
          "Estimated total heterogeneity", 
          "")

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
          out = "figs/re_out_no_banerjee.tex",
          title= "Random effects meta-analysis (all studies excluding \\citet{banerjee2010can} and \\citet{banerjee2011informed})",
          label = "re_model_no_banerjee",
          digits = 3,
          column.sep.width = "30pt",
          rownames = FALSE, 
          summary = FALSE,
          notes = "\\parbox[t]{\\textwidth}{\\footnotesize \\textit{Note:} Standard errors in parenthesis. Figures rounded to nearest thousandth decimal place.}"
          )

################################################################################
# Export models: moderated model excluding Banerjee et. al studies
################################################################################
# Create row labels
Value = c("Constant", "" ,
          "Survey experiment moderator", "",
          "Residual heterogenity with moderator", "",
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
          out = "figs/me_mod_out_no_banerjee.tex",
          title= "Mixed effects meta-analysis with survey experiment moderator (excluding \\citet{banerjee2010can} and \\citet{banerjee2011informed})",
          label = "me_mod_no_banerjee",
          digits = 3,
          rownames = FALSE, 
          summary = FALSE,
          notes = "\\parbox[t]{\\textwidth}{\\footnotesize \\textit{Note:} Standard errors in parenthesis. Figures rounded to nearest thousandth decimal place.}"
          )