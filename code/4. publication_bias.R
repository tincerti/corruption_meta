################################################################################
# Libraries and Import
################################################################################
rm(list=ls())

# Libraries
library(tidyverse)
library(metafor)
library(stargazer)

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

# Buntaine, Jablonski, Nielson & Pickering (Councillor)
meta$p_reported = with(meta, ifelse(author_reduced == "Buntain et al. (Councillor)",
                       0.015,
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
# Trim and fill
################################################################################
# Perform trim and fill analysis
trimfill_all = trimfill(re)
funnel(trimfill)
trimfill_field = trimfill(re_field)
trimfill_survey = trimfill(re_survey)

# Export funnel plot with trimfill method
funnel(trimfill_all, 
          back = "grey95", col = "steelblue2",
          digits = c(1,2))

dev.copy(pdf,'figs/funnel_trimfill.pdf')
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
# P-curve: survey experiments
################################################################################
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
             color = "steelblue2", size = 1.5) + 
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
  
################################################################################
# Create table of all p-values
################################################################################
# Clean and compile data frame
meta$p_reported = as.numeric(meta$p_reported)
meta$p_reported = round(meta$p_reported, 3)
meta$p_replicated = as.numeric(meta$p_replicated)
meta$p_replicated = round(meta$p_replicated, 3)

# Clean and compile data frame
p_table = meta %>% 
  select(author_reduced, type, p_reported, p_replicated) %>%
  arrange(p_reported) %>%
  mutate(p_reported = round(p_reported, 3)) %>%
  mutate(p_replicated = round(p_replicated, 3)) %>%
  mutate(author_reduced = as.character(author_reduced)) %>%
  rename(Study = author_reduced, `Experiment Type` = type,
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
          #column.sep.width = "30pt",
          rownames = FALSE, 
          summary = FALSE
          )
  
################################################################################
# Plot binned p-values in visualizable format
################################################################################
# Converted reported p_values to numeric
meta$p = as.numeric(gsub("<", '', meta$p_reported))
meta$p = with(meta, ifelse(is.na(p), as.numeric(gsub(">", '', p_reported)), p))

# Clean study names
meta$author_reduced = with(meta, 
                           ifelse(author_reduced == "Boas, Hidalgo, & Melo" &
                                  field == 1, "Boas, Hidalgo, & Melo (Field)",
                                  author_reduced))
meta$studies = 1
meta$experiment = as.factor(meta$survey)
meta$experiment = with(meta, ifelse(experiment == 1, "Survey", "Field"))

# Univariate
ggplot(meta, 
       aes(x = p, y = studies, fill = experiment)) +
  geom_bar(stat = "identity") + 
  xlab("Reported p-value (binned by 0.01)") +
  scale_fill_manual(values = c("Field" = "seagreen2", 
                               "Survey" = "firebrick2")) +
  labs(color='Experiment', fill = 'Experiment')  +
  scale_fill_manual(values = c("seagreen3", "steelblue2")) + 
  scale_x_continuous(breaks = seq(from = 0, to = 1, by = 0.01)) +
  scale_y_continuous(breaks = seq(from = 0, to = 20, by = 2)) +
  theme_classic() +
  theme(legend.position="bottom") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        axis.title.y=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))

# Bar chart
ggplot(meta, 
       aes(x = reorder(author_reduced, -p), 
           y = p)) +
  geom_bar(width = 0.5, stat = "identity", 
           color = "steelblue2", fill = "steelblue2") +
  geom_bar(data = subset(meta, field == 1), width = 0.5,
           stat = "identity", color = "seagreen3", fill = "seagreen3") +
  coord_flip() +
  ylab("Reported p-value (binned by 0.01)") +
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.01)) +
  theme_classic() +
  theme(legend.position="none") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        axis.title.y=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("figs/p_bar.pdf", height = 3.5, width = 6)


hist(meta$p)
