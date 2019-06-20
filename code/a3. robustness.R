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

# Keep field experiments and remove Banarjee papers
field = meta %>% 
  filter(type == "Field" | type == "Natural") %>%
  arrange(ate_vote) %>%
  mutate(author_reduced = reorder(author_reduced, -ate_vote)) %>%
  filter(author_reduced != "Banerjee et al. (2011)" & 
         author_reduced != "Banerjee et al. (2010)")
  
# Keep survey experiments only and add De Figueiredo et al. 
survey = meta %>% filter(type == "Survey")

defig = data.frame(type="Survey", year = 2011, 
                   author = "De Figueiredo, Hidalgo, & Kasahara", 
                   author_reduced = "De Figueiredo et al.", country = "Brazil", 
                   ate_vote = 0.01, se_vote = NA, ci_upper = 0.07, 
                   ci_lower = -0.07, p_reported = 0.42, published = 0, N = 133, Notes = NA)

survey = rbind(survey, defig)

# Keep survey experiments only and add De Figueiredo et al. 
survey = survey %>% arrange(ate_vote) %>%
  mutate(author_reduced = reorder(author_reduced, -ate_vote))

# Calculate SE intervals for De Figueiredo et al. 
survey$se_vote = with(survey, ifelse(is.na(se_vote), (ate_vote - ci_lower)/1.96, se_vote))

# Random effects model
re_field = rma.uni(yi = ate_vote, sei = se_vote, data = field)
re_survey = rma.uni(yi = ate_vote, sei = se_vote, data = survey)

# Fixed effects model: weighted
fe_field = rma.uni(yi = ate_vote, sei = se_vote, weights = N,
                   method = "FE", data = field)

fe_survey = rma.uni(yi = ate_vote, sei = se_vote, weights = N,
                   method = "FE", data = survey)

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
                     ci_lower = NA, p_reported = NA, published = NA, N = NA, Notes = NA)

meta_re = data.frame(type="Field", year=NA, author = "Random effects model", 
                     author_reduced = "Random effects model", country = NA, 
                     ate_vote = ate_vote_re, se_vote = se_vote_re, ci_upper = NA, 
                     ci_lower = NA, p_reported = NA, published = NA, N = NA, Notes = NA)

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
  scale_x_continuous(limits = c(-90, 30), breaks=seq(-90,30, 10)) +
  theme_classic() +
  theme(axis.title.y=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size = 8)) +
  theme(axis.text.x = element_text(size = 8)) +
  theme(legend.position = "none")

ggsave("figs/field_no_banerjee.pdf", height = 3.5, width = 6)

################################################################################
# Plot survey results
################################################################################
# Add meta-anlaysis parameters
ate_vote_fe = coef(fe_survey)
se_vote_fe = summary(fe_survey)$se
ate_vote_re = coef(re_survey)
se_vote_re = summary(re_survey)$se

meta_fe = data.frame(type="Survey", year=NA, author = "Fixed effects model", 
                     author_reduced = "Fixed effects model", country = NA, 
                     ate_vote = ate_vote_fe, se_vote = se_vote_fe, ci_upper = NA, 
                     ci_lower = NA, p_reported = NA, published = NA, N = NA, 
                     Notes = NA)

meta_re = data.frame(type="Survey", year=NA, author = "Random effects model", 
                  author_reduced = "Random effects model", country = NA, 
                  ate_vote = ate_vote_re, se_vote = se_vote_re, ci_upper = NA, 
                  ci_lower = NA, p_reported = NA, published = NA, N = NA, 
                  Notes = NA)

survey = rbind(survey, meta_fe, meta_re)

# Re-order factor levels
survey$author_reduced = fct_relevel(survey$author_reduced, "Fixed effects model", after = 0)
survey$author_reduced = fct_relevel(survey$author_reduced, "Random effects model", after = 0)

# Mutliply effect size by 100
survey$ate_vote = survey$ate_vote*100
survey$se_vote = survey$se_vote*100

# Plot survey results
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
  scale_x_continuous(limits = c(-90, 30), breaks=seq(-90,30, 10)) +
  theme_classic() +
  theme(axis.title.y=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size = 8)) +
  theme(axis.text.x = element_text(size = 8)) +
  theme(legend.position = "none")

ggsave("figs/survey_defig.pdf", height = 4, width = 6)