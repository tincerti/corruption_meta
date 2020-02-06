################################################################################
# Libraries and Import
################################################################################
rm(list=ls())

# Libraries
library(tidyverse)
library(metafor)

# Data import
load(file="data/meta_results.RData")

################################################################################
# Run meta-analysis for lab experiments
################################################################################
# Fixed effects model without moderators
fe = rma.uni(yi = ate_vote, sei = se_vote, weighted = TRUE,
             data = lab, method = "FE")

# Random effects model without moderators
re = rma(yi = ate_vote, sei = se_vote, data = lab)

################################################################################
# Prepare estimates for plotting
################################################################################
# Add meta-anlaysis parameters
ate_vote_fe = coef(fe)
se_vote_fe = summary(fe)$se
ate_vote_re = coef(re)
se_vote_re = summary(re)$se

meta_fe = data.frame(type="Lab", year=NA, author = "Fixed effects model", 
                     author_reduced = "Fixed effects model", country = NA, 
                     ate_vote = ate_vote_fe, se_vote = se_vote_fe, ci_upper = NA, 
                     ci_lower = NA, p_reported = NA, p_replicated = NA, 
                     published = NA, N = NA, Notes = NA)

meta_re = data.frame(type="Lab", year=NA, author = "Random effects model", 
                     author_reduced = "Random effects model", country = NA, 
                     ate_vote = ate_vote_fe, se_vote = se_vote_fe, ci_upper = NA, 
                     ci_lower = NA, p_reported = NA, p_replicated = NA, 
                     published = NA, N = NA, Notes = NA)

lab = rbind(lab, meta_fe, meta_re)

# Re-order factor levels
lab$author_reduced = fct_relevel(lab$author_reduced, 
                                 "Fixed effects model", after = 0)

lab$author_reduced = fct_relevel(lab$author_reduced, 
                                 "Random effects model", after = 0)

# Mutliply effect size by 100
lab$ate_vote = lab$ate_vote*100
lab$se_vote = lab$se_vote*100

################################################################################
# Plot lab results
################################################################################

ggplot(lab, aes(ate_vote, author_reduced)) +
  geom_point(color = "steelblue2", size = 1.5) + 
  geom_point(data = subset(lab, 
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

ggsave("figs/lab_meta.pdf", height = 4, width = 6)
