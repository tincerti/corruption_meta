################################################################################
# Libraries and Import
################################################################################
rm(list=ls())

# Libraries
library(tidyverse)

# Data import
load(file="data/meta_results.RData")

################################################################################
# Plot results
################################################################################
# Rename Boas articles
field = field %>% mutate(author_reduced = fct_recode(author_reduced, 
         "Boas et al. (Field)" = "Boas et al."))
  
survey = survey %>% mutate(author_reduced = fct_recode(author_reduced, 
                               "Boas et al. (Survey)" = "Boas et al."))

# Combine field and survey
meta = rbind(field, survey)
meta = meta %>% mutate(author_reduced = reorder(author_reduced, -ate_vote))

# Mutliply effect size by 100
meta$ate_vote = meta$ate_vote*100
meta$se_vote = meta$se_vote*100

# Change from binary published to text
meta$published = with(meta, ifelse(published == 1, "Yes", "No"))

# Plot field results
ggplot(meta, aes(ate_vote, author_reduced, label = published)) +
  geom_point(aes(color = published, fill = published), size = 1.5) +
  geom_errorbarh(aes(y = author_reduced, 
                     xmin = ate_vote - 1.96*se_vote, 
                     xmax = ate_vote + 1.96*se_vote),
                color="grey30", size=0.5, alpha = 0.5, height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Change in vote share (percentage points)") + 
  scale_x_continuous(limits = c(-70, 30), breaks=seq(-70, 30, 10)) +
  scale_color_manual(values = c("Yes" = "seagreen2", 
                                "No" = "firebrick2")) +
  labs(color='Published', fill = 'Published')  +
  geom_text(aes(label = type, x = 30, y = author_reduced), size = 3) +
  scale_y_discrete(breaks=meta$author_reduced, 
                   labels = meta$author_reduced) +
  theme_classic() +
  theme(axis.title.y=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size = 9)) +
  theme(axis.text.x = element_text(size = 9)) +
  theme(legend.position = "bottom") +
  theme(legend.text=element_text(size=12)) +
  guides(colour = guide_legend(override.aes = list(size=5)))

ggsave("figs/published.pdf", height = 7, width = 6.5)