################################################################################
# Libraries and Import
################################################################################
rm(list=ls())

# Libraries
library(tidyverse)
library(metafor)
library(margins)
library(foreign)

# Data import
fz = read.dta('data/franchino_zucchini.dta')

################################################################################
# Define combinations
################################################################################
# Remove NA outcome values - not sure why these are here
fz = fz %>% filter(!is.na(Y))

# Pool corruption into one treatment
fz$corrupt = with(fz, ifelse(corruption == "Convicted of corruption" |
                             corruption == "Investigated for corruption", 1, 0))

# Education
fz$m1 = with(fz, ifelse(corrupt == 1 & education == "Licenza media", 1, 0))
fz$m2 = with(fz, ifelse(corrupt == 1 & education == "Diploma superiore", 1, 0))
fz$m3 = with(fz, ifelse(corrupt == 1 & education == "Laurea", 1, 0))

# Taxspend
fz$m4 = with(fz, ifelse(corrupt == 1 & taxspend == "More social services", 1, 0))
fz$m5 = with(fz, ifelse(corrupt == 1 & taxspend == "Cut taxes", 1, 0))

# Same sex marraige
fz$m6 = with(fz, ifelse(corrupt == 1 & samesex == "No rights", 1, 0))
#fz$m7 = with(fz, ifelse(corrupt == 1 & samesex == "Some rights", 1, 0))
fz$m7 = with(fz, ifelse(corrupt == 1 & samesex == "Same rights", 1, 0))

# Combinations
fz$category = with(fz, ifelse(corrupt == 1 &
                        taxspend == "Cut taxes" & 
                        samesex == "No rights", 1, NA))

fz$category = with(fz, ifelse(corrupt == 1 &
                        taxspend == "More social services" & 
                        samesex == "No rights", 2, category))

fz$category = with(fz, ifelse(corrupt == 1 &
                        taxspend == "Maintain level of provision" & 
                        samesex == "No rights", 3, category))
#
fz$category = with(fz, ifelse(corrupt == 1 &
                        taxspend == "Cut taxes" & 
                        samesex == "Some rights", 4, category))

fz$category = with(fz, ifelse(corrupt == 1 &
                        taxspend == "More social services" & 
                        samesex == "Some rights", 5, category))

fz$category = with(fz, ifelse(corrupt == 1 &
                        taxspend == "Maintain level of provision" & 
                        samesex == "Some rights", 6, category))

#
fz$category = with(fz, ifelse(corrupt == 1 &
                        taxspend == "Cut taxes" & 
                        samesex == "Same rights", 7, category))

fz$category = with(fz, ifelse(corrupt == 1 &
                        taxspend == "More social services" & 
                        samesex == "Same rights", 8, category))

fz$category = with(fz, ifelse(corrupt == 1 &
                        taxspend == "Maintain level of provision" & 
                        samesex == "Same rights", 9, category))

################################################################################
# Calculate joint marginal means
################################################################################
mm <- lm(Y ~ as.factor(category), data = fz)
mm_df = summary(prediction(mm, 
                           at = list(category = c(1:9)),
                           calculate_se = TRUE))

prediction(mm, at = list(category = c(1:9)), calculate_se = TRUE)
margins(mm)

# Clean margins dataframe
mm_df <- mm_df %>%
  rename(combs = factor) %>%
  arrange(AME) %>%              
  mutate(combs = factor(combs, unique(combs)))

mm_df$win = .7265 + mm_df$AME
mm_df$upper_win = .7265 + mm_df$upper
mm_df$lower_win = .7265 + mm_df$lower

################################################################################
# Create plot
################################################################################
# Line breakes for labels
addline_format <- function(x,...){
    gsub('\\s','\n',x)
}

# Plot
ggplot(mm_df, aes(combs, win)) +
  geom_point(color = "steelblue2", size = 2) + 
  geom_errorbar(aes(ymin = lower_win, ymax = upper_win),
                position = position_dodge(),
                color="black", size=0.5, alpha = 0.5, width = 0.25) +
  geom_hline(yintercept = .5, linetype = "dashed") +
  ylab("Probability of choosing candidate") + 
  scale_x_discrete("", labels = addline_format(c(
                                  "m1" = "Cut taxes + No rights",
                                  "m2" = "More services + No rights",
                                  "m3" = "No change + Some rights",
                                  "m4" = "Cut taxes + Some rights",
                                  "m5" = "More services + Some rights",
                                  "m6" = "No change + Some rights",
                                  "m7" = "Cut taxes + Same rights",
                                  "m8" = "More services + Same rights",
                                  "m9" = "No change + Same rights"))) +
  scale_y_continuous(breaks = seq(0, .75, by = .1),
                     labels = scales::percent_format(accuracy = 1)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size = 10)) +
  theme(axis.text.x = element_text(size = 8)) +
  theme(legend.position = "none")

# Save conjoint plot
ggsave("figs/conjoint/fz_mm.pdf", height = 3.5, width = 6.5)