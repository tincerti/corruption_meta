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
gcb1 = read_excel("data/Global_Corruption_Barometer_2017_Global_Results.xlsx",
                  sheet = "Q2_B", skip = 6)
gcb2 = read_excel("data/Global_Corruption_Barometer_2017_Global_Results.xlsx",
                  sheet = "Q2_C", skip = 6)
gcb5 = read_excel("data/Global_Corruption_Barometer_2017_Global_Results.xlsx",
                  sheet = "Q5", skip = 4)
gcb6 = read_excel("data/Global_Corruption_Barometer_2017_Global_Results.xlsx",
                  sheet = "Q6", skip = 4)

################################################################################
# Clean and combine data
################################################################################

# Clean gcb data
gcb1 = gcb1 %>% 
  select(Country, `NET NONE/ SOME`, `NET MOST/ALL`) %>%
  rename(country = Country, rep_few = `NET NONE/ SOME`, rep_most = `NET MOST/ALL`)
gcb1 = gcb1[-1,]

gcb2 = gcb2 %>% 
  select(Country, `NET NONE/ SOME`, `NET MOST/ALL`) %>%
  rename(country = Country, gov_few = `NET NONE/ SOME`, gov_most = `NET MOST/ALL`)
gcb2 = gcb2[-1,]

gcb5 = gcb5 %>% 
  select(Country, AGREE, DISAGREE) %>%
  rename(country = Country, ordinary_agree = AGREE, ordinary_disagree = DISAGREE)
gcb5 = gcb5[-1,]

gcb6 = gcb6 %>% 
  select(Country, AGREE, DISAGREE) %>% 
  rename(country = Country, social_agree = AGREE, social_disagree = DISAGREE)
gcb6 = gcb6[-1,]

gcb = left_join(gcb1, gcb2, by = "country")
gcb = left_join(gcb, gcb5, by = "country")
gcb = left_join(gcb, gcb6, by = "country")

meta = left_join(meta, gcb, by = "country")

################################################################################
# Run meta-analysis with norms moderator and interaction
################################################################################
# Keep survey experiments only
survey = meta %>% 
  filter(type == "Survey") %>%
  arrange(ate_vote) %>%
  mutate(author_reduced = reorder(author_reduced, -ate_vote))

# Mixed effects model with norms moderators
me_mod_rep = rma(yi = ate_vote, sei = se_vote, 
                    mods = rep_most, data = survey)

me_mod_gov = rma(yi = ate_vote, sei = se_vote, 
                    mods = gov_most, data = survey)

me_mod_social = rma(yi = ate_vote, sei = se_vote, 
                    mods = social_agree, data = survey)

me_mod_ordinary = rma(yi = ate_vote, sei = se_vote, 
                       mods = ordinary_agree, data = survey)

predict(me_mod_rep, newmods = cbind(seq(from = 0, to = 1, by = .1)), addx = TRUE)
predict(me_mod_gov, newmods = cbind(seq(from = 0, to = 1, by = .1)), addx = TRUE)
predict(me_mod_social, newmods = cbind(seq(from = 0, to = 1, by = .1)), addx = TRUE)
predict(me_mod_ordinary, newmods = cbind(seq(from = 0, to = 1, by = .1)), addx = TRUE)
