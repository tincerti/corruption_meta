################################################################################
# Libraries and Import
################################################################################
rm(list=ls())

# Libraries
library(tidyverse)
library(cjoint)
library(foreign)
library(readstata13)

# Import data
fz = read.dta('data/franchino_zucchini.dta')
b = read.dta13('data/choosing_crook_clean.dta')

################################################################################
# Analysis: Franchino and Zucchini
################################################################################
# Remove NA outcome values - not sure why these are here
fz = fz %>% filter(!is.na(Y))

# Reduce to one corruption measure
fz$Corrupt = with(fz, ifelse(corruption == "Convicted of corruption" |
                             corruption == "Investigated for corruption", 
                             "Yes", "No"))

# Define attribute lists: Corruption
fz$Corrupt <- factor(fz$Corrupt,
       levels = c("No", "Yes"), 
       labels = c("No", "Yes"))

# Define attribute lists: Education
fz$Education <- factor(fz$education,
       levels = c("Licenza media", "Diploma superiore", "Laurea"), 
       labels = c("Junior high", "High School", "College"))

# Define attribute lists: Income
fz$Income <- factor(fz$income,
       levels = c("Less than 900 euro a month", 
                  "Between 900 and 3000 euro a month", 
                  "More than 3000 euro a month"), 
       labels = c("Less than 900 euros", 
                  "900 to 3000 euros", 
                  "Greater than 3000 euros"))

# Define attribute lists: tax policy
fz$`Tax policy` <- factor(fz$taxspend,
       levels = c("Maintain level of provision", 
                  "Cut taxes", 
                  "More social services"), 
       labels = c("Maintain level of provision", 
                  "Cut taxes", 
                  "More social services"))

# Define attribute lists: same sex marriage
fz$`Same sex marriage` <- factor(fz$samesex,
       levels = c("Some rights", 
                  "No rights", 
                  "Same rights"), 
       labels = c("Some rights", 
                  "No rights", 
                  "Same rights"))

# Run AMCE regression and store results
reg_all <- amce(formula = Y ~ `Corrupt` + `Education` + `Income` + 
                              `Tax policy` + `Same sex marriage`, 
                data = fz, cluster=TRUE, respondent.id = "IDContatto")

################################################################################
# Analysis: Breitenstein
################################################################################
# Reduce to one corruption measure
b$Corrupt = with(b, ifelse(corrupt == "Corrupt", "Yes", "No"))

# Define attribute lists: Corruption
b$Corrupt <- factor(b$Corrupt,
       levels = c("No", "Yes"), 
       labels = c("No", "Yes"))

# Define attribute lists: Co-partisanship
b$Party <- factor(b$samep,
       levels = c("0", "1"), 
       labels = c("Different party", "Co-partisan"))

# Define attribute lists: Economic performance
b$Economy <- factor(b$nperformance,
       levels = c("bad", 
                  "good"), 
       labels = c("Low performance", 
                  "High performance"))

# Define attribute lists: Experience
b$Experience <- factor(b$nqualities,
       levels = c("low", 
                  "high"), 
       labels = c("Low experience", 
                  "High experience"))

# Define attribute lists: Gender
b$Gender <- factor(b$ngender,
       levels = c("man", 
                  "woman"), 
       labels = c("Male", 
                  "Female"))

# Run AMCE regression and store results
reg_b <- amce(formula = Y ~ `Corrupt` + `Party` + `Economy` + 
                            `Experience` + `Gender`, 
                data = b, cluster=TRUE, respondent.id = "id")


################################################################################
# Plot
################################################################################
# Create conjoint plot: Breitenstein
plot(reg_b,
     group.order = c("Corrupt", "Gender", "Party", 
                     "Economy", "Experience"),
     xlab = "Change in probability of voting",
     point.size = 0.25,
     colors = c("firebrick1", "black", "grey60", "black", "grey60"),
     plot.theme = theme_bw() + 
     theme(panel.grid.major = element_line(colour = "grey98")) + 
     theme(legend.position="none") +
     theme(axis.title.x = element_text(size = 10)) +
     theme(axis.ticks = element_blank()) + 
     theme(plot.title = element_text(size=12)) +
     theme(plot.title = element_text(hjust = 0.5)),
     text.color = "black",
     text.size = 14
)

amce_chart = last_plot()

# Save conjoint plot
ggsave("figs/b_amce.pdf", height = 4, width = 6)

# Create conjoint plot: Franchino and Zucchini
plot(reg_all,
     group.order = c("Corrupt", "Education", "Income", 
                     "Same sex marriage", "Tax policy"),
     xlab = "Change in probability of voting",
     point.size = 0.25,
     colors = c("firebrick1", "black", "grey60", "black", "grey60"),
     plot.theme = theme_bw() + 
     theme(panel.grid.major = element_line(colour = "grey98")) + 
     theme(legend.position="none") +
     theme(axis.title.x = element_text(size = 10)) +
     theme(axis.ticks = element_blank()) + 
     theme(plot.title = element_text(size=12)) +
     theme(plot.title = element_text(hjust = 0.5)),
     text.color = "black",
     text.size = 14
)

amce_chart = last_plot()

# Save conjoint plot
ggsave("figs/fz_amce.pdf", height = 4, width = 6)