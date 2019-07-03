################################################################################
# Libraries and Import
################################################################################
rm(list=ls())

# Set seed
set.seed(300) # Estimates will of course vary slightly with change in seed

# Libraries
library(foreign)
library(readstata13)
library(tidyverse)
library(gbm)
library(rpart)
library(rpart.plot)
library(caTools)

# Import all conjoint experiments
fz = read.dta('data/franchino_zucchini.dta')
mv = read.dta('data/mares_visconti.dta')
b = read.dta13('data/choosing_crook_clean.dta')
eggers = readRDS("data/experiment_data_eggers.Rds", refhook = NULL)

################################################################################
# Data setup: Breitenstein
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
       labels = c("Different", "Co-partisan"))

# Define attribute lists: Economic performance
b$Economy <- factor(b$nperformance,
       levels = c("bad", 
                  "good"), 
       labels = c("Bad", 
                  "Good"))

# Define attribute lists: Experience
b$Experience <- factor(b$nqualities,
       levels = c("low", 
                  "high"),
       labels = c("Low",
                  "High"))

# Define attribute lists: Gender
b$Gender <- factor(b$ngender,
       levels = c("man", 
                  "woman"), 
       labels = c("Male", 
                  "Female"))

b$candidate2 = b$candidate

# Add clean challenger variable
b$Challenger = with(b, ifelse(lead(Corrupt, 1) == "No" &
                       candidate == 1 & lead(candidate, 1) == 2,
                       "Clean", NA))

b$Challenger = with(b, ifelse(lag(Corrupt, 1) == "No" &
                       candidate == 2 & lag(candidate, 1) == 1,
                       "Clean", Challenger))

b$Challenger = with(b, ifelse(lead(Corrupt, 1) == "Yes" &
                       candidate == 1 & lead(candidate, 1) == 2,
                       "Corrupt", Challenger))

b$Challenger = with(b, ifelse(lag(Corrupt, 1) == "Yes" &
                       candidate == 2 & lag(candidate, 1) == 1,
                       "Corrupt", Challenger))

# Create datasets consisting of corrupt candidate and clean challenger only
clean = b %>% 
  filter((Corrupt == "Yes" & Challenger == "Clean") |
          Corrupt == "No" & Challenger == "Corrupt")

################################################################################
# Predictions: Breitenstein
################################################################################
# Convert outcome variable to binary for classification
b$Y = as.factor(b$Y)

# Split data into training and test
sample = sample.split(b, SplitRatio = .9) # From caTools package
train = subset(b, sample == TRUE)
test  = subset(b, sample == FALSE)

# Run classification tree (uses package rpart)
b_tree <- rpart(Y ~ Corrupt + Party + Economy + Experience + Gender,
                    data = train, 
                    cp = 0,
                    method = 'class')

# Pick tree size that minimizes classification error rate and prune tree
bestcp <- b_tree$cptable[which.min(b_tree$cptable[,"xerror"]),"CP"]
plotcp(b_tree)
b_tree_pruned <- prune(b_tree, cp = bestcp)

# Plot classification tree
rpart.plot(b_tree_pruned, extra = 7, type = 5, cex = 0.6)

# Save plot
dev.copy(pdf,'figs/b_tree.pdf', width = 7, height = 3.5)
dev.off()

################################################################################
# Figure A12: Analysis with clean challenger only 
################################################################################
# Reduce clean dataframe to corrupt candidate only
clean_reduced = clean %>% filter(Corrupt == "Yes")

# Split data into training and test
sample = sample.split(clean_reduced, SplitRatio = .9) # From caTools package
train = subset(clean_reduced, sample == TRUE)
test  = subset(clean_reduced, sample == FALSE)

# Run classification tree (uses package rpart)
b_tree_clean <- rpart(Y ~ Corrupt + Party + Economy + Experience + Gender,
                      data = train, 
                      cp = -0.01,
                      method = 'class')

# Pick tree size that minimizes classification error rate and prune tree
bestcp <- b_tree_clean$cptable[which.min(b_tree_clean$cptable[,"xerror"]),"CP"]
plotcp(b_tree_clean)
printcp(b_tree_clean)
b_tree_clean_pruned <- prune(b_tree_clean, cp = -0.01)

# Plot classification tree
rpart.plot(b_tree_clean_pruned, extra = 7, type = 5, cex = 0.55)

# Save plot
dev.copy(pdf,'figs/b_tree_clean.pdf', width = 7, height = 3.5)
dev.off()

################################################################################
# Boosted tree
################################################################################
boost_fit = gbm(Y ~ `Corrupt` + as.factor(Challenger) + `Party` + `Economy` + `Experience` + `Gender`,
                data = train, distribution= "gaussian", n.trees = 5000, 
                interaction.depth = 4)

################################################################################
# Data setup: Franchino and Zucchini
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