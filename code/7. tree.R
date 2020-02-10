################################################################################
# Libraries and Import
################################################################################
rm(list=ls())

# Set seed
set.seed(200) # Estimates will of course vary slightly with change in seed

# Libraries
library(foreign)
library(readstata13)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caTools)

# Import conjoint experiment
b = read.dta13('data/choosing_crook_clean.dta')
ckh = read.dta13('data/chauchard_klasnja_harish.dta', nonint.factors = TRUE)

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
sample = sample.split(b, SplitRatio = .9)
train = subset(b, sample == TRUE)
test  = subset(b, sample == FALSE)

# Run classification tree
b_tree <- rpart(Y ~ Corrupt + Party + Economy + Experience + Gender,
                    data = train, 
                    cp = 0,
                    method = 'class')

# Pick tree size that minimizes classification error rate and prune tree
# Using lowest rel_error + xstd < x_error rule 
bestcp <- b_tree$cptable[which.min(b_tree$cptable[,"xerror"]),"CP"]
plotcp(b_tree)
printcp(b_tree)
b_tree_pruned <- prune(b_tree, cp = bestcp)

# Examine correct classification rate
test$t_pred = predict(b_tree_pruned, test, type = "class")
mean(test$Y == test$t_pred)

# Plot classification tree
rpart.plot(b_tree_pruned, extra = 7, type = 5, cex = 0.6)

# Save plot
dev.copy(pdf,'figs/b_tree.pdf', width = 7, height = 3.5)
dev.off()

################################################################################
# Analysis with clean challenger only 
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
# Choosing different CP for visualization: error rate remains constant

# Examine correct classification rate
test$t_pred = predict(b_tree_pruned, test, type = "class")
mean(test$Y == test$t_pred)

# Plot classification tree
rpart.plot(b_tree_clean_pruned, extra = 7, type = 5, cex = 0.6)

# Save plot
dev.copy(pdf,'figs/b_tree_clean.pdf', width = 7, height = 3.5)
dev.off()

################################################################################
# Data setup: Chauchard, Klasnja, and Harish
################################################################################
# Remove NA outcome values - not sure why these are here
ckh = ckh %>% filter(!is.na(dv_vote))

# Create corruption measure
ckh$Corrupt = with(ckh, ifelse(legal == "Suspicion", "Yes", "No"))

# Define attribute lists: Corruption
ckh$Corrupt <- factor(ckh$Corrupt,
       levels = c("No", "Yes"), 
       labels = c("No", "Yes"))

# Define attribute lists: Partisanship
ckh$Copartisan = with(ckh, ifelse(out_party == 1, "No", "Yes"))
ckh$Copartisan <- factor(ckh$Copartisan,
       levels = c("No", "Yes"), 
       labels = c("No", "Yes"))

# Define attribute lists: Ethnicity
ckh$Coethnic = with(ckh, ifelse(co_ethn_dummy == 0, "No", "Yes"))
ckh$Coethnic <- factor(ckh$Coethnic,
       levels = c("No", "Yes"), 
       labels = c("No", "Yes"))

# Define attribute lists: Record
ckh$Performance <- factor(ckh$record,
       levels = c("Good", "Disappointing"), 
       labels = c("Good", "Bad"))

# Define attribute lists: Criminality
ckh$Criminality <- factor(ckh$crime,
       levels = c("No crime", "Crime"), 
       labels = c("Not a criminal", "Criminal"))

# Define attribute lists: Background
ckh$Background <- factor(ckh$family,
       levels = c("Poor", "Middle-income", "Rich"), 
       labels = c("Poor", "Middle income", "Rich"))

# Define attribute lists: Wealth
ckh$`2010 wealth` <- factor(ckh$w2010r,
       levels = c("1", "2", "3"), 
       labels = c("Low", "Medium", "High"))

# Define attribute lists: Wealth increase
ckh$`Wealth increase` <- factor(ckh$wmultr,
       levels = c("1", "2", "3"), 
       labels = c("None", "Low", "High"))

################################################################################
# Predictions: Chauchard, Klasnja, and Harish
################################################################################
# Convert outcome variable to binary for classification
ckh$dv_vote = as.factor(ckh$dv_vote)
ckh_corrupt = ckh %>% filter(Corrupt == "Yes")

# Split data into training and test
sample = sample.split(ckh_corrupt, SplitRatio = .9) # From caTools package
train = subset(ckh_corrupt, sample == TRUE)
test  = subset(ckh_corrupt, sample == FALSE)

# Run classification tree (uses package rpart)
ckh_tree <- rpart(dv_vote ~ `Copartisan` + `Coethnic` + 
                          `Criminality` + `Performance` + 
                          `Wealth increase`,
                    data = train, 
                    cp = 0,
                    method = 'class')

# Pick tree size that minimizes classification error rate and prune tree
plotcp(ckh_tree)
printcp(ckh_tree)
ckh_tree_pruned <- prune(ckh_tree, cp = 0) 
# Using lowest rel_error + xstd < x_error rule

# Examine correct classification rate
test$t_pred = predict(ckh_tree_pruned, test, type = "class")
mean(test$dv_vote == test$t_pred)

# Plot classification tree
rpart.plot(ckh_tree_pruned, extra = 7, type = 5, cex = 0.5)

# Save plot
dev.copy(pdf,'figs/ckh_tree_corrupt.pdf', width = 7, height = 3.5)
dev.off()