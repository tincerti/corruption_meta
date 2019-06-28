################################################################################
# Libraries and Import
################################################################################
library(foreign)
library(readstata13)
library(estimatr)
library(tidyverse)

library(randomForest)
library(gbm)
library(ISLR)
library(rpart)
library(rpart.plot)
library(caTools)
library(caret)

# Import all conjoint experiments
fz = read.dta('data/franchino_zucchini.dta')
mv = read.dta('data/mares_visconti.dta')
b = read.dta13('data/choosing_crook_clean.dta')

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

################################################################################
# Predictions: Breitenstein
################################################################################
b$Y = as.factor(b$Y)

# Split data into training and test
sample = sample.split(b, SplitRatio = .9)
train = subset(b, sample == TRUE)
test  = subset(b, sample == FALSE)

# rpart
clean = train %>% filter((Corrupt == "Yes" & Challenger == "Clean") |
                          Corrupt == "No" & Challenger == "Corrupt")
clean = clean %>% filter(Corrupt == "Yes")

rpart_tree <- rpart(Y ~ Corrupt + Party + Economy + Experience + Gender,
                    data = clean, 
                    cp = -10,
                    method = 'class')

plotcp(rpart_tree)
printcp(rpart_tree)

rpart.plot(rpart_tree, 
           extra = 7, 
           type = 5)

# Boosted tree
boost_fit = gbm(Y ~ `Corrupt` + as.factor(Challenger) + `Party` + `Economy` + `Experience` + `Gender`,
                data = train, distribution= "gaussian", n.trees = 5000, 
                interaction.depth = 4)

# Importance plot
par(mar = c(5, 8, 1, 1))
summary(
  boost_fit, 
  cBars = 5,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
  )

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

################################################################################
# Predictions: Breitenstein
################################################################################
fz$Y = as.factor(fz$Y)

# Split data into training and test
sample = sample.split(fz, SplitRatio = .9)
train = subset(fz, sample == TRUE)
test  = subset(fz, sample == FALSE)

# rpart prediction
rpart_tree <- rpart(Y ~ `Corrupt` + `Education` + `Income` + 
                    `Tax policy` + `Same sex marriage`,
                    data = train, 
                    cp = 0,
                    method = 'class')

plotcp(rpart_tree)

rpart.plot(rpart_tree, 
           extra = 7, 
           type = 5)