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
# Data setup
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

################################################################################
# Predictions
################################################################################
b$Y = as.factor(b$Y)

# Split data into training and test
sample = sample.split(b, SplitRatio = .9)
train = subset(b, sample == TRUE)
test  = subset(b, sample == FALSE)

# rpart
rpart_tree <- rpart(Y ~ `Corrupt` + `Party` + `Economy` + `Experience` + `Gender`,
                    data = train, 
                    cp = 0.002,
                    method = 'class')

plotcp(rpart_tree)
printcp(rpart_tree)

rpart.plot(rpart_tree, extra = 7, type = 3)

#tree_plot = last_plot()

# Save plot
dev.copy(pdf,'figs/tree.pdf')

# Calculate correct classification rate
test$Y_pred = predict(rpart_tree, newdata = test, type = "class")
mean(test$Y == test$Y_pred)

# Caret
train.rpart <- caret::train(Y ~ `Corrupt` + `Party` + `Economy` + `Experience` 
                            + `Gender`,
                     method = "rpart",
                     tuneLength = 50,
                     trControl = tc,
                     tuneGrid = trgrid)
train.rpart

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

dtree_fit <- caret::train(Y ~ `Corrupt` + `Party` + `Economy` + `Experience` 
                            + `Gender`,
                   data = b, 
                   method = "rpart",
                   #parms = list(split = "information"),
                   trControl = trctrl,
                   tuneLength = 10)

rpart.plot(dtree_fit$finalModel,
           type = 3,
           extra = 8,
           faclen = 1,
           clip.facs = TRUE,
           clip.right.labs = TRUE)





# Boosted tree
boost_fit = gbm(Y ~ `Corrupt` + `Party` + `Economy` + `Experience` + `Gender`,
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