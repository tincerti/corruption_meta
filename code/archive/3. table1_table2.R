################################################################################
# Libraries and Import
################################################################################
rm(list=ls())

# Libraries
library(tidyverse)
library(metafor)
library(stargazer)

# Data import
load(file="data/meta_results.RData")

################################################################################
# Create Table 1 and Table 2: General cleaning
################################################################################
# Add author-year variable
meta$author_year = with(meta, paste(author, year, sep=" "))

################################################################################
# Create Table 1: Field experiments
################################################################################
# Keep field experiments only
table1 = meta %>% filter(type == "Field" | type == "Natural")

# Sort alphabetically
table1 = table1 %>% arrange(author_year)

# Add treatment type
table1$treatment = NA
table1$treatment[1] = "Fliers"
table1$treatment[2] = "Newspaper"
table1$treatment[3] = "Canvas/Newspaper"
table1$treatment[4] = "Fliers"
table1$treatment[5] = "SMS"
table1$treatment[6] = "SMS"
table1$treatment[7] = "Fliers"
table1$treatment[8] = "Fliers"
table1$treatment[9] = "Fliers"
table1$treatment[10] = "Audits"

# Keep necessary variables only
table1 = table1 %>% select(author_year, country, treatment, ate_vote)

# Output table
stargazer(table1)

