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
ckh = read.dta13('data/chauchard_klasnja_harish.dta', nonint.factors = TRUE)

################################################################################
# Analysis: Chauchard, Klasnja, and Harish
################################################################################
# Remove NA outcome values - not sure why these are here
ckh = ckh %>% filter(!is.na(dv_vote))

# Legality
ckh$Legal <- factor(ckh$legal,
       levels = c("N/A", "No suspicion", "Suspicion"), 
       labels = c("N/A", "No suspicion", "Suspicion"))

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

# Define attribute lists
attribute_list <- list()
attribute_list[["Corrupt"]] <- c("Yes", "No")
attribute_list[["Legal"]] <- c("N/A", "Suspicion", "No suspicion")
attribute_list[["Copartisan"]] <- c("Yes", "No")
attribute_list[["Coethnic"]] <- c("Yes", "No")
attribute_list[["Criminality"]] <- c("Not a criminal", "Criminal")
attribute_list[["Performance"]] <- c("Good", "Bad")
attribute_list[["Background"]] <- c("Poor", "Middle income", "Rich")
attribute_list[["Wealth increase"]] <- c("None", "Low", "High")

# Define conlficting levels
constraint_list <- list()
constraint_list[[1]] <- list()
constraint_list[[1]][["Wealth increase"]] <- c("None")
#constraint_list[[1]][["Corrupt"]] <- c("Yes")
constraint_list[[1]][["Legal"]] <- c("No suspicion", "Suspicion")

constraint_list[[2]] <- list()
constraint_list[[2]][["Wealth increase"]] <- c("Low", "High")
#constraint_list[[1]][["Corrupt"]] <- c("Yes")
constraint_list[[2]][["Legal"]] <- c("N/A")

design <- makeDesign(type='constraints', 
                     attribute.levels=attribute_list,
                     constraints=constraint_list)

# Run AMCE regression and store results
reg_ckh <- amce(formula = dv_vote ~ `Legal` + `Copartisan` + `Coethnic` + 
                          `Criminality` + `Performance` + `Background`, 
                data = ckh, cluster=TRUE, design=design, respondent.id = "id")

reg = lm(dv_vote ~ `Wealth increase` + Legal + `Wealth increase`:Legal +
                   `Copartisan` + `Coethnic` + `Criminality` + `Performance` + 
                   `Background`, data = ckh)

reg = lm(dv_vote ~ `Wealth increase`, data = ckh)
reg = lm(dv_vote ~ `Wealth increase` + Legal + `Wealth increase`:Legal, 
         data = ckh)
reg = lm(dv_vote ~ as.factor(wmultr) + legal + as.factor(wmultr):legal, data = ckh)

summary(reg)
summary(reg_ckh)

# Create conjoint plot: Chauchard, Klasnja, and Harish
plot(reg_ckh,
     group.order = c("Corrupt", "Copartisan", "Coethnic", "Criminality",
                     "Performance", "Background", "Wealth increase"),
     xlab = "Change in probability of voting",
     point.size = 0.25,
     colors = c("grey60", "grey60", "black", "firebrick1", "black", "grey60", "black"),
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
ggsave("figs/ckh_amce.pdf", height = 4, width = 6)