################################################################################
# Libraries and Import
################################################################################
library(survey)
library(tidyverse)
library(huxtable)
library(foreign)
library(readxl)
library(readstata13)
library(cjoint)

# Data imports
results = read_excel("data/study_results.xlsx")
wsw17 = read.dta('data/Weitz-ShapiroWinters_JOP_Credibility_ReplicationData.dta')
wsw13 = read.dta('data/WintersWeitzShapiro_2013_Replication.dta')
wsw16 = read.dta('data/WintersWeitz-Shapiro_PRQ_Specificity_ReplicationData.dta')
wsw18 = read.dta13('data/WintersWeitz-Shapiro_PSRM_Argentina_ReplicationData.dta')
fz = read.dta('data/franchino_zucchini.dta')
mv = read.dta('data/mares_visconti.dta')

# Define scaling function
scale01 <- function(x)
  (x - min(x, na.rm = T)) / diff(range(x, na.rm = T))

################################################################################
# Winters/Weitz-Shapiro JOP 2017
################################################################################
# Rescale outcome variable
wsw17$vote_vig_cont <- scale01(wsw17$voteintent)

# Create high quality variable
wsw17$high = with(wsw17, ifelse(vinheta == "VINHETA 4" | vinheta == "VINHETA 6",
                                   1, 0))

# Create low quality variable
wsw17$low = with(wsw17, ifelse(vinheta == "VINHETA 5" | vinheta == "VINHETA 7",
                                   1, 0))

# Create vote dummy
wsw17$vote = with(wsw17, ifelse(voteintent > 2, 1, 0))

# Run models: high quality
reg.wsw17.high = lm(vote ~ high, 
                    data = subset(wsw17, 
                              vinheta == "VINHETA 1" | vinheta == "VINHETA 2" |
                              vinheta == "VINHETA 4" | vinheta == "VINHETA 6"))

# Run models: low quality
reg.wsw17.low = lm(vote ~ low, 
                    data = subset(wsw17, 
                              vinheta == "VINHETA 1" | vinheta == "VINHETA 2" |
                              vinheta == "VINHETA 5" | vinheta == "VINHETA 7"))

# Extract point estimates
atehigh.wsw17 = summary(reg.wsw17.high)$coef[2, 1]
atelow.wsw17 = summary(reg.wsw17.low)$coef[2, 1]

# Extract standard errors
sehigh.wsw17 = summary(reg.wsw17.high)$coef[2, 2]
selow.wsw17 = summary(reg.wsw17.low)$coef[2, 2]

# Extract number of observations
nhigh.wsw17 = nobs(reg.wsw17.high)
nlow.wsw17 = nobs(reg.wsw17.low)

################################################################################
# Winters/Weitz-Shapiro PSRM
################################################################################
# Create high quality variable
wsw18$high = with(wsw18, ifelse(bribes_credible == 1, 1, 0))

# Create low quality variable
wsw18$low = with(wsw18, ifelse(bribes_less == 1, 1, 0))

# Create vote dummy
wsw18$vote = with(wsw18, ifelse(voteintent > 2, 1, 0))

# Run models: high quality
reg.wsw18.high = lm(vote ~ high, 
                    data = subset(wsw18, bribes_credible == 1 | 
                                    control == 1 | clean == 1))

# Run models: high quality
reg.wsw18.low = lm(vote ~ low, 
                    data = subset(wsw18, bribes_less == 1 | 
                                    control == 1 | clean == 1))

# Extract point estimates
atehigh.wsw18 = summary(reg.wsw18.high)$coef[2, 1]
atelow.wsw18 = summary(reg.wsw18.low)$coef[2, 1]

# Extract standard errors
sehigh.wsw18 = summary(reg.wsw18.high)$coef[2, 2]
selow.wsw18 = summary(reg.wsw18.low)$coef[2, 2]

# Extract number of observations
nhigh.wsw18 = nobs(reg.wsw18.high)
nlow.wsw18 = nobs(reg.wsw18.low)

################################################################################
# Breitenstein (No replication data?)
################################################################################
# Average treatment effects of judge and partisan accusations
atehigh.b = -.266
atelow.b = -.222

sehigh.b = 0.00769
selow.b = 0.00793

nhigh.b = 2275/2
nlow.b = 2275/2

################################################################################
# Mares and Visconti 2019
################################################################################
# Run model
mv_model = lm(outcome ~ atinte, data = mv)

# Extract point estimates
atehigh.mv = summary(mv_model, cluster = idnum)$coef[3, 1]
atelow.mv = summary(mv_model, cluster = idnum)$coef[2, 1]

# Extract standard errors
sehigh.mv = summary(mv_model, cluster = idnum)$coef[3, 2]
selow.mv = summary(mv_model, cluster = idnum)$coef[2, 2]

# Extract number of observations
nhigh.mv = 502
nlow.mv = 502

################################################################################
# Franchino and Zucchini 2014
################################################################################
# Remove NA outcome values - not sure why these are here
fz = fz %>% filter(!is.na(Y))

# Run model
fz_corrupt = lm(Y ~ corruption, data = fz)

# Extract point estimates
atehigh.fz = summary(fz_corrupt, cluster = IDContatto)$coef[3, 1]
atelow.fz = summary(fz_corrupt, cluster = IDContatto)$coef[2, 1]

# Extract standard errors
sehigh.fz = summary(fz_corrupt, cluster = IDContatto)$coef[3, 2]
selow.fz = summary(fz_corrupt, cluster = IDContatto)$coef[2, 2]

# Extract number of observations
nhigh.fz = 347
nlow.fz = 347

################################################################################
# Create dataframe
################################################################################
# Winters/Weitz-Shapiro JOP 2017: high
wsw17_high = data.frame(type="High quality", year=2016, author = "Winters & Weitz-Shapiro", 
                   author_reduced = "Winters & Weitz-Shapiro 2017", country = "Brazil", 
                   ate_vote = atehigh.wsw17, se_vote = sehigh.wsw17, ci_upper = NA, 
                   ci_lower = NA, N = nhigh.wsw17, published = 1, Notes = NA)

# Winters/Weitz-Shapiro JOP 2017: low
wsw17_low = data.frame(type="Low quality", year=2016, author = "Winters & Weitz-Shapiro", 
                   author_reduced = "Winters & Weitz-Shapiro 2017", country = "Brazil", 
                   ate_vote = atelow.wsw17, se_vote = selow.wsw17, ci_upper = NA, 
                   ci_lower = NA, N = nlow.wsw17, published = 1, Notes = NA)

# Winters/Weitz-Shapiro PSRM
wsw18_high = data.frame(type="High quality", year=2018 , author = "Winters & Weitz-Shapiro", 
                   author_reduced = "Winters & Weitz-Shapiro 2018", country = "Argentina", 
                   ate_vote = atehigh.wsw18, se_vote = sehigh.wsw18, ci_upper = NA, 
                   ci_lower = NA,N = nhigh.wsw18, published = 1, Notes = NA)

# Winters/Weitz-Shapiro PSRM
wsw18_low = data.frame(type="Low quality", year=2018 , author = "Winters & Weitz-Shapiro", 
                   author_reduced = "Winters & Weitz-Shapiro 2018", country = "Argentina", 
                   ate_vote = atelow.wsw18, se_vote = selow.wsw18, ci_upper = NA, 
                   ci_lower = NA,N = nlow.wsw18, published = 1, Notes = NA)

# Breitenstein 2019
b_high = data.frame(type="High quality", year=2019 , author = "Breitenstein", 
                   author_reduced = "Breitenstein", country = "Spain", 
                   ate_vote = atehigh.b, se_vote = sehigh.b, ci_upper = NA, 
                   ci_lower = NA, N = nhigh.b, published = 1, Notes = NA)

# Breitenstein 2019
b_low = data.frame(type="Low quality", year=2019 , author = "Breitenstein", 
                   author_reduced = "Breitenstein", country = "Spain", 
                   ate_vote = atelow.b, se_vote = selow.b, ci_upper = NA, 
                   ci_lower = NA, N = nlow.b, published = 1, Notes = NA)

# Mares and Visconti 2019
mv_high = data.frame(type="High quality", year=2019 , author = "Mares & Visconti", 
                   author_reduced = "Mares & Visconti", country = "Romania", 
                   ate_vote = atehigh.mv, se_vote = sehigh.mv, ci_upper = NA, 
                   ci_lower = NA, N = nhigh.mv, published = 1, Notes = NA)

# Mares and Visconti 2019
mv_low = data.frame(type="Low quality", year=2019 , author = "Mares & Visconti", 
                   author_reduced = "Mares & Visconti", country = "Romania", 
                   ate_vote = atelow.mv, se_vote = selow.mv, ci_upper = NA, 
                   ci_lower = NA, N = nlow.mv, published = 1, Notes = NA)

# Franchino and Zucchini 2015
fz_high = data.frame(type="High quality", year=2014 , author = "Franchino and Zucchini", 
                   author_reduced = "Franchino and Zucchini", country = "Italy", 
                   ate_vote = atehigh.fz, se_vote = sehigh.fz, ci_upper = NA, 
                   ci_lower = NA, N = nhigh.fz, published = 1, Notes = NA)

# Franchino and Zucchini 2015
fz_low = data.frame(type="Low quality", year=2014 , author = "Franchino and Zucchini", 
                   author_reduced = "Franchino and Zucchini", country = "Italy", 
                   ate_vote = atelow.fz, se_vote = selow.fz, ci_upper = NA, 
                   ci_lower = NA, N = nlow.fz, published = 1, Notes = NA)

# Combine dataframes
quality = rbind(wsw17_high, wsw17_low, wsw18_high, wsw18_low, b_high, b_low,
                mv_high, mv_low, fz_high, fz_low) 

################################################################################
# Plot
################################################################################
# Create author-quality variable
quality$author_quality = paste(quality$author_reduced, quality$type, sep=" - ")

# Line breakes for labels
addline_format <- function(x,...){
    gsub(' - ','\n',x)
}

# Re-order
quality = quality %>% arrange(ate_vote) %>%
    mutate(author_quality = reorder(author_quality, -ate_vote))

# Plot
ggplot(quality, aes(ate_vote, author_quality, label = type)) +
  geom_point(aes(color = type, fill = type), size = 1.5) +
  geom_errorbarh(aes(y = author_quality, 
                     xmin = ate_vote - 1.96*se_vote, 
                     xmax = ate_vote + 1.96*se_vote),
                color="grey30", size=0.5, alpha = 0.5, height = 0.2) +
  geom_text(aes(label = country, x = 0.25, y = author_quality), size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlab("Change in vote share (percentage points)") + 
  scale_x_continuous(limits = c(-.9, 0.3), breaks=seq(-.9,0.3, .1)) +
  scale_color_manual(values = c("High quality" = "seagreen2", 
                                "Low quality" = "firebrick2")) +
  labs(color='Information', fill = 'Information')  +
  scale_y_discrete(breaks=quality$author_quality, 
                   labels = quality$author_reduced) +
  theme_classic() +
  theme(axis.title.y=element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text=element_text(size = 9)) +
  theme(axis.text.x = element_text(size = 9)) +
  theme(legend.position = "bottom") +
  theme(legend.text=element_text(size=12)) +
  guides(colour = guide_legend(override.aes = list(size=5)))

ggsave("figs/quality.pdf", height = 5, width = 6)

