################################################################################
# Libraries and Import
################################################################################
library(survey)
library(huxtable)
library(foreign)
library(readxl)
library(readstata13)
library(estimatr)

# Data imports
results = read_excel("data/study_results.xlsx")
wsw17 = read.dta('data/Weitz-ShapiroWinters_JOP_Credibility_ReplicationData.dta')
wsw13 = read.dta('data/WintersWeitzShapiro_2013_Replication.dta')
wsw16 = read.dta('data/WintersWeitz-Shapiro_PRQ_Specificity_ReplicationData.dta')
wsw18 = read.dta13('data/WintersWeitz-Shapiro_PSRM_Argentina_ReplicationData.dta')
fz = read.dta('data/franchino_zucchini.dta')
mv = read.dta('data/mares_visconti.dta')
b = read.dta13('data/choosing_crook_clean.dta')

################################################################################
# Winters/Weitz-Shapiro JOP
################################################################################
# Create all variable
wsw17$corrupt = with(wsw17, ifelse(vinheta != "VINHETA 1" & vinheta != "VINHETA 2",
                                   1, 0))

# Create vote dummy
wsw17$vote = with(wsw17, ifelse(voteintent > 2, 1, 0))

# Run models
reg.wsw17.all = lm(vote ~ corrupt, data = wsw17)

# Extract point estimates
ate.wsw17 = summary(reg.wsw17.all)$coef[2, 1]

# Extract standard errors
se.wsw17 = summary(reg.wsw17.all)$coef[2, 2]

# Extract number of observations
n.wsw17 = nobs(reg.wsw17.all)

# Reported p-value
p.wsw17 = "<0.01"

################################################################################
# Winters/Weitz-Shapiro CP
################################################################################
# All corrupt vignettes
wsw13$vote_vig_cont <- ifelse(wsw13$votescale > 2, 1, 0)
reg.wsw13 = lm(vote_vig_cont ~ corruptvignette, data = wsw13)

# Extract point estimates
ate.wsw13 <- summary(reg.wsw13)$coef[2, 1]

# Extract standard errors
se.wsw13 = summary(reg.wsw13)$coef[2, 2]

# Extract number of observations
n.wsw13 = nobs(reg.wsw13)

# Reported p-value
p.wsw13 = "<0.01"

################################################################################
# Winters/Weitz-Shapiro PRQ
################################################################################
# Create all variable
wsw16$corrupt = with(wsw16, ifelse(vinheta != "VINHETA 1" & vinheta != "VINHETA 2",
                                   1, 0))

# Create vote dummy
wsw16$vote = with(wsw16, ifelse(voteintent > 2, 1, 0))

# Run models
reg.wsw16.all = lm(vote ~ corrupt, data = wsw16) # Same experiment

# Reported p-value
p.wsw16 = "<0.01"

################################################################################
# Winters/Weitz-Shapiro PSRM
################################################################################
# Create all variable
wsw18$corrupt = with(wsw18, ifelse(vignette <= 3 | vignette >= 10, 0, 1))

# Create vote dummy
wsw18$vote = with(wsw18, ifelse(voteintent > 2, 1, 0))

# Run model
reg.wsw18 = lm(vote ~ corrupt, data = wsw18)

# Extract point estimates
ate.wsw18 = summary(reg.wsw18)$coef[2, 1]

# Extract standard errors
se.wsw18 = summary(reg.wsw18)$coef[2, 2]

# Extract number of observations
n.wsw18 = nobs(reg.wsw18)

# Reported p-value
p.wsw18 = "<0.01"

################################################################################
# Klasnja & Tucker
################################################################################
ate.sweden = ((-0.768/4 + -.814/4))/2
se.sweden = ((.090/4 + .097/4))/2
n.sweden = 1852
p.sweden = "<0.01"

ate.moldova = ((0.031/4) + (-.503/4))/2
se.moldova = ((.165/4 + .166/4))/2
n.moldova = 459
p.moldova = "<0.01"

################################################################################
# De Figuerido working paper survey experiment
################################################################################
ate.defig = .01
se.defig = (.07 - .01)/1.96 # estimate standard error from confidence interval
n.defig = 200
p.defig = ">0.1"

################################################################################
# Mares and Visconti 2019
################################################################################
# Pool corruption into one treatment
mv$corrupt = with(mv, ifelse(atinte == "Sentenced" | 
                             atinte == "Investigated", 1, 0))

# Run model
mv_corrupt = lm_robust(outcome ~ corrupt, data = mv, clusters = idnum)

# Extract point estimates
ate.mv = summary(mv_corrupt, cluster = idnum)$coef[2, 1]

# Extract standard errors
se.mv = summary(mv_corrupt, cluster = idnum)$coef[2, 2]

# Extract number of observations
n.mv = 502

# Reported p-value
p.mv = "<0.01"

################################################################################
# Breitenstein (No replication data?)
################################################################################
# Pool corruption into one treatment
b$corrupt = with(b, ifelse(corruption == "Honest", 0, 1))
# Run model
reg.b = lm_robust(Y ~ corrupt, data = b, clusters = id)
summary(reg.b)

# Extract point estimates
ate.b = summary(reg.b, cluster = idnum)$coef[2, 1]

# Extract standard errors
se.b = summary(reg.b, cluster = idnum)$coef[2, 2]

# Extract number of observations
n.b = 2275

# Reported p-value
p.b = "<0.01"

################################################################################
# Franchino and Zucchini 2014
################################################################################
# Remove NA outcome values - not sure why these are here
fz = fz %>% filter(!is.na(Y))

# Pool corruption into one treatment
fz$corrupt = with(fz, ifelse(corruption == "Convicted of corruption" |
                             corruption == "Investigated for corruption", 1, 0))

# Run model
fz_corrupt = lm_robust(Y ~ corrupt, data = fz, clusters = IDContatto)
summary(fz_corrupt, cluster = "IDContatto")

# Extract point estimates
ate.fz = summary(fz_corrupt, cluster = IDContatto)$coef[2, 1]

# Extract standard errors
se.fz = summary(fz_corrupt, cluster = IDContatto)$coef[2, 2]

# Extract number of observations
n.fz = 347

# Reported p-value
p.fz = "<0.01"

################################################################################
# Eggers, Vivyan, and Wagner 2017
################################################################################
eggers = readRDS("data/experiment_data_eggers.Rds", refhook = NULL)

# Run model
evw_corrupt <- lm_robust(voteinc ~ mp.misconduct, data = eggers, clusters = id)
summary(evw_corrupt, cluster = "id")

# Extract point estimates
ate.evw = summary(evw_corrupt, cluster = id)$coef[2, 1]

# Extract standard errors
se.evw = summary(evw_corrupt, cluster = id)$coef[2, 2]

# Extract number of observations
n.evw = 1962

# Reported p-value
p.evw = "<0.01"

################################################################################
# Agerberg (2019)
################################################################################
ate.ager = (-.33 + -.32)/2 # Average of two corruption treatments
se.ager = 0.011 # SE identical across two corruption treatments
n.ager = 2017
p.ager = "<0.01"

################################################################################
# Avenberg
################################################################################
ate.aven = -3.37/6
se.aven = 0.2/6
n.aven = 744
p.aven = "<0.01"

################################################################################
# Vera Rojas
################################################################################
ate.vera = -1.18/6
se.vera = 0.11/6
n.vera = 1308
p.vera = "<0.01"

################################################################################
# Azfar and Nelson (lab)
################################################################################
ate.an = -.252
t.stat.an = 2.57
se.an = ate.an/t.stat.an
n.an = 132
p.an = "<0.01"

################################################################################
# Add to meta analysis dataframe
################################################################################
# Winters/Weitz-Shapiro 2017
wsw17 = data.frame(type="Survey", year=2016 , author = "Winters & Weitz-Shapiro", 
                   author_reduced = "Winters & Weitz-Shapiro 2017", country = "Brazil", 
                   ate_vote = ate.wsw17, se_vote = se.wsw17, ci_upper = NA,
                   p_reported = p.wsw17, ci_lower = NA, N = n.wsw17, 
                   published = 1, Notes = NA)

# Winters/Weitz-Shapiro 2013
wsw13 = data.frame(type="Survey", year=2013 , author = "Winters & Weitz-Shapiro", 
                   author_reduced = "Winters & Weitz-Shapiro 2013", country = "Brazil", 
                   ate_vote = ate.wsw13, se_vote = se.wsw13, ci_upper = NA,
                   p_reported = p.wsw13,
                   ci_lower = NA,N = n.wsw13, published = 1, Notes = NA)

# Winters/Weitz-Shapiro 2018
wsw18 = data.frame(type="Survey", year=2018 , author = "Winters & Weitz-Shapiro", 
                   author_reduced = "Winters & Weitz-Shapiro 2018", country = "Argentina", 
                   ate_vote = ate.wsw18, se_vote = se.wsw18, ci_upper = NA, 
                   p_reported = p.wsw18, ci_lower = NA,N = n.wsw18, 
                   published = 1, Notes = NA)

# Klasna and Tucker Sweden
kt_sweden = data.frame(type="Survey", year=2013 , author = "Klasna & Tucker", 
                   author_reduced = "Klasna & Tucker (Sweden)", country = "Sweden", 
                   ate_vote = ate.sweden, se_vote = se.wsw13, ci_upper = NA,
                   p_reported = p.sweden, ci_lower = NA, N = n.sweden, 
                   published = 1, Notes = NA)

# Klasna and Tucker Moldova
kt_moldova = data.frame(type="Survey", year=2013 , author = "Klasna & Tucker", 
                   author_reduced = "Klasna & Tucker (Moldova)", country = "Moldova", 
                   ate_vote = ate.moldova, se_vote = se.moldova, ci_upper = NA, 
                   p_reported = p.moldova,
                   ci_lower = NA, published = 1, N = n.moldova, Notes = NA)

# Mares and Visconti 2019
mv = data.frame(type="Survey", year=2019 , author = "Mares & Visconti", 
                   author_reduced = "Mares & Visconti", country = "Romania", 
                   ate_vote = ate.mv, se_vote = se.mv, ci_upper = NA, 
                   p_reported = p.mv, ci_lower = NA, N = n.mv, published = 1, 
                   Notes = NA)

# Breitenstein 2019
b = data.frame(type="Survey", year=2019 , author = "Breitenstein", 
                   author_reduced = "Breitenstein", country = "Spain", 
                   ate_vote = ate.b, se_vote = se.b, ci_upper = NA,
                   p_reported = p.b, ci_lower = NA, N = n.b, published = 1, 
                   Notes = NA)

# Franchino and Zucchini 2014
fz = data.frame(type="Survey", year=2014 , author = "Franchino and Zucchini", 
                   author_reduced = "Franchino and Zucchini", country = "Italy", 
                   ate_vote = ate.fz, se_vote = se.fz, ci_upper = NA, 
                   ci_lower = NA, N = n.fz,  p_reported = p.fz, 
                   published = 1, Notes = NA)

# Eggers, Vivyan, and Wagner 2017
evw = data.frame(type="Survey", year=2017 , author = "Eggers, Vivyan, and Wagner", 
                   author_reduced = "Eggers et al.", country = "UK", 
                   ate_vote = ate.evw, se_vote = se.evw, ci_upper = NA, 
                   p_reported = p.evw, ci_lower = NA, N = n.evw, 
                   published = 1, Notes = NA)

# Agerberg 2019
ager = data.frame(type="Survey", year=2019 , author = "Agerberg", 
                   author_reduced = "Agerberg", country = "Spain", 
                   ate_vote = ate.ager, se_vote = se.ager, ci_upper = NA, 
                   ci_lower = NA, N = n.ager,  p_reported = p.ager,
                   published = 1, Notes = NA)

# Azfar and Nelson 2007
an = data.frame(type="Lab", year=2007 , author = "Azfar and Nelson", 
                   author_reduced = "Azfar and Nelson", country = "USA", 
                   ate_vote = ate.an, se_vote = se.an, ci_upper = NA, 
                   ci_lower = NA, N = n.an,  p_reported = p.an,
                   published = 1, Notes = NA)

# Avenberg
aven = data.frame(type="Survey", year=2019 , author = "Avenberg", 
                   author_reduced = "Avenberg", country = "Brazil", 
                   ate_vote = ate.aven, se_vote = se.aven, ci_upper = NA, 
                   ci_lower = NA, N = n.aven,  p_reported = p.aven,
                   published = 1, Notes = NA)

vera = data.frame(type="Survey", year=2019 , author = "Vera Rojas", 
                   author_reduced = "Vera Rojas", country = "Peru", 
                   ate_vote = ate.vera, se_vote = se.vera, ci_upper = NA, 
                   ci_lower = NA, N = n.vera,  p_reported = p.vera,
                   published = 1, Notes = NA)

# Combine dataframes
meta = rbind(results, wsw17, wsw13, wsw18, kt_sweden, kt_moldova,
             mv, b, fz, evw, ager, an, aven, vera) 

# Save combined dataframe
save(meta, file = "data/meta.RData")
