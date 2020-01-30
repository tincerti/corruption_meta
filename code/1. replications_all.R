################################################################################
# Libraries and Import
################################################################################
rm(list=ls())

# Libraries
library(foreign)
library(readxl)
library(readstata13)
library(estimatr)
library(tidyverse)

# Data imports
results = read_excel("data/study_results.xlsx") # Estimates not from replications
wsw17 = read.dta('data/Weitz-ShapiroWinters_JOP_Credibility_ReplicationData.dta')
wsw13 = read.dta('data/WintersWeitzShapiro_2013_Replication.dta')
wsw16 = read.dta('data/WintersWeitz-Shapiro_PRQ_Specificity_ReplicationData.dta')
wsw18 = read.dta13('data/WintersWeitz-Shapiro_PSRM_Argentina_ReplicationData.dta')
fz = read.dta('data/franchino_zucchini.dta')
mv = read.dta('data/mares_visconti.dta')
b = read.dta13('data/choosing_crook_clean.dta')
klt = read.dta('data/analysis-data.dta')
bhm = read.csv('data/panel_cleaned.csv')
ckh = read.dta13('data/chauchard_klasnja_harish.dta')

################################################################################
# Boas, Hidalgo, and Melo: American Journal of Political Science 2018
################################################################################
# Run model
bhm_model = lm_robust(vote_vig ~ vig_treatment, data = bhm)

# Extract point estimates
ate.bhm = summary(bhm_model)$coef[2, 1]

# Extract standard errors
se.bhm = summary(bhm_model)$coef[2, 2]

# Extract number of observations
n.bhm = nobs(bhm_model)

# Reported binned p-value
p.bhm = "<0.01"

# Reported p-value
p_report.bhm = 0

# Replicated p-value
p_rep.bhm = summary(bhm_model)$coef[2, 4]

################################################################################
# Winters and Weitz-Shapiro: Comparative Politics 2013
################################################################################
# All corrupt vignettes
wsw13$vote_vig_cont <- ifelse(wsw13$votescale > 2, 1, 0)
reg.wsw13 = lm_robust(vote_vig_cont ~ corruptvignette, data = wsw13)

# Extract point estimates
ate.wsw13 <- summary(reg.wsw13)$coef[2, 1]

# Extract standard errors
se.wsw13 = summary(reg.wsw13)$coef[2, 2]

# Extract number of observations
n.wsw13 = nobs(reg.wsw13)

# Reported binned p-value
p.wsw13 = "<0.01"

# Reported p-value
p_report.wsw13 = 0

# Replicated p-value
p_rep.wsw13 = summary(reg.wsw13)$coef[2, 4]

################################################################################
# Winters and Weitz-Shapiro: Political Research Quarterly 2016
################################################################################
# Create all variable
wsw16$corrupt = with(wsw16, ifelse(vinheta != "VINHETA 1" & vinheta != "VINHETA 2",
                                   1, 0))

# Create vote dummy
wsw16$vote = with(wsw16, ifelse(voteintent > 2, 1, 0))

# Run models
reg.wsw16.all = lm_robust(vote ~ corrupt, data = wsw16) # Same experiment as JOP 2017

# Reported p-value
p.wsw16 = "<0.01"

# Reported p-value
p_report.wsw16 = 0

# Replicated p-value
p_rep.wsw16 = summary(reg.wsw16.all)$coef[2, 4]

################################################################################
# Winters and Weitz-Shapiro: Journal of Politics 2017
################################################################################
# Create all variable
wsw17$corrupt = with(wsw17, ifelse(vinheta != "VINHETA 1" & 
                            vinheta != "VINHETA 2",
                            1, 0))

# Create vote dummy
wsw17$vote = with(wsw17, ifelse(voteintent > 2, 1, 0))

# Run models
reg.wsw17.all = lm_robust(vote ~ corrupt, data = wsw17) # Same experiment as PRQ 2016

# Extract point estimates
ate.wsw17 = summary(reg.wsw17.all)$coef[2, 1]

# Extract standard errors
se.wsw17 = summary(reg.wsw17.all)$coef[2, 2]

# Extract number of observations
n.wsw17 = nobs(reg.wsw17.all)

# Reported p-value
p.wsw17 = "<0.01"

# Reported p-value
p_report.wsw17 = 0

# Replicated p-value
p_rep.wsw17 = summary(reg.wsw17.all)$coef[2, 4]

################################################################################
# Winters and Weitz-Shapiro: Politial Science Research and Methods 2018
################################################################################
# Create all variable
wsw18$corrupt = with(wsw18, ifelse(vignette <= 3 | vignette >= 10, 0, 1))

# Create vote dummy
wsw18$vote = with(wsw18, ifelse(voteintent > 2, 1, 0))

# Run model
reg.wsw18 = lm_robust(vote ~ corrupt, data = wsw18)

# Extract point estimates
ate.wsw18 = summary(reg.wsw18)$coef[2, 1]

# Extract standard errors
se.wsw18 = summary(reg.wsw18)$coef[2, 2]

# Extract number of observations
n.wsw18 = nobs(reg.wsw18)

# Reported p-value
p.wsw18 = "<0.01"

# Reported p-value
p_report.wsw18 = 0

# Replicated p-value
p_rep.wsw18 = summary(reg.wsw18)$coef[2, 4]

################################################################################
# Klasnja & Tucker: Electoral Studies 2013
################################################################################
ate.sweden = ((-0.768/4 + -.814/4))/2
se.sweden = ((.090/4 + .097/4))/2
n.sweden = 1852
p.sweden = "<0.01"
p.report.sweden = 2*pt(-abs(ate.sweden/se.sweden), df = n.sweden-1)
p.rep.sweden = NA

ate.moldova = ((0.031/4) + (-.503/4))/2
se.moldova = ((.165/4 + .166/4))/2
n.moldova = 459
p.moldova = "<0.01"
p.report.moldova = 2*pt(-abs(ate.moldova/se.moldova), df = n.moldova-1)
p.rep.moldova = NA

################################################################################
# De Figuerido, Hidalgo, and Kasahara: Working Paper
################################################################################
ate.defig = .01
se.defig = (.07 - .01)/1.96 # estimate standard error from confidence interval
n.defig = 200
p.defig = ">0.1"
p.report.defig = 2*pt(-abs(ate.defig/se.defig), df = n.defig-1)
p.rep.defig = NA

################################################################################
# Mares and Visconti: Political Science Research and Methods 2019
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

# Reported p-value
p_report.mv = 0

# Replicated p-value
p_rep.mv = summary(mv_corrupt)$coef[2, 4]

################################################################################
# Breitenstein: Research and Politics 2019
################################################################################
# Pool corruption into one treatment
b$corrupt = with(b, ifelse(corruption == "Honest", 0, 1))
# Run model
reg.b = lm_robust(Y ~ corrupt, data = b, clusters = id)

# Extract point estimates
ate.b = summary(reg.b, cluster = idnum)$coef[2, 1]

# Extract standard errors
se.b = summary(reg.b, cluster = idnum)$coef[2, 2]

# Extract number of observations
n.b = 2275

# Reported p-value
p.b = "<0.01"

# Reported p-value
p_report.b = 0

# Replicated p-value
p_rep.b = summary(reg.b)$coef[2, 4]

################################################################################
# Franchino and Zucchini: Political Science Research and Methods 2014
################################################################################
# Remove NA outcome values - not sure why these are here
fz = fz %>% filter(!is.na(Y))

# Pool corruption into one treatment
fz$corrupt = with(fz, ifelse(corruption == "Convicted of corruption" |
                             corruption == "Investigated for corruption", 1, 0))

# Run model
fz_corrupt = lm_robust(Y ~ corrupt, data = fz, clusters = IDContatto)

# Extract point estimates
ate.fz = summary(fz_corrupt, cluster = IDContatto)$coef[2, 1]

# Extract standard errors
se.fz = summary(fz_corrupt, cluster = IDContatto)$coef[2, 2]

# Extract number of observations
n.fz = 347

# Reported p-value
p.fz = "<0.01"

# Reported p-value
p_report.fz = 0

# Replicated p-value
p_rep.fz = summary(fz_corrupt)$coef[2, 4]

################################################################################
# Eggers, Vivyan, and Wagner: Journal of Politics 2017
################################################################################
eggers = readRDS("data/experiment_data_eggers.Rds", refhook = NULL)

# Run model
evw_corrupt <- lm_robust(voteinc ~ mp.misconduct, data = eggers, clusters = id)

# Extract point estimates
ate.evw = summary(evw_corrupt, cluster = id)$coef[2, 1]

# Extract standard errors
se.evw = summary(evw_corrupt, cluster = id)$coef[2, 2]

# Extract number of observations
n.evw = 1962

# Reported p-value
p.evw = "<0.01"

# Reported p-value
p_report.evw = 0

# Replicated p-value
p_rep.evw = summary(evw_corrupt)$coef[2, 4]

################################################################################
# Klasnja, Lupu, and Tucker: Working Paper
################################################################################
# Pool corruption into one treatment
klt$corrupt = with(klt, ifelse(corrupttreat == "No bribes", 0, 1))

# Split dataset into individual countries
klt_arg = klt %>% filter(country == "Argentina")
klt_chile = klt %>% filter(country == "Chile")
klt_uru = klt %>% filter(country == "Uruguay")

# Run models
klt_corrupt_arg = lm_robust(vote ~ corrupt, data = klt_arg, clusters = uniq_id)
klt_corrupt_chile = lm_robust(vote ~ corrupt, data = klt_chile, clusters = uniq_id)
klt_corrupt_uru = lm_robust(vote ~ corrupt, data = klt_uru, clusters = uniq_id)

# Extract point estimates
ate.klt_arg = summary(klt_corrupt_arg, cluster = uniq_id)$coef[2, 1]
ate.klt_chile = summary(klt_corrupt_chile, cluster = uniq_id)$coef[2, 1]
ate.klt_uru = summary(klt_corrupt_uru, cluster = uniq_id)$coef[2, 1]

# Extract standard errors
se.klt_arg = summary(klt_corrupt_arg, cluster = idnum)$coef[2, 2]
se.klt_chile = summary(klt_corrupt_chile, cluster = idnum)$coef[2, 2]
se.klt_uru = summary(klt_corrupt_uru, cluster = idnum)$coef[2, 2]

# Extract number of observations
n.klt_arg = 1528
n.klt_chile = 1625
n.klt_uru = 1514

# Reported p-value
p.klt_arg = "<0.01"
p.klt_chile = "<0.01"
p.klt_uru = "<0.01"

# Reported p-value
p_report.klt_arg = 0
p_report.klt_chile = 0
p_report.klt_uru = 0

# Replicated p-value
p_rep.klt_arg = summary(klt_corrupt_arg)$coef[2, 4]
p_rep.klt_chile = summary(klt_corrupt_chile)$coef[2, 4]
p_rep.klt_uru = summary(klt_corrupt_uru)$coef[2, 4]

################################################################################
# Chauchard, Klasnja, and Harish: Journal of Politics 2019
################################################################################
# Pool corruption into one treatment
ckh = ckh %>% filter(legal != 0)
ckh$corrupt = with(ckh, ifelse(legal == 2, 1, 0))

# Run model
ckh_corrupt = lm_robust(dv_vote ~ corrupt, data = ckh, clusters = id)

# Extract point estimates
ate.ckh = summary(ckh_corrupt, cluster = id)$coef[2, 1]

# Extract standard errors
se.ckh = summary(ckh_corrupt, cluster = id)$coef[2, 2]

# Extract number of observations
n.ckh = length(unique(ckh$id))

# Reported p-value
p.ckh = "<0.01"

# Reported p-value
p_report.ckh = 0

# Replicated p-value
p_rep.ckh = summary(ckh_corrupt)$coef[2, 4]

################################################################################
# Agerberg: Comparative Political Studies 2019
################################################################################
ate.ager = (-.33 + -.32)/2 # Average of two corruption treatments
se.ager = 0.011 # SE identical across two corruption treatments
n.ager = 2017
p.ager = "<0.01"
p.report.ager = 2*pt(-abs(ate.ager/se.ager), df = n.ager-1)
p.rep.ager = NA

################################################################################
# Avenberg: Journal of Politics in Latin America 2019
################################################################################
ate.aven = -3.37/6
se.aven = 0.2/6
n.aven = 744
p.aven = "<0.01"
p.report.aven = 2*pt(-abs(ate.aven/se.aven), df = n.aven-1)
p.rep.aven = NA

################################################################################
# Vera Rojas: Political Studies 2019
################################################################################
ate.vera = -1.18/6
se.vera = 0.11/6
n.vera = 1308
p.vera = "<0.01"
p.report.vera = 0
p.rep.vera = NA

################################################################################
# Azfar and Nelson: Public Choice 2007 (lab)
################################################################################
ate.an = -.252
t.stat.an = 2.57
se.an = ate.an/t.stat.an
n.an = 132
p.an = "<0.01"
p.report.an = 2*pt(-abs(t.stat.an), df = n.an-1)
p.rep.an = NA

################################################################################
# Add each study to meta analysis dataframe
################################################################################
# Boas, Hidalgo, and Melo: American Journal of Political Science 2018
bhm = data.frame(type="Survey", year=2018 , author = "Boas, Hidalgo, and Melo", 
                   author_reduced = "Boas et al.", country = "Brazil", 
                   ate_vote = ate.bhm, se_vote = se.bhm, ci_upper = NA,
                   p_reported = p_report.bhm, p_replicated = p_rep.bhm,
                   ci_lower = NA, N = n.bhm, 
                   published = 1, Notes = NA)

# Winters and Weitz-Shapiro: Journal of Politics 2017
wsw17 = data.frame(type="Survey", year=2016 , author = "Winters & Weitz-Shapiro", 
                   author_reduced = "Winters & Weitz-Shapiro 2017", country = "Brazil", 
                   ate_vote = ate.wsw17, se_vote = se.wsw17, ci_upper = NA,
                   p_reported = p_report.wsw17, p_replicated = p_rep.wsw17, 
                   ci_lower = NA, N = n.wsw17, 
                   published = 1, Notes = NA)

# Winters and Weitz-Shapiro: Comparative Politics 2013
wsw13 = data.frame(type="Survey", year=2013 , author = "Winters & Weitz-Shapiro", 
                   author_reduced = "Winters & Weitz-Shapiro 2013", country = "Brazil", 
                   ate_vote = ate.wsw13, se_vote = se.wsw13, ci_upper = NA,
                   p_reported = p_report.wsw13, p_replicated = p_rep.wsw13,
                   ci_lower = NA,N = n.wsw13, published = 1, Notes = NA)

# Winters and Weitz-Shapiro: Politial Science Research and Methods 2018
wsw18 = data.frame(type="Survey", year=2018 , author = "Winters & Weitz-Shapiro", 
                   author_reduced = "Winters & Weitz-Shapiro 2018", country = "Argentina", 
                   ate_vote = ate.wsw18, se_vote = se.wsw18, ci_upper = NA, 
                   p_reported = p_report.wsw18, p_replicated = p_rep.wsw18, 
                   ci_lower = NA, N = n.wsw18, 
                   published = 1, Notes = NA)

# Klasnja & Tucker: Electoral Studies 2013 -  Sweden
kt_sweden = data.frame(type="Survey", year=2013 , author = "Klasna & Tucker", 
                   author_reduced = "Klasna & Tucker (Sweden)", country = "Sweden", 
                   ate_vote = ate.sweden, se_vote = se.wsw13, ci_upper = NA,
                   p_reported = p.report.sweden, p_replicated = p.rep.sweden,
                   ci_lower = NA, N = n.sweden, 
                   published = 1, Notes = NA)

# Klasnja & Tucker: Electoral Studies 2013 - Moldova
kt_moldova = data.frame(type="Survey", year=2013 , author = "Klasna & Tucker", 
                   author_reduced = "Klasna & Tucker (Moldova)", country = "Moldova", 
                   ate_vote = ate.moldova, se_vote = se.moldova, ci_upper = NA, 
                   p_reported = p.report.moldova, p_replicated = p.rep.moldova,
                   ci_lower = NA, published = 1, N = n.moldova, Notes = NA)

# Mares and Visconti: Political Science Research and Methods 2019
mv = data.frame(type="Survey", year=2019 , author = "Mares & Visconti", 
                   author_reduced = "Mares & Visconti", country = "Romania", 
                   ate_vote = ate.mv, se_vote = se.mv, ci_upper = NA, 
                   p_reported = p_report.mv, p_replicated = p_rep.mv,
                   ci_lower = NA, N = n.mv, published = 1, 
                   Notes = NA)

# Breitenstein: Research and Politics 2019
b = data.frame(type="Survey", year=2019 , author = "Breitenstein", 
                   author_reduced = "Breitenstein", country = "Spain", 
                   ate_vote = ate.b, se_vote = se.b, ci_upper = NA,
                   p_reported = p_report.b, p_replicated = p_rep.b,
                   ci_lower = NA, N = n.b, published = 1, 
                   Notes = NA)

# Franchino and Zucchini: Political Science Research and Methods 2014
fz = data.frame(type="Survey", year=2014 , author = "Franchino and Zucchini", 
                   author_reduced = "Franchino and Zucchini", country = "Italy", 
                   ate_vote = ate.fz, se_vote = se.fz, 
                   p_reported = p_report.fz, p_replicated = p_rep.fz,
                   ci_upper = NA, ci_lower = NA, N = n.fz, 
                   published = 1, Notes = NA)

# Eggers, Vivyan, and Wagner: Journal of Politics 2017
evw = data.frame(type="Survey", year=2017 , author = "Eggers, Vivyan, and Wagner", 
                   author_reduced = "Eggers et al.", country = "UK", 
                   ate_vote = ate.evw, se_vote = se.evw, 
                   p_reported = p_report.evw, p_replicated = p_rep.evw,
                   ci_upper = NA, ci_lower = NA, N = n.evw, 
                   published = 1, Notes = NA)

# Klasnja, Lupu, and Tucker: Working Paper - Argentina
klt_arg = data.frame(type="Survey", year=2017 , author = "Klasna, Lupu, and Tucker", 
                   author_reduced = "Klasna et al. (Argentina)", country = "Argentina", 
                   ate_vote = ate.klt_arg, se_vote = se.klt_arg, ci_upper = NA, 
                   p_reported = p_report.klt_arg, p_replicated = p_rep.klt_arg,
                   ci_lower = NA, N = n.klt_arg, published = 0, Notes = NA)

# Klasnja, Lupu, and Tucker: Working Paper - Chile
klt_chile = data.frame(type="Survey", year=2017 , author = "Klasna, Lupu, and Tucker", 
                   author_reduced = "Klasna et al. (Chile)", country = "Chile", 
                   ate_vote = ate.klt_chile, se_vote = se.klt_chile, ci_upper = NA, 
                   p_reported = p_report.klt_chile, p_replicated = p_rep.klt_chile,
                   ci_lower = NA, N = n.klt_chile, 
                   published = 0, Notes = NA)

# Klasnja, Lupu, and Tucker: Working Paper - Uruguay
klt_uru = data.frame(type="Survey", year=2017 , author = "Klasna, Lupu, and Tucker", 
                   author_reduced = "Klasna et al. (Uruguay)", country = "Uruguay", 
                   ate_vote = ate.klt_uru, se_vote = se.klt_uru, ci_upper = NA,
                   p_reported = p_report.klt_uru, p_replicated = p_rep.klt_uru,
                   ci_lower = NA, N = n.klt_uru, 
                   published = 0, Notes = NA)

# Chauchard, Klasnja, and Harish: Journal of Politics 2019
ckh = data.frame(type="Survey", year=2019, author = "Chauchard, Klasnja, and Harish", 
                   author_reduced = "Chauchard et al.", country = "India", 
                   ate_vote = ate.ckh, se_vote = se.ckh, ci_upper = NA, 
                   p_reported = p_report.ckh, p_replicated = p_rep.ckh,
                   ci_lower = NA, N = n.ckh, 
                   published = 1, Notes = NA)

# Agerberg: Comparative Political Studies 2019
ager = data.frame(type="Survey", year=2019 , author = "Agerberg", 
                   author_reduced = "Agerberg", country = "Spain", 
                   ate_vote = ate.ager, se_vote = se.ager, ci_upper = NA, 
                   ci_lower = NA, N = n.ager,
                   p_reported = p.report.ager, p_replicated = p.rep.ager,
                   published = 1, Notes = NA)

# Avenberg: Journal of Politics in Latin America 2019
aven = data.frame(type="Survey", year=2019 , author = "Avenberg", 
                   author_reduced = "Avenberg", country = "Brazil", 
                   ate_vote = ate.aven, se_vote = se.aven, ci_upper = NA, 
                   ci_lower = NA, N = n.aven,
                   p_reported = p.report.aven, p_replicated = p.rep.aven,
                   published = 1, Notes = NA)

# Vera Rojas: Political Studies 2019
vera = data.frame(type="Survey", year=2019 , author = "Vera Rojas", 
                   author_reduced = "Vera Rojas", country = "Peru", 
                   ate_vote = ate.vera, se_vote = se.vera, ci_upper = NA, 
                   ci_lower = NA, N = n.vera,
                   p_reported = p.report.vera, p_replicated = p.rep.vera,
                   published = 1, Notes = NA)

# Azfar and Nelson: Public Choice 2007 (lab)
an = data.frame(type="Lab", year=2007 , author = "Azfar and Nelson", 
                   author_reduced = "Azfar and Nelson", country = "USA", 
                   ate_vote = ate.an, se_vote = se.an, ci_upper = NA, 
                   ci_lower = NA, N = n.an,
                   p_reported = p.report.an, p_replicated = p.rep.an,
                   published = 1, Notes = NA)

# Combine dataframes
meta = rbind(results, bhm, wsw17, wsw13, wsw18, kt_sweden, kt_moldova, ckh,
             mv, b, fz, evw, klt_arg, klt_chile, klt_uru, ager, an, aven, vera) 

# Save combined dataframe
save(meta, file = "data/meta.RData")
