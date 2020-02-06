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
eggers = readRDS("data/experiment_data_eggers.Rds", refhook = NULL)

################################################################################
# Boas, Hidalgo, and Melo: American Journal of Political Science 2018
################################################################################
# Run model
bhm_model = lm_robust(vote_vig ~ vig_treatment, data = bhm)

# Extract point estimates
ate_bhm = summary(bhm_model)$coef[2, 1]

# Extract standard errors
se_bhm = summary(bhm_model)$coef[2, 2]

# Extract number of observations
n_bhm = nobs(bhm_model)

# Reported binned p-value
p_bhm = "<0.01"

# Reported p-value
p_report_bhm = 0

# Replicated p-value
p_rep_bhm = summary(bhm_model)$coef[2, 4]

################################################################################
# Winters and Weitz-Shapiro: Comparative Politics 2013
################################################################################
# All corrupt vignettes
wsw13$vote_vig_cont <- ifelse(wsw13$votescale > 2, 1, 0)
reg_wsw13 = lm_robust(vote_vig_cont ~ corruptvignette, data = wsw13)

# Extract point estimates
ate_wsw13 <- summary(reg_wsw13)$coef[2, 1]

# Extract standard errors
se_wsw13 = summary(reg_wsw13)$coef[2, 2]

# Extract number of observations
n_wsw13 = nobs(reg_wsw13)

# Reported binned p-value
p_wsw13 = "<0.01"

# Reported p-value
p_report_wsw13 = 0

# Replicated p-value
p_rep_wsw13 = summary(reg_wsw13)$coef[2, 4]

################################################################################
# Winters and Weitz-Shapiro: Political Research Quarterly 2016
################################################################################
# Create all variable
wsw16$corrupt = with(wsw16, ifelse(vinheta != "VINHETA 1" & vinheta != "VINHETA 2",
                                   1, 0))

# Create vote dummy
wsw16$vote = with(wsw16, ifelse(voteintent > 2, 1, 0))

# Run models
reg_wsw16_all = lm_robust(vote ~ corrupt, data = wsw16) # Same experiment as JOP 2017

# Reported p-value
p_wsw16 = "<0.01"

# Reported p-value
p_report_wsw16 = 0

# Replicated p-value
p_rep_wsw16 = summary(reg_wsw16_all)$coef[2, 4]

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
reg_wsw17_all = lm_robust(vote ~ corrupt, data = wsw17) # Same experiment as PRQ 2016

# Extract point estimates
ate_wsw17 = summary(reg_wsw17_all)$coef[2, 1]

# Extract standard errors
se_wsw17 = summary(reg_wsw17_all)$coef[2, 2]

# Extract number of observations
n_wsw17 = nobs(reg_wsw17_all)

# Reported p-value
p_wsw17 = "<0.01"

# Reported p-value
p_report_wsw17 = 0

# Replicated p-value
p_rep_wsw17 = summary(reg_wsw17_all)$coef[2, 4]

################################################################################
# Winters and Weitz-Shapiro: Politial Science Research and Methods 2018
################################################################################
# Create all variable
wsw18$corrupt = with(wsw18, ifelse(vignette <= 3 | vignette >= 10, 0, 1))

# Create vote dummy
wsw18$vote = with(wsw18, ifelse(voteintent > 2, 1, 0))

# Run model
reg_wsw18 = lm_robust(vote ~ corrupt, data = wsw18)

# Extract point estimates
ate_wsw18 = summary(reg_wsw18)$coef[2, 1]

# Extract standard errors
se_wsw18 = summary(reg_wsw18)$coef[2, 2]

# Extract number of observations
n_wsw18 = nobs(reg_wsw18)

# Reported p-value
p_wsw18 = "<0.01"

# Reported p-value
p_report_wsw18 = 0

# Replicated p-value
p_rep_wsw18 = summary(reg_wsw18)$coef[2, 4]

################################################################################
# Klasnja & Tucker: Electoral Studies 2013
################################################################################
ate_sweden = ((-0.768/4 + -.814/4))/2
se_sweden = ((.090/4 + .097/4))/2
n_sweden = 1852
p_sweden = "<0.01"
p_report_sweden = 2*pt(-abs(ate_sweden/se_sweden), df = n_sweden-1)
p_rep_sweden = NA

ate_moldova = ((0.031/4) + (-.503/4))/2
se_moldova = ((.165/4 + .166/4))/2
n_moldova = 459
p_moldova = "<0.01"
p_report_moldova = 2*pt(-abs(ate_moldova/se_moldova), df = n_moldova-1)
p_rep_moldova = NA

################################################################################
# De Figuerido, Hidalgo, and Kasahara: Working Paper
################################################################################
ate_defig = .01
se_defig = (.07 - .01)/1.96 # estimate standard error from confidence interval
n_defig = 200
p_defig = ">0.1"
p_report.defig = 2*pt(-abs(ate_defig/se_defig), df = n_defig-1)
p_rep.defig = NA

################################################################################
# Mares and Visconti: Political Science Research and Methods 2019
################################################################################
# Pool corruption into one treatment
mv$corrupt = with(mv, ifelse(atinte == "Sentenced" | 
                             atinte == "Investigated", 1, 0))

# Run model
mv_corrupt = lm_robust(outcome ~ corrupt, data = mv, clusters = idnum)

# Extract point estimates
ate_mv = summary(mv_corrupt, cluster = idnum)$coef[2, 1]

# Extract standard errors
se_mv = summary(mv_corrupt, cluster = idnum)$coef[2, 2]

# Extract number of observations
n_mv = 502

# Reported p-value
p_mv = "<0.01"

# Reported p-value
p_report_mv = 0

# Replicated p-value
p_rep_mv = summary(mv_corrupt)$coef[2, 4]

################################################################################
# Breitenstein: Research and Politics 2019
################################################################################
# Pool corruption into one treatment
b$corrupt = with(b, ifelse(corruption == "Honest", 0, 1))
# Run model
reg_b = lm_robust(Y ~ corrupt, data = b, clusters = id)

# Extract point estimates
ate_b = summary(reg_b, cluster = idnum)$coef[2, 1]

# Extract standard errors
se_b = summary(reg_b, cluster = idnum)$coef[2, 2]

# Extract number of observations
n_b = 2275

# Reported p-value
p_b = "<0.01"

# Reported p-value
p_report_b = 0

# Replicated p-value
p_rep_b = summary(reg_b)$coef[2, 4]

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
ate_fz = summary(fz_corrupt, cluster = IDContatto)$coef[2, 1]

# Extract standard errors
se_fz = summary(fz_corrupt, cluster = IDContatto)$coef[2, 2]

# Extract number of observations
n_fz = 347

# Reported p-value
p_fz = "<0.01"

# Reported p-value
p_report_fz = 0

# Replicated p-value
p_rep_fz = summary(fz_corrupt)$coef[2, 4]

################################################################################
# Eggers, Vivyan, and Wagner: Journal of Politics 2017
################################################################################
# Run model
evw_corrupt <- lm_robust(voteinc ~ mp.misconduct, data = eggers, clusters = id)

# Extract point estimates
ate_evw = summary(evw_corrupt, cluster = id)$coef[2, 1]

# Extract standard errors
se_evw = summary(evw_corrupt, cluster = id)$coef[2, 2]

# Extract number of observations
n_evw = 1962

# Reported p-value
p_evw = "<0.01"

# Reported p-value
p_report_evw = 0

# Replicated p-value
p_rep_evw = summary(evw_corrupt)$coef[2, 4]

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
ate_klt_arg = summary(klt_corrupt_arg, cluster = uniq_id)$coef[2, 1]
ate_klt_chile = summary(klt_corrupt_chile, cluster = uniq_id)$coef[2, 1]
ate_klt_uru = summary(klt_corrupt_uru, cluster = uniq_id)$coef[2, 1]

# Extract standard errors
se_klt_arg = summary(klt_corrupt_arg, cluster = idnum)$coef[2, 2]
se_klt_chile = summary(klt_corrupt_chile, cluster = idnum)$coef[2, 2]
se_klt_uru = summary(klt_corrupt_uru, cluster = idnum)$coef[2, 2]

# Extract number of observations
n_klt_arg = 1528
n_klt_chile = 1625
n_klt_uru = 1514

# Reported p-value
p_klt_arg = "<0.01"
p_klt_chile = "<0.01"
p_klt_uru = "<0.01"

# Reported p-value
p_report_klt_arg = 0
p_report_klt_chile = 0
p_report_klt_uru = 0

# Replicated p-value
p_rep_klt_arg = summary(klt_corrupt_arg)$coef[2, 4]
p_rep_klt_chile = summary(klt_corrupt_chile)$coef[2, 4]
p_rep_klt_uru = summary(klt_corrupt_uru)$coef[2, 4]

################################################################################
# Chauchard, Klasnja, and Harish: Journal of Politics 2019
################################################################################
# Pool corruption into one treatment
ckh = ckh %>% filter(legal != 0)
ckh$corrupt = with(ckh, ifelse(legal == 2, 1, 0))

# Run model
ckh_corrupt = lm_robust(dv_vote ~ corrupt, data = ckh, clusters = id)

# Extract point estimates
ate_ckh = summary(ckh_corrupt, cluster = id)$coef[2, 1]

# Extract standard errors
se_ckh = summary(ckh_corrupt, cluster = id)$coef[2, 2]

# Extract number of observations
n_ckh = length(unique(ckh$id))

# Reported p-value
p_ckh = "<0.01"

# Reported p-value
p_report_ckh = 0

# Replicated p-value
p_rep_ckh = summary(ckh_corrupt)$coef[2, 4]

################################################################################
# Agerberg: Comparative Political Studies 2019
################################################################################
ate_ager = (-.33 + -.32)/2 # Average of two corruption treatments
se_ager = 0.011 # SE identical across two corruption treatments
n_ager = 2017
p_ager = "<0.01"
p_report_ager = 2*pt(-abs(ate_ager/se_ager), df = n_ager-1)
p_rep_ager = NA

################################################################################
# Avenberg: Journal of Politics in Latin America 2019
################################################################################
ate_aven = -3.37/6
se_aven = 0.2/6
n_aven = 744
p_aven = "<0.01"
p_report_aven = 2*pt(-abs(ate_aven/se_aven), df = n_aven-1)
p_rep_aven = NA

################################################################################
# Vera Rojas: Political Studies 2019
################################################################################
ate_vera = -1.18/6
se_vera = 0.11/6
n_vera = 1308
p_vera = "<0.01"
p_report_vera = 0
p_rep_vera = NA

################################################################################
# Azfar and Nelson: Public Choice 2007 (lab)
################################################################################
ate_an = -.252
t_stat_an = 2.57
se_an = ate_an/t_stat_an
n_an = 132
p_an = "<0.01"
p_report_an = 2*pt(-abs(t_stat_an), df = n_an-1)
p_rep_an = NA

################################################################################
# Add each study to meta analysis dataframe
################################################################################
# Boas, Hidalgo, and Melo: American Journal of Political Science 2018
bhm = data.frame(type="Survey", year=2018 , author = "Boas, Hidalgo, and Melo", 
                   author_reduced = "Boas et al.", country = "Brazil", 
                   ate_vote = ate_bhm, se_vote = se_bhm, ci_upper = NA,
                   p_reported = p_report_bhm, p_replicated = p_rep_bhm,
                   ci_lower = NA, N = n_bhm, 
                   published = 1, Notes = NA)

# Winters and Weitz-Shapiro: Journal of Politics 2017
wsw17 = data.frame(type="Survey", year=2016 , author = "Winters & Weitz-Shapiro", 
                   author_reduced = "Winters & Weitz-Shapiro 2017", country = "Brazil", 
                   ate_vote = ate_wsw17, se_vote = se_wsw17, ci_upper = NA,
                   p_reported = p_report_wsw17, p_replicated = p_rep_wsw17, 
                   ci_lower = NA, N = n_wsw17, 
                   published = 1, Notes = NA)

# Winters and Weitz-Shapiro: Comparative Politics 2013
wsw13 = data.frame(type="Survey", year=2013 , author = "Winters & Weitz-Shapiro", 
                   author_reduced = "Winters & Weitz-Shapiro 2013", country = "Brazil", 
                   ate_vote = ate_wsw13, se_vote = se_wsw13, ci_upper = NA,
                   p_reported = p_report_wsw13, p_replicated = p_rep_wsw13,
                   ci_lower = NA,N = n_wsw13, published = 1, Notes = NA)

# Winters and Weitz-Shapiro: Politial Science Research and Methods 2018
wsw18 = data.frame(type="Survey", year=2018 , author = "Winters & Weitz-Shapiro", 
                   author_reduced = "Winters & Weitz-Shapiro 2018", country = "Argentina", 
                   ate_vote = ate_wsw18, se_vote = se_wsw18, ci_upper = NA, 
                   p_reported = p_report_wsw18, p_replicated = p_rep_wsw18, 
                   ci_lower = NA, N = n_wsw18, 
                   published = 1, Notes = NA)

# Klasnja & Tucker: Electoral Studies 2013 -  Sweden
kt_sweden = data.frame(type="Survey", year=2013 , author = "Klasna & Tucker", 
                   author_reduced = "Klasna & Tucker (Sweden)", country = "Sweden", 
                   ate_vote = ate_sweden, se_vote = se_sweden, ci_upper = NA,
                   p_reported = p_report_sweden, p_replicated = p_rep_sweden,
                   ci_lower = NA, N = n_sweden, 
                   published = 1, Notes = NA)

# Klasnja & Tucker: Electoral Studies 2013 - Moldova
kt_moldova = data.frame(type="Survey", year=2013 , author = "Klasna & Tucker", 
                   author_reduced = "Klasna & Tucker (Moldova)", country = "Moldova", 
                   ate_vote = ate_moldova, se_vote = se_moldova, ci_upper = NA, 
                   p_reported = p_report_moldova, p_replicated = p_rep_moldova,
                   ci_lower = NA, published = 1, N = n_moldova, Notes = NA)

# Mares and Visconti: Political Science Research and Methods 2019
mv = data.frame(type="Survey", year=2019 , author = "Mares & Visconti", 
                   author_reduced = "Mares & Visconti", country = "Romania", 
                   ate_vote = ate_mv, se_vote = se_mv, ci_upper = NA, 
                   p_reported = p_report_mv, p_replicated = p_rep_mv,
                   ci_lower = NA, N = n_mv, published = 1, 
                   Notes = NA)

# Breitenstein: Research and Politics 2019
b = data.frame(type="Survey", year=2019 , author = "Breitenstein", 
                   author_reduced = "Breitenstein", country = "Spain", 
                   ate_vote = ate_b, se_vote = se_b, ci_upper = NA,
                   p_reported = p_report_b, p_replicated = p_rep_b,
                   ci_lower = NA, N = n_b, published = 1, 
                   Notes = NA)

# Franchino and Zucchini: Political Science Research and Methods 2014
fz = data.frame(type="Survey", year=2014 , author = "Franchino and Zucchini", 
                   author_reduced = "Franchino and Zucchini", country = "Italy", 
                   ate_vote = ate_fz, se_vote = se_fz, 
                   p_reported = p_report_fz, p_replicated = p_rep_fz,
                   ci_upper = NA, ci_lower = NA, N = n_fz, 
                   published = 1, Notes = NA)

# Eggers, Vivyan, and Wagner: Journal of Politics 2017
evw = data.frame(type="Survey", year=2017 , author = "Eggers, Vivyan, and Wagner", 
                   author_reduced = "Eggers et al.", country = "UK", 
                   ate_vote = ate_evw, se_vote = se_evw, 
                   p_reported = p_report_evw, p_replicated = p_rep_evw,
                   ci_upper = NA, ci_lower = NA, N = n_evw, 
                   published = 1, Notes = NA)

# Klasnja, Lupu, and Tucker: Working Paper - Argentina
klt_arg = data.frame(type="Survey", year=2017 , author = "Klasna, Lupu, and Tucker", 
                   author_reduced = "Klasna et al. (Argentina)", country = "Argentina", 
                   ate_vote = ate_klt_arg, se_vote = se_klt_arg, ci_upper = NA, 
                   p_reported = p_report_klt_arg, p_replicated = p_rep_klt_arg,
                   ci_lower = NA, N = n_klt_arg, published = 0, Notes = NA)

# Klasnja, Lupu, and Tucker: Working Paper - Chile
klt_chile = data.frame(type="Survey", year=2017 , author = "Klasna, Lupu, and Tucker", 
                   author_reduced = "Klasna et al. (Chile)", country = "Chile", 
                   ate_vote = ate_klt_chile, se_vote = se_klt_chile, ci_upper = NA, 
                   p_reported = p_report_klt_chile, p_replicated = p_rep_klt_chile,
                   ci_lower = NA, N = n_klt_chile, 
                   published = 0, Notes = NA)

# Klasnja, Lupu, and Tucker: Working Paper - Uruguay
klt_uru = data.frame(type="Survey", year=2017 , author = "Klasna, Lupu, and Tucker", 
                   author_reduced = "Klasna et al. (Uruguay)", country = "Uruguay", 
                   ate_vote = ate_klt_uru, se_vote = se_klt_uru, ci_upper = NA,
                   p_reported = p_report_klt_uru, p_replicated = p_rep_klt_uru,
                   ci_lower = NA, N = n_klt_uru, 
                   published = 0, Notes = NA)

# Chauchard, Klasnja, and Harish: Journal of Politics 2019
ckh = data.frame(type="Survey", year=2019, author = "Chauchard, Klasnja, and Harish", 
                   author_reduced = "Chauchard et al.", country = "India", 
                   ate_vote = ate_ckh, se_vote = se_ckh, ci_upper = NA, 
                   p_reported = p_report_ckh, p_replicated = p_rep_ckh,
                   ci_lower = NA, N = n_ckh, 
                   published = 1, Notes = NA)

# Agerberg: Comparative Political Studies 2019
ager = data.frame(type="Survey", year=2019 , author = "Agerberg", 
                   author_reduced = "Agerberg", country = "Spain", 
                   ate_vote = ate_ager, se_vote = se_ager, ci_upper = NA, 
                   ci_lower = NA, N = n_ager,
                   p_reported = p_report_ager, p_replicated = p_rep_ager,
                   published = 1, Notes = NA)

# Avenberg: Journal of Politics in Latin America 2019
aven = data.frame(type="Survey", year=2019 , author = "Avenberg", 
                   author_reduced = "Avenberg", country = "Brazil", 
                   ate_vote = ate_aven, se_vote = se_aven, ci_upper = NA, 
                   ci_lower = NA, N = n_aven,
                   p_reported = p_report_aven, p_replicated = p_rep_aven,
                   published = 1, Notes = NA)

# Vera Rojas: Political Studies 2019
vera = data.frame(type="Survey", year=2019 , author = "Vera Rojas", 
                   author_reduced = "Vera Rojas", country = "Peru", 
                   ate_vote = ate_vera, se_vote = se_vera, ci_upper = NA, 
                   ci_lower = NA, N = n_vera,
                   p_reported = p_report_vera, p_replicated = p_rep_vera,
                   published = 1, Notes = NA)

# Azfar and Nelson: Public Choice 2007 (lab)
an = data.frame(type="Lab", year=2007 , author = "Azfar and Nelson", 
                   author_reduced = "Azfar and Nelson", country = "USA", 
                   ate_vote = ate_an, se_vote = se_an, ci_upper = NA, 
                   ci_lower = NA, N = n_an,
                   p_reported = p_report_an, p_replicated = p_rep_an,
                   published = 1, Notes = NA)

# Combine dataframes
meta = rbind(results, bhm, wsw17, wsw13, wsw18, kt_sweden, kt_moldova, ckh,
             mv, b, fz, evw, klt_arg, klt_chile, klt_uru, ager, an, aven, vera) 

# Save combined dataframe
save(meta, file = "data/meta.RData")
