################################################################################
# Libraries and Import
################################################################################
library(survey)
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


# Define scaling function
scale01 <- function(x)
  (x - min(x, na.rm = T)) / diff(range(x, na.rm = T))

################################################################################
# Winters/Weitz-Shapiro JOP
################################################################################
# Rescale outcome variable
wsw17$vote_vig_cont <- scale01(wsw17$voteintent)

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

################################################################################
# Winters/Weitz-Shapiro CP
################################################################################
# Rescale outcome variable
wsw13$vote_vig_cont <- scale01(wsw13$votescale)

# All corrupt vignettes
wsw13$vote_vig_cont <- ifelse(wsw13$votescale > 2, 1, 0)
reg.wsw13 = lm(vote_vig_cont ~ corruptvignette, data = wsw13)

# Extract point estimates
ate.wsw13 <- summary(reg.wsw13)$coef[2, 1]

# Extract standard errors
se.wsw13 = summary(reg.wsw13)$coef[2, 2]

# Extract number of observations
n.wsw13 = nobs(reg.wsw13)

################################################################################
# Winters/Weitz-Shapiro 2015
################################################################################
ate.wsw15 = -1.53/3
t_val = qt(0.01/2, df = 1974) # Calculating the t-value using quantile function
se.wsw15 = abs(ate.wsw15)/abs(t_val) # Calculating standard error
n.wsw15 = 1974

################################################################################
# Winters/Weitz-Shapiro PRQ
################################################################################
# Rescale outcome variable
wsw16$vote_vig_cont <- scale01(wsw17$voteintent)

# Create all variable
wsw16$corrupt = with(wsw16, ifelse(vinheta != "VINHETA 1" & vinheta != "VINHETA 2",
                                   1, 0))

# Create vote dummy
wsw16$vote = with(wsw16, ifelse(voteintent > 2, 1, 0))

# Run models
reg.wsw16.all = lm(vote ~ corrupt, data = wsw16) # Same experiment

################################################################################
# Winters/Weitz-Shapiro PSRM
################################################################################
# Create all variable
wsw18$corrupt = with(wsw18, ifelse(vignette <= 3 | vignette >= 10, 0, 1))

# Create vote dummy
wsw18$vote = with(wsw18, ifelse(voteintent > 2, 1, 0))

# Run model
reg.wsw18 = lm(vote ~ corrupt, data = wsw18) # Same experiment

# Extract point estimates
ate.wsw18 = summary(reg.wsw18)$coef[2, 1]

# Extract standard errors
se.wsw18 = summary(reg.wsw18)$coef[2, 2]

# Extract number of observations
n.wsw18 = nobs(reg.wsw18)

################################################################################
# Klasnja & Tucker
################################################################################
ate.sweden = ((-0.768/4 + -.814/4))/2
se.sweden = ((.090/4 + .097/4))/2
n.sweden = 1852

ate.moldova = ((0.031/4) + (-.503/4))/2
se.moldova = ((.165/4 + .166/4))/2
n.moldova = 459

################################################################################
# De Figuerido working paper survey experiment
################################################################################
ate.defig = .01
se.defig = (.07 - .01)/1.96 # estimate standard error from confidence interval
n.defig = 200

################################################################################
# Mares and Visconti 2019
################################################################################
ate.mv.inv = -.18
ate.mv.conv = -.33
ate.mv.avg = (ate.mv.inv + ate.mv.conv)/2

se.mv.inv = .016
se.mv.conv = .017
se.mv.avg = (se.mv.inv + se.mv.conv)/2

n.mv = 503

################################################################################
# Breitenstein (No replication data?)
################################################################################
# Average treatment effects of judge and partisan accusations
ate.b.judge = -.266
ate.b.party = -.222
ate.b.avg = (ate.b.judge + ate.b.party)/2

se.b.party = 0.00769
se.b.judge = 0.00793
se.b.avg = (se.b.party + se.b.judge)/2

n.b = 2275

################################################################################
# Franchino and Zucchini 2014
################################################################################
# Remove NA outcome values - not sure why these are here
fz = fz %>% filter(!is.na(Y))

# Pool corruption into one treatment
fz$corrupt = with(fz, ifelse(corruption == "Convicted of corruption" |
                             corruption == "Investigated for corruption", 1, 0))

# Run model
fz_corrupt = lm(Y ~ corrupt, data = fz)
summary(fz_corrupt, cluster = "IDContatto")

# Extract point estimates
ate.fz = summary(fz_corrupt, cluster = IDContatto)$coef[2, 1]

# Extract standard errors
se.fz = summary(fz_corrupt, cluster = IDContatto)$coef[2, 2]

# Extract number of observations
n.fz = 347

################################################################################
# Eggers, Vivyan, and Wagner 2017
################################################################################
eggers = readRDS("data/experiment_data_eggers.Rds", refhook = NULL)

# Run model
evw_corrupt <- lm(voteinc ~ mp.misconduct, data = eggers)
summary(evw_corrupt, cluster = "id")

# Extract point estimates
ate.evw = summary(evw_corrupt, cluster = id)$coef[2, 1]

# Extract standard errors
se.evw = summary(evw_corrupt, cluster = id)$coef[2, 2]

# Extract number of observations
n.evw = 1962

################################################################################
# Add to meta analysis dataframe
################################################################################
# Winters/Weitz-Shapiro 2017
wsw17 = data.frame(type="Survey", year=2016 , author = "Winters & Weitz-Shapiro", 
                   author_reduced = "Winters & Weitz-Shapiro 2017", country = "Brazil", 
                   ate_vote = ate.wsw17, se_vote = se.wsw17, ci_upper = NA, 
                   ci_lower = NA, N = n.wsw17, published = 1, Notes = NA)

# Winters/Weitz-Shapiro 2013
wsw13 = data.frame(type="Survey", year=2013 , author = "Winters & Weitz-Shapiro", 
                   author_reduced = "Winters & Weitz-Shapiro 2013", country = "Brazil", 
                   ate_vote = ate.wsw13, se_vote = se.wsw13, ci_upper = NA, 
                   ci_lower = NA,N = n.wsw13, published = 1, Notes = NA)

# Winters/Weitz-Shapiro 2015
wsw15 = data.frame(type="Survey", year=2015 , author = "Winters & Weitz-Shapiro", 
                   author_reduced = "Winters & Weitz-Shapiro 2015", country = "Brazil", 
                   ate_vote = ate.wsw15, se_vote = se.wsw15, ci_upper = NA, 
                   ci_lower = NA,N = n.wsw15, published = 1, Notes = NA)

# Winters/Weitz-Shapiro 2018
wsw18 = data.frame(type="Survey", year=2018 , author = "Winters & Weitz-Shapiro", 
                   author_reduced = "Winters & Weitz-Shapiro 2018", country = "Argentina", 
                   ate_vote = ate.wsw18, se_vote = se.wsw18, ci_upper = NA, 
                   ci_lower = NA,N = n.wsw18, published = 1, Notes = NA)

# Klasna and Tucker Sweden
kt_sweden = data.frame(type="Survey", year=2013 , author = "Klasna & Tucker", 
                   author_reduced = "Klasna & Tucker (Sweden)", country = "Sweden", 
                   ate_vote = ate.sweden, se_vote = se.wsw13, ci_upper = NA, 
                   ci_lower = NA, N = n.sweden, published = 1, Notes = NA)

# Klasna and Tucker Moldova
kt_moldova = data.frame(type="Survey", year=2013 , author = "Klasna & Tucker", 
                   author_reduced = "Klasna & Tucker (Moldova)", country = "Moldova", 
                   ate_vote = ate.moldova, se_vote = se.moldova, ci_upper = NA, 
                   ci_lower = NA, published = 1, N = n.moldova, Notes = NA)

# De Figuerido et al. Brazil 2011
# defig = data.frame(type="Survey", year=2011 , author = "De Figueiredo, Hidalgo, & Kasahara",
#                    author_reduced = "De Figueiredo et al.", country = "Brazil",
#                    ate_vote = ate.defig, se_vote = se.defig, ci_upper = NA,
#                    ci_lower = NA, published = 0, N = n.defig, Notes = NA)

# Mares and Visconti 2019
mv = data.frame(type="Survey", year=2019 , author = "Mares & Visconti", 
                   author_reduced = "Mares & Visconti", country = "Romania", 
                   ate_vote = ate.mv.avg, se_vote = se.mv.avg, ci_upper = NA, 
                   ci_lower = NA, N = n.mv, published = 1, Notes = NA)

# Breitenstein 2019
b = data.frame(type="Survey", year=2019 , author = "Breitenstein", 
                   author_reduced = "Breitenstein", country = "Spain", 
                   ate_vote = ate.b.avg, se_vote = se.b.avg, ci_upper = NA, 
                   ci_lower = NA, N = n.b, published = 1, Notes = NA)

# Franchino and Zucchini 2014
fz = data.frame(type="Survey", year=2014 , author = "Franchino and Zucchini", 
                   author_reduced = "Franchino and Zucchini", country = "Italy", 
                   ate_vote = ate.fz, se_vote = se.fz, ci_upper = NA, 
                   ci_lower = NA, N = n.fz, published = 1, Notes = NA)

# Eggers, Vivyan, and Wagner 2017
evw = data.frame(type="Survey", year=2017 , author = "Eggers, Vivyan, and Wagner", 
                   author_reduced = "Eggers et al.", country = "UK", 
                   ate_vote = ate.evw, se_vote = se.evw, ci_upper = NA, 
                   ci_lower = NA, N = n.evw, published = 1, Notes = NA)

# Combine dataframes
meta = rbind(results, wsw17, wsw13, wsw15, wsw18, kt_sweden, kt_moldova,
             mv, b, fz, evw) 
#            defig, wsw13comp, wsw13nocomp, wsw13noinfo)

# Save combined dataframe
save(meta, file = "data/meta.RData")


















################################################################################
# Avenburg dissertation
################################################################################
# Avenburg dissertation. Coefficient from paper; 1-7 scale, rescaled 0-1
point.est.av <- -3.37/6
ci.av <- c(point.est.av - qt(0.975, (774 - 6)) * 0.2/6, 
           point.est.av + qt(0.975, (774 - 6)) * 0.2/6)

################################################################################
# Vera Rojas working paper
################################################################################

point.est.vr.comp <- -16.59/100
point.est.vr.nocomp <- -22.89/100
ci.vr.comp <- c(-22.23, -10.94)/100
ci.vr.nocomp <- c(-27.84, -17.93)/100

ci = est - 1.96*se
ci - est = - 1.96*se
ci - est/-1.96

se = -.35/-.3 - 1.96

