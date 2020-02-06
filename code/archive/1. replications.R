################################################################################
# Libraries and Import
################################################################################
library(survey)
library(huxtable)
library(foreign)
library(readxl)

# Data imports
results = read_excel("data/study_results.xlsx")
wsw16 <- read.dta('data/Weitz-ShapiroWinters_JOP_Credibility_ReplicationData.dta')
wsw13 <- read.dta('data/WintersWeitzShapiro_2013_Replication.dta')

# Define scaling function
scale01 <- function(x)
  (x - min(x, na.rm = T)) / diff(range(x, na.rm = T))

################################################################################
# Winters/Weitz-Shapiro JOP
################################################################################
# Rescale outcome variable
wsw16$vote_vig_cont <- scale01(wsw16$voteintent)

# Create all variable
wsw16$corrupt = with(wsw16, ifelse(vinheta != "VINHETA 1" & vinheta != "VINHETA 2",
                                   1, 0))
wsw16$vote = with(wsw16, ifelse(voteintent > 2, 1, 0))

# Run models
reg.wsw16.all = lm(vote ~ corrupt, data = wsw16)
reg.wsw16.cred = lm(vote_vig_cont ~ cred_vs_pure, data = wsw16)
reg.wsw16.nocred = lm(vote_vig_cont ~ less_vs_pure, data = wsw16)
reg.wsw16.nosource = lm(vote_vig_cont ~ nosource_vs_pure, data = wsw16)

# Extract point estimates
point.est.wsw16.cred = summary(reg.wsw16.cred)$coef[2, 1]
point.est.wsw16.nocred = summary(reg.wsw16.nocred)$coef[2, 1]
point.est.wsw16.nosource = summary(reg.wsw16.nosource)$coef[2, 1]
point.est.wsw16.all = summary(reg.wsw16.all)$coef[2, 1]

# Extract standard errors
se.wsw16.cred = summary(reg.wsw16.cred)$coef[2, 2]
se.wsw16.nocred = summary(reg.wsw16.nocred)$coef[2, 2]
se.wsw16.nosource = summary(reg.wsw16.nosource)$coef[2, 2]

# Extract number of observations
n.wsw16.cred = nobs(reg.wsw16.cred)
n.wsw16.nocred = nobs(reg.wsw16.nocred)
n.wsw16.nosource = nobs(reg.wsw16.nosource)

################################################################################
# Winters/Weitz-Shapiro CP
################################################################################
# Rescale outcome variable
wsw13$vote_vig_cont <- scale01(wsw13$votescale)

# Run models
reg.wsw13.comp <-
  with(wsw13[wsw13$competentvignette == 1, ], lm(vote_vig_cont ~ corruptvignette))
reg.wsw13.nocomp <-
  with(wsw13[wsw13$incompetentvignette == 1, ], lm(vote_vig_cont ~ corruptvignette))
reg.wsw13.noinfo <-
  with(wsw13[is.na(wsw13$competentvignette), ], lm(vote_vig_cont ~ corruptvignette))

# All corrupt vignettes
wsw13$vote_vig_cont <- ifelse(wsw13$votescale > 2, 1, 0)
reg = lm(vote_vig_cont ~ corruptvignette, data = wsw13)

# Extract point estimates
point.est.wsw13.comp <- summary(reg.wsw13.comp)$coef[2, 1]
point.est.wsw13.nocomp <- summary(reg.wsw13.nocomp)$coef[2]
point.est.wsw13.noinfo <- summary(reg.wsw13.noinfo)$coef[2]

# Extract standard errors
se.wsw13.comp = summary(reg.wsw13.comp)$coef[2, 2]
se.wsw13.nocomp = summary(reg.wsw13.nocomp)$coef[2, 2]
se.wsw13.noinfo = summary(reg.wsw13.noinfo)$coef[2, 2]

# Extract number of observations
n.wsw13.comp = nobs(reg.wsw13.comp)
n.wsw13.nocomp = nobs(reg.wsw13.nocomp)
n.wsw13.noinfo = nobs(reg.wsw13.noinfo)

################################################################################
# Add to meta analysis dataframe
################################################################################
# Winters/Weitz-Shapiro JOP
wsw16cred = data.frame(type="Survey", year=2016 , author = "Winters & Weitz-Shapiro", 
                   author_reduced = "Winters & Weitz-Shapiro (High Credibility)", country = "Brazil", 
                   ate_vote = point.est.wsw16.cred, se_vote = se.wsw16.cred,
                   ate_elect = NA, se_elect = NA, N = n.wsw16.cred, Notes = NA)

wsw16nocred = data.frame(type="Survey", year=2016 , author = "Winters & Weitz-Shapiro", 
                   author_reduced = "Winters & Weitz-Shapiro (Low Credibility)", country = "Brazil", 
                   ate_vote = point.est.wsw16.nocred, se_vote = se.wsw16.nocred,
                   ate_elect = NA, se_elect = NA, N = n.wsw16.nocred, Notes = NA)

wsw16nosource = data.frame(type="Survey", year=2016 , author = "Winters & Weitz-Shapiro", 
                   author_reduced = "Winters & Weitz-Shapiro (No Source)", country = "Brazil", 
                   ate_vote = point.est.wsw16.nosource, se_vote = se.wsw16.nosource,
                   ate_elect = NA, se_elect = NA, N = n.wsw16.nosource, Notes = NA)

# Winters/Weitz-Shapiro CP
wsw13comp = data.frame(type="Survey", year=2013 , author = "Winters & Weitz-Shapiro", 
                   author_reduced = "Winters & Weitz-Shapiro (Competent)", country = "Brazil", 
                   ate_vote = point.est.wsw13.comp, se_vote = se.wsw13.comp,
                   ate_elect = NA, se_elect = NA, N = n.wsw13.comp, Notes = NA)

wsw13nocomp = data.frame(type="Survey", year=2013 , author = "Winters & Weitz-Shapiro", 
                   author_reduced = "Winters & Weitz-Shapiro (Not competent)", country = "Brazil", 
                   ate_vote = point.est.wsw13.nocomp, se_vote = se.wsw13.nocomp,
                   ate_elect = NA, se_elect = NA, N = n.wsw13.nocomp, Notes = NA)

wsw13noinfo = data.frame(type="Survey", year=2013 , author = "Winters & Weitz-Shapiro", 
                   author_reduced = "Winters & Weitz-Shapiro (No info)", country = "Brazil", 
                   ate_vote = point.est.wsw13.noinfo, se_vote = se.wsw13.noinfo,
                   ate_elect = NA, se_elect = NA, N = n.wsw13.noinfo, Notes = NA)

# Combine dataframes
meta = rbind(results, wsw16cred, wsw16nocred, wsw16nosource) 
#             wsw13comp, wsw13nocomp, wsw13noinfo)

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

# Botero et al. PRQ. Coefficients from appendix; 1-4 scale, rescaled 0-1; with controls

point.est.botero.news <- -0.19/3
ci.botero.news <- c(point.est.botero.news - qt(0.975, 638) * 0.081/3, point.est.botero.news +
                      qt(0.975, 638) * 0.081/3)

point.est.botero.courts <- 0.00019/3
ci.botero.courts <- c(point.est.botero.courts - qt(0.975, 638) * 0.0801/3, point.est.botero.courts +
                        qt(0.975, 638) * 0.0801/3)

# Klasnja & Tucker ES

point.est.kt.sweden.goodecon <- -0.768/4
ci.kt.sweden.goodecon <- c(point.est.kt.sweden.goodecon - qt(0.975, (1852 - 4)) * 0.09/4, point.est.kt.sweden.goodecon +
                             qt(0.975, (1852 - 4)) * 0.09/4)

point.est.kt.sweden.badecon <- -0.814/4
ci.kt.sweden.badecon <- c(point.est.kt.sweden.badecon - qt(0.975, (1852 - 4)) * 0.097/4, point.est.kt.sweden.badecon +
                            qt(0.975, (1852 - 4)) * 0.097/4)

point.est.kt.moldova.goodecon <- 0.031/4
ci.kt.moldova.goodecon <- c(point.est.kt.moldova.goodecon - qt(0.975, (459 - 4)) * 0.165/4,
                            point.est.kt.moldova.goodecon + qt(0.975, (459 - 4)) * 0.165/4)

point.est.kt.moldova.badecon <- -0.503/4
ci.kt.moldova.badecon <- c(point.est.kt.moldova.badecon - qt(0.975, (459 - 4)) * 0.166/4, point.est.kt.moldova.badecon +
                             qt(0.975, (459 - 4)) * 0.166/4)