################################################################################
# Libraries and Import
################################################################################
set.seed(09102018)

library("cowplot")
library("survey")
library("huxtable")
library(foreign)

##Number of permutations for permutation tests
permute_num <- 2000

# Scaling function
scale01 <- function(x)
  (x - min(x, na.rm = T)) / diff(range(x, na.rm = T))

################################################################################
# Winters/Weitz-Shapiro JOP
################################################################################

# Winters/Weitz-Shapiro JOP

wsw16 <- read.dta('data/Weitz-ShapiroWinters_JOP_Credibility_ReplicationData.dta')
wsw16$vote_vig_cont <- scale01(wsw16$voteintent)

reg.wsw16.cred = lm(vote_vig_cont ~ cred_vs_pure, data = wsw16)
reg.wsw16.nocred = lm(vote_vig_cont ~ less_vs_pure, data = wsw16)
reg.wsw16.nosource = lm(vote_vig_cont ~ nosource_vs_pure, data = wsw16)

point.est.wsw.cred = summary(reg.wsw16.cred)$coef[2, 1]
point.est.wsw.nocred = summary(reg.wsw16.nocred)$coef[2, 1]
point.est.wsw.nosource = summary(reg.wsw16.nosource)$coef[2, 1]

se.wsw.cred = summary(reg.wsw16.cred)$coef[2, 2]
se.wsw.nocred = summary(reg.wsw16.nocred)$coef[2, 2]
se.wsw.nosource = summary(reg.wsw16.nosource)$coef[2, 2]

# Winters/Weitz-Shapiro CP

wsw13 <- foreign::read.dta('/data/WintersWeitzShapiro_2013_Replication.dta')
wsw13$vote_vig_cont <- scale01(wsw13$votescale)

reg.wsw13.comp <-
  with(wsw13[wsw13$competentvignette == 1, ], lm(vote_vig_cont ~ corruptvignette))
reg.wsw13.nocomp <-
  with(wsw13[wsw13$incompetentvignette == 1, ], lm(vote_vig_cont ~ corruptvignette))
reg.wsw13.noinfo <-
  with(wsw13[is.na(wsw13$competentvignette), ], lm(vote_vig_cont ~ corruptvignette))

point.est.wsw.comp <- coef(reg.wsw13.comp)[2]
point.est.wsw.nocomp <- coef(reg.wsw13.nocomp)[2]
point.est.wsw.noinfo <- coef(reg.wsw13.noinfo)[2]

ci.wsw.comp <- confint(reg.wsw13.comp)[2, ]
ci.wsw.nocomp <- confint(reg.wsw13.nocomp)[2, ]
ci.wsw.noinfo <- confint(reg.wsw13.noinfo)[2, ]

# Avenburg dissertation. Coefficient from paper; 1-7 scale, rescaled 0-1

point.est.av <- -3.37/6
ci.av <- c(point.est.av - qt(0.975, (774 - 6)) * 0.2/6, point.est.av + qt(0.975, (774 - 6)) *
             0.2/6)

# Vera Rojas working paper. Coefficients and CIs from paper; 0-100 scale, rescaled 0-1

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

# Gather coefficients

point.est1 <- c(point.est.wsw.cred, point.est.wsw.comp, NA, point.est.botero.news, point.est.kt.sweden.goodecon,
                point.est.kt.moldova.goodecon, point.est.vr.comp)
ci1 <- rbind(ci.wsw.cred, ci.wsw.comp, NA, ci.botero.news, ci.kt.sweden.goodecon, ci.kt.moldova.goodecon,
             ci.vr.comp)

point.est2 <- c(point.est.wsw.nocred, point.est.wsw.nocomp, point.est.av, point.est.botero.courts,
                point.est.kt.sweden.badecon, point.est.kt.moldova.badecon, point.est.vr.nocomp)
ci2 <- rbind(ci.wsw.nocred, ci.wsw.nocomp, ci.av, ci.botero.courts, ci.kt.sweden.badecon, ci.kt.moldova.badecon,
             ci.vr.nocomp)

point.est3 <- c(point.est.wsw.nosource, point.est.wsw.noinfo, NA, NA, NA, NA, NA)
ci3 <- rbind(ci.wsw.nosource, ci.wsw.noinfo, NA, NA, NA, NA, NA)

# Plot

lwd <- 3
adjust <- 0.25
cex <- 1.25
cex.point <- 1.5

pdf(file = "./figures/figure1.pdf", width = 10, height = 6)

par(mar = c(3, 11, 1, 16) + 0.01)
plot(NULL, axes = F, frame.plot = T, xlab = "", ylab = "", xlim = c(-1, 0.25), ylim = c(0.75,
                                                                                        7.25),
     main = "", sub = "")
Axis(side = 1, at = seq(-1, 0.25, 0.25), labels = seq(-1, 0.25, 0.25), cex.axis = cex)
abline(v = 0, lwd = 1, lty = 2)
Axis(side = 2, at = 8:0, labels = c("", "Weitz-Shapiro &\nWinters 2017 (Brazil)", "Winters & Weitz-\nShapiro 2013 (Brazil)",
                                    "Avenburg\n2016 (Brazil)", "Botero et al.\n2015 (Colombia)", "Klasnja & Tucker\n2013 (Sweden)",
                                    "Klasnja & Tucker\n2013 (Moldova)", "Vera Rojas\n2017 (Peru)", ""), las = 1, cex.axis = cex)
abline(h = 1:6 + 0.5, lwd = 1, lty = 3)
segments(ci1[, 1], 7:1 + 1 * adjust, ci1[, 2], 7:1 + 1 * adjust, lwd = lwd)
segments(ci2[, 1], 7:1 + 0 * adjust, ci2[, 2], 7:1 + 0 * adjust, lwd = lwd)
segments(ci3[, 1], 7:1 - 1 * adjust, ci3[, 2], 7:1 - 1 * adjust, lwd = lwd)
points(point.est1, 7:1 + 1 * adjust, pch = c(24, 24, NA, 24, 24, 24, 24),
       bg = c("white", "black", NA, "white", "black", "black", "black"), cex = cex.point)
points(point.est2, 7:1 + 0 * adjust, pch = c(25, 25, 24, 25, 25, 25, 25),
       bg = c("white", "black", "white", "white", "black", "black", "black"), cex = cex.point)
points(point.est3, 7:1 - 1 * adjust, pch = 21, bg = c("white", "black", NA, NA, NA, NA, NA),
       cex = cex.point)

legend("right", inset = c(-0.675, 0), xpd = T, pch = c(24, 25, 21, 24, 25, 21),
       pt.bg = c(rep("white", 3), rep("black", 3)),
       legend = c("More credible source", "Less credible source", "Source unspecified", "Competent politician", "Incompetent politician", "Competence unspecified"),
       cex = cex,
       pt.cex = cex.point)

dev.off()


# Figure 2 ----------------------------------------------------------------

load("./data/lapop_corruption.RData")

pdf(file='./figures/figure2.pdf',height=5,width=7)
par(mar=c(4,4,1,1)+.01)
plot(c(2004,2017),c(0,30),type='n',ylab='Percent Spontaneously Mentioning Corruption',xlab='Year')
apply(lapop_corruption,1,function(x) lines(as.numeric(colnames(lapop_corruption))[!is.na(x)], na.exclude(x), lty=3, lwd=2))
lines(as.numeric(colnames(lapop_corruption))[!is.na(lapop_corruption['Brazil',])], na.exclude(lapop_corruption['Brazil',]), lwd=4)
legend('topleft',legend=c('Brazil','Other Countries'),lwd=c(4,2),lty=c(1,3))
dev.off()


# Figure 3 ----------------------------------------------------------------
source("estimation_functions.R")
panel <- read_csv("./data/panel_cleaned.csv")


##Impute missing covariates using procedure specified in PAP

impute_mean <- function(var, block){
  var_data <- group_by(data.frame(var = var, block = block), block) %>%
    mutate(var_mean = mean(var, na.rm = TRUE))
  var_data$var[is.na(var_data$var)] <- var_data$var_mean[is.na(var_data$var)]
  var_data$var[is.na(var_data$var)] <- mean(var_data$var, na.rm = TRUE)
  var_data$var
}

panel$years_edu <- impute_mean(panel$years_edu, panel$codesetor)
panel$govt_eval_baseline <- impute_mean(panel$govt_eval_baseline, panel$codesetor)
panel$confid_fedgov <- impute_mean(panel$confid_fedgov, panel$codesetor)
panel$confid_justice <- impute_mean(panel$confid_justice, panel$codesetor)
panel$confid_tce <- impute_mean(panel$confid_tce, panel$codesetor)
panel$confid_muni <- impute_mean(panel$confid_muni, panel$codesetor)
panel$acc_responsible <- impute_mean(panel$acc_responsible, panel$codesetor)
panel$edu_responsible <- impute_mean(panel$edu_responsible, panel$codesetor)
panel$prob_vote_buying <- impute_mean(panel$prob_vote_buying, panel$codesetor)
panel$prob_vote_monitoring <- impute_mean(panel$prob_vote_monitoring, panel$codesetor)
panel$prob_vote_count <- impute_mean(panel$prob_vote_count, panel$codesetor)
panel$ana_prior <- impute_mean(panel$ana_prior, panel$codesetor)
panel$income <- impute_mean(panel$income, panel$codesetor)
panel$flier <- factor(panel$flier)
levels(panel$flier) <- c("Education", "Accounts")

## Define datasets

acc_contr_all <- filter(panel,  ana != 1 & attrited != 1)
acc_contr_good <- filter(panel, acc_rejected == 0 &
                           ana != 1 & attrited != 1)
acc_contr_bad <- filter(panel,  acc_rejected == 1 &
                          ana != 1 & attrited != 1)

## Specify Possible Covariates

covars <- c("age", "politics_interest", "turnout_2012",
            "vote_2012", "turnout_2014",
            "vote_2014", "partisan", "muni_biggest_prob", "politician_helped",
            "govt_eval_baseline", "acc_eval_baseline",
            "uncertain_acc_baseline", "edu_eval_baseline",
            "uncertain_edu_baseline", "tce_knowledge", "ana_knowledge",
            "child_school", "confid_fedgov",
            "confid_justice", "confid_tce", "confid_muni",
            "acc_responsible", "edu_responsible",
            "prob_vote_buying", "prob_vote_monitoring",
            "prob_vote_count", "prob_vote_monitoring",
            "prob_vote_count", "acc_rejected_prior", "tce_prior_cert",
            "ana_prior", "edu_prior_uncert",
            "years_edu", "race", "religion", "income", "relative_wellbeing",
            "female", "edu_rank1", "acc_rank1")

models <- list()
models$goodacc <- est_ate(outcome = "vote",
                          treatment = "accounts",
                          covars = covars,
                          block = "codesetor",
                          data = acc_contr_good,
                          permute=FALSE)

models$badacc <- est_ate(outcome = "vote",
                         treatment = "accounts",
                         covars = covars,
                         block = "codesetor",
                         data = acc_contr_bad,
                         permute = FALSE)


vig_models <- list()
vig_models$vote_vig <- est_ate(outcome = "vote_vig",
                               treatment = "vig_treatment",
                               covars = covars,
                               block = "codesetor",
                               data = filter(panel, acc_rejected == 1 &
                                               (ana == 1 | (accounts == 0 & ana == 0 & flier == "Education"))),
                               permute = FALSE)

vig_models$vote_vig_strict <- est_ate(outcome = "vote_vig_strict",
                                      treatment = "vig_treatment",
                                      covars = covars,
                                      block = "codesetor",
                                      data = filter(panel, acc_rejected == 1 &
                                                      (ana == 1 | (accounts == 0 & ana == 0 & flier == "Education"))),
                                      permute = FALSE)
vig_models$vote_vig_fsnoinfo<- est_ate(outcome = "vote_vig",
                                       treatment = "vig_treatment",
                                       covars = covars,
                                       block = "codesetor",
                                       data = filter(panel, (ana == 1 | (accounts == 0 & ana == 0 & flier == "Education"))),
                                       permute = FALSE)
vig_models$vote_vig_fs <- est_ate(outcome = "vote_vig",
                                  treatment = "vig_treatment",
                                  covars = covars,
                                  block = "codesetor",
                                  data = panel,
                                  permute = FALSE)

est.rej.vote<-c(vig_models[[1]]$ate_unadj, models[[2]]$ate_unadj)
est.app.vote<-c(NA, models[[1]]$ate_unadj)
ci.rej.vote<-cbind(c(vig_models[[1]]$ate_unadj - qnorm(.975) * vig_models[[1]]$ate_se_unadj, vig_models[[1]]$ate_unadj + qnorm(.975) * vig_models[[1]]$ate_se_unadj), c(models[[2]]$ate_unadj - qnorm(.975) * models[[2]]$ate_se_unadj, models[[2]]$ate_unadj + qnorm(.975) * models[[2]]$ate_se_unadj))
ci.app.vote<-cbind(c(NA,NA),c(models[[1]]$ate_unadj - qnorm(.975) * models[[1]]$ate_se_unadj, models[[1]]$ate_unadj + qnorm(.975) * models[[1]]$ate_se_unadj))

lwd<-2
adjust<-0.15
nvars<-length(est.rej.vote)
cex<-1
cex.point<-1.5

pdf(file='./figures/figure3.pdf',width=6.5,height=3)
par(mar=c(3,4,1,1)+.01)
plot(est.rej.vote, nvars:1+1*adjust, type='p',axes=F,frame.plot=T,xlab='',ylab='', pch=19,xlim=c(-.75,.75),ylim=c(0.5,2.5),main='',sub='',cex=cex.point)
Axis(side=1,at=seq(-.75, .75, 0.25),labels= seq(-.75, .75, 0.25),cex.axis=cex)
Axis(side=2,at=c(2,1),labels=c('Vignette\nExperiment','Field\nExperiment'),las=3,cex.axis=cex, tick=F)
abline(h=1:(nvars-1)+.5,lwd=1,lty=3)
abline(v=0,lwd=1,lty=2)
segments(ci.rej.vote[1,],nvars:1+1*adjust, ci.rej.vote[2,],nvars:1+1*adjust,lwd=lwd)
segments(ci.app.vote[1,],nvars:1-1*adjust, ci.app.vote[2,],nvars:1-1*adjust,lwd=lwd)
points(est.app.vote, nvars:1-1*adjust, pch=21, bg='white', cex=cex.point)

legend('bottomright',pch=c(19,1),legend=c('Accounts Rejected','Accounts Approved'),bg='white',cex=cex,pt.cex=cex.point)

dev.off()



# Table 1 -----------------------------------------------------------------

sample <- read.csv('./data/panel_muni_sample.csv')
results <- read.csv('./data/incumb_results.csv',fileEncoding='latin1')
sample$muni<-as.character(sample$muni)

ra_munis<-results[results$muni %in% c('TRINDADE','SANTA FILOMENA','BODOCO','SANTA MARIA DA BOA VISTA','FLORES','CUSTODIA','TABIRA','SERTANIA','BOM CONSELHO','ITAIBA','PEDRA','CAETES','GAMELEIRA','PRIMAVERA'),c('muni','name','accounts','edu','elected')]

ra_munis<-merge(ra_munis,sample[,c('mayor','mun','codetse')],by.x='muni',by.y='mun')
ra_munis$zona<-NA
ra_munis$zona[ra_munis$muni %in% c('TRINDADE','SANTA FILOMENA','BODOCO','SANTA MARIA DA BOA VISTA')]<-'oeste'
ra_munis$zona[ra_munis$muni %in% c('FLORES','CUSTODIA','TABIRA','SERTANIA')]<-'norte'
ra_munis$zona[ra_munis$muni %in% c('BOM CONSELHO','ITAIBA','PEDRA','CAETES')]<-'sul'
ra_munis$zona[ra_munis$muni %in% c('GAMELEIRA','PRIMAVERA')]<-'leste'
ra_munis<-ra_munis[order(ra_munis$zona),]

ra_munis$accounts<-ifelse(ra_munis$accounts=='rejected','rejeitadas','aprovadas')
ra_munis$elected<-ifelse(ra_munis$elected==1,'eleito','não eleito')
ra_munis$edu<-NULL
names(ra_munis)<-c('município','nome_urna_eletrônica','contas','resultado_eleição','nome','código','zona')
ra_munis<-ra_munis[,c('zona','município','código','nome','nome_urna_eletrônica','contas','resultado_eleição')]

muni.table<-ra_munis[,c('zona','município')]
muni.table<-merge(muni.table,sample[,c('mun','pop2010','muni')],by.x='município',by.y='mun')
muni.table<-merge(muni.table,results[,c('accounts','margin','muni')],by.x='município',by.y='muni')
muni.table$region<-factor(1* (muni.table$zona=='oeste')+2*(muni.table$zona=='norte')+3* (muni.table$zona=='leste')+4* (muni.table$zona=='sul'), labels=c('West','North','East','South'))
muni.table$accounts<-as.character(muni.table$accounts)
muni.table$accounts<-ifelse(muni.table$accounts=='accepted', 'Approved', 'Rejected')
muni.table<-muni.table[order(muni.table$region,muni.table$accounts),c('region','muni','accounts','pop2010','margin')]
muni.table$pop2010<-prettyNum(muni.table$pop2010,big.mark=',')
muni.table$margin<-round(muni.table$margin,1)
muni.table$muni<-gsub('ó',"\\\\'o", muni.table$muni)
muni.table$muni<-gsub('é',"\\\\'e", muni.table$muni)
muni.table$muni<-gsub('í',"\\\\'i", muni.table$muni)
muni.table$muni<-gsub('â',"\\\\^a", muni.table$muni)
muni.table$margin<-as.character(muni.table$margin)
names(muni.table)<-c('Region','Municipality','Accounts','Population','Vote Margin')

muni.table.latex <- Hmisc::latex(muni.table,
                                 file='./tables/table1.tex',
                                 rowname=NULL, collabel.just=rep('c',ncol(muni.table)), col.just=rep('c',ncol(muni.table)),
                                 booktabs = F, ctable = T, where = "htp", caption = 'Case Study Municipalities')


# Figure 4 ----------------------------------------------------------------

campaign_issues <- 100* sort(prop.table(table(panel$campaign_issue)),decreasing=T)[c(1:8,10)]
biggest_problem <- 100* sort(prop.table(table(panel$muni_biggest_prob_noimpute)),decreasing=T)[c(1:6,8:9,13)]
names(biggest_problem)[9]<-'Corrupção/contas do município'
campaign_issues<-campaign_issues[names(biggest_problem)]
issue_data<-rbind(biggest_problem, campaign_issues)
colnames(issue_data)<-c('Health','Crime','Jobs','Drought','Sanitation','Schools','Street\nPaving',"Poor\nGovernment",'Corruption/\nAccounts')
rownames(issue_data)<-c('Biggest Problem','Biggest Campaign Issue')

pdf(file='./figures/figure4.pdf',height=4,width=6)
par(mar=c(6,4,1,1))
barplot(issue_data,ylim=c(0,50),ylab='Percent',beside=T,las=2,legend.text=T,args.legend=list(x='topright'))
dev.off()


# Figure 5 ----------------------------------------------------------------

secao_results <- read_csv("./data/pe_secao_results.csv")
##Remove Recife
secao_results <- filter(secao_results, nome_municipio != "RECIFE")

#Find candidates who ran multiple times and belong to the same party
cands1216 <- select(secao_results, ano_eleicao, sigla_ue, nome_municipio, sigla_partido, cpf_candidato,
                    nome_candidato, nome_urna_candidato) %>%
  distinct() %>%
  filter(ano_eleicao == 2012 | ano_eleicao == 2016) %>%
  group_by(cpf_candidato, sigla_partido) %>%
  mutate(n = n()) %>%
  filter(!is.na(sigla_partido)) %>%
  arrange(nome_municipio, sigla_partido, ano_eleicao, nome_candidato)

secao_results$reran16 <- 0
secao_results$reran16[secao_results$cpf_candidato %in% cands1216$cpf_candidato[cands1216$n == 2]] <- 1

party_overtime <- gather(select(secao_results, ano_eleicao, codigo_municipio, nome_municipio,
                                num_zona, num_secao, num_votavel, sigla_partido, votes_pct, reran16),
                         key = "variable", value = "value", -codigo_municipio, -nome_municipio,
                         -num_zona, -num_secao, -ano_eleicao, -num_votavel, -reran16, -sigla_partido) %>%
  mutate(ano_eleicao = paste0("vote", ano_eleicao)) %>%
  spread(ano_eleicao, value) %>%
  filter(num_votavel %in% c(95, 96) == FALSE & nome_municipio != "RECIFE") %>%
  filter(!is.na(vote2012) & !is.na(vote2016))
party_overtime$variable <- NULL

mean_votebyparty <- group_by(party_overtime, sigla_partido) %>%
  summarise(vote = mean(vote2016, na.rm = TRUE)) %>%
  arrange(desc(vote))


party_overtime$cand_id <- paste(party_overtime$codigo_municipio, party_overtime$sigla_partido)
party_overtime <- group_by(party_overtime, cand_id) %>%
  mutate(mean_vote2012 = mean(vote2012, na.rm = TRUE),
         mean_vote2016 = mean(vote2016, na.rm = TRUE))
party_overtime$demeaned2012 <- party_overtime$vote2012 - party_overtime$mean_vote2012
party_overtime$demeaned2016 <- party_overtime$vote2016 - party_overtime$mean_vote2016

polfam_manual <- readxl::read_excel("./data/cands1216_potential_dynasties.xlsx") %>%
  filter(dynasty == 1)
polfam_2016 <- filter(secao_results, ano_eleicao == 2016 &
                        cpf_candidato %in% polfam_manual$cpf_candidato) %>%
  left_join(select(filter(polfam_manual), cpf_candidato, match_cpf))
polfam_2016 <- left_join(polfam_2016, select(filter(secao_results, ano_eleicao == 2012),
                                             cpf_candidato, votes_pct,
                                             num_zona, num_secao),
                         by = c("match_cpf" = "cpf_candidato",
                                "num_zona" = "num_zona",
                                "num_secao" = "num_secao")) %>%
  rename("vote2012" = "votes_pct.y",
         "vote2016" = "votes_pct.x") %>%
  select(ano_eleicao, sigla_uf, codigo_municipio, nome_municipio,
         num_zona, num_secao, descricao_cargo, num_votavel, qtde_votos,
         total_votes, vote2016, sigla_partido, cpf_candidato,
         nome_candidato, nome_urna_candidato, reran16, match_cpf, vote2012) %>%
  rename("family_member_cpf" = "match_cpf")
polfam_2016 <- group_by(polfam_2016, cpf_candidato) %>%
  mutate(mean_vote2012 = mean(vote2012, na.rm = TRUE),
         mean_vote2016 = mean(vote2016, na.rm = TRUE))
polfam_2016$demeaned2012 <- polfam_2016$vote2012 - polfam_2016$mean_vote2012
polfam_2016$demeaned2016 <- polfam_2016$vote2016 - polfam_2016$mean_vote2016
polfam_2016 <- polfam_2016[polfam_2016$reran16 == 0, ]
polfam_2016$cand_id <- paste(polfam_2016$codigo_municipio, polfam_2016$sigla_partido)

party_plot <- ggplot(filter(party_overtime, reran16 == 0 &
                              cand_id %in% polfam_2016$cand_id == FALSE),
                     aes(x = demeaned2012, y = demeaned2016)) +
  geom_point(size = .5, alpha = .2) +
  geom_smooth(method = "lm", se = FALSE, size = 2, color = "darkred") +
  geom_text(x=20, y=15, label="Slope: 0.19", size = 8) +
  xlim(-30, 30) +
  ylim(-25, 25) +
  theme_bw() +
  theme(axis.text.x = element_text(size=16),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size=16),
        axis.title.y = element_text(size = 16),
        plot.title = element_text(size=22)) +
  xlab("2012 Vote % (Demeaned by Candidate Average)") +
  ylab("2016 Vote % (Demeaned by Candidate Average)") +
  ggtitle("Same-Party (Different Candidate) Correlation")


fam_plot <- ggplot(polfam_2016, aes(x = demeaned2012, y = demeaned2016)) +
  geom_point(size = .5, alpha = .2) +
  geom_smooth(method = "lm", se = FALSE, size = 2, color = "darkred") +
  geom_text(x=20, y=15, label="Slope: 0.51", size = 8) +
  theme_bw() +
  theme(axis.text.x = element_text(size=16),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size=16),
        axis.title.y = element_text(size = 16),
        plot.title = element_text(size=22)) +
  xlim(-30, 30) +
  ylim(-25, 25) +
  xlab("2012 Vote % (Demeaned by Candidate Average)") +
  ylab("2016 Vote % (Demeaned by Candidate Average)") +
  ggtitle("Same-Family (Different Candidate) Correlation")


pdf(file = "./figures/figure5.pdf", width = 20, height = 8)
plot_grid(party_plot, fam_plot)
dev.off()

####
##Appendix
###


# Appendix Figure 1  ------------------------------------------------------
conf.fed <- lm(panel$confid_fedgov_noimpute -1 ~ 1)
conf.jus <- lm(panel$confid_justice_noimpute - 1 ~ 1)
conf.tce <- lm(panel$confid_tce_noimpute - 1 ~ 1)
conf.mun <- lm(panel$confid_muni_noimpute - 1 ~ 1)

conf.est <- sapply(list(conf.fed,conf.jus,conf.tce,conf.mun), coef)
conf.ci <- sapply(list(conf.fed,conf.jus,conf.tce,conf.mun), confint)

panel$pid[panel$pid=='DEMOCRATAS (DEM) / PFL']<-'DEM'
panel$pid[panel$pid=='PFL']<-'DEM'
panel$pid[panel$pid=='Partido verde']<-'PV'
panel$pid[! panel$pid %in% c('PT','PMDB','PSDM','PSB','DEM','PTB','PR','PDT','PC do B','PTB','PHS','PTC','PP','PR','PRP','PPR','PMN','PSD','PSC','PRB','PSOL','PV','PL','PRL','PDS','PROS')]<-NA

conf.fed.pt <- lm(panel$confid_fedgov_noimpute[panel$pid == "PT"] - 1 ~ 1)
conf.jus.pt <- lm(panel$confid_justice_noimpute[panel$pid == "PT"] - 1 ~ 1)
conf.tce.pt <- lm(panel$confid_tce_noimpute[panel$pid == "PT"] - 1 ~ 1)
conf.mun.pt <- lm(panel$confid_muni_noimpute[panel$pid == "PT"] - 1 ~ 1)

conf.est.pt <- sapply(list(conf.fed.pt, conf.jus.pt, conf.tce.pt, conf.mun.pt), coef)
conf.ci.pt <- sapply(list(conf.fed.pt, conf.jus.pt, conf.tce.pt, conf.mun.pt), confint)

pdf(file = "./figures/appendix_figure1.pdf", height = 5, width = 6)
par(mar = c(4, 5, 3, 1))
conf.plot <- gplots::barplot2(rbind(conf.est, conf.est.pt), ci.u = rbind(conf.ci[1, ], conf.ci.pt[1, ]),
                              ci.l = rbind(conf.ci[2, ], conf.ci.pt[2, ]), plot.ci = T, names.arg = NULL,
                              ylim = c(0, 6), yaxt = "n", beside = T, col = c("grey40", "grey80"),
                              legend.text = c("All Respondents",  "PT Identifiers"))
Axis(side = 1, at = apply(conf.plot, 2, mean), labels = c("Federal\nGovernment",
                                                          "Justice\nSystem", "State Accounts\nCourt",
                                                          "Municipal\nGovernment"), line = 1,
     tick = F)
Axis(side = 2, at = 0:6, labels = 1:7)
Axis(side = 2, at = c(0, 6), labels = c("Don't trust\nat all", "Trust\ncompletely"),
     line = 1.5, tick = F)
dev.off()



# Appendix Figure 5 --------------------------------------------------------
load("./data/elec_results.RData")
load("./data/cand_merge_data.RData")

endline <- read.csv("./data/endline_cleaned.csv", as.is = T)
endline <- endline[!is.na(endline$vote_choice) & endline$vote_choice != 9, ]
endline$codesetor <- as.character(endline$codesetor)
endline$group <- NULL

sample <- read.csv("./data/panel_muni_sample.csv", as.is = T)
r1_data <- read.csv("./data/data_questionnaire_r1_english.csv",
                    as.is = T)
endline <- merge(endline, r1_data[, c("qnum", "group")])



incumbvotes <- tapply((results$votes + results$votes_pend) * results$incumb, results$codetse,
                      sum)
oppvotes <- tapply((results$votes + results$votes_pend) * (!results$incumb), results$codetse,
                   sum)
winnervotes <- tapply((results$votes + results$votes_pend) * (results$elected ==
                                                                "s"), results$codetse, sum)
loservotes <- tapply((results$votes + results$votes_pend) * (results$elected == "n"),
                     results$codetse, sum)

munistats <- munistats[, c("codetse", "voters", "abstention", "blank", "null")]

munistats$voters <- as.numeric(as.character(munistats$voters))
munistats$abstention <- as.numeric(as.character(munistats$abstention))
munistats$blank <- as.numeric(as.character(munistats$blank))
munistats$null <- as.numeric(as.character(munistats$null))

munistats <- merge(munistats, data.frame(incumbvotes = incumbvotes), by.x = "codetse",
                   by.y = "row.names")
munistats <- merge(munistats, data.frame(oppvotes = oppvotes), by.x = "codetse",
                   by.y = "row.names")
munistats <- merge(munistats, data.frame(winnervotes = winnervotes), by.x = "codetse",
                   by.y = "row.names")
munistats <- merge(munistats, data.frame(loservotes = loservotes), by.x = "codetse",
                   by.y = "row.names")

munistats <- merge(munistats, unique(results[, c("codetse", "votes_pend_muni")]))

munistats$null <- munistats$null - munistats$votes_pend_muni
munistats$votes_pend_muni <- NULL

# Check that all registered voters are accounted for
which(apply(munistats[, c("abstention", "blank", "null", "incumbvotes", "oppvotes")],
            1, sum) != munistats$voters)
which(apply(munistats[, c("abstention", "blank", "null", "winnervotes", "loservotes")],
            1, sum) != munistats$voters)

# Calculate and merge in proportion of endline sample from each municipio,
# accounts status
muniprop <- prop.table(table(endline$codeibge))
muniprop <- merge(data.frame(muniprop), sample[, c("codetse", "codeibge")], by.x = "Var1",
                  by.y = "codeibge")
munistats <- merge(munistats, muniprop[, c("codetse", "Freq")])
names(munistats)[which(names(munistats) == "Freq")] <- "weight"
munistats <- merge(munistats, sample[, c("codetse", "status")])

# Population parameters
pop.incumb <- with(munistats, weighted.mean(incumbvotes/voters, w = weight))
pop.opp <- with(munistats, weighted.mean(oppvotes/voters, w = weight))
pop.winner <- with(munistats, weighted.mean(winnervotes/voters, w = weight))
pop.loser <- with(munistats, weighted.mean(loservotes/voters, w = weight))
pop.blanknull <- with(munistats, weighted.mean((blank + null)/voters, w = weight))
pop.abstain <- with(munistats, weighted.mean(abstention/voters, w = weight))

pop.vote <- c(pop.incumb, pop.opp, pop.winner, pop.loser, pop.blanknull, pop.abstain)

pop.vote.turnedout <- c(pop.incumb, pop.opp, pop.winner, pop.loser, pop.blanknull,
                        NA)/(1 - pop.abstain)

# Sample estimates. Starting by recoding original vote choice variable.

endline$vote_choice[endline$qnum == 702] <- "Branco/Nulo"  # Data entry error; can remove once dataset is corrected
merge_data$id <- paste(merge_data$codeibge, merge_data$order)
endline$id <- paste(endline$codeibge, endline$vote_choice)
endline <- merge(endline, merge_data[c("incumbent", "winner", "id")], all.x = T,
                 all.y = F)
endline$incumbent[endline$turnout == 0 | endline$vote_choice == "Branco/Nulo"] <- 0
endline$winner[endline$turnout == 0 | endline$vote_choice == "Branco/Nulo"] <- 0
endline$abstain <- as.numeric(endline$turnout == 0)
endline$blanknull <- as.numeric(endline$vote_choice == "Branco/Nulo" & endline$turnout ==
                                  1)
endline$opposition <- as.numeric(endline$turnout == 1 & endline$incumbent == 0 &
                                   endline$blanknull == 0)
endline$loser <- as.numeric(endline$turnout == 1 & endline$winner == 0 & endline$blanknull ==
                              0)

# Check that appropriate dummies are mutually exclusive
table(apply(endline[, c("incumbent", "opposition", "abstain", "blanknull")], 1, sum,
            na.rm = T))
table(apply(endline[, c("winner", "loser", "abstain", "blanknull")], 1, sum, na.rm = T))
sum(c(endline$incumbent, endline$opposition, endline$abstain, endline$blanknull),
    na.rm = T)
sum(c(endline$winner, endline$loser, endline$abstain, endline$blanknull), na.rm = T)

# Calculcate vote proportions
sample.incumb <- mean(endline$incumbent)
sample.opp <- mean(endline$opposition)
sample.winner <- mean(endline$winner)
sample.loser <- mean(endline$loser)
sample.blanknull <- mean(endline$blanknull)
sample.abstain <- mean(endline$abstain)

sample.vote <- c(sample.incumb, sample.opp, sample.winner, sample.loser, sample.blanknull,
                 sample.abstain)
sample.vote.turnedout <- c(sample.incumb, sample.opp, sample.winner, sample.loser,
                           sample.blanknull, NA)/(1 - sample.abstain)

control.incumb <- mean(endline$incumbent[endline$group == "Control"])
control.opp <- mean(endline$opposition[endline$group == "Control"])
control.winner <- mean(endline$winner[endline$group == "Control"])
control.loser <- mean(endline$loser[endline$group == "Control"])
control.blanknull <- mean(endline$blanknull[endline$group == "Control"])
control.abstain <- mean(endline$abstain[endline$group == "Control"])

control.vote <- c(control.incumb, control.opp, control.winner, control.loser, control.blanknull,
                  control.abstain)
control.vote.turnedout <- c(control.incumb, control.opp, control.winner, control.loser,
                            control.blanknull, NA)/(1 - control.abstain)


design.endline <- svydesign(ids = ~codesetor, data = endline)
design.endline.turnedout <- svydesign(ids = ~codesetor, data = endline[!endline$abstain,
                                                                       ])
design.endline.control <- svydesign(ids = ~codesetor, data = endline[endline$group ==
                                                                       "Control", ])
design.endline.control.turnedout <- svydesign(ids = ~codesetor, data = endline[!endline$abstain &
                                                                                 endline$group == "Control", ])

# Calculate confidence intervals

ci.incumb <- confint(svymean(~incumbent, design = design.endline))
ci.opp <- confint(svymean(~opposition, design = design.endline))
ci.winner <- confint(svymean(~winner, design = design.endline))
ci.loser <- confint(svymean(~loser, design = design.endline))
ci.blanknull <- confint(svymean(~blanknull, design = design.endline))
ci.abstain <- confint(svymean(~abstain, design = design.endline))

ci <- rbind(ci.incumb, ci.opp, ci.winner, ci.loser, ci.blanknull, ci.abstain)

ci.incumb.turnedout <- confint(svymean(~incumbent, design = design.endline.turnedout))
ci.opp.turnedout <- confint(svymean(~opposition, design = design.endline.turnedout))
ci.winner.turnedout <- confint(svymean(~winner, design = design.endline.turnedout))
ci.loser.turnedout <- confint(svymean(~loser, design = design.endline.turnedout))
ci.blanknull.turnedout <- confint(svymean(~blanknull, design = design.endline.turnedout))

ci.turnedout <- rbind(ci.incumb.turnedout, ci.opp.turnedout, ci.winner.turnedout,
                      ci.loser.turnedout, ci.blanknull.turnedout, NA)

ci.incumb.control <- confint(svymean(~incumbent, design = design.endline.control))
ci.opp.control <- confint(svymean(~opposition, design = design.endline.control))
ci.winner.control <- confint(svymean(~winner, design = design.endline.control))
ci.loser.control <- confint(svymean(~loser, design = design.endline.control))
ci.blanknull.control <- confint(svymean(~blanknull, design = design.endline.control))
ci.abstain.control <- confint(svymean(~abstain, design = design.endline.control))

ci.control <- rbind(ci.incumb.control, ci.opp.control, ci.winner.control, ci.loser.control,
                    ci.blanknull.control, ci.abstain.control)

ci.incumb.control.turnedout <- confint(svymean(~incumbent, design = design.endline.control.turnedout))
ci.opp.control.turnedout <- confint(svymean(~opposition, design = design.endline.control.turnedout))
ci.winner.control.turnedout <- confint(svymean(~winner, design = design.endline.control.turnedout))
ci.loser.control.turnedout <- confint(svymean(~loser, design = design.endline.control.turnedout))
ci.blanknull.control.turnedout <- confint(svymean(~blanknull, design = design.endline.control.turnedout))

ci.control.turnedout <- rbind(ci.incumb.control.turnedout, ci.opp.control.turnedout,
                              ci.winner.control.turnedout, ci.loser.control.turnedout, ci.blanknull.control.turnedout,
                              NA)

lwd<-3
adjust<-0.25
nvars <- 3
cex<-1.2
cex.point<-1.5

pdf(file='./figures/appendix_figure5.pdf',width=10,height=6)
par(mfrow=c(1,2))
par(mar=c(3,6,2,0)+.01)
plot(pop.vote[c(1, 3, 6)], nvars:1+1*adjust, type='p',axes=F,frame.plot=T,xlab='',ylab='',pch=19,xlim=c(0,0.75),ylim=c(1-1*adjust,nvars+1*adjust),main='All Voters',sub='',cex=cex.point)
Axis(side=1,at=seq(0,.75,.25),labels= seq(0,.75,.25),cex.axis=cex)
Axis(side=2,at=c((nvars+1):0),labels=c('','Incumbent','Winner','Abstention',''),las=1,cex.axis=cex)
abline(h=1:(nvars-1)+.5,lwd=1,lty=3)
segments(ci[c(1, 3, 6),1],nvars:1+0*adjust, ci[c(1, 3, 6),2],nvars:1+0*adjust,lwd=lwd)
segments(ci.control[c(1, 3, 6),1],nvars:1-1*adjust, ci.control[c(1, 3, 6),2],nvars:1-1*adjust,lwd=lwd)
points(sample.vote[c(1, 3, 6)], nvars:1+0*adjust, pch=21, bg='white',cex=cex.point)
points(control.vote[c(1, 3, 6)], nvars:1-1*adjust, pch=25, bg='white',cex=cex.point)

par(mar=c(3,3,2,3)+.01)
plot(pop.vote.turnedout[c(1, 3, 6)], nvars:1+1*adjust, type='p',axes=F,frame.plot=T,xlab='',ylab='',pch=19,xlim=c(0,0.75),ylim=c(1-1*adjust,nvars+1*adjust),main='Voters Who Turned Out',sub='',cex=cex.point)
Axis(side=1,at=seq(0,.75,.25),labels= seq(0,.75,.25),cex.axis=cex)
abline(h=1:(nvars-1)+.5,lwd=1,lty=3)
segments(ci.turnedout[c(1, 3, 6),1],nvars:1+0*adjust, ci.turnedout[c(1, 3, 6),2],nvars:1+0*adjust,lwd=lwd)
segments(ci.control.turnedout[c(1, 3, 6),1],nvars:1-1*adjust, ci.control.turnedout[c(1, 3, 6), 2],nvars:1-1*adjust,lwd=lwd)
points(sample.vote.turnedout[c(1, 3, 6)], nvars:1+0*adjust, pch=21, bg='white',cex=cex.point)
points(control.vote.turnedout[c(1, 3, 6)], nvars:1-1*adjust, pch=25, bg='white',cex=cex.point)

legend('bottomright',pch=c(19,21,25),legend=c('Election Results','Full Sample','Control Group'),bg='white',cex=cex,pt.cex=cex.point)
dev.off()


# Appendix Figure 6 -------------------------------------------------------

panel$vote_vig_cont <- scale01(-1 * panel$vote_vig_cont)
panel$vote_vig <- as.numeric(panel$vote_vig_cont > 0.5)
panel$vote_vig_strict <- as.numeric(panel$vote_vig_cont==1)

reg_vig_cont <- lm(vote_vig_cont ~ vig_treatment, data = filter(panel, acc_rejected == 1 &
                                                                  (ana == 1 | (accounts == 0 & ana == 0 & flier == "Education"))))

reg_vig <- lm(vote_vig ~ vig_treatment, data = filter(panel, acc_rejected == 1 &
                                                        (ana == 1 | (accounts == 0 & ana == 0 & flier == "Education"))))

reg_vig_strict <- lm(vote_vig_strict ~ vig_treatment, data = filter(panel, acc_rejected == 1 &
                                                                      (ana == 1 | (accounts == 0 & ana == 0 & flier == "Education"))))

wsw16$vote_vig_cont<-scale01(wsw16$voteintent)
wsw16$vote_vig<-scale01(wsw16$vote_vig_cont > 0.5)
wsw16$vote_vig_strict<-scale01(wsw16$vote_vig_cont == 1)
reg_vig_cont_wsw16<-lm(vote_vig_cont ~cred_vs_pure,data=wsw16)
reg_vig_wsw16<-lm(I(voteintent == 3 | voteintent == 4)~cred_vs_pure,data=wsw16)
reg_vig_strict_wsw16<-lm(I(voteintent==4)~cred_vs_pure,data=wsw16)


point.est.bhm<-sapply(list(reg_vig_cont, reg_vig_strict, reg_vig), function(x) coef(x)[2])
ci.bhm<-sapply(list(reg_vig_cont, reg_vig_strict, reg_vig), function(x) confint(x)[2,])
point.est.wsw16<-sapply(list(reg_vig_cont_wsw16, reg_vig_strict_wsw16, reg_vig_wsw16), function(x) coef(x)[2])
ci.wsw16<-sapply(list(reg_vig_cont_wsw16, reg_vig_strict_wsw16, reg_vig_wsw16), function(x) confint(x)[2,])


lwd<-2
adjust<-0.15
nvars<-length(point.est.bhm)
cex<-1
cex.point<-1.5

pdf(file='./figures/appendix_figure6.pdf',width=6.5,height=4)

par(mar=c(3,4,1,1)+.01)
plot(point.est.bhm, nvars:1+1*adjust, type='p',axes=F,frame.plot=T,xlab='',ylab='',pch=19, xlim=c(-.75,.75),ylim=c(0.75,3.25),main='',sub='',cex=cex.point)
Axis(side=1,at=seq(-.75, .75, 0.25),labels= seq(-.75, .75, 0.25),cex.axis=cex)
Axis(side=2,at=c((nvars+1):0),labels=c('','Vote\n(Continuous)','"Great\nChance"','"Great" or\n"Some Chance"',''),las=3,cex.axis=cex)
Axis(side=2,line=8,at= sum(c(1-1.5*adjust,nvars+1.5*adjust))/2,labels='Measurement of Vote Intention',las=3,cex.axis=cex,tick=F)
abline(h=1:(nvars-1)+.5,lwd=1,lty=3)
abline(v=0,lwd=1,lty=2)
segments(ci.bhm[1,],nvars:1+1*adjust, ci.bhm[2,],nvars:1+1*adjust,lwd=lwd)
segments(ci.wsw16[1,],nvars:1-1*adjust, ci.wsw16[2,],nvars:1-1*adjust,lwd=lwd)
points(point.est.wsw16, nvars:1-1*adjust, pch=21, bg='white',cex=cex.point)

legend('bottomright',pch=c(19,21),legend=c('This Study','Weitz-Shapiro/Winters'),cex=cex,pt.cex=cex.point)

dev.off()


# Appendix Figure 7 -------------------------------------------------------
vignette_plotdata <- (select(bind_rows(vig_models), outcome, ate_est, ate_se)) %>%
  filter(outcome == "vote_vig")
vignette_plotdata$Sample <- c("Accounts Rejected Municipalities\nNo Flier", "All Municipalities\nNo Flier", "All Respondents")
ggplot(vignette_plotdata, aes(y = ate_est, x = Sample)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ate_est - 1.96 * ate_se, ymax = ate_est + 1.96 * ate_se), width = 0) +
  ylim(-.5, 0) +
  ylab("Estimate") +
  geom_hline(yintercept = 0, linetype = 2) +
  coord_flip()
ggsave("./figures/appendix_figure7.pdf", width = 6, height = 4)


# Appendix Table 2 --------------------------------------------------------

results_table <- data.frame(
  vars = c("", "ATE Estimate", "SE", "n", "Accounts Status", "Covariate Adjusted?"),
  good_unadj = with(models, c("(1)", round(goodacc$ate_unadj, 4), round(goodacc$ate_se_unadj, 4), goodacc$n, "Approved", "")),
  good_adj = with(models, c( "(2)", round(goodacc$ate_est, 4), round(goodacc$ate_se, 4), goodacc$n, "Approved", "\\checkmark")),
  bad_unadj = with(models, c( "(3)", round(badacc$ate_unadj, 4), round(badacc$ate_se_unadj, 4), badacc$n, "Rejected", "")),
  bad_adj = with(models, c( "(4)", round(badacc$ate_est, 4), round(badacc$ate_se, 4), badacc$n, "Rejected", "\\checkmark"))) %>%
  huxtable()
number_format(results_table[2:3, ]) <- 3
bold(results_table[, 1]) <- TRUE
align(results_table[, 1]) <- "left"
align(results_table[, -1]) <- "center"
top_border(results_table[1, ]) <- 1
bottom_border(results_table[1, ]) <- 1
bottom_border(results_table[nrow(results_table), ]) <- 1
escape_contents(results_table[nrow(results_table), ]) <- FALSE
number_format(results_table)[4, ] <- 0
number_format(results_table)[2:3, ] <- 2
number_format(results_table)[1, ] <- 0
width(results_table) <- .75

results_table
cat(to_latex(results_table, tabular_only = TRUE),
    file = "./tables/appendix_table2.tex")

# Appendix Table 3 --------------------------------------------------------
appendix_covars <- c("female", "age", "years_edu", "same_race_mayor", "relative_wellbeing",
                     "mayor_pid", "turnout_2012", "vote_2012", "prob_vote_count", "prob_vote_buying",
                     "info_corrupt_welfare1", "source_cred1_survey", "prob_vote_monitoring", "free_fair",
                     "competitiveness")

##Note: competitiveness is defined at the municipal level so perfectly balanced due to block design
balance <- lapply(appendix_covars[appendix_covars != "competitiveness"],
                  function(x) try(est_ate(outcome = x,
                                          treatment = "accounts",
                                          block = "codesetor",
                                          covars = c(),
                                          data = acc_contr_all,
                                          permute = TRUE,
                                          permute_num = permute_num)))

bal_table <- hux(
  variable = sapply(balance, function(x) x$outcome),
  ate_est = sapply(balance, function(x) x$ate_est),
  se = sapply(balance, function(x) x$ate_se),
  perm_pvalue = sapply(balance, function(x) x$perm_pvalue))
bal_table$variable <- c("Female", "Age", "Education", "Same Party as Mayor",
                        "Same Race as Mayor", "Relative Wellbeing", "2012 Turnout", "2012 Incumbent Vote",
                        "Prob. Vote Count Accurate", "Prob. Vote Buying", "Wants Info about Corruption",
                        "Finds Info from Surveyors Credible", "Prob Vote Can't Be Monitored", "Free and Fair Scale")
bal_table <- bal_table[order(bal_table$variable), ]
names(bal_table) <- c("Variable", "Estimate", "SE", "Perm. P-Value")
bal_table <- add_colnames(bal_table)
top_border(bal_table)[1, ] <- TRUE
bottom_border(bal_table)[1, ] <- TRUE
align(bal_table)[, 2:4] <- "center"
number_format(bal_table) <- 3
width(bal_table) <- 1


print_screen(bal_table)
sink("./tables/appendix_table3.tex")
print_latex(bal_table, tabular_only = TRUE)
sink()



# Appendix Table 4 --------------------------------------------------------

models_attr<- list()
models_attr$full <- est_ate(outcome = "attrited",
                            treatment = "accounts",
                            covars = c(),
                            block = "codesetor",
                            data = filter(panel, ana != 1),
                            permute = TRUE,
                            permute_num = permute_num)
models_attr$good <- est_ate(outcome = "attrited",
                            treatment = "accounts",
                            covars = c(),
                            block = "codesetor",
                            data = filter(panel, ana != 1 & acc_rejected == 0),
                            permute = TRUE,
                            permute_num = permute_num)
models_attr$bad <- est_ate(outcome = "attrited",
                           treatment = "accounts",
                           covars = c(),
                           block = "codesetor",
                           data = filter(panel, ana != 1 & acc_rejected == 1),
                           permute = TRUE,
                           permute_num = permute_num)


attr_table <- data.frame(
  vars = c("", "ATE Estimate", "SE", "Perm. P-Value", "n", "Accounts Status"),
  full = (c("(1)", round(models_attr$full$ate_unadj, 4),
            round(models_attr$full$ate_se_unadj, 4), round(models_attr$full$perm_pvalue, 3),
            models_attr$full$n, "Both")),
  good = (c( "(2)", round(models_attr$good$ate_est, 4),
             round(models_attr$good$ate_se, 4), round(models_attr$good$perm_pvalue, 3),
             models_attr$good$n, "Approved")),
  bad = (c( "(3)", round(models_attr$bad$ate_unadj, 4),
            round(models_attr$bad$ate_se_unadj, 4), round(models_attr$bad$perm_pvalue, 3),
            models_attr$bad$n, "Rejected"))) %>%
  huxtable()
number_format(attr_table[2:3, ]) <- 3
bold(attr_table[, 1]) <- TRUE
align(attr_table[, 1]) <- "left"
align(attr_table[, -1]) <- "center"
top_border(attr_table[1, ]) <- 1
bottom_border(attr_table[1, ]) <- 1
bottom_border(attr_table[nrow(attr_table), ]) <- 1
number_format(attr_table)[5, ] <- 0
number_format(attr_table)[1, ] <- 0
number_format(attr_table)[2:3, ] <- 2
# width(attr_table) <- .75
attr_table
cat(to_latex(attr_table, tabular_only = TRUE),
    file = "./tables/appendix_table4.tex")

pr_s1_d1 <- models_attr$bad$contr_mean + coef(models_attr$bad$unadj_mod[[1]])["accounts"]
pr_s1_d0 <- models_attr$bad$contr_mean
pr_y1_d1 <- models$badacc$contr_mean + coef(models$badacc$unadj_mod[[1]])["accounts"]
pr_y1_d0 <- models$badacc$contr_mean

##Lower Bound
(pr_s1_d1 / pr_s1_d0 *(pr_y1_d1 - ((pr_s1_d1 - pr_s1_d0) / pr_s1_d1))) - pr_y1_d0

##Upper Bound
(pr_y1_d1 / (pr_s1_d0 / pr_s1_d1)) - pr_y1_d0


# Appendix Figure 8 -------------------------------------------------------
load("./data/elec_results_full.RData")
results$pct_votes_current <- 100 * (results$votes + results$votes_pend)/results$turnout

sample <- read.csv("./data/panel_muni_sample.csv", as.is = T)

pe_vote_2012 <- read.csv2("./data/pe_votacao_2012.csv", as.is = T, fileEncoding = "latin1")
pe_vote_2012 <- pe_vote_2012[-1 * which(pe_vote_2012$Abrangência == "Total Geral"),
                             ]
pe_vote_2012$Qt.Comparecimento <- as.numeric(gsub("\\.", "", pe_vote_2012$Qt.Comparecimento))

load("./data/pe_cand_data_12_16.RData")

results <- merge(results, unique(cands16[, c("SIGLA_UE", "CPF_CANDIDATO", "NOME_URNA_CANDIDATO")]),
                 by.x = c("codetse", "name"), by.y = c("SIGLA_UE", "NOME_URNA_CANDIDATO"), all.x = T)

load("./data/pe_results_12.RData")
names(pe_vote_2012)[1] <- "Abrangencia"

results12$TOTAL_VOTOS <- as.numeric(results12$TOTAL_VOTOS)
vote_cand12 <- with(results12, aggregate(list(votes = TOTAL_VOTOS),
                                         list(SQ_CANDIDATO = SQ_CANDIDATO,
                                              NOME_MUNICIPIO = NOME_MUNICIPIO), sum))
vote_cand12 <- merge(vote_cand12, pe_vote_2012[, c("Abrangencia", "Qt.Comparecimento")],
                     by.x = "NOME_MUNICIPIO", by.y = "Abrangencia")
vote_cand12$pct <- 100 * vote_cand12$votes/vote_cand12$Qt.Comparecimento
vote_cand12 <- merge(vote_cand12, unique(cands12[, c("SEQUENCIAL_CANDIDATO", "CPF_CANDIDATO")]),
                     by.x = "SQ_CANDIDATO", by.y = "SEQUENCIAL_CANDIDATO")

# Reduce to incumbents
incumbs <- results[which(results$CPF_CANDIDATO %in% cands12$CPF_CANDIDATO[cands12$DESC_SIT_TOT_TURNO == "ELEITO"]), ]

incumbs$pct_votes_prior <- sapply(incumbs$CPF_CANDIDATO, function(x) vote_cand12$pct[which(vote_cand12$CPF_CANDIDATO == x)])
incumbs$acc_rej <- as.numeric(incumbs$CPF_CANDIDATO) %in% sample$CPF_CANDIDATO[sample$status != "APROVADO COM RESSALVA"]

pdf(file = "./figures/appendix_figure8.pdf", width = 7, height = 6)
par(mar = c(4.5, 4.5, 1, 1) + 0.01)
plot(incumbs$pct_votes_prior[!incumbs$acc_rej], incumbs$pct_votes_current[!incumbs$acc_rej],
     pch = 1, xlab = "Percentage of Votes, 2012", ylab = "Percentage of Votes, 2016",
     cex = 1.25, cex.axis = 1.5, cex.lab = 1.5)
points(incumbs$pct_votes_prior[incumbs$acc_rej], incumbs$pct_votes_current[incumbs$acc_rej],
       pch = 19, cex = 1.25)
abline(lm(incumbs$pct_votes_current ~ incumbs$pct_votes_prior), lwd = 2, lty = 1)

legend("bottomright", pch = c(1, 19, NA), lty = c(NA, NA, 1), lwd = 2,
       legend = c("Accounts Approved", "Accounts Rejected", "Regression Line"), cex = 1.25)
dev.off()

# Appendix Figure 9 -------------------------------------------------------

pilot <- foreign::read.spss("./data/pilot_data.sav", to.data.frame = T, use.value.labels = F)
sample_pilot <- read.csv("./data/pilot_muni_sample.csv", as.is = T, encoding = "UTF-8")
sample_pilot$status <- substr(sample_pilot$status, 1,1)

pilot$approval <- ifelse(pilot$p19 == 9, NA, 6 - pilot$p19)
pilot$rerun <- pilot$muni %in% sample_pilot$codeibge[sample_pilot$rerun]
pilot$acc_rej <- pilot$muni %in% sample_pilot$codeibge[sample_pilot$status == "R"]

reg.rerun.rej <- with(pilot[pilot$rerun & pilot$acc_rej, ], lm(approval ~ 1))
reg.rerun.app <- with(pilot[pilot$rerun & !pilot$acc_rej, ], lm(approval ~ 1))
reg.norerun.rej <- with(pilot[!pilot$rerun & pilot$acc_rej, ], lm(approval ~ 1))
reg.norerun.app <- with(pilot[!pilot$rerun & !pilot$acc_rej, ], lm(approval ~ 1))

app.est <- sapply(list(reg.rerun.app, reg.norerun.app, reg.rerun.rej, reg.norerun.rej),
                  coef)
app.ci <- sapply(list(reg.rerun.app, reg.norerun.app, reg.rerun.rej, reg.norerun.rej),
                 confint)

pdf(file = "./figures/appendix_figure9.pdf", height = 5, width = 7)
par(mar = c(4.5, 7.5, 1, 1) + 0.01)
app.plot <- gplots::barplot2(app.est - 1, ci.u = app.ci[2, ] - 1,
                             ci.l = app.ci[1, ] - 1,
                             plot.ci = T, names.arg = "", ylim = c(0, 4), yaxt = "n")
Axis(side = 1, at = app.plot, labels = c("Accounts\nApproved,\nReran", "Accounts\nApproved,\nBowed Out",
                                         "Accounts\nRejected,\nReran", "Accounts\nRejected,\nBowed Out"), line = 2.5,
     tick = F, cex.axis = 1.25)
Axis(side = 2, at = 0:4, labels = c("Terrible (1)", "Bad (2)", "So-so (3)", "Good (4)",
                                    "Great (5)"), las = 2, cex.axis = 1.25)
dev.off()

# Appendix Table 5 -------------------------------------------------------

pilot_data <- haven::read_spss("./data/pilot_data.sav")
pilot_estimdata <- select(pilot_data, quest, setor, grupo, p59) %>%
  rename(vote_intent = p59) %>%
  mutate(setor = as.character(setor),
         vote_intent = haven::as_factor(vote_intent),
         grupo = haven::as_factor(grupo))
levels(pilot_estimdata$grupo) <- c("Control", "Accounts", "Zika", "Education")
pilot_estimdata <- filter(pilot_estimdata, grupo == "Control" | grupo == "Accounts")
##Load in questionaire data
questionaire_data <- read.csv('./data/data_questionnaire_pilot.csv', stringsAsFactors = FALSE, encoding = 'latin1')
questionaire_data <- select(questionaire_data, qnum, codesetor, codeibge, muni)
questionaire_data$codesetor <- as.character(questionaire_data$codesetor)
questionaire_data$qnum <- as.numeric(questionaire_data$qnum)
pilot_estimdata <- right_join(questionaire_data, pilot_estimdata, by = c('qnum' = 'quest'))
pilot_estimdata$codeibge <- as.character(pilot_estimdata$codeibge)
##Load in municipal level data
load('./data/sample_pilot.RData')
load("./data/sample_pilot_replace.RData")
setores.replace$people_sample <- NULL
setores <- (bind_rows(setores, setores.replace))
muni_data <- unique(select(setores, codeibge, status))
pilot_estimdata <- left_join(pilot_estimdata, muni_data, by = c('codeibge' = 'codeibge'))

pilot_estimdata$accounts_treat <- ifelse(pilot_estimdata$grupo == "Accounts", 1, 0)
pilot_estimdata$vote_incumb <- ifelse(pilot_estimdata$vote_intent == "Prefeito Atual", 1, 0)
pilot_estimdata$accounts_rejected <- ifelse(pilot_estimdata$status == "APROVADO COM RESSALVA", 0, 1)

models_pilot <- list()
models_pilot$approved <- est_ate(outcome = "vote_incumb",
                                 treatment = "accounts_treat",
                                 covars = c(),
                                 block = "codesetor",
                                 data = filter(pilot_estimdata, accounts_rejected == 0),
                                 permute=FALSE)
models_pilot$rejected <- est_ate(outcome = "vote_incumb",
                                 treatment = "accounts_treat",
                                 covars = c(),
                                 block = "codesetor",
                                 data = filter(pilot_estimdata, accounts_rejected == 1),
                                 permute=FALSE)


pilot_results_table <- data.frame(
  vars = c("", "ATE Estimate", "SE", "n", "Accounts Status"),
  approved = with(models_pilot, c("(1)", round(approved$ate_unadj, 4), round(approved$ate_se_unadj, 4), approved$n, "Approved")),
  rejected = with(models_pilot, c( "(3)", round(rejected$ate_unadj, 4), round(rejected$ate_se_unadj, 4), rejected$n, "Rejected"))) %>%
  huxtable()
number_format(pilot_results_table[2:3, ]) <- 3
bold(pilot_results_table[, 1]) <- TRUE
align(pilot_results_table[, 1]) <- "left"
align(pilot_results_table[, -1]) <- "center"
top_border(pilot_results_table[1, ]) <- 1
bottom_border(pilot_results_table[1, ]) <- 1
bottom_border(pilot_results_table[nrow(pilot_results_table), ]) <- 1
escape_contents(pilot_results_table[nrow(pilot_results_table), ]) <- FALSE
number_format(pilot_results_table)[4, ] <- 0
number_format(pilot_results_table)[1, ] <- 0
number_format(pilot_results_table)[2, ] <- 2
#  width(pilot_results_table) <- .75

pilot_results_table
cat(to_latex(pilot_results_table, tabular_only = TRUE),
    file = "./tables/appendix_table5.tex")

# Appendix Figure 10 ------------------------------------------------------
hetby_govteval <- lm(vote ~ accounts * govt_eval_baseline + codesetor, data = acc_contr_bad)
hetby_govteval_vcov <- sandwich::vcovHC(hetby_govteval, type = "HC2")
ame <- coef(hetby_govteval)["accounts"] + acc_contr_bad[, "govt_eval_baseline"] * coef(hetby_govteval)[paste0("accounts", ":", "govt_eval_baseline")]
ame_var <- hetby_govteval_vcov["accounts", "accounts"] +
  acc_contr_bad[, "govt_eval_baseline"]^2 * hetby_govteval_vcov[paste0("accounts", ":", "govt_eval_baseline"), paste0("accounts", ":", "govt_eval_baseline")] +
  2 * acc_contr_bad[, "govt_eval_baseline"] * hetby_govteval_vcov[paste0("accounts", ":", "govt_eval_baseline"), "accounts"]
ame_plotdata <- data.frame(mod = acc_contr_bad$govt_eval_baseline, ame = unlist(ame), ame_se = sqrt(unlist(ame_var)))

govteval_ameplot <- ggplot(ame_plotdata, aes(x = mod, y = ame)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, lty = 2, size = 1) +
  geom_ribbon(aes(ymin = ame - 1.96 * ame_se,
                  ymax = ame + 1.96 * ame_se), alpha = .1) +
  ylab("Average Marginal Effect of Treatment") +
  xlab("Evaluation of Government at Baseline\n(Higher Numbers Indicate More Negative Evaluation)")

ggsave("./figures/appendix_figure10.pdf", width = 6, height = 4)


# Appendix Figure 11 ---------------------------------------------------------------
load("./data/pe_surnames.RData")
unique_cands_pe<-tapply(results_pe$name, results_pe$codetse, function(x) length(unique(x)))
results_pe_unique<-results_pe[-1*which(duplicated(paste(results_pe$codetse,results_pe$name))),]

num_unique_names<-function(x) {
  overlaps<-0
  for(i in 1:length(x)) {
    overlaps<-overlaps+(length(intersect(x[[i]],unlist(x[-i])))>0)
  }
  return(length(x)-overlaps)
}

unique_names_pe<-tapply(results_pe_unique$surname, results_pe_unique$codetse, num_unique_names)

qual_munis<-read.csv('./data/qual_pe_muni_case_studies.csv',fileEncoding='latin1')

pdf('./figures/appendix_figure11.pdf', height=4,width=6)
par(mar=c(5,5,1,1))
hist(unique_names_pe/unique_cands_pe,ylim=c(0,40),xlab='Non-Overlapping Surnames / Unique Top-2 Candidates',ylab='Number of Municipalities',main='',col='lightgrey')
segments((unique_names_pe/unique_cands_pe)[which(names((unique_names_pe/unique_cands_pe)) %in% qual_munis$código)],0,(unique_names_pe/unique_cands_pe)[which(names((unique_names_pe/unique_cands_pe)) %in% qual_munis$código)],2,col='black',lwd=5)
dev.off()

# Appendix Table 6 --------------------------------------------------------


panel$prior<-factor(panel$acc_rejected_prior,labels=c('Approved','Rejected'))

tab.prior<-100*prop.table(table(panel$acc_status,panel$prior),1)

panel$surprised_corrupt<- panel$pol_honesty_noimpute
panel$surprised_corrupt[panel$surprised_corrupt == "NS/NR"] <- NA
panel$surprised_corrupt <- factor(panel$surprised_corrupt, levels = c("Muito surpreso(a)", "Mais ou menos surpreso(a)",
                                                                      "Pouco surpreso(a)", "Não ficaria surpreso(a) de jeito nenhum"))
100*prop.table(table(panel$surprised_corrupt[panel$group=='(1) Controle']))

tab.surprised_corrupt<-100*prop.table(table(panel$acc_status[panel$group=='(1) Controle'],
                                            panel$surprised_corrupt[panel$group=='(1) Controle']),1)

corrupt.table<-apply(round(cbind(tab.prior, tab.surprised_corrupt)),2,function(x) paste0(x,'%'))
rownames(corrupt.table)<-c('Approved','Rejected')
colnames(corrupt.table)<-c('Approved','Rejected','Very','Somewhat','Not very','Not at all')

corrupt.table.note<-'NOTE: Priors on accounts status were measured in the baseline survey just before delivery of treatment information. The question about how surprised respondents would be to learn of credible cases of corruption involving the mayor was asked in the endline survey; responses shown are for the control group.'

corrupt.table.latex <- Hmisc::latex(corrupt.table,file='./tables/appendix_table6.tex',
                                    rowlabel = '', collabel.just=rep('c',ncol(corrupt.table)), col.just=rep('c',ncol(corrupt.table)), rgroup='Accounts Status', n.rgroup=2,cgroup=c('Prior on Accounts Status','How Surprised to Learn of Corruption'), n.cgroup=c(2,4), caption = '\\textbf{Baseline Assessment of the Likelihood of Mayoral Malfeasance}', extracolsize='normalsize', insert.bottom= corrupt.table.note, booktabs = F, ctable = T, where = "htp")


# Appendix Table 7 --------------------------------------------------------

load("./data/data_rosettastone.RData")

br$codetse <- as.numeric(br$codetse)
polfam_manual <- readxl::read_excel("./data/cands1216_potential_dynasties.xlsx") %>%
  filter(dynasty == 1)
polfam_manual <- left_join(polfam_manual, select(br, codetse, codeibge, codeipea),
                           by = c("sigla_ue" = "codetse"))
polfam_manual$codeibge <- as.numeric(polfam_manual$codeibge)
acc_contr_good$dynastic <- ifelse(acc_contr_good$codeibge %in% polfam_manual$codeibge, 1, 0)

dynastic_hetero <- list()
dynastic_hetero$good_nondyn <- est_ate(outcome = "vote",
                                       treatment = "accounts",
                                       covars = c(),
                                       block = "codesetor",
                                       data = filter(acc_contr_good, dynastic == 0),
                                       permute = FALSE)
dynastic_hetero$good_dyn <- est_ate(outcome = "vote",
                                    treatment = "accounts",
                                    covars = c(),
                                    block = "codesetor",
                                    data = filter(acc_contr_good, dynastic == 1),
                                    permute = FALSE)
dynastic_hetero <- bind_rows(dynastic_hetero)
dynastic_hetero$dynastic <- c("Non-Dynastic", "Dynastic")
dynastic_hetero$accounts <- c("Accounts Approved", "Accounts Approved")

dynhetero_table <- tibble(Sample = c("Approved Accounts, Non-Dynastic", "Approved Accounts, Dynastic"),
                          Estimate = dynastic_hetero$ate_est,
                          "t-statistic" = dynastic_hetero$ate_tstat,
                          n = dynastic_hetero$n) %>%
  huxtable(add_colnames=TRUE) %>%
  set_bold(1, 1:4, TRUE) %>%
  set_bottom_border(1, 1:4, 1) %>%
  set_width(.8) %>%
  set_number_format(2:3, 2, 3)
dynhetero_table

cat(to_latex(dynhetero_table, tabular_only = TRUE),
    file = "./tables/appendix_table7.tex")

####
##Calculations Cited in the Paper
###

#"While some countries have had higher spikes in response to particular scandals,
#on average Brazil has the highest levels of popular concern with corruption in the region.
#Moreover, in 2017, corruption was tied  with the economy as the most commonly cited problem."

sort(apply(lapop_corruption,1,mean,na.rm=T))

#"In the post-electoral wave of the survey analyzed below, we asked
#respondents whether mayors who had their accounts rejected by the TCE
#should have the right to run for reelection. In the full sample of
#respondents, 91\% answered ``no.'' Even among respondents who reported
#voting for the incumbent mayor and had been informed of the rejection of
#his or her accounts, 83\% said that such mayors should not have the
#right to run again."
prop.table(table(panel$rejected_should_rerun))
with(filter(acc_contr_bad,  accounts == 1 & vote == 1),
     prop.table(table(rejected_should_rerun)))

#"The median margin of victory in the 2016 mayoral elections in all of
#Brazil was 11.7 percentage points. Pernambuco was somewhat more
#competitive, at 10 percentage points; our 47 sampled municipalities were
#even more competitive, at 9.4 percentage points; and the seven accounts
#rejected municipalities were the most competitive of all, at 9.2
#percentage points."

load("./data/results_mayors_2016.RData")
results16$TOTAL_VOTOS<-as.numeric(results16$TOTAL_VOTOS)
vote_cand16<-with(results16, aggregate(list(votes=TOTAL_VOTOS),list(SQ_CANDIDATO= SQ_CANDIDATO,SIGLA_UE=SIGLA_UE),sum))
vote_winner16<-with(vote_cand16,aggregate(list(votes=votes),list(SIGLA_UE=SIGLA_UE),max))
vote_runnerup16<-with(vote_cand16,aggregate(list(votes=votes),list(SIGLA_UE=SIGLA_UE),function(x) max(x[x!=max(x)])))
vote_runnerup16$votes[vote_runnerup16$votes == -Inf]<-NA # When there is only one candidate listed in results.
vic_margin16<-data.frame(SIGLA_UE=vote_winner16$SIGLA_UE, vic_margin16=vote_winner16$votes-vote_runnerup16$votes)
vote_muni16<-with(results16, aggregate(list(votes16=TOTAL_VOTOS),list(SIGLA_UE=SIGLA_UE),sum))
vic_margin16<-merge(vic_margin16,vote_muni16)
vic_margin16$margin_pct<-100* vic_margin16$vic_margin16/vic_margin16$votes16
vic_margin16$margin_pct[vic_margin16$margin_pct==100]<-NA # Margins of 100% mean someone was disqualified

summary(vic_margin16$margin_pct)
summary(vic_margin16$margin_pct[vic_margin16$SIGLA_UE %in% results16$SIGLA_UE[results16$SIGLA_UF=='PE']])
summary(vic_margin16$margin_pct[vic_margin16$SIGLA_UE %in% sample$codetse])
summary(vic_margin16$margin_pct[vic_margin16$SIGLA_UE %in% sample$codetse[sample$status=='REJEIÇÃO']])

#"While mayoral approval at baseline is significantly higher in
#municipalities with approved accounts, the difference is relatively
#small: 0.27 points on a 5-point scale, or about two-tenths of a standard
#deviation."
t.test(panel$govt_eval_baseline_noimpute[panel$acc_rejected == 0], panel$govt_eval_baseline_noimpute[panel$acc_rejected == 1])
mean(with(panel[panel$acc_rejected==1,], tapply(govt_eval_baseline_noimpute, codeibge,mean,na.rm=T))) - mean(with(panel[panel$acc_rejected==0,], tapply(govt_eval_baseline_noimpute, codeibge,mean,na.rm=T)))
sd(panel$govt_eval_baseline_noimpute, na.rm=T)

#"of 13 first-term mayors whose accounts were rejected, 6 chose not to
#run for reelection (46\%), versus 27 out of 116 (27.1\%) whose accounts
#had been approved or not yet judged"

cands12$incumb.can.rerun <- cands12$DESC_SIT_TOT_TURNO == 'ELEITO' & !cands12$NUM_TITULO_ELEITORAL_CANDIDATO %in% cands08$NUM_TITULO_ELEITORAL_CANDIDATO[cands08$DESC_SIT_TOT_TURNO == 'ELEITO']

cands12$incumb.did.rerun <- cands12$DESC_SIT_TOT_TURNO == 'ELEITO'&cands12$NUM_TITULO_ELEITORAL_CANDIDATO %in% cands16$NUM_TITULO_ELEITORAL_CANDIDATO

cands12$incumb.reran.won <- cands12$DESC_SIT_TOT_TURNO == 'ELEITO'&cands12$NUM_TITULO_ELEITORAL_CANDIDATO %in% cands16$NUM_TITULO_ELEITORAL_CANDIDATO[cands16$DESC_SIT_TOT_TURNO == 'ELEITO']

with(cands12[cands12$SIGLA_UE %in% sample_pilot$codetse[sample_pilot$status=='REJEIÇÃO'],], table(incumb.can.rerun, incumb.did.rerun))

with(cands12[!cands12$SIGLA_UE %in% sample_pilot$codetse[sample_pilot$status=='REJEIÇÃO'],], table(incumb.can.rerun, incumb.did.rerun))

#"We might expect a larger treatment effect among the few voters who do
#prioritize municipal corruption or malfeasance. Treatment effects do
#appear larger in this subgroup, though the sample size is so small---37
#in accounts rejected municipalities and 26 in accounts approved
#municipalities---that these effects cannot be estimated with any precision."
acc_contr_bad$muni_biggest_prob <- stringi::stri_trans_general(acc_contr_bad$muni_biggest_prob, "Latin-ASCII")
acc_contr_good$muni_biggest_prob <- stringi::stri_trans_general(acc_contr_good$muni_biggest_prob, "Latin-ASCII")
acc_contr_bad$acc_salient_spont <- ifelse(acc_contr_bad$muni_biggest_prob %in%
                                            c("Combate a corrupcao/moralizacao da administracao",
                                              "Atraso no pagamento do funcionalismo publico",
                                              "Atraso no pagamento dos aposentados",
                                              "Ma administracao"), 1, 0)
acc_contr_good$acc_salient_spont <- ifelse(acc_contr_good$muni_biggest_prob %in%
                                             c("Combate a corrupcao/moralizacao da administracao",
                                               "Atraso no pagamento do funcionalismo publico",
                                               "Atraso no pagamento dos aposentados",
                                               "Ma administracao"), 1, 0)

good_acc_salient <- est_ate(outcome = "vote",
                            treatment = "accounts",
                            covars = c(),
                            moderator = "acc_salient_spont",
                            block = "codesetor",
                            data = acc_contr_good,
                            permute=FALSE)


bad_acc_salient <- est_ate(outcome = "vote",
                           treatment = "accounts",
                           covars = c(),
                           moderator = "acc_salient_spont",
                           block = "codesetor",
                           data = acc_contr_bad,
                           permute=FALSE)


#"At the start of 2016, only around 30\% of Brazilians identified with a
#political party (Samuels and Zucco 2018); in our survey, the figure was
#26\%."

panel$partyid_onballot<-NA
for(i in 1:nrow(panel)){
  panel$partyid_onballot[i]<-ifelse(is.na(panel$pid[i]), NA, panel$pid[i] %in% merge_data$party[merge_data$codeibge==panel$codeibge[i]])
}

mean(!is.na(panel$pid)) # Overall, can name valid party

#"In our survey, only 16\% of voters identified with a party that was
#running a candidate for mayor in their town."
mean(panel$partyid_onballot %in% T) # Overall, identifies with a party running a candidate for mayor in 2016


#"We found that 24\% of municipalities had at least one mayoral candidate
#in 2016 with a family tie to a candidate in the previous election."
length(unique(polfam_manual$sigla_ue)) / 183 #183 municipios in Pernambuco (excluding Recife and Fernando de Noronha)


#####Session Information

# R version 3.5.1 (2018-07-02)
# Platform: x86_64-apple-darwin17.7.0 (64-bit)
# Running under: macOS High Sierra 10.13.6
#
# Matrix products: default
# BLAS: /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
# LAPACK: /usr/local/Cellar/openblas/0.3.3/lib/libopenblasp-r0.3.3.dylib
#
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base
#
# loaded via a namespace (and not attached):
#   [1] gtools_3.8.1        zoo_1.8-3           tidyselect_0.2.4    purrr_0.2.5         haven_1.1.2         splines_3.5.1
# [7] lattice_0.20-35     colorspace_1.3-2    htmltools_0.3.6     base64enc_0.1-3     survival_2.42-6     rlang_0.2.2
# [13] pillar_1.3.0        foreign_0.8-71      glue_1.3.0          RColorBrewer_1.1-2  readxl_1.1.0        bindrcpp_0.2.2
# [19] bindr_0.1.1         plyr_1.8.4          stringr_1.3.1       munsell_0.5.0       gtable_0.2.0        cellranger_1.1.0
# [25] htmlwidgets_1.2     caTools_1.17.1.1    forcats_0.3.0       latticeExtra_0.6-28 knitr_1.20          htmlTable_1.12
# [31] Rcpp_0.12.18        KernSmooth_2.23-15  acepack_1.4.1       scales_1.0.0        backports_1.1.2     checkmate_1.8.5
# [37] gdata_2.18.0        Hmisc_4.1-1         gplots_3.0.1        gridExtra_2.3       hms_0.4.2           ggplot2_3.0.0
# [43] digest_0.6.16       stringi_1.2.4       dplyr_0.7.6         grid_3.5.1          tools_3.5.1         bitops_1.0-6
# [49] sandwich_2.5-0      magrittr_1.5        lazyeval_0.2.1      tibble_1.4.2        Formula_1.2-3       cluster_2.0.7-1
# [55] crayon_1.3.4        pkgconfig_2.0.2     Matrix_1.2-14       data.table_1.11.4   assertthat_0.2.0    rstudioapi_0.7
# [61] R6_2.2.2            rpart_4.1-13        nnet_7.3-12         compiler_3.5.1