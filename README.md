## Replication code and data for ‘‘Corruption information and vote share: A meta-analysis and lessons for experimental design.’’ American Political Science Review, 114.3 (2020): 761-774.

Required Software: Stata (Version 14 or Above), R (3.6.1 or Above). 
Additional required software packages and libraries are specified in the replication code.

Overview: These files replicate all analyses in Trevor Incerti, “Corruption information and vote share: A meta-analysis and lessons for experimental design.” To replicate all analyses and figures, download all data files to a folder entitled “/data”, create a second folder entitled “/figs” in the same directory, and run all code files in numerical order. 

### Replication Code

1. replications_all.R
- Replicates all survey experiments with available replication data and/or compiles data needed for subsequent analysis for all included studies.

2. meta.R
- Conducts meta-analysis, including fixed effects estimation, random effects estimation, mixed effects estimation with moderator, and heterogeneity analysis. 
- Creates Online Appendix Tables A.3-A.5.

3. plot.R
- Plots meta-analysis results from "2. meta.R."
- Creates Figures 1 and 2.  

4. publication_bias.R
- Conducts all analyses related to publication bias: p-curve, examination of funnel plot asymmetry, regression tests for funnel plot asymmetry, trim and fill, and PET-PEESE. 
- Creates Online Appendix Tables A.10-A.14 and A.4-A.12. 

5. predicted_probabilities.do
- (Requires Stata) Loads replication data for Breitenstein 2019, Franchino and Zucchini 2014, Mares and Visconti 2019, and Chauchard, Klasnja and Harish 2019, and conducts all predicted probabilities calculations. 
- Creates Figure 4 and Online Appendix Figures A.14-A.15, A.18-A.21, and A.23-A.25.

6. amce.R
- Loads replication data for Breitenstein 2019, Franchino and Zucchini 2014, and Mares and Visconti 2019, and calculates Average Marginal Component Effects (AMCEs).
- Creates Figure 3 and Online Appendix Figures A.17 and A.22. 

7. tree.R
- Loads replication data for Breitenstein 2019 and Chauchard, Klasnja and Harish 2019 and calculates predicted probabilities using recursive partitioning (classification trees). 
- Creates Figure 5 and Online Appendix Figures A.16 and A.26. 


%%%%% Results in the Online Appendix only start here %%%%%

a1. lab.R
- Conducts meta-analysis of lab experiments. 
- Creates Online Appendix Figure A.1.

a2. robustness.R
- Conducts field experiment meta-analysis excluding Banerjee et al. studies. Conducts mixed effects estimation with moderator and heterogeneity analysis excluding Banerjee et al. studies. Conducts survey experiment meta-analysis including De Figueiredo, Hidalgo and Kasahara 2011. 
- Creates Online Appendix Tables A.6-A.9 and Figures A.2-A.3.

a3. publication_plot.R
- Creates plot of all experiments (point estimates and 95% confidence intervals) by publication status. 
- Creates Online Appendix Figure A.12.

a4. quality.R
- Replicates survey experiments that vary quality of corruption information and plots point estimates and 95% confidence intervals. 
- Creates Online Appendix Figure A.13. 


### Data Files (relative path "~/data")

Replication data for survey experiments:
- Choosing_crook.dta (Breitenstein Research and Politics 2019 raw data)
- Choosing_crook_recode.do (cleans data for Breitenstein Research and Politics 2019 experiment)
- choosing_crook_clean.dta (Breitenstein Research and Politics 2019)
- panel_cleaned.csv (Boas, Hidalgo, and Melo: AJPS 2018)
- chauchard_klasnja_harish.dta (Chauchard, Klasnja, and Harish JOP 2019)
- experiment_data_eggers.Rds (Eggers, Vivyan, and Wagner JOP 2017)
- franchino_zucchini.dta (Franchino and Zucchini PSRM 2014)
- analysis-data.dta (Klasnja, Lupu, and Tucker Working Paper)
- mares_visconti.dta (Mares and Visconti PSRM 2019)
- WintersWeitzShapiro_2013_Replication.dta (Winters and Weitz-Shapiro Comparative Politics 2013)
- WintersWeitz-Shapiro_PRQ_Specificity_ReplicationData.dta (Winters and Weitz-Shapiro PRQ 2016)
- Weitz-ShapiroWinters_JOP_Credibility_ReplicationData.dta (Winters and Weitz-Shapiro Journal of Politics 2017)
- WintersWeitz-Shapiro_PSRM_Argentina_ReplicationData.dta (Winters and Weitz-Shapiro PSRM 2018)


Point estimates and standard errors from field experiments and survey experiments without replication data
- study_results.xlsx
