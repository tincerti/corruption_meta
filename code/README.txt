

Title: Replication File for “Corruption information and vote share: A meta-analysis and lessons for experimental design.”

Author: Trevor Incerti

Correspondence: Trevor Incerti, Yale University (trevor.incerti@yale.edu)

Date: February 9th, 2020

Required Software: Stata (Version 14 or Above), R (3.6.1 or Above)
The required software packages are specified in the replication code.

Overview: These files replicate all analyses in Trevor Incerti, “Corruption information and vote share: A meta-analysis and lessons for experimental design.” To replicate all analyses and figures, download all data files to a folder entitled “~/data”, create a second folder entitled “~/figs” in the same directory, and run all code files in order. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% File Structure %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 


%%%%%%%%%%%%%% Replication Code %%%%%%%%%%%%%%

1. replications_all.R
- Replicates all survey experiments with available replication data and/or compiles data needed for subsequent analysis for all included studies.

2. meta.R
- Conducts meta-analysis, including fixed effects estimation, random effects estimation, mixed effects moderator analysis, and heterogeneity analysis. 
- Creates Online Appendix Tables A.3-A.5.

3. plot.R
- Plots meta-analysis results from "2. meta.R."
- Creates Figures 1 and 2.  

4. publication_bias.R
- Conducts all analyses related to publication bias, including p-curve, examination of funnel plot asymmetry, regression test for funnel plot asymmetry, trim and fill, and PET-PEESE. 
- Creates Online Appendix Tables A.10 - A.14 and A.4 - A.12. 

5. predicted_probabilities.do
- (Requires Stata) The file loads replication data for Breitenstein, Franchino and Zucchini, Mares and Visconti, and Chauchard, Klasnja and Harish, and conducts all predicted probabilities calculations. 
- Creates Figure 4 and Online Appendix Figures A.14-A.15, A.18-A.21, and A.23-A.25.

6. amce.R
This file loads replication data for Breitenstein, Franchino and Zucchini, and Mares and Visconti, and calculates Average Marginal Component Effects.
- Creates Figure 3 and Online Appendix Figures A.17 and A.22. 

7. tree.R
- This file loads replication data for Breitenstein, Franchino and Zucchini, Mares and Visconti, and Chauchard, Klasnja and Harish and calculates predicted probabilities using recursive partitioning (classification trees). 
- Creates Figure 5 and Online Appendix Figures A.16 and A.26. 


%%%%% Results in the Online Appendix only start here %%%%%

a1. lab.R
- Conducts meta-analysis of lab experiments. 
- Creates Online Appendix Figure A.1.

a2. robustness.R
- Conducts field experiment meta-analysis, including fixed effects estimation, random effects estimation, mixed effects moderator analysis, and heterogeneity analysis excluding Banerjee et al. studies. Conducts survey experiment meta-analysis including De Figueiredo, Hidalgo and Kasahara (2011). 
- Creates Online Appendix Tables A.6-A.9 and Figures A.2-A.3.

a3. publication_plot.R
- Creates plot of all experiments (point estimates and standard errors) by publication status. 
- Creates Online Appendix Figure A.12.

a4. quality.R
- Replicates survey experiments that vary quality of corruption information. 
- Creates Online Appendix Figure A.13. 


%%%%%%%%%%%%%% Data Files (relative path "~/data") %%%%%%%%%%%%%%

Replication data for survey experiments:
- WintersWeitzShapiro_2013_Replication.dta- analysis-data.dta (Klasnja, Lupu, and Tucker)- Argentina LAPOP AmericasBarometer 2017 V1.0_W.dta- chauchard_klasnja_harish.dta- Chile LAPOP AmericasBarometer 2017 V1.0_W.dta
- Choosing_crook.dta- Choosing_crook_recode.do (cleans data for Breitenstein experiment)- choosing_crook_clean.dta- eggers.dta- experiment_data_eggers.rds- franchino_zucchini.dta- mares_visconti.dta- panel_cleaned.csv (Boas, Hidalgo, and Melo)- Uruguay LAPOP AmericasBarometer 2017 V1.0_W.dta- Weitz-ShapiroWinters_JOP_Credibility_ReplicationData.dta- WintersWeitz-Shapiro_PRQ_Specificity_ReplicationData.dta- WintersWeitz-Shapiro_PSRM_Argentina_ReplicationData.dta

Point estimates and standard errors from field experiments
- study_results.xlsx