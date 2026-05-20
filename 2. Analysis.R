library(meta)
library(metafor)
library(rms)
library(car)
library(tidyr)
library(dplyr)
library(writexl)

#1. Data-driven, nonlinear effects of PP on Cognition----

#Model 1: Cognition ~ Age + (1|Participant)

#Model 2A: Cognition ~ PP + Age + (1|Participant)
lmer(Memory_wscore ~ PP+Age+(1|HHIDPN), data = HRS_data_7)
#Model 2B: Cognition ~ Spline (PP) + Age + (1|Participant)
lmer(Memory_wscore ~ rcs(PP,3)+Age+(1|HHIDPN), data = HRS_data_7)
#Model 3A: Cognition ~ Spline (PP baseline) * Spline (PP change) + Age + (1|Participant)
lmer(Memory_wscore~ rcs(PP_change,3)*rcs(PP_Base,3)+Age+(1|ID) , data = CHARLS_data_7)
#Model 3B: Cognition ~ Spline (PP baseline) * PP change + Age + (1|Participant)
lmer(Memory_wscore~ PP_change*rcs(PP_Base,3)+Age+(1|ID) , data = CHARLS_data_7)
#Model 3C: Cognition ~ Spline (PP baseline) * PP change + Age + Covariates + (1|Participant)

#Comparion on AIC
AIC
#Comparion on BIC
BIC
#chisq
anova(Model_1, Modle_2A)
#comparision of different knots
anova(lmer(Memory_wscore ~ rcs(PP,3)+Age+(1|HHIDPN), data = HRS_data_7),
      lmer(Memory_wscore ~ rcs(PP,4)+Age+(1|HHIDPN), data = HRS_data_7))
#R square
r2_nakagawa(model1)


#Two-step individual participant data (IPD) approach: linear mixed-effects models were conducted separately within each cohort, and the estimates were subsequently pooled using meta-analysis.
#Pool effect
#Perform random-effects meta-analysis on the interaction effect 
meta_PP_memory <- metagen(TE = B,seTE = std,data = frame_pp_memory,sm = "MD",studlab = Study,common =F,subgroup = `Model`) 
meta::forest(meta_PP_memory,leftcols="studlab",test.subgroup.random=F,overall=F,col.subgroup="black",
             overall.hetstat=F,fontsize=9,header.line="below",
             digits = 3,    digits.se = 3 )


#2. Threshold effects of PP on cognition

HRS_data_7 $PP_group_T <- relevel(HRS_data_7 $PP_group_T,ref = "30-50")
lmer(Memory_wscore ~  PP_group_T*Time + Baseline_age +Base_wave+PP_group_assessment_n +
       SEX+MAP_Base+Education+Baseline_Hearing_loss+Baseline_Depression+Baseline_inactivity+
       Baseline_Diabetes+Baseline_Smoking+Baseline_Hypertension+Baseline_Obesity+
       Baseline_Drinking+ Baseline_unmarried+Baseline_Visual_loss+(1|HHIDPN), data = HRS_data_7)

#pool effect
meta_pp_group_high <- metagen(TE = B,seTE = std,data = frame_pp_group_high,sm = "MD",studlab = Study,common =F,subgroup = `Outcome`)
meta::forest(meta_pp_group_high,leftcols="studlab",test.subgroup.random=F,overall=F,col.subgroup="black",
             overall.hetstat=F,fontsize=9,header.line="below",
             digits = 3,    digits.se = 3 )


#3. Stratified analysis (Table 4)
#3.1 SBP 
#define SBP group based on current SBP levels and history of hypertension 
HRS_data_7$SBP_total_group <- NULL
HRS_data_7[SBP_group_T=="Low"&Baseline_Hypertension==1,SBP_total_group:="Normal"]
HRS_data_7[!(SBP_group_T=="Low"&Baseline_Hypertension==1),SBP_total_group:="High"]
summary(HRS_data_7[Base_wave==wave]$SBP_total_group)
lmer(Memory_wscore ~ PP_group_T*Age_change_demean+Age_base+(1|HHIDPN)+SEX+MAP_Base+Base_wave+PP_group_assessment_n+ 
       Education+Baseline_Hearing_loss+Baseline_Depression+Baseline_inactivity+ 
       Baseline_Diabetes+Baseline_Smoking+Baseline_Obesity+Baseline_Drinking+ 
       Baseline_unmarried+Baseline_Visual_loss , data = HRS_data_7[SBP_total_group=="Normal"])
lmer(Memory_wscore ~ PP_group_T*Age_change_demean+Age_base+(1|HHIDPN)+SEX+MAP_Base+Base_wave+PP_group_assessment_n+ 
       Education+Baseline_Hearing_loss+Baseline_Depression+Baseline_inactivity+ 
       Baseline_Diabetes+Baseline_Smoking+Baseline_Obesity+Baseline_Drinking+ 
       Baseline_unmarried+Baseline_Visual_loss , data = HRS_data_7[SBP_total_group=="High"])
#3.2 Race
HRS_data_7 <- left_join(HRS_data_7,COV_HRS_1[,c("HHIDPN", "Race")])
lmer(Memory_wscore ~ PP_group_T*Age_change_demean+Age_base+(1|HHIDPN)+SEX+MAP_Base+Base_wave+PP_group_assessment_n+ #4
       Education+Baseline_Hearing_loss+Baseline_Depression+Baseline_inactivity+ #4  
       Baseline_Diabetes+Baseline_Smoking+Baseline_Hypertension+Baseline_Obesity+Baseline_Drinking+ #5
       Baseline_unmarried+Baseline_Visual_loss , data = HRS_data_7[Race=="White"])
#3.3 Age
lmer(Memory_wscore ~ PP_group_T*Age_change_demean+Age_base+(1|HHIDPN)+SEX+MAP_Base+Base_wave+PP_group_assessment_n+ #4
       Education+Baseline_Hearing_loss+Baseline_Depression+Baseline_inactivity+ #4  
       Baseline_Diabetes+Baseline_Smoking+Baseline_Hypertension+Baseline_Obesity+Baseline_Drinking+ #5
       Baseline_unmarried+Baseline_Visual_loss , data = HRS_data_7[Age_base<60])
lmer(Memory_wscore ~ PP_group_T*Age_change_demean+Age_base+(1|ID)+SEX+MAP_Base+Base_wave+PP_group_assessment_n+ #4
       Education+Baseline_Hearing_loss+Baseline_Depression+Baseline_inactivity+ #4  
       Baseline_Diabetes+Baseline_Smoking+Baseline_Hypertension+Baseline_Obesity+Baseline_Drinking+ #5
       Baseline_unmarried+Baseline_Visual_loss ,  data = HRS_data_7[Age_base>=60])
#3.4 Sex
lmer(Memory_wscore ~ PP_group_T*Age_change_demean+Age_base+(1|ID)+MAP_Base+Base_wave+PP_group_assessment_n+ #4
       Education+Baseline_Hearing_loss+Baseline_Depression+Baseline_inactivity+ #4  
       Baseline_Diabetes+Baseline_Smoking+Baseline_Hypertension+Baseline_Obesity+Baseline_Drinking+ #5
       Baseline_unmarried+Baseline_Visual_loss ,  data = HRS_data_7[SEX=="Female"])





































































