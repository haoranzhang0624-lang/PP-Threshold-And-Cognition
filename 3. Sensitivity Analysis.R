#1.Association of PP  with cognition Stratified by different pp groups

lmer(Memory_wscore ~ PP+Age+(1|HHIDPN), data = HRS_data_7[PP_group_T=="<30"])

#2. Distinguish participants with a single PP wave exceeding the threshold from those with more than one wave exceeding the threshold.
#PP wave exceeding the threhold: 1 time vs >1 times----
summary(HRS_data_7$PP_group_T_time)
lmer(Memory_wscore ~ PP_group_T_time* Time +Age_base+(1|HHIDPN)+SEX+MAP_Base+Base_wave+PP_group_assessment_n+ #4
       Education+Baseline_Hearing_loss+Baseline_Depression+Baseline_inactivity+ #4  
       Baseline_Diabetes+Baseline_Smoking+Baseline_Hypertension+Baseline_Obesity+Baseline_Drinking+ #5
       Baseline_unmarried+Baseline_Visual_loss , data = HRS_data_7)

#3. Antimedication use
#Address inconsistencies in the data (baseline hypertension was no, antiBP should be no)
lmer(Memory_wscore ~ PP_group_T* Time +Age_base+(1|HHIDPN)+SEX+MAP_Base+ 
       Education+Baseline_Hearing_loss+Baseline_Depression+Baseline_inactivity+ 
       Baseline_Diabetes+Baseline_Smoking+Baseline_Hypertension+Baseline_Obesity+Baseline_Drinking+ 
       Baseline_unmarried+Baseline_Visual_loss +AntiBP, data = HRS_data_7)


#4. Excluding participants without PP follow-up assessments 
lmer(Memory_wscore ~ PP_group_T* Time +Age_base+(1|HHIDPN)+SEX+MAP_Base+Base_wave+PP_group_assessment_n+ #4
       Education+Baseline_Hearing_loss+Baseline_Depression+Baseline_inactivity+ #4  
       Baseline_Diabetes+Baseline_Smoking+Baseline_Hypertension+Baseline_Obesity+Baseline_Drinking+ #5
       Baseline_unmarried+Baseline_Visual_loss , data = HRS_data_7[PP_group_assessment_n>1])
#3-1.Differences in visiting patterns
#PP follow-up assessments<=2
lmer(Memory_wscore ~ PP_group_T* Time +Age_base+(1|HHIDPN)+SEX+MAP_Base+Base_wave+ #4
       Education+Baseline_Hearing_loss+Baseline_Depression+Baseline_inactivity+ #4  
       Baseline_Diabetes+Baseline_Smoking+Baseline_Hypertension+Baseline_Obesity+Baseline_Drinking+ #5
       Baseline_unmarried+Baseline_Visual_loss , data = HRS_data_7[PP_group_assessment_n<=2])
##PP follow-up assessments>2
lmer(Memory_wscore ~ PP_group_T* Time +Age_base+(1|HHIDPN)+SEX+MAP_Base+Base_wave+ #4
       Education+Baseline_Hearing_loss+Baseline_Depression+Baseline_inactivity+ #4  
       Baseline_Diabetes+Baseline_Smoking+Baseline_Hypertension+Baseline_Obesity+Baseline_Drinking+ #5
       Baseline_unmarried+Baseline_Visual_loss , data = HRS_data_7[PP_group_assessment_n>2])
#using "metagen" package to pool effect


#5. excluding participants with heart and stroke

lmer(Memory_wscore ~ PP_group_T_time* Time +Age_base+(1|HHIDPN)+SEX+MAP_Base+ #4
       Education+Baseline_Hearing_loss+Baseline_Depression+Baseline_inactivity+ #4  
       Baseline_Diabetes+Baseline_Smoking+Baseline_Hypertension+Baseline_Obesity+Baseline_Drinking+ #5
       Baseline_unmarried+Baseline_Visual_loss , data = HRS_data_S4_1)

#5. complete data analysis
#using data 6

#6. One-step IPD
#pool data first 

lmer(Memory_wscore ~ PP_group_T * Time + Age_base +(1 | ID_N) + (1 | Study) +
    SEX + MAP_Base + Base_wave + PP_group_assessment_n + Education + Baseline_Hearing_loss + Baseline_Depression +
    Baseline_inactivity + Baseline_Diabetes + Baseline_Smoking +Baseline_Hypertension + Baseline_Obesity + Baseline_Drinking +
    Baseline_unmarried + Baseline_Visual_loss,data = Pool_1,
  control = lmerControl(optimizer = "bobyqa",optCtrl = list(maxfun = 2e5)))



colnames(ELSA_data_7)

write_xlsx(HRS_data_7, "HRS_data.xlsx")
write_xlsx(CHARLS_data_7, "CHARLS_data.xlsx")
write_xlsx(ELSA_data_7, "ELSA_data.xlsx")


















