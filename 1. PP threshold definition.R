library(data.table)
library(marginaleffects)
library(dplyr)
library(zoo)
library(rms)
library(ggplot2)
library(performance)
library(partR2)
library(scales)
library(splines)
library(lmerTest)

#Dataset 5 is in long format; determine the PP and SBP group of each participant at each wave
HRS_data_5$PP_group <- NULL
HRS_data_5[PP>=60,PP_group:=3]
HRS_data_5[PP>=50&PP<60,PP_group:=2]
HRS_data_5[PP>=30&PP<50,PP_group:=1]
HRS_data_5[PP<30,PP_group:=0]
HRS_data_5$PP_group <- as.factor(HRS_data_5$PP_group)
summary(HRS_data_5$PP_group)
HRS_data_5$SBP_group <- NULL
HRS_data_5[SBP>=140,SBP_group:=2]
HRS_data_5[SBP<140,SBP_group:=1]
HRS_data_5$SBP_group <- factor(HRS_data_5$SBP_group )
summary(HRS_data_5$SBP_group )
HRS_PP_Group <- dcast(HRS_data_5,formula = HHIDPN ~ wave,value.var = "PP_group")
colnames(HRS_PP_Group ) <- c("HHIDPN","PP_group_wave0","PP_group_wave1","PP_group_wave2",
                             "PP_group_wave3","PP_group_wave4","PP_group_wave5","PP_group_wave6",
                             "PP_group_wave7")
HRS_PP_Group <- as.data.table(HRS_PP_Group)
#Determine each participant's highest and lowest PP group across waves
pp_cols <- paste0("PP_group_wave", 0:7)
HRS_PP_Group$PP_group_max <- apply(
  HRS_PP_Group[, ..pp_cols],
  1,
  max,
  na.rm = TRUE
)
HRS_PP_Group$PP_group_min <- apply(
  HRS_PP_Group[, ..pp_cols],
  1,
  min,
  na.rm = TRUE
)
#number of available PP assessments/visits
HRS_PP_Group$PP_group_assessment_n <- apply(
  HRS_PP_Group[, ..pp_cols],
  1,
  function(x) sum(!is.na(x), na.rm = TRUE)
)
#number of PP groups==2/3
HRS_PP_Group[, PP_group_eq2_n :=
               apply(.SD, 1, function(x) sum(x == 2, na.rm = TRUE)),
             .SDcols = pp_cols
]

HRS_PP_Group[, PP_group_eq3_n :=
               apply(.SD, 1, function(x) sum(x == 3, na.rm = TRUE)),
             .SDcols = pp_cols
]

#identify PP threshold group
HRS_PP_Group[PP_group_max%in%c(0,1)&PP_group_min==0,PP_group_T:="<30"] 
HRS_PP_Group[PP_group_max==1&PP_group_min==1,PP_group_T:="30-50"]      #根据纵向所有数据分组
HRS_PP_Group[PP_group_max==2,PP_group_T:="<60"]
HRS_PP_Group[PP_group_max==3,PP_group_T:=">=60"]
HRS_PP_Group$PP_group_T <- as.factor(HRS_PP_Group$PP_group_T)
summary(HRS_PP_Group$PP_group_T)
#for sensitivity analysis
#Distinguished participants with a single PP wave exceeding the threshold from those with more than one wave exceeding the threshold
summary(factor(HRS_PP_Group[PP_group_max==2]$PP_group_eq2_n))
summary(factor(HRS_PP_Group[PP_group_max==3]$PP_group_eq3_n))
HRS_PP_Group[PP_group_max%in%c(0,1)&PP_group_min==0,PP_group_T_time:="<30"] 
HRS_PP_Group[PP_group_max==1&PP_group_min==1,PP_group_T_time:="30-50"]      #根据纵向所有数据分组
HRS_PP_Group[PP_group_max==2&PP_group_eq2_n==1,PP_group_T_time:="<60 & 1 time"]
HRS_PP_Group[PP_group_max==2&PP_group_eq2_n>1,PP_group_T_time:="<60 & >1 times"]
HRS_PP_Group[PP_group_max==3&PP_group_eq3_n==1,PP_group_T_time:=">=60 & 1 time"]
HRS_PP_Group[PP_group_max==3&PP_group_eq3_n>1,PP_group_T_time:=">=60 & >1 times"]
#identify SBP group
HRS_SBP_Group <- dcast(HRS_data_5,formula = HHIDPN ~ wave,value.var = "SBP_group")
colnames(HRS_SBP_Group ) <- c("HHIDPN","SBP_group_wave0","SBP_group_wave1","SBP_group_wave2",
                              "SBP_group_wave3","SBP_group_wave4","SBP_group_wave5","SBP_group_wave6",
                              "SBP_group_wave7")
HRS_SBP_Group <- as.data.table(HRS_SBP_Group)
SBP_cols <- paste0("SBP_group_wave", 0:7)
HRS_SBP_Group$SBP_group_max <- apply(
  HRS_SBP_Group[, ..SBP_cols],
  1,
  max,
  na.rm = TRUE
)
HRS_SBP_Group$SBP_group_min <- apply(
  HRS_SBP_Group[, ..SBP_cols],
  1,
  min,
  na.rm = TRUE
)
summary(factor(HRS_SBP_Group$SBP_group_min))
HRS_SBP_Group[SBP_group_max==2,SBP_group_T:="High"] 
HRS_SBP_Group[SBP_group_max==1,SBP_group_T:="Low"]      #根据纵向所有数据分组
HRS_SBP_Group$SBP_group_T <- as.factor(HRS_SBP_Group$SBP_group_T)
summary(HRS_SBP_Group$SBP_group_T)

HRS_data_5_T <- left_join(HRS_data_5,HRS_PP_Group[,c("HHIDPN", "PP_group_T","PP_group_T_time","PP_group_assessment_n")])
HRS_data_5_T <- left_join(HRS_data_5_T,HRS_SBP_Group[,c("HHIDPN", "SBP_group_T")])

#complete case 
colnames(COV_HRS_1)
HRS_data_6 <- left_join(HRS_data_5_T,COV_HRS_1[,c("HHIDPN","Education" , "Race" ,"Baseline_Smoking",     
                                                  "Baseline_Drinking" ,  "Baseline_Diabetes"  ,   "Baseline_Hypertension", 
                                                  "Baseline_CESD" , "Baseline_Depression" ,"Baseline_PMBMI" , "Baseline_Obesity" ,    
                                                  "Baseline_Hearing_loss"  ,      
                                                  "Baseline_Visual_loss","Baseline_inactivity","Baseline_unmarried")],by="HHIDPN")
table1(~Age_base+SEX+Race+Age_change+SBP+PP+MAP+Memory_wscore+Orientation_wscore+
         Execution_wscore+Education+factor(Baseline_unmarried)+Baseline_Smoking+Baseline_Drinking+Baseline_Diabetes+
         Baseline_Hypertension+Baseline_Hearing_loss+Baseline_Visual_loss +Baseline_Obesity+     
         Baseline_Depression+factor(Baseline_inactivity),data=HRS_data_6[Base_wave==wave])
#imputed case
HRS_data_7 <- left_join(HRS_data_5_T,IMP_HRS_total[,c("HHIDPN","Education",
                                                      "Baseline_Smoking","Baseline_Drinking" ,
                                                      "Baseline_Diabetes" , "Baseline_Hypertension", 
                                                      "Baseline_Hearing_loss", "Baseline_Visual_loss",
                                                      "Baseline_Obesity","Baseline_Depression",
                                                      "Baseline_inactivity"  , "Baseline_unmarried")],by="HHIDPN")
#demean
HRS_data_7$time_demean <- HRS_data_7$time-mean(HRS_data_7$time)


























