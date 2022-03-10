library(tidyverse)
library(readxl)
library(xlsx)
library(scales)
library(robustbase)
library(data.table)
library(dplyr)
library(Hmisc)
library(robmed)
source("Polyvict_Functions.R")

# Loads data and sets sex/medication use/diagnostic status as factors
setwd('/Users/rcorr/Documents/Belger/Analysis/CodeToUpload/AcuteStressDataAnalysis/PrivateData/Chapter3')

# loads the dataframe with 75 subjects (excluded 5 subjects for neuroimaging issues)
df_complete = readRDS(file = "df_complete_2021_09_01.rds")
df_complete$Sex = factor(df_complete$Sex)
df_complete$Medication = factor(df_complete$Medication)
df_complete$SCIDNum = factor(df_complete$SCIDNum)

# loads the dataframe with full 80 subjects
dfs = readRDS(file = "dfs_2021_09_01.rds")
df_complete_all = dfs$df_complete_all
df_complete_all$SCIDNum = factor(df_complete_all$SCIDNum, labels=c("No","Yes"))

# The ‘SMDM’ method—which was developed for use with smaller samples (Koller and Stahel, 2011)
# and the ‘optimal’ psi were used for robust regression analyses.
ctrl = lmrob.control(method='SMDM', psi = 'optimal')

# continuous variables in the main paper analysis
continousvars = c('TotPolyJVQ5','STAIT','AgeInt','Hippocampus_L','Hippocampus_R','Amygdala_L','Amygdala_R')

# continous variables featured in the supplementary results section
supplementvars = c('STAIS1','STAIS2','RatingsChange32','S2Rating','S3Rating','TotPolyJVQ5','Hippocampus_L','Hippocampus_R','Amygdala_L','Amygdala_R')

# the five polyvictimization categories columns and full category names
PolyvictCats = c('A_ConventionalCrimesAny', 'B_ChildMaltreatmentAny', 'C_PeerSibilingVictAny', 'D_SexualVictAny', 'E_WitnessIndiectAny')
PolyvictStrings = c('Conventional Crimes', 'Child Maltreatment',	'Peer/Sibiling Victimization',	'Sexual Victimization',	'Witnessing/Indirect Victimization')

# writes results to a text file
sink("Paper_Results_2021_09_01_SMDM_Optimal.txt")
cat("Demographics - 80 Subj \n")
SampleDemographicsReviewer2(df_complete_all)

cat("Polyvictimization Frequencies - 75 Subj \n")
PolyvictFreq75 = FrequencyTableWide(df_complete, PolyvictCats, PolyvictStrings)
table(df_complete$TotPolyJVQ5)

cat(paste0("\n", "\n", "\n"))
cat("Variable Relationships - 75 Subj \n")
CorrTable75 = CorrelationTable(df_complete, continousvars)
cat(paste0("\n", "\n"))
CorrTable75Supplement = CorrelationTable(df_complete, supplementvars)
cat(paste0("\n", "\n"))
SexT75 = tTable(df_complete, "Sex", continousvars)
cat(paste0("\n", "\n"))
MedT75 = tTable(df_complete, "Medication", continousvars)
cat(paste0("\n", "\n"))
SCIDT75 = tTable(df_complete, "SCIDNum", continousvars)
cat(paste0("\n", "\n"))
ChiSquaredSexMed = chisq.test(x = table(df_complete$Sex,df_complete$Medication))
ChiSquaredSexMed

cat("Polyvictimization Race (White Black Other) ANOVA \n")
summary(aov(TotPolyJVQ5 ~ RaceWhiteBlackOther, data = df_complete))

cat(paste0("\n", "\n", "\n","\n", "\n"))

cat(" \n Moderation Check - 75 Subj \n")
df_complete$xz = df_complete$TotPolyJVQ5 * df_complete$Hippocampus_L
summary(lm(STAIT~TotPolyJVQ5+Hippocampus_L+xz,data=df_complete))
summary(lm(STAIT~TotPolyJVQ5+Hippocampus_L+xz+Sex+Medication+AgeInt,data=df_complete))

df_complete$xz = df_complete$TotPolyJVQ5 * df_complete$Hippocampus_R
summary(lm(STAIT~TotPolyJVQ5+Hippocampus_R+xz,data=df_complete))
summary(lm(STAIT~TotPolyJVQ5+Hippocampus_R+xz+Sex+Medication+AgeInt,data=df_complete))

df_complete$xz = df_complete$TotPolyJVQ5 * df_complete$Amygdala_L
summary(lm(STAIT~TotPolyJVQ5+Amygdala_L+xz,data=df_complete))
summary(lm(STAIT~TotPolyJVQ5+Amygdala_L+xz+Sex+Medication+AgeInt,data=df_complete))

df_complete$xz = df_complete$TotPolyJVQ5 * df_complete$Amygdala_R
summary(lm(STAIT~TotPolyJVQ5+Amygdala_R+xz,data=df_complete))
summary(lm(STAIT~TotPolyJVQ5+Amygdala_R+xz+Sex+Medication+AgeInt,data=df_complete))

cat(" \n Hippocampus R - 75 Subj \n")
cat(" \n ****x → m***** \n")
g1 = lmrob(Hippocampus_R~TotPolyJVQ5+Sex+Medication+AgeInt, df_complete, control=ctrl)
summary(g1)

cat(" \n \n \n ****x → y***** \n")
g2 = lmrob(STAIT~TotPolyJVQ5+Sex+Medication+AgeInt, df_complete, control=ctrl)
summary(g2)

cat(" \n \n \n ****m → y***** \n")
g3 = lmrob(STAIT~Hippocampus_R+Sex+Medication+AgeInt, df_complete, control=ctrl)
summary(g3)

cat(paste0("\n", "\n", "\n","\n", "\n", "\n","\n", "\n", "\n"))
cat("****Robust Mediation - Sex + Medication + AgeInt Standardized**** \n")
#model = RobustMediation(df_complete,'TotPolyJVQ5','STAIT','Hippocampus_R', covariates=c("Sex","AgeInt","Medication"),control=ctrl)
model = readRDS("HippocampusR75_MainSAM_2021_09_01.rds")
summary(model)
RobustMediationPrintExtras(model)

cat(paste0("\n", "\n", "\n","\n", "\n", "\n","\n", "\n", "\n"))
cat("VIF Values Unstandard \n")
car::vif(lmrob(Hippocampus_R~TotPolyJVQ5+Sex+Medication+AgeInt, df_complete, control=ctrl))
car::vif(lmrob(STAIT~TotPolyJVQ5+Hippocampus_R+Sex+Medication+AgeInt, df_complete, control=ctrl))
car::vif(lmrob(STAIT~TotPolyJVQ5+Sex+Medication+AgeInt, df_complete, control=ctrl))
cat("\n \n")

cat(paste0("\n", "\n", "\n"))
cat("For Graph - Unstandardized \n")
model_xy = lmrob(STAIT~TotPolyJVQ5+Medication+Sex+AgeInt, df_complete, control=ctrl)
model_mx = lmrob(Hippocampus_R~TotPolyJVQ5+Medication+Sex+AgeInt, df_complete, control=ctrl)
model_my = lmrob(STAIT~Hippocampus_R+TotPolyJVQ5+Medication+Sex+AgeInt, df_complete, control=ctrl)

cat(paste0("\n", "\n", "\n"))
cat("More to Print \n")
t.test(as.formula(paste("Hippocampus_R","~","Sex")),df_complete)
t.test(as.formula(paste("TotPolyJVQ5","~","Sex")),df_complete)
t.test(as.formula(paste("STAIT","~","Sex")),df_complete)
cor.test(df_complete$TotPolyJVQ5, df_complete$EducationAvg,  method = "spearman", exact=FALSE)
paste0("N=",sum(!is.na(df_complete$EducationAvg)))
sink()

sink("Extra_Results_Poly5_2021_09_01_SMDM_Optimal.txt")
cat("Demographics - 75 Subj \n")
SampleDemographicsReviewer2(df_complete)

cat(paste0("\n", "\n"))
cat("Polyvictimization Frequencies - 80 Subj \n")
PolyvictFreq80 = FrequencyTable(df_complete_all, PolyvictCats, PolyvictStrings)

#STAIT wasn't related to Amygdala L or R or Hippo L
summary(lmrob(STAIT~Hippocampus_L+Sex+Medication+AgeInt, data=df_complete, control=ctrl))
summary(lmrob(STAIT~Amygdala_L+Sex+Medication+AgeInt, data=df_complete, control=ctrl))
summary(lmrob(STAIT~Amygdala_R+Sex+Medication+AgeInt, data=df_complete, control=ctrl))

cat(paste0("\n", "\n","Left Hippocampus","\n", "\n"))
model1_HippoL = RobustMediation(df_complete,'TotPolyJVQ5','STAIT','Hippocampus_L', covariates=c('Sex','Medication','AgeInt'),control=ctrl,R=10000)

cat(paste0("\n", "\n","Left Amygdala","\n", "\n"))
model1_AmygdalaL = RobustMediation(df_complete,'TotPolyJVQ5','STAIT','Amygdala_L', covariates=c('Sex','Medication','AgeInt'),control=ctrl,R=10000)

cat(paste0("\n", "\n","Right Amygdala","\n", "\n"))
model1_AmygdalaR = RobustMediation(df_complete,'TotPolyJVQ5','STAIT','Amygdala_R', covariates=c('Sex','Medication','AgeInt'),control=ctrl,R=10000)


cat("Hippocampus L and Amygdala - 75 Subj - Doesn't Work \n")
g1_HippoL = lmrob(Hippocampus_L~TotPolyJVQ5+Sex+Medication+AgeInt, df_complete, control=ctrl)
summary(g1_HippoL)

g1_AmyL = lmrob(Amygdala_L~TotPolyJVQ5+Sex+Medication+AgeInt, data=df_complete, control=ctrl)
summary(g1_AmyL)

g1_AmyR = lmrob(Amygdala_R~TotPolyJVQ5+Sex+Medication+AgeInt, data=df_complete, control=ctrl)
summary(g1_AmyR)

cat("****Robust Mediation - Sex + Medication + AgeInt + SCID **** \n")
#model1_CtrlSCID = RobustMediation(df_complete,'TotPolyJVQ5','STAIT','Hippocampus_R', covariates=c('Sex','Medication','AgeInt','SCIDNum'),control=ctrl,R=10000)
model1_CtrlSCID = readRDS("HippocampusR75_SCID_2021_09_01.rds")
summary(model1_CtrlSCID)
RobustMediationPrintExtras(model1_CtrlSCID)

sink()



# saves the r-weights for the main robust mediation model 
rweights_hippo = model$fit$fit_mx$rweights
rweights_stait = model$fit$fit_ymx$rweights

df_complete[paste0("Weights_Hippo_R_mx")] = rweights_hippo
df_complete[paste0("Weights_STAIT_ymx")] = rweights_stait


# saves dataframes
write.xlsx(df_complete, file = "df_Poly5_2021_09_01.xlsx", sheetName="df75", append=TRUE)
write.xlsx(df_complete_all, file = "df_Poly5_2021_09_01.xlsx", sheetName="df80", append=TRUE)

# saves correlations and t-tables
write.xlsx(CorrTable75$correlationTable, file = "CorrTTables_2021_09_01.xlsx", sheetName="CorrTable75", append=TRUE, showNA = FALSE)
write.xlsx(CorrTable75Supplement$correlationTable, file = "CorrTTables_2021_09_01.xlsx", sheetName="CorrTable75Supplement", append=TRUE, showNA = FALSE)
write.xlsx(SexT75, file = "CorrTTables_2021_09_01.xlsx", sheetName="SexT75", append=TRUE, showNA = FALSE)
write.xlsx(MedT75, file = "CorrTTables_2021_09_01.xlsx", sheetName="MedT75", append=TRUE, showNA = FALSE)
write.xlsx(SCIDT75, file = "CorrTTables_2021_09_01.xlsx", sheetName="SCIDT75", append=TRUE, showNA = FALSE)

# saves image of environment
save.image(file="FullAnalysis_2021_09_01.RData") 
