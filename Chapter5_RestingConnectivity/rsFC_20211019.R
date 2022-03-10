library(tidyverse)
library(readxl)
library(xlsx)
library(scales)
library(data.table)
library(dplyr)
library(Hmisc)
library(robmed)
setwd('/Users/rcorr/Documents/Belger/Analysis/CodeToUpload/AcuteStressDataAnalysis/Chapter5_RestingConnectivity')
source("Polyvict_Functions.R")

setwd('/Users/rcorr/Documents/Belger/Analysis/CodeToUpload/AcuteStressDataAnalysis/PrivateData/Chapter5')
# loads data from all subjects
df100 <- read_excel("rsFC_CONN_20211022.xlsx", sheet="df_filt_100HaveSAMS_PreQC")
df100$Sex = factor(df100$Sex,labels=c("Male","Female"))
df100$Medication = factor(df100$Medication, labels=c("No","Yes"))
df100$SCID = factor(df100$SCIDNum, labels=c("No","Yes"))
df100Race = data.frame(to.dummy(df100$Race, "is"))
df100Race$is.Other = df100Race$is.10 + df100Race$is.NA
df100$isWhite = df100Race$is.8
df100$isBlack = df100Race$is.5
df100$isOther = df100Race$is.Other
df100Race2 = df100[c("isWhite", "isBlack", "isOther")]
Step1 = factor(apply(df100Race2, 1, function(x) names(x)[x == 1]),levels=names(df100Race2),ordered=FALSE)
df100$RaceWhiteBlackOther = str_remove(Step1, "is")

# loads and formats df with 98 subjects with quality neuroimaging data
df98 <- read_excel("rsFC_CONN_20211022.xlsx", sheet="df_filt_98ScanSAMS")
df98$Sex = factor(df98$Sex,labels=c("Male","Female"))
df98$Medication = factor(df98$Medication, labels=c("No","Yes"))
df98$SCID = factor(df98$SCIDNum, labels=c("No","Yes"))
df98Race = data.frame(to.dummy(df98$Race, "is"))
df98Race$is.Other = df98Race$is.10 + df98Race$is.NA
df98$isWhite = df98Race$is.8
df98$isBlack = df98Race$is.5
df98$isOther = df98Race$is.Other
df98Race2 = df98[c("isWhite", "isBlack", "isOther")]
Step1 = factor(apply(df98Race2, 1, function(x) names(x)[x == 1]),levels=names(df98Race2),ordered=FALSE)
df98$RaceWhiteBlackOther = str_remove(Step1, "is")

# list of continous x variables
continousvars = c('Age','TotPolyJVQ5')

# the five polyvictimization categories columns and full category names
PolyvictCats = c('A_ConventionalCrimesAny', 'B_ChildMaltreatmentAny', 'C_PeerSibilingVictAny', 'D_SexualVictAny', 'E_WitnessIndiectAny')
PolyvictStrings = c('Conventional Crimes', 'Child Maltreatment',	'Peer/Sibiling Victimization',	'Sexual Victimization',	'Witnessing/Indirect Victimization')


# sinks demographic information for rsFC sample
sink("rsFC_Demographics_2021_12_08.txt")

cat("Demographics - 100 Subj \n")
SampleDemographics(df100)
cat(c("Female N =",table(df100$Sex)['Female']))

cat("\n Polyvictimization Frequencies - 98 Subj \n")
PolyvictFreq = FrequencyTableWide(df98, PolyvictCats, PolyvictStrings)
table(df98$TotPolyJVQ5)

cat(paste0("\n", "\n", "\n"))
cat("Variable Relationships - 100 Subj \n")
cat(paste0("\n", "Age + Polyvict", "\n"))
rcorr(as.matrix(df100[c('Age','TotPolyJVQ5','STAIT')]))
cat(paste0("\n", "\n"))
SexT = tTable(df100, "Sex", continousvars)
cat(paste0("\n", "\n"))
MedT = tTable(df100, "Medication", continousvars)
cat(paste0("\n", "\n"))
SCIDT = tTable(df100, "SCID", continousvars)
cat(paste0("\n", "\n"))
ChiSquaredSexMed = chisq.test(x = table(df100$Sex,df100$Medication))
ChiSquaredSexMed
cat("Polyvictimization Race (White Black Other) ANOVA \n")
summary(aov(TotPolyJVQ5 ~ RaceWhiteBlackOther, data = df100))
cat("\n SES Spearman\n")
cor.test(df100$TotPolyJVQ5, df100$EducationAvg,  method = "spearman", exact=FALSE)
cat(paste0("\n", "\n"))

cat(paste0("\n", "\n", "\n","\n", "\n"))

cat("Variable Relationships - 98 Subj \n")
cat(paste0("\n", "Age + Polyvict", "\n"))
rcorr(as.matrix(df98[c('Age','TotPolyJVQ5')]))
cat(paste0("\n", "\n"))
SexT = tTable(df98, "Sex", continousvars)
cat(paste0("\n", "\n"))
MedT = tTable(df98, "Medication", continousvars)
cat(paste0("\n", "\n"))
SCIDT = tTable(df98, "SCID", continousvars)
cat(paste0("\n", "\n"))
ChiSquaredSexMed = chisq.test(x = table(df98$Sex,df98$Medication))
ChiSquaredSexMed

cat("Polyvictimization Race (White Black Other) ANOVA \n")
summary(aov(TotPolyJVQ5 ~ RaceWhiteBlackOther, data = df98))

cat("\n SES Spearman\n")
cor.test(df98$TotPolyJVQ5, df98$EducationAvg,  method = "spearman", exact=FALSE)
cat(paste0("\n", "\n"))

cat(paste0("\n", "\n", "\n","\n", "\n"))
sink()



