options(stringsAsFactors=FALSE);

library(tidyverse)
library(modelr)
library(readxl)
library(xlsx)
library(Hmisc)
library(robustbase)
library(data.table)


#Uploads reformated MIST data and properly formats variables
#setwd('/nas/longleaf/home/rcorr/neuroanalytics/neuroanalytics/FinalProject')
#df <- read_excel("MISTReformatedData.xlsx")
df <- read_excel("MISTReformatedDataRandom.xlsx")
df$Sex <- factor(df$Sex,levels = c(0,1),labels = c("Male", "Female"))
df$CortResponder <- factor(df$S2S6ResidResp,levels = c(0,1),labels = c("NonResp", "Resp"))
df$HRVDifMIST <- df$MISTAvgDifRS1Raw
df$HRVDifMISTHR <- df$MISTAvgDifRS1HR
df69 <- df[which(!is.na(df$STAIT) & !is.na(df$CortResponder) & !is.na(df$HRVDifMIST)),]
HRVvars <- c('RS1Raw','MIST1Raw','MIST2Raw','MIST3Raw','RS2Raw','RS1HR','MIST1HR','MIST2HR','MIST3HR','RS2HR')
RatingCondition = c('S1Rating','S2Rating','S3Rating','S4Rating','S5Rating','S6Rating')

#Simplifies ROI names to just the experimental>control contrasy
ROIs <- c('Cope4_vmPFC_L', 'Cope4_VS_L', 'Cope4_VS_R', 'Cope4_PCC_L', 'Cope4_OFC_L', 'Cope4_Putamen_R', 'Cope4_Hippocampus_L', 'Cope4_ACC_R', 'Cope4_AntInsula_R', 'Cope4_dlPFC_R')
shortROIs <- c('vmPFC_L', 'VS_L', 'VS_R', 'PCC_L', 'OFC_L', 'Putamen_R', 'Hippocampus_L', 'ACC_R', 'AntInsula_R', 'dlPFC_R')
setnames(df, old = ROIs, new = shortROIs)

# Calculates paired t-tests of HR and self-reported stress ratings data to show
# changes during the MIST and correlations/t-tests/chi squares to make sure 
# no dependent variables were related to each other then saves them as a .txt file
sink("DependentVariablesResults.txt")
t.test(df$RS1Raw,df$MISTAvgRaw,paired=TRUE)
t.test(df$RS2Raw,df$MISTAvgRaw,paired=TRUE)
t.test(df$RS1Raw,df$RS2Raw,paired=TRUE)
t.test(df$S2Rating,df$S3Rating,paired=TRUE)

listxvars = c('Sex','Age','CortResponder','HRVDifMIST','STAIT')
factorxvars = c('Sex','CortResponder')
continousxvars = c('Age','HRVDifMIST','STAIT')

CorrelationsXVars <- rcorr(as.matrix(df[continousxvars]))
print(CorrelationsXVars)
for (factorxvar in factorxvars) {
  for (continousxvar in continousxvars) {
    print(t.test(as.formula(paste(continousxvar,"~",factorxvar)),df69))
  }
}

ChiSquaredSexCort = chisq.test(x = table(df69$Sex,df69$CortResponder))
ChiSquaredSexCort
sink()

#Dataframe containing info to be used by MISTGraphs.R
statsoutput <- df69[c('Subject',listxvars,shortROIs,'AgeInt',HRVvars,RatingCondition)]

# Uses lmrob (from the robustbase package) to create robust linear models between
# each ROI and Sex+Age+CortResponder+HRVDifMIST+STAIT then saves the models to 
# a txt file and adds the weights of residuals as a new column in statsoutput
sink("RLMResults.txt")
for (ROI in shortROIs){
  formulapaste =as.formula(paste(ROI,"~Sex+Age+CortResponder+HRVDifMIST+STAIT"))
  g1 = lmrob(formula=formulapaste,data=df69,method="MM",psi="opt")
  statsoutput[paste0("ModelUsing_",ROI)] = g1$rweights
  print(ROI)
  print(summary(g1))
}
#Checks if for the right putamen there was a significant interaction between Sex and STAIT
print(summary(lmrob(formula=as.formula(paste("Putamen_R","~Sex+Age+CortResponder+HRVDifMIST+STAIT+Sex:STAIT")),data=df69,method="MM",psi="opt")))
sink()

#Saves statsoutput as an xlsx file
#write.xlsx(statsoutput, "MISTReformatedDataStats.xlsx")
write.xlsx(statsoutput, "MISTReformatedDataStatsRandom.xlsx")
