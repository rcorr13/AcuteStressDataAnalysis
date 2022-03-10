options(stringsAsFactors=FALSE);

library(tidyverse)
library(modelr)
library(readxl)
library(xlsx)
library(Hmisc)
library(scales)
library(robustbase)
library(data.table)
library(testit)

#Uploads reformated MIST data and properly formats variables
setwd('/Users/rcorr/Documents/Belger/Analysis/CodeToUpload/AcuteStressDataAnalysis/PrivateData/Chapter2')
df <- read_excel("AllCopeROIsJune2020.xlsx")
df$Sex <- factor(df$Sex,levels = c(0,1),labels = c("Male", "Female"))
df$CortResponder <- factor(df$AUCiS2S6PostCorrResidResp,levels = c(0,1),labels = c("NonResp", "Resp"))

# names of HR and HRV variables, self-reported stress ratings, and cortisol variables
HRVvars <- c('RS1Raw','MIST1Raw','MIST2Raw','MIST3Raw','RS2Raw','RS1HR','MIST1HR','MIST2HR','MIST3HR','RS2HR','RS1AgeD','MIST1AgeD','MIST2AgeD','MIST3AgeD','RS2AgeD','MISTAgeD','MISTAvgRaw','MISTAvgHR')
RatingCondition = c('S1Rating','S2Rating','S3Rating','S4Rating','S5Rating','S6Rating')
RawCortvars <- c('S1CortOG','S2CortOG','S3CortOG','S4CortOG','S5CortOG','S6CortOG')
             
#Simplifies ROI names to just the experimental>control contrasy
ROIs <- c('Cope4_vmPFC_L', 'Cope4_VS_L', 'Cope4_VS_R', 'Cope4_PCC_L', 'Cope4_Putamen_R', 'Cope4_OFC_L', 'Cope4_Hippocampus_L', 'Cope4_Precuneus_R', 'Cope4_ACC_R', 'Cope4_AntInsula_R', 'Cope4_dlPFC_R')
shortROIs <- c('vmPFC_L', 'VS_L', 'VS_R', 'PCC_L','Putamen_R','OFC_L', 'Hippocampus_L','Precuneus_R', 'ACC_R', 'AntInsula_R', 'dlPFC_R')
setnames(df, old = ROIs, new = shortROIs)

# Calculates paired t-tests of HR and self-reported stress ratings data to show
# changes during the MIST and correlations/t-tests/chi squares to make sure 
# no dependent variables were related to each other then saves them as a .txt file
sink("DependentVariablesCorrelationsAugust2020.txt")
t.test(df$RS1Raw,df$MISTAvgRaw,paired=TRUE)
t.test(df$RS2Raw,df$MISTAvgRaw,paired=TRUE)
t.test(df$RS1Raw,df$RS2Raw,paired=TRUE)
t.test(df$S2Rating,df$S3Rating,paired=TRUE)

listxvars = c('Sex','Age','AUCiS2S6PostCorrResid','HRVDifMIST','STAIT')
factorxvars = c('Sex','Medication')
continousxvars = c('Age','HRVDifMIST','AUCiS2S6PostCorrResid','STAIT')

CorrelationsXVars <- rcorr(as.matrix(df[continousxvars]))
print(CorrelationsXVars)
for (factorxvar in factorxvars) {
  for (continousxvar in continousxvars) {
    print(t.test(as.formula(paste(continousxvar,"~",factorxvar)),df))
  }
}
ChiSquaredSexMed = chisq.test(x = table(df$Sex,df$Medication))
ChiSquaredSexMed
sink()

#Stats for the "Self-Report, Autonomic, and Endocrine Stress Response" Results Section

sink("ResultsSectionASRJuly2020.txt")
t.test(df$S2Rating,df$S3Rating,paired=TRUE)
t.test(df$RS1Raw,df$MISTAvgRaw,paired=TRUE)
t.test(df$RS2Raw,df$MISTAvgRaw,paired=TRUE)
t.test(df$RS1HR,df$MISTAvgHR,paired=TRUE)
t.test(df$RS2HR,df$MISTAvgHR,paired=TRUE)
t.test(df$S2CortOG,df$S4CortOG,paired=TRUE)
sink()


# dataframe containing info to be used by MISTGraphs.R
# will additionally have model weights added below
statsoutput <- df[c('Subject',listxvars,shortROIs,HRVvars,RatingCondition,RawCortvars,'CortResponder','TotalJVQ')]

# eliminates subjects without anxiety and cortisol data
df97 <- df[which(!is.na(df$STAIT) & !is.na(df$AUCiS2S6PostCorrResid)),]
weightsoutput <- df97[c('Subject')]

# sets up dataframe to fill with RLM results
IndVars = c('(Intercept)','SexFemale','Age','STAIT','AUCiS2S6PostCorrResid')
columns2 = c('ROI','N','IndependentVar','coefs','stderror','tvalues','pvalues','pflag','warnings')
optlength2 = (length(IndVars)*length(ROIs))
dfoptions = data.frame(matrix(ncol=length(columns2),nrow=optlength2,dimnames=list(seq(1,optlength2), columns2)))
rownum = 1 
sink("RLMResults07022020.txt")
setwd('/Users/rcorr/Documents/Belger/Analysis/CodeToUpload/AcuteStressDataAnalysis/PrivateData/Chapter2/Rdata')

# for each ROI runs a robust linear model with sex, age, trait anxiety, and cortisol release
for (ROI in shortROIs){
  formulapaste =as.formula(paste(ROI,"~Sex+Age+STAIT+AUCiS2S6PostCorrResid"))
  g1 = lmrob(formula=formulapaste,data=df,method="S",psi="lqq")
  # saves the RLM results
  save(g1, file=paste0(ROI,"_g1.Rdata"))
  # adds flag if there is a warning on the model (typically indicating model breakdown)
  if (has_warning(lmrob(formula=formulapaste,data=df,method='S',psi='lqq'))){
    warningflag = "WARNING"
  } else {
    warningflag = ''
  }
  rweights = g1$rweights
  N = length(rweights)
  UsingN = sum(rweights==1)
  OutliersN = sum(rweights==0)
  WeightedN = sum(rweights>0&rweights<1)
  summary = summary(g1)
  coeftable = (summary(g1)$coefficients)
  g1beta = (coeftable)[,"Estimate"]
  g1stderr = (coeftable)[,"Std. Error"]
  g1tval = (coeftable)[,"t value"]
  g1pval= (coeftable)[,"Pr(>|t|)"]
  for (IndVar in IndVars) {
    # flags significant p values
    if ((g1pval[[IndVar]]<(.05/length(ROIs))) & IndVar !="(Intercept)" & !is.na(g1pval[[IndVar]])) {
      pflag = paste0(c(ROI,"-",IndVar," ",sprintf(g1pval[[IndVar]],fmt = '%.4f'),"<",sprintf((.05/length(ROIs)),fmt = '%.4f')," (U=",UsingN,",W=",WeightedN,",O=",OutliersN,",O%=",percent(OutliersN/N),")"),collapse="")
    } else {
      pflag = ''
    }
    dfoptions[rownum,] = data.frame(ROI,N,IndVar,g1beta[[IndVar]],g1stderr[[IndVar]],g1tval[[IndVar]],g1pval[[IndVar]],pflag,warningflag)
    rownum = rownum + 1
  }
  weightsoutput[paste0("Weights_",ROI)] = rweights
  print(ROI)
  print(summary(g1))
}
sink()

# merges data with model weights information
fulloutput = merge(statsoutput, weightsoutput, by=c("Subject","Subject"), all=TRUE)
saveRDS(dfoptions, file = "MISTDataR07022020.rds")

setwd('/Users/rcorr/Documents/Belger/Analysis/CodeToUpload/AcuteStressDataAnalysis/PrivateData/Chapter2')
#Saves statsoutput as an xlsx file
write.xlsx(fulloutput, "MISTData07022020.xlsx", showNA=FALSE)
write.xlsx(dfoptions, "RLMResultsMIST07022020.xlsx", showNA=FALSE)



# repeats the above analysis but additionally including medication as a covariate (for supplement)
IndVars = c('(Intercept)','SexFemale','Age','STAIT','AUCiS2S6PostCorrResid','Medication')
columns2 = c('ROI','N','IndependentVar','coefs','stderror','tvalues','pvalues','pflag','warnings')
optlength2 = (length(IndVars)*length(ROIs))
dfoptions = data.frame(matrix(ncol=length(columns2),nrow=optlength2,dimnames=list(seq(1,optlength2), columns2)))
rownum = 1 
sink("RLMResults07022020Medication.txt")
for (ROI in shortROIs){
  formulapaste =as.formula(paste(ROI,"~Sex+Age+STAIT+AUCiS2S6PostCorrResid+Medication"))
  g1 = lmrob(formula=formulapaste,data=df,method="S",psi="lqq")
  if (has_warning(lmrob(formula=formulapaste,data=df,method='S',psi='lqq'))){
    warningflag = "WARNING"
  } else {
    warningflag = ''
  }
  rweights = g1$rweights
  N = length(rweights)
  UsingN = sum(rweights==1)
  OutliersN = sum(rweights==0)
  WeightedN = sum(rweights>0&rweights<1)
  summary = summary(g1)
  coeftable = (summary(g1)$coefficients)
  g1beta = (coeftable)[,"Estimate"]
  g1stderr = (coeftable)[,"Std. Error"]
  g1tval = (coeftable)[,"t value"]
  g1pval= (coeftable)[,"Pr(>|t|)"]
  for (IndVar in IndVars) {
    if ((g1pval[[IndVar]]<(.05/length(ROIs))) & IndVar !="(Intercept)" & !is.na(g1pval[[IndVar]])) {
      pflag = paste0(c(ROI,"-",IndVar," ",sprintf(g1pval[[IndVar]],fmt = '%.4f'),"<",sprintf((.05/length(ROIs)),fmt = '%.4f')," (U=",UsingN,",W=",WeightedN,",O=",OutliersN,",O%=",percent(OutliersN/N),")"),collapse="")
    } else {
      pflag = ''
    }
    dfoptions[rownum,] = data.frame(ROI,N,IndVar,g1beta[[IndVar]],g1stderr[[IndVar]],g1tval[[IndVar]],g1pval[[IndVar]],pflag,warningflag)
    rownum = rownum + 1
  }
  print(ROI)
  print(summary(g1))
}
sink()
write.xlsx(dfoptions, "RLMResultsMIST07022020Medication.xlsx", showNA=FALSE)


