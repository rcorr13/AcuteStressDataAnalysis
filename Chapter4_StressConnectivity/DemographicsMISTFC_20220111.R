options(stringsAsFactors=FALSE);

library(tidyverse)
library(readxl)
library(robustbase)
library(sets)
library(cairoDevice)
library(ggpubr)
library(cowplot)
library(lemon)

library(scales)
library(varhandle)
library(Hmisc)

# creates sample demographics table 
SampleDemographics <- function(df) {
  categories = c("Total (N)", "Sex (% Female)", "Age (Years)", "Race (% White)", "On Medication", "DSM-IV Diagnosis", "STAIT", "JVQ", "Polyvictimization") 
  demographics <- data.frame(matrix(ncol = 9, nrow = 1))
  colnames(demographics) <- categories
  N = nrow(df)
  demographics["Total (N)"] = N
  demographics["Sex (% Female)"] = label_percent()(as.vector(table(df$Sex)['Female'])/N)
  demographics["Age (Years)"] = paste0(as.character(round(mean(df$Age),1)),' (',as.character(round(sd(df$Age),1)),')')
  demographics["Race (% White)"] = label_percent()(as.vector(table(df$Race)['White'])/N)
  demographics["On Medication"] = label_percent()(as.vector(table(df$Medication)['Yes'])/N)
  demographics["DSM-IV Diagnosis"] = label_percent()(as.vector(table(df$SCIDNum)['1'])/N)
  demographics["STAIT"] = paste0(as.character(round(mean(df$STAIT),1)),' (',as.character(round(sd(df$STAIT),1)),')')
  demographics["JVQ"] = paste0(as.character(round(mean(df$TotalJVQ),1)),' (',as.character(round(sd(df$TotalJVQ),1)),')')
  demographics["Polyvictimization"] = paste0(as.character(round(mean(df$TotPolyJVQ5),1)),' (',as.character(round(sd(df$TotPolyJVQ5),1)),')')
  
  demographics = t(demographics)
  row.names(demographics) <- categories
  colnames(demographics) <- c("Demographics")
  return(demographics)
}

# creates a t-value table using dataframe df and compares groups in the column named groupBy 
# for each variable in variable list, table provides mean and standard deviation for each variable split by group
# and t-value and its associated p-value (with asterices denoting significance)
tTable <- function(df, groupBy, variableList) {
  finalTable <- data.frame(matrix(ncol = 6, nrow = length(variableList)))
  categoriesN = count(df, df[groupBy])
  columnNames = c()
  catChoices = sapply(df[groupBy], levels)
  for (category in catChoices) {
    columnNames = c(columnNames,paste0(groupBy,"-",category,"-Mean"),paste0(groupBy,"-",category,"-SD"))
  }
  columnNames = c(columnNames, "t", "p")
  colnames(finalTable) <- columnNames
  row.names(finalTable) <- variableList
  
  for (var in variableList) {
    t.results = t.test(as.formula(paste(var,"~",groupBy)),df)
    finalTable[var, "t"] = round(t.results$statistic,2)
    pval = round(t.results$p.value,3)
    if (pval < 0.001) {
      pval = paste0(substring(as.character(pval), 2),"***")
    } else if (pval < 0.01) {
      pval = paste0(substring(as.character(pval), 2),"**")
    } else if (pval < 0.05) {
      pval = paste0(substring(as.character(pval), 2),"*")
    } else {
      pval = substring(as.character(pval), 2)
    }
    finalTable[var, "p"] =  pval
    means = tapply(df[[var]], df[[groupBy]], mean)
    stddevs = tapply(df[[var]], df[[groupBy]], sd)
    for (category in catChoices) {
      finalTable[var,paste0(groupBy,"-",category,"-Mean")] = round(means[category],2)
      finalTable[var,paste0(groupBy,"-",category,"-SD")] = round(stddevs[category],2)
    }
  }
  Nline = paste0(as.character((categoriesN[1,1])[[1]])," N=", as.character((categoriesN[1,2])[[1]]),", ",as.character((categoriesN[2,1])[[1]])," N=",as.character((categoriesN[2,2])[[1]]))
  print(finalTable)
  cat(Nline)
  return(finalTable)
}

# creates a table for displaying chi-squared results including observed and expected values for each grouping
ChiTable <- function(df, varA, varB) {
  ChiResult = chisq.test(x = table(df[[varA]],df[[varB]]))
  r1 = ChiResult$method
  r2 = paste(c("X:", paste(c(varA,","),collapse=""), "Y:", varB), collapse=" ")
  r3 = capture.output(print(ChiResult))[5]
  r4 = "Observed"
  r5 = ChiResult$observed
  r6 = "Expected"
  r7 = ChiResult$expecte
  paste(c(r1,r2,r3,'\nObserved'), collapse = '\n') %>% cat()
  print(ChiResult$observed)
  cat('\nExpected')
  print(ChiResult$expected)
  return(ChiResult)
}
  

# loads data and correctly factors variables
setwd('/Users/rcorr/Documents/Belger/Analysis/CodeToUpload/AcuteStressDataAnalysis/PrivateData/Chapter4')
df <- read_excel("Resub_Results_Using.xlsx", sheet="df_filt")
df$Sex = factor(df$Sex)
df$Medication = factor(df$Medication, labels=c("No","Yes"))
df$SCID = factor(df$SCID, labels=c("No","Yes"))
df$isWhite <- factor(df$isWhite,levels = c(0,1),labels = c("No", "Yes"))
df$isBlack <- factor(df$isBlack,levels = c(0,1),labels = c("No", "Yes"))
df$isOther <- factor(df$isOther,levels = c(0,1),labels = c("No", "Yes"))
df$RaceWhiteBlackOther = factor(df$RaceWhiteBlackOther)

#uses only valid neuroimaging data
df73 = df[!is.na(df$PCC_Insula),]

# sinks the demographics information as a text file - used
sink('DemographicsInfoResub.txt')
demographics = SampleDemographics(df)
print(demographics,quote=FALSE)

#prints the frequency of levels of PV exposure
cat(paste0("\n", "\n"))
table(df73$TotPolyJVQ5)
cat(paste0("\n", "\n"))
table(df73$skewPV)
cat(paste0("\n", "\n"))

cat("Variable Relationships TotPolyJVQ5 - 73 Subj \n")
cat(paste0("\n", "\n"))
cor.test(df73$TotPolyJVQ5, df73$Age,  method = "pearson", exact=FALSE)

cat("\n SES Spearman\n")
cor.test(df73$TotPolyJVQ5, df73$EducationAvg,  method = "spearman", exact=FALSE)
cat(paste0("\n", "\n"))

continousvars = c("TotPolyJVQ5","Age","EducationAvg","skewPV")
SexT = tTable(df73, "Sex", continousvars)
cat(paste0("\n", "\n"))
MedT = tTable(df73, "Medication", continousvars)
cat(paste0("\n", "\n"))
SCIDT = tTable(df73, "SCID", continousvars)
cat(paste0("\n", "\n"))
ChiSquaredSexMed = ChiTable(df73, "Sex", "Medication")
cat(paste0("\n", "\n"))

t.test(as.formula(paste("TotPolyJVQ5","~","Sex")),df73)
t.test(as.formula(paste("Age","~","Sex")),df73)
t.test(as.formula(paste("TotPolyJVQ5","~","Medication")),df73)
t.test(as.formula(paste("Age","~","Medication")),df73)
t.test(as.formula(paste("TotPolyJVQ5","~","SCID")),df73)
t.test(as.formula(paste("Age","~","SCID")),df73)

cat("Polyvictimization Race (White Black Other) ANOVA \n")
summary(aov(TotPolyJVQ5 ~ RaceWhiteBlackOther, data = df73))
cat("\nPolyvictimization Race ANOVA \n")
summary(aov(TotPolyJVQ5 ~ Race, data = df73))
cat("\nPolyvictimization isWhite ANOVA \n")
summary(aov(TotPolyJVQ5 ~ isWhite, data = df73))

cat(paste0("\n", "\n"))
cat("Variable Relationships skewPV - 73 Subj \n")

cat(paste0("\n", "\n"))
cor.test(df73$skewPV, df73$Age,  method = "pearson", exact=FALSE)

cat("\n SES Spearman\n")
cor.test(df73$skewPV, df73$EducationAvg,  method = "spearman", exact=FALSE)
cat(paste0("\n", "\n"))

SexT = tTable(df73, "Sex", continousvars)
cat(paste0("\n", "\n"))
MedT = tTable(df73, "Medication", continousvars)
cat(paste0("\n", "\n"))
SCIDT = tTable(df73, "SCID", continousvars)
cat(paste0("\n", "\n"))
ChiSquaredSexMed = ChiTable(df73, "Sex", "Medication")
cat(paste0("\n", "\n"))

t.test(as.formula(paste("skewPV","~","Sex")),df73)
t.test(as.formula(paste("Age","~","Sex")),df73)
t.test(as.formula(paste("skewPV","~","Medication")),df73)
t.test(as.formula(paste("Age","~","Medication")),df73)
t.test(as.formula(paste("skewPV","~","SCID")),df73)
t.test(as.formula(paste("Age","~","SCID")),df73)

cat("Polyvictimization Race (White Black Other) ANOVA \n")
summary(aov(skewPV ~ RaceWhiteBlackOther, data = df73))
cat("\nPolyvictimization Race ANOVA \n")
summary(aov(skewPV ~ Race, data = df73))
cat("\nPolyvictimization isWhite ANOVA \n")
summary(aov(skewPV ~ isWhite, data = df73))

sink()


# checked to confirm polyvict wasn't correlated with QA
cor.test(df73$skewPV, df73$InvalidScans,  method = "spearman", exact=FALSE)
cor.test(df73$skewPV, df73$ValidScans,  method = "spearman", exact=FALSE)
cor.test(df73$skewPV, df73$PercentInvalid,  method = "spearman", exact=FALSE)
cor.test(df73$skewPV, df73$MaxMotion,  method = "spearman", exact=FALSE)
cor.test(df73$skewPV, df73$MeanMotion,  method = "spearman", exact=FALSE)
cor.test(df73$skewPV, df73$MaxGSChange,  method = "spearman", exact=FALSE)
cor.test(df73$skewPV, df73$MeanGSChange,  method = "spearman", exact=FALSE)

cor.test(df73$skewPV, df73$InvalidScans,  method = "pearson", exact=FALSE)
cor.test(df73$skewPV, df73$ValidScans,  method = "pearson", exact=FALSE)
cor.test(df73$skewPV, df73$PercentInvalid,  method = "pearson", exact=FALSE)
cor.test(df73$skewPV, df73$MaxMotion,  method = "pearson", exact=FALSE)
cor.test(df73$skewPV, df73$MeanMotion,  method = "pearson", exact=FALSE)
cor.test(df73$skewPV, df73$MaxGSChange,  method = "pearson", exact=FALSE)
cor.test(df73$skewPV, df73$MeanGSChange,  method = "pearson", exact=FALSE)