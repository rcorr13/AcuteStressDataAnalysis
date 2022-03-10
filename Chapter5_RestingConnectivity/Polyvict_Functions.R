library(tidyverse)
library(readxl)
library(xlsx)
library(scales)
library(robustbase)
library(data.table)
library(dplyr)
library(Hmisc)
library(robmed)

# creates sample demographics table
SampleDemographics <- function(df) {
  categories = c("Total (N)", "Sex (% Female)", "Age (Years)", "Race (% White)","Race (% Black)", "Race (% Other)", "On Medication", "DSM-IV Diagnosis", "STAIT") 
  demographics <- data.frame(matrix(ncol = 9, nrow = 1))
  colnames(demographics) <- categories
  N = nrow(df)
  demographics["Total (N)"] = N
  demographics["Sex (% Female)"] = label_percent()(as.vector(table(df$Sex)['Female'])/N)
  demographics["Age (Years)"] = paste0(as.character(round(mean(df$AgeInt),1)),' (',as.character(round(sd(df$AgeInt),1)),')')
  demographics["Race (% White)"] = label_percent()(as.vector(table(df$RaceWhiteBlackOther)['White'])/N)
  demographics["Race (% Black)"] = label_percent()(as.vector(table(df$RaceWhiteBlackOther)['Black'])/N)
  demographics["Race (% Other)"] = label_percent()(as.vector(table(df$RaceWhiteBlackOther)['Other'])/N)
  demographics["On Medication"] = label_percent()(as.vector(table(df$Medication)['Yes'])/N)
  demographics["DSM-IV Diagnosis"] = label_percent()(as.vector(table(df$SCID)['Yes'])/N)
  df = df %>% drop_na(STAIT)
  demographics["STAIT"] = paste0(as.character(round(mean(df$STAIT),1)),' (',as.character(round(sd(df$STAIT),1)),')')
  demographics = transpose(demographics)
  row.names(demographics) <- categories
  colnames(demographics) <- c("Demographics")
  return(demographics)
}


# creates frequency table for dataframe df for the specified columns (re-named using newnames)
# default labels are "None" and "Any"
FrequencyTable <-function(df, columns, newnames, labels = c("None", "Any")) {
  dfUsing = df[columns]
  dfUsing <- lapply(dfUsing[columns], factor, levels = c(0,1), labels = labels)
  freqs = lapply(dfUsing, table)
  finalTable <- data.frame(matrix(ncol = (3), nrow = (2*length(columns))))
  colnames(finalTable) <- c("Category","Label","Frequency")
  
  for (i in 1:(length(columns))) {
    rownum = i
    newName = newnames[i]
    for (j in 1:(length(freqs[[i]]))) {
      label = (names(freqs[[i]][j]))
      total = ((freqs[[i]][j]))[[1]] 
      finalTable[rownum,] = c(newName,label,total)
      rownum = rownum + length(columns)
    }
  }
  print(finalTable)
  return(finalTable)
}

# creates a wide version of the above frequency table
FrequencyTableWide <-function(df, columns, newnames, labels = c("None", "Any")) {
  dfUsing = df[columns]
  dfUsing <- lapply(dfUsing[columns], factor, levels = c(0,1), labels = labels)
  freqs = lapply(dfUsing, table)
  freqtable <- data.frame(matrix(ncol = (3), nrow = (2*length(columns))))
  colnames(freqtable) <- c("Category","Label","Frequency")
  for (i in 1:(length(columns))) {
    rownum = i
    newName = newnames[i]
    for (j in 1:(length(freqs[[i]]))) {
      label = (names(freqs[[i]][j]))
      total = ((freqs[[i]][j]))[[1]] 
      freqtable[rownum,] = c(newName,label,total)
      rownum = rownum + length(columns)
    }
  }  
  finalTable = pivot_wider(freqtable, id_cols = 'Category', names_from = 'Label', values_from = 'Frequency')
  finalTable[labels[1]] = as.numeric(as.character(finalTable[[labels[1]]]))
  finalTable[labels[2]] = as.numeric(as.character(finalTable[[labels[2]]]))
  return(finalTable)
}


# creates a correlation table using datafram df and variables in variableList
# first column of output is variable mean, second is standard deviation, third is value range
# following columns are r values with asterices indicating statistical signficance
CorrelationTable <- function(df, variableList) {
  usedList = c()
  
  finalTable <- data.frame(matrix(ncol = (2+length(variableList)), nrow = length(variableList)))
  colnames(finalTable) <- c("Mean","SD","Range",variableList[-length(variableList)])
  row.names(finalTable) <- variableList
  
  corrmat = rcorr((as.matrix(df[variableList])))
  means = colMeans(as.matrix(df[variableList]),na.rm = TRUE)
  stddevs = apply((as.matrix(df[variableList])),2,sd,na.rm = TRUE)
  ranges = t(sapply(df[variableList], range, na.rm = TRUE))
  for (var in variableList) {
    finalTable[var,"Mean"] = round(means[var],2)
    finalTable[var,"SD"] = round(stddevs[var],1)
    finalTable[var,"Range"] = paste0("[", round(ranges[var,1],1), ", ", round(ranges[var,2],1), "]")
    if (length(usedList) > 0) {
      for (i in usedList) {
        rval = round(corrmat$r[var,][i],2)
        pval = round(corrmat$P[var,][i],3)
        if (pval < 0.001) {
          rval = paste0(rval,"***")
        } else if (pval < 0.01) {
          rval = paste0(rval,"**")
        } else if (pval < 0.05) {
          rval = paste0(rval,"*")
        }
        finalTable[var, i] = paste0(rval)
      }
    }
    usedList = c(usedList,var)
  }
  print(finalTable, na.print = "")
  returnMe = list(correlationTable=finalTable,correlationMatrixRaw=corrmat)
  return(returnMe)
}


# creates a t-value table using dataframe df and compares groups in the column named groupBy 
# for each variable in variable list, table provides mean and standard deviation for each variable split by group
# and t-value and its associated p-value (with asterices denoting significance)
tTable <- function(df, groupBy, variableList) {
  finalTable <- data.frame(matrix(ncol = 6, nrow = length(variableList)))
  df = as.data.frame(df)
  categoriesN = dplyr::count(df, df[groupBy])
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
      pval = paste0("<.001***")
    } else if (pval == 0.001) {
      pval = paste0(substring(as.character(pval), 2),"***")
    } else if (pval < 0.01) {
      pval = paste0(substring(as.character(pval), 2),"**")
    } else if (pval < 0.05) {
      pval = paste0(substring(as.character(pval), 2),"*")
    } else {
      pval = substring(as.character(pval), 2)
    }
    finalTable[var, "p"] =  pval
    means = tapply(df[[var]], df[[groupBy]], mean, na.rm=TRUE)
    stddevs = tapply(df[[var]], df[[groupBy]], sd, na.rm=TRUE)
    for (category in catChoices) {
      finalTable[var,paste0(groupBy,"-",category,"-Mean")] = round(means[category],2)
      finalTable[var,paste0(groupBy,"-",category,"-SD")] = round(stddevs[category],2)
    }
  }
  Nline = paste0(as.character((categoriesN[1,1])[[1]])," N=", as.character((categoriesN[1,2])[[1]]),", ",as.character((categoriesN[2,1])[[1]])," N=",as.character((categoriesN[2,2])[[1]]))
  print(finalTable)
  #cat("***p<=.001, ** p<=.01, *p<=.05 /n")
  cat(Nline)
  for (var in variableList) {
    countNA = sum(is.na(df[[var]]))
    if (countNA > 0) {
      newN = dim(df)[1] - countNA
      cat(paste0("\n","*For ", var, " N=",newN))
    }
  }
  cat("\n")
  return(finalTable)
}


# runs a robust mediation model using the R robustbase library for values in dataframe df
# xvar is the x-variable column name, yvar is y-variable, and mvar is the suspected mediator
# covariates are optional (default to none) and other optional settings are set to the
# the default standard for test_mediaiton in robustbase
RobustMediation <- function(df, xvar, yvar, mvar, covariates = NULL, method = "regression", robust=TRUE, R=10000, control = lmrob.control()) {
  mediation <- test_mediation(df, xvar, yvar, mvar, covariates=covariates, method=method, robust=robust, R=R, control=control)
  print(summary(mediation))
  cat("\n")
  cat(paste0("indirect p-value: ",p_value(mediation), "\n"))
  print(coef(mediation))
  cat(paste0('x → m ', round(coef(mediation)[1],4), "\n"))
  cat(paste0('m → y ', round(coef(mediation)[2],4), "\n"))
  cat(paste0('x → y (direct) ', round(coef(mediation)[3],4), "\n"))
  cat(paste0('x → y (indirect) ', round(coef(mediation)[5],4), "\n"))
  cat(paste0('x → y (total) ', round(coef(mediation)[4],4), "\n"))
  return(mediation)
}

# takes the mediation model from robustbase test_mediation output and prints the coefficents for specific relationships
RobustMediationPrintExtras <-function(mediation) {
  cat(paste0("indirect p-value: ",p_value(mediation), "\n"))
  print(coef(mediation))
  cat(paste0('x → m ', round(coef(mediation)[1],4), "\n"))
  cat(paste0('m → y ', round(coef(mediation)[2],4), "\n"))
  cat(paste0('x → y (direct) ', round(coef(mediation)[3],4), "\n"))
  cat(paste0('x → y (indirect) ', round(coef(mediation)[5],4), "\n"))
  cat(paste0('x → y (total) ', round(coef(mediation)[4],4), "\n"))
}
