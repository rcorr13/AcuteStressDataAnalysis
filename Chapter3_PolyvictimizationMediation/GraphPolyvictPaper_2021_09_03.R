options(stringsAsFactors=FALSE);

library(tidyverse)
library(readxl)
library(robustbase)
library(sets)
library(ggpubr)
library(cowplot)
library(lemon)
source("Polyvict_Functions.R")

# plots a simple linear model using values from dataframe df, sigvar as the x variable column name, and yvar as the y variable column name
# xname is the desired x-axis label and yname is the y-axis label
# line color is option (default is "blue")
PaperSimplePlotLM <-function(df, yvar, sigvar, yname, xname, color="blue") {
  x1 <- df[sigvar]
  y1 <- df[yvar]
  df_x1 <- as.data.frame((c(x1,y1)),col.names=c('x1','y1'))
  graphplot = ggplot(df_x1, aes(x=x1, y=y1)) + 
    geom_point()+
    geom_smooth(method=lm, color=color) +
    theme_classic(base_size = 10)  +
    xlab(xname) + ylab(yname) +
    theme(text =element_text(face="bold", color="black",family="Arial"),
          axis.text.x = element_text(face="bold", color="black"),
          axis.text.y = element_text(face="bold", color="black"),
          axis.ticks.x = element_blank(),
          legend.position = "none")
  if (xname == "Polyvictimization") {
    graphplot = graphplot + scale_x_continuous(breaks=seq(0,5,1))
  }
  return(graphplot)
}

#creates a table of frequencies of different variables 
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

#Loads data
setwd('/Users/rcorr/Documents/Belger/Analysis/CodeToUpload/AcuteStressDataAnalysis/PrivateData/Chapter3')
df_complete = readRDS(file = "df_complete_2021_09_01.rds")

#Creates frequency table for each polyvictimization category
PolyvictCats = c('A_ConventionalCrimesAny', 'B_ChildMaltreatmentAny', 'C_PeerSibilingVictAny', 'D_SexualVictAny', 'E_WitnessIndiectAny')
PolyvictStrings = c('Conventional Crimes', 'Child Maltreatment',	'Peer/Sibiling Victimization',	'Sexual Victimization',	'Witnessing/Indirect Victimization')
PolyvictFreq75 = FrequencyTable(df_complete, PolyvictCats, PolyvictStrings)
PolyvictFreq75$Label = factor(PolyvictFreq75$Label, levels = c("None", "Any"))
PolyvictFreq75$Frequency = as.numeric(PolyvictFreq75$Frequency)

# graphs frequency of each individual victimization category
ggplot(PolyvictFreq75,aes(x=Category,y=Frequency,fill=factor(Label)))+
  geom_bar(stat="identity",position="dodge")+
  theme_classic(base_size = 15) +
  scale_fill_manual(name="Frequency",
                    labels=c("None", "Any"),
                    values=c("#B6B6B6","#FFC000")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(limits=c(0,65),expand = c(0,0)) +
  xlab("Victimization Category")+ylab("Total (# of subjects)") + 
  theme(text=element_text(face="bold", color="black",family="Arial"),
        axis.text.x = element_text(face="plain", color="black"),
        axis.text.y = element_text(face="plain", color="black"),
        legend.title = element_blank())

ggsave("Figure2.eps", device=cairo_ps, units="mm", width=170, height=90, dpi=7480)


# creates the individual plots for each step of the mediation model
graph_xy = PaperSimplePlotLM(df_complete, 'STAIT', 'TotPolyJVQ5', 'Trait Anxiety', 'Polyvictimization', color='#0070C0')
graph_mx = PaperSimplePlotLM(df_complete, 'Hippocampus_R', 'TotPolyJVQ5', 'Hippocampus (R) Beta', 'Polyvictimization', color='#D80402')
graph_my = PaperSimplePlotLM(df_complete, 'STAIT', 'Hippocampus_R', 'Trait Anxiety', 'Hippocampus (R) Beta', color='#0070C0')

# arranges three figures for paper  
figure3 <- ggarrange(graph_xy, graph_mx, graph_my, 
          font.label = list(size = 11, color = "black", face = "bold", family="Arial"),
          ncol = 3, nrow = 1,  align = "hv", 
          labels="AUTO",
          #common.legend = TRUE, legend = "bottom",
          widths = c(1.2, 1.2, 1.2), heights = c(1))
ggsave("Figure3.eps", device=cairo_ps, units="in", width=8, height=2.5, dpi=7480)





