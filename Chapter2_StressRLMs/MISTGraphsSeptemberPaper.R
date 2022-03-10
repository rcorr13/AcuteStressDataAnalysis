options(stringsAsFactors=FALSE);

library(tidyverse)
library(readxl)
library(robustbase)
library(sets)
library(cairoDevice)
library(ggpubr)
library(cowplot)
library(lemon)

#Calculates standard error of a dataframe column
stderr <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}

#Calculates means, standard deviation, and standard error of a dataframe column
getgraphstats = function(df,Conditions){
  df <- df[Conditions]
  ConditionMeans <- sapply(X=df[,Conditions], FUN = function(x) mean(x,na.rm = TRUE))
  ConditionStddev <- sapply(X=df[,Conditions], FUN = function(x) sd(x,na.rm = TRUE))
  ConditionStderr <- sapply(X=df[,Conditions], FUN = function(x) stderr(x,na.rm = TRUE))
  resultslist <- list("ConditionMeans"=ConditionMeans,"ConditionStddev"=ConditionStddev,"ConditionStderr"=ConditionStderr)
  return(resultslist)
}

# using dataframe df plots sigvar vs yvar, eliminating data based on the outlier columns and coloring
# based on model weights
Plot_XvsY_weightedLM_NoOutliers <- function(df, yvar, sigvar, xname, yname, legendposition="hidden", savefigure=TRUE) {
  ysplit = as.list(strsplit(yvar, '_')[[1]])
  x1 <- df[sigvar]
  y1 <- df[yvar]
  ROIweights = df[paste0("Weights_",yvar)]
  df_x1 <- as.data.frame((c(x1,y1,ROIweights)),col.names=c('x1','y1','rweights'))
  df_x1$outliers <- factor(ifelse(df_x1$rweights>0, "Using", "Outlier"))
  graphmodel = lm(formula = y1 ~ x1, data = df_x1, weights=rweights)  
  df_x1 = df_x1[which(df_x1$outliers=="Using"),]
  
  xminmax = data.frame(x1=c((min(df_x1$x1)-.5),(max(df_x1$x1)+.5)))
  yminmax = predict(graphmodel,newdata=xminmax)
  
  graphplot <- ggplot(data=df_x1,aes(x=x1, y=y1)) +
    theme_classic(base_size = 10)  +
    geom_point(aes(color=rweights),alpha=1,size=1) +
    scale_colour_gradient("Model Weights", low = "white", high = "black") +
    geom_segment(aes(x=xminmax[1,1], y=yminmax[1],xend=xminmax[2,1],yend=yminmax[2]), linetype=1, color="blue",size=.7) +
    xlab(xname) + 
    ylab(yname) +
    theme(text =element_text(face="bold", color="black",family="Helvetica"),
          axis.text.x = element_text(face="plain", color="black"),
          axis.text.y = element_text(face="plain", color="black"),
          axis.ticks.x = element_blank(),
          legend.position = legendposition)
  
  if (isTRUE(savefigure)) {
    ggsave(paste0(sigvar,"_",ysplit[1],"_",ysplit[2],"_NoOutlier_August.eps"), device=cairo_ps, units="mm", width=80, height=80, dpi=3500)
  }
  
  return(graphplot)
}

# Uploads reformated MIST data and properly formats variables
setwd('/Users/rcorr/Documents/Belger/Analysis/MIST/AnalysisJuly2020/')
df <- read_excel("MISTData07022020.xlsx")
setwd('/Users/rcorr/Documents/Belger/Analysis/MIST/AnalysisJuly2020/PaperGraphs')

# ROIs using and times of each saliva collection point (for graph's X axis to be sized proportionally)
# Also organizes ratings into a graphable form 
ROIs <- c('vmPFC_L', 'VS_L', 'VS_R','OFC_L','Putamen_R', 'Hippocampus_L', 'ACC_R','Precuneus_R','AntInsula_R', 'dlPFC_R')
SalivaTimes <- c(0,23.5,41.5,58,73.5,93.5)
SalivaTimes2 <- c(0,18,34.5,50,70)
RatingCondition = c('S2Rating','S3Rating','S4Rating','S5Rating','S6Rating')
TimePoints = c('S2','S3','S4','S5','S6')
ConditionOrder = c(1,2,3,4,5)
RatingStats <- getgraphstats(df,RatingCondition)
Ratingmeans = RatingStats$ConditionMeans
Ratingstddev = RatingStats$ConditionStddev
Ratingstderr = RatingStats$ConditionStderr
Ratingtable <- data.frame(TimePoints, SalivaTimes2, RatingCondition, Ratingmeans, Ratingstddev, Ratingstderr, ConditionOrder)
Ratingtable$TimePoints <- factor(Ratingtable$TimePoints, levels = Ratingtable$TimePoints[order(Ratingtable$ConditionOrder)])
Ratingtable$RatingCondition <- factor(Ratingtable$RatingCondition, levels = Ratingtable$RatingCondition[order(Ratingtable$ConditionOrder)])


#Creates line graph of Self-Reported Stress Ratings (using standard error)
Ratingplot <- ggplot(data=Ratingtable,aes(x=SalivaTimes2,y=Ratingmeans,group=1)) +
  geom_line(size=.45) +
  geom_errorbar(aes(ymin=Ratingmeans-Ratingstderr, ymax=Ratingmeans+Ratingstderr), width=3.5,
                position=position_dodge(0.05),size=.3) +
  geom_point(size=1) +
  theme_classic(base_size = 7) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(3,15,1)) +
  # ggtitle("Self-Reported Stress Ratings") +
  xlab("Time Point") + ylab("Stress Rating") +
  theme(text =element_text(face="plain", color="black",family="Helvetica"),
        axis.text.x = element_text(face="plain", color="black"),
        axis.text.y = element_text(face="plain", color="black"),
        axis.ticks.x = element_blank(),
        legend.position = "none")
ggsave("StressRatings.eps", device=cairo_ps, units="mm", width=80, height=80, dpi=3500)



#Organized Heart Rate Variability (HRV) and Heart Rate (HR) variables in a table for the Heart Rate Graphs
HRVvars <- c('Subject','RS1Raw','MIST1Raw','MIST2Raw','MIST3Raw','RS2Raw','RS1HR','MIST1HR','MIST2HR','MIST3HR','RS2HR')
HRVdf <- df[HRVvars]

#Calculates HRV change from Resting State 1 (RS1) baseline
df$RS1 <- df$RS1Raw - df$RS1Raw
df$MIST1 <- df$MIST1Raw - df$RS1Raw
df$MIST2 <- df$MIST2Raw - df$RS1Raw
df$MIST3 <- df$MIST3Raw - df$RS1Raw
df$RS2 <- df$RS2Raw - df$RS1Raw

HRVCondition = c('RS1','MIST1','MIST2','MIST3','RS2')
HRCondition = c('RS1HR','MIST1HR','MIST2HR','MIST3HR','RS2HR')
ConditionOrder = c(1,2,3,4,5)

HRVStats <- getgraphstats(df,HRVCondition)
HRStats <- getgraphstats(df,HRCondition)

HRVmeans = HRVStats$ConditionMeans
HRVstddev = HRVStats$ConditionStddev
HRVstderr = HRVStats$ConditionStderr

HRmeans = HRStats$ConditionMeans
HRstddev = HRStats$ConditionStddev
HRstderr = HRStats$ConditionStderr

HRVColor = c("#dd3a31","#88ccef","#5798ea","#4e71c8","#da817f")
HRVtable <- data.frame(HRVCondition, HRCondition, HRVmeans, HRVstddev, HRVstderr, HRmeans, HRstddev, HRstderr, HRVColor, ConditionOrder)
HRVtable$HRVCondition <- factor(HRVtable$HRVCondition, levels = HRVtable$HRVCondition[order(HRVtable$ConditionOrder)])
HRVtable$HRCondition <- factor(HRVtable$HRCondition, levels = HRVtable$HRCondition[order(HRVtable$ConditionOrder)])
HRVtable$HRVColor <- factor(HRVtable$HRVColor, levels = HRVtable$HRVColor[order(HRVtable$ConditionOrder)])

#Needed for doted line in line graphs
HRVMIST1y = as.numeric(HRVmeans['MIST1'])
HRRS1y = as.numeric(HRmeans['RS1HR'])
HRMIST1y = as.numeric(HRmeans['MIST1HR'])

#graphs HRV with dotted line between RS1 and MIST 1
HRVplot <- ggplot(data=HRVtable,aes(x=HRVCondition,y=HRVmeans,group=1)) +
  geom_line(size=.45) +
  geom_segment(aes(x="RS1", y=0,xend="MIST1",yend=HRVMIST1y), linetype="dashed", color="white",size=1.2) +
  geom_errorbar(aes(ymin=HRVmeans-HRVstderr, ymax=HRVmeans+HRVstderr), width=.3,
                position=position_dodge(0.05),size=.3) +
  geom_point(size=1) +
  theme_classic(base_size = 7) +
  #ggtitle("High-Frequency Heart Rate Variability") +
  xlab("Condition") + 
  ylab("\u0394 HF-HRV") +
  scale_x_discrete(position = 'top') +
  theme(text =element_text(face="plain", color="black",family="Helvetica"),
        axis.text.x = element_text(face="plain", color="black"),
        axis.text.y = element_text(face="plain", color="black"),
        axis.ticks.x = element_blank(),
        legend.position = "none")
ggsave("HRVstderr.eps", device=cairo_ps, units="mm", width=80, height=80, dpi=3500)

#graphs HR with dotted line between RS1 and MIST 1
HRplot <- ggplot(data=HRVtable,aes(x=HRVCondition,y=HRmeans,group=1)) +
  geom_line(size=.45) +
  geom_segment(aes(x="RS1", y=HRRS1y,xend="MIST1",yend=HRMIST1y), linetype="dashed", color="white", size=.6) +
  geom_errorbar(aes(ymin=HRmeans-HRstderr, ymax=HRmeans+HRstderr), width=.3,
                position=position_dodge(0.05),size=.3) +
  geom_point(size=1) +
  theme_classic(base_size = 7) +
  #ggtitle("Heart Rate") +
  xlab("Condition") + 
  ylab("Heart Rate") +
  scale_x_discrete(position = 'bottom') +
  theme(text =element_text(face="plain", color="black",family="Helvetica"),
        axis.text.x = element_text(face="plain", color="black"),
        axis.text.y = element_text(face="plain", color="black"),
        axis.ticks.x = element_blank(),
        legend.position = "none")
ggsave("HRstderr.eps", device=cairo_ps, units="mm", width=80, height=80, dpi=3500)

# arranges cortisol values for graphing
CortCondition2 = c('S2CortOG','S3CortOG','S4CortOG','S5CortOG','S6CortOG')
TimePoints2 = c('T1','T2','T3','T4','T5')
ConditionOrder2 = c(1,2,3,4,5)

CortStats <- getgraphstats(df,CortCondition2)
Cortmeans = CortStats$ConditionMeans
Cortstddev = CortStats$ConditionStddev
Cortstderr = CortStats$ConditionStderr

CortColor2 = c("#fbdd00","#4e71c8","#f6f1a6","#c698ed","#c9caca")
Corttable2 <- data.frame(TimePoints2, SalivaTimes2, CortCondition2, Cortmeans, Cortstddev, Cortstderr, CortColor2, ConditionOrder2)
Corttable2$TimePoints2 <- factor(Corttable2$TimePoints2, levels = Corttable2$TimePoints2[order(Corttable2$ConditionOrder2)])
Corttable2$CortCondition2 <- factor(Corttable2$CortCondition2, levels = Corttable2$CortCondition2[order(Corttable2$ConditionOrder2)])
Corttable2$CortColor2 <- factor(Corttable2$CortColor2, levels = Corttable2$CortColor2[order(Corttable2$ConditionOrder2)])

#Creates line graph of cortisol raw values not split 
Cortplot <- ggplot(data=Corttable2) +
  geom_errorbar(aes(x=SalivaTimes2,y=Cortmeans,ymin=Cortmeans-Cortstderr, ymax=Cortmeans+Cortstderr), width=3, position=position_dodge(0.05), size=.3) +
  geom_line(aes(x=SalivaTimes2,y=Cortmeans),size=.45) +
  geom_point(aes(x=SalivaTimes2,y=Cortmeans), size=1) +
  theme_classic(base_size = 7) +
  scale_x_continuous(expand = c(0, 0)) +
  #scale_y_continuous(breaks=seq(-.4,.8,.4)) +
  #ggtitle("Salivary Cortisol (u/dL)") +
  xlab("Time Point") + ylab("Salivary Cortisol (u/dL)") +
  theme(text =element_text(face="plain", color="black",family="Helvetica"),
        axis.text.x = element_text(face="plain", color="black"),
        axis.text.y = element_text(face="plain", color="black"),
        axis.ticks.x = element_blank(),
        legend.position = "none")
ggsave("Cortstderr.eps", device=cairo_ps, units="mm", width=80, height=80, dpi=3500)


figure1 <- ggarrange(Ratingplot, HRplot, HRVplot, Cortplot, 
                     font.label = list(size = 11, color = "black", face = "bold",family="Helvetica"),
                     ncol = 2, nrow = 2,  align = "hv", 
                     labels="AUTO",
                     widths = c(2, 2), heights = c(1.8, 1.8))

ggsave("Figure1.eps", device=cairo_ps, units="mm", width=190, height=190, dpi=7480)

df97 = df[which(!is.na(df$STAIT) & !is.na(df$AUCiS2S6PostCorrResid)),]

#Plots bar graphs of Putamen activation for males and females (without major outliers)
dfPutamenNO = df97[which(df97$Weights_Putamen_R>0),]
Putamen_Sex_NO <- ggplot(data=dfPutamenNO,aes(x=Sex,y=Putamen_R)) +
  geom_hline(yintercept=0,color = "gray") +
  stat_boxplot(geom = "errorbar", width = 0.2, color=c("#ea2628","#335bf3")) +
  geom_boxplot(color=c("#ea2628","#335bf3"),fill=c("#ea6c5f","#6288e7")) +
  theme_classic(base_size = 10) +
  #ggtitle("Sex vs Putamen (R)") +
  xlab("Sex") + ylab("Putamen (R) Beta") +
  theme(text =element_text(face="bold", color="black",family="Helvetica"),
        axis.text.x = element_text(face="plain", color="black"),
        axis.text.y = element_text(face="plain", color="black"),
        axis.ticks.x = element_blank(),
        legend.position = "none")
ggsave("SexPutamenNO.eps", device=cairo_ps, units="mm", width=80, height=80, dpi=3500)

# plots line graphs for identified relationships with shading based on LM model weights in df97 dataframe
Hippo_Cort_NO = Plot_XvsY_weightedLM_NoOutliers(df97,'Hippocampus_L','AUCiS2S6PostCorrResid','Cortisol AUCi','Hippocampus (L) Beta')
Hippo_STAIT_NO = Plot_XvsY_weightedLM_NoOutliers(df97,'Hippocampus_L','STAIT','Trait Anxiety','Hippocampus (L) Beta')
VS_STAIT_NO = Plot_XvsY_weightedLM_NoOutliers(df97,'VS_L','STAIT','Trait Anxiety','Ventral Striatum (L) Beta')
Putamen_STAIT_NO = Plot_XvsY_weightedLM_NoOutliers(df97,'Putamen_R','STAIT','Trait Anxiety','Putamen (R) Beta')

# creates the legend for the model weights
legendrweights <- g_legend(Hippo_Cort_NO + theme(legend.position='right'))

#combines prior graphs into one 2x3 figure 
figure3NO <- ggarrange(Hippo_Cort_NO, Hippo_STAIT_NO, VS_STAIT_NO, Putamen_STAIT_NO, Putamen_Sex_NO, legendrweights, 
                       font.label = list(size = 11, color = "black", face = "bold",family="Helvetica"),
                       ncol = 3, nrow = 2,  align = "hv", 
                       labels = c("A", "B", "C", "D", "E", ""),
                       widths = c(2, 2,2), heights = c(1.8, 1.8))
ggsave("Figure3NO.eps", device=cairo_ps, units="mm", width=190, height=120, dpi=7480)