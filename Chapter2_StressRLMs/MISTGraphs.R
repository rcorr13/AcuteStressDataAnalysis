options(stringsAsFactors=FALSE);

library(tidyverse)
library(readxl)
library(robustbase)
library(personograph)

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

# Adapted from crPlots - creates a component+residual plot for a robust linear
# model with listxvars as the independent varibables, yvar as the dependent variable,
# and sigvar as the X axis. Marks outlier datapoints with a red X.
crPlot_MarkOutliers<-function(df, listxvars, yvar, sigvar, modelusing) {
  model = lmrob(formula=as.formula(paste(yvar,"~",listxvars)),data=df,method="MM",psi="opt")
  ysplit = as.list(strsplit(yvar, '_')[[1]])
  yname = paste0(ysplit[1], " (", ysplit[2], ")") 
  
  x1 <- df[sigvar]
  x1beta = x1*model$coefficients[sigvar]
  x1beta_resid = x1beta + model$residuals
  df_x1 <- as.data.frame((c(x1,x1beta_resid,df[modelusing])),col.names=c('x1_col','x1beta_resid_col','outliers'))
  #df_x1$outliers <- factor(df_x1$outliers,levels = c(0,1),labels = c("Outlier", "Using"))
  df_x1$outliers <- factor(ifelse(df_x1$outliers>0, "Using", "Outlier"))
  
  graphmodel = lm(formula = x1beta_resid_col ~ x1_col, data = df_x1)  
  xminmax = data.frame(x1_col=c((min(x1)-.5),(max(x1)+.5)))
  yminmax = predict(graphmodel,newdata=xminmax)
  
  ggplot(data=df_x1,aes(x=x1_col, y=x1beta_resid_col)) +
    theme_classic(base_size = 15)  +
    geom_point(aes(group=outliers,shape=outliers,color=outliers),alpha=0.6,size=3) +
    scale_shape_manual(values=c(4, 16))+
    scale_color_manual(values=c('red','blue'))+
    geom_segment(aes(x=xminmax[1,1], y=yminmax[1],xend=xminmax[2,1],yend=yminmax[2]), linetype=1, color="blue",size=1) +
    ggtitle(paste(yname,"vs",sigvar)) +
    xlab(sigvar) + ylab(paste(yname,"Residuals")) +
    theme(text =element_text(face="bold", color="black",family="Arial"),
          axis.text.x = element_text(face="bold", color="black"),
          axis.text.y = element_text(face="bold", color="black"),
          axis.ticks.x = element_blank(),
          legend.position = "none")
  ggsave(paste0(sigvar,"_",ysplit[1],"_",ysplit[2],".png"), width = 4, height = 4)
}

#Uploads reformated MIST data and properly formats variables
#setwd('/nas/longleaf/home/rcorr/neuroanalytics/neuroanalytics/FinalProject')
#df <- read_excel("MISTReformatedData.xlsx")
df  <- read_excel("MISTReformatedDataRandom.xlsx")
df$AgeInt <- factor(df$AgeInt, levels = c(9,10,11,12,13,14,15,16))
df$Sex <- factor(df$Sex,levels = c(0,1),labels = c("Male", "Female"))
df$CortResponder <- factor(df$S2S6ResidResp,levels = c(0,1),labels = c("NonResp", "Resp"))

#ROIs using and times of each saliva collection point (for graph's X axis to be sized proportionally)
ROIs <- c('vmPFC_L', 'VS_L', 'VS_R', 'PCC_L', 'OFC_L', 'Putamen_R', 'Hippocampus_L', 'ACC_R', 'AntInsula_R', 'dlPFC_R')
SalivaTimes <- c(0,23.5,41.5,58,73.5,93.5)

#Stacked bar plot of sex and age distribution
ggplot(df, aes(AgeInt)) +
  geom_bar(aes(fill = Sex)) +
  scale_fill_manual(values=c("#6288e7","#ea6c5f")) +
  theme_classic(base_size = 15) +
  scale_y_continuous(expand = c(0, 0)) +
  ggtitle("Demographics") +
  xlab("Age") + ylab("Count") +
  theme(text =element_text(face="bold", color="black",family="Arial"),
        axis.text.x = element_text(face="bold", color="black"),
        axis.text.y = element_text(face="bold", color="black"),
        axis.ticks.x = element_blank())
ggsave("SexAge.png", width = 7, height = 5)


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

#Creates line graph of HRV color-coded for each specific time point (using standard error)
ggplot(data=HRVtable,aes(x=HRVCondition,y=HRVmeans,group=1)) +
  geom_line(size=1.2) +
  geom_segment(aes(x="RS1", y=0,xend="MIST1",yend=HRVMIST1y), linetype="dashed", color="white",size=1.2) +
  geom_errorbar(aes(ymin=HRVmeans-HRVstderr, ymax=HRVmeans+HRVstderr), width=.3,
                position=position_dodge(0.05),size=.75) +
  geom_point(aes(color=HRVColor), size=3.5) +
  scale_color_manual(values=HRVColor) +
  theme_classic(base_size = 15) +
  #ggtitle("High Frequency Heart Rate Variability") +
  #xlab("Condition") + 
  ylab("\u0394 HF-HRV") +
  scale_x_discrete(position = 'top') +
  theme(text =element_text(face="bold", color="black",family="Arial"),
        axis.text.x = element_text(face="bold", color="black"),
        axis.text.y = element_text(face="bold", color="black"),
        axis.ticks.x = element_blank(),
        legend.position = "none")
ggsave("HRVstderr.png", width = 5, height = 4)

#Creates line graph of HR color-coded for each specific time point (using standard error)
ggplot(data=HRVtable,aes(x=HRVCondition,y=HRmeans,group=1)) +
  geom_line(size=1.2) +
  geom_segment(aes(x="RS1", y=HRRS1y,xend="MIST1",yend=HRMIST1y), linetype="dashed", color="white", size=1.2) +
  geom_errorbar(aes(ymin=HRmeans-HRstderr, ymax=HRmeans+HRstderr), width=.3,
                position=position_dodge(0.05),size=.75) +
  geom_point(aes(color=HRVColor), size=3.5) +
  scale_color_manual(values=HRVColor) +
  theme_classic(base_size = 15) +
  ggtitle("Heart Rate") +
  xlab("Condition") + ylab("Heart Rate") +
  theme(text =element_text(face="bold", color="black",family="Arial"),
        axis.text.x = element_text(face="bold", color="black"),
        axis.text.y = element_text(face="bold", color="black"),
        axis.ticks.x = element_blank(),
        legend.position = "none")
ggsave("HRstderr.png", width = 10, height = 5)

#Organizes Self-Reported Stress Ratings in a table for graphing
RatingCondition = c('S1Rating','S2Rating','S3Rating','S4Rating','S5Rating','S6Rating')
TimePoints = c('S1','S2','S3','S4','S5','S6')
ConditionOrder = c(1,2,3,4,5,6)

RatingStats <- getgraphstats(df,RatingCondition)

Ratingmeans = RatingStats$ConditionMeans
Ratingstddev = RatingStats$ConditionStddev
Ratingstderr = RatingStats$ConditionStderr

RatingColor = c("#e68cba","#fbdd00","#4e71c8","#f6f1a6","#c698ed","#c9caca")
Ratingtable <- data.frame(TimePoints, SalivaTimes, RatingCondition, Ratingmeans, Ratingstddev, Ratingstderr, RatingColor, ConditionOrder)
Ratingtable$TimePoints <- factor(Ratingtable$TimePoints, levels = Ratingtable$TimePoints[order(Ratingtable$ConditionOrder)])
Ratingtable$RatingCondition <- factor(Ratingtable$RatingCondition, levels = Ratingtable$RatingCondition[order(Ratingtable$ConditionOrder)])
Ratingtable$RatingColor <- factor(Ratingtable$RatingColor, levels = Ratingtable$RatingColor[order(Ratingtable$ConditionOrder)])

#Creates line graph of Self-Reported Stress Ratings color-coded for each specific time point (using standard error)
ggplot(data=Ratingtable,aes(x=SalivaTimes,y=Ratingmeans,group=1)) +
  geom_line(size=1.5) +
  geom_errorbar(aes(ymin=Ratingmeans-Ratingstderr, ymax=Ratingmeans+Ratingstderr), width=3,
                position=position_dodge(0.05),size=.75) +
  geom_point(aes(color=RatingColor), size=3.5) +
  scale_color_manual(values=RatingColor) +
  theme_classic(base_size = 15) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(3,15,1)) +
#  scale_y_continuous(expand = c(0, 0)) +
  ggtitle("Self-Reported Stress Ratings") +
  xlab("Time Point") + ylab("Stress Rating") +
  theme(text =element_text(face="bold", color="black",family="Arial"),
        axis.text.x = element_text(face="bold", color="black"),
        axis.text.y = element_text(face="bold", color="black"),
        axis.ticks.x = element_blank(),
        legend.position = "none")
ggsave("StressRatings.png", width = 8, height = 4)


#Organizes residual cortisol variables in a table for the cortisol graphs (ploted as change from S2)
CortCondition = c('S1ResidFromS2','S2ResidFromS2','S3ResidFromS2','S4ResidFromS2','S5ResidFromS2','S6ResidFromS2')
TimePoints = c('S1','S2','S3','S4','S5','S6')
ConditionOrder = c(1,2,3,4,5,6)

CortNonRespStats <- getgraphstats((df[which(df$CortResponder=="NonResp"), ]),CortCondition)
CortNonRespmeans = CortNonRespStats$ConditionMeans
CortNonRespstddev = CortNonRespStats$ConditionStddev
CortNonRespstderr = CortNonRespStats$ConditionStderr

CortRespStats <- getgraphstats((df[which(df$CortResponder=="Resp"), ]),CortCondition)
CortRespmeans = CortRespStats$ConditionMeans
CortRespstddev = CortRespStats$ConditionStddev
CortRespstderr = CortRespStats$ConditionStderr

CortColor = c("#91e1d6","#04b050")
Corttable <- data.frame(TimePoints, SalivaTimes, CortCondition, CortNonRespmeans, CortNonRespstddev, CortNonRespstderr, CortRespmeans, CortRespstddev, CortRespstderr, CortColor, ConditionOrder)
Corttable$TimePoints <- factor(Corttable$TimePoints, levels = Corttable$TimePoints[order(Corttable$ConditionOrder)])
Corttable$CortCondition <- factor(Corttable$CortCondition, levels = Corttable$CortCondition[order(Corttable$ConditionOrder)])

#Creates line graph of cortisol residuals split into responder/non-responder for each specific time point (using standard error)
ggplot(data=Corttable) +
  geom_errorbar(aes(x=SalivaTimes,y=CortNonRespmeans,ymin=CortNonRespmeans-CortNonRespstderr, ymax=CortNonRespmeans+CortNonRespstderr), width=4, position=position_dodge(0.05), color=CortColor[1],size=.75) +
  geom_errorbar(aes(x=SalivaTimes,y=CortRespmeans,ymin=CortRespmeans-CortRespstderr, ymax=CortRespmeans+CortRespstderr), width=4, position=position_dodge(0.05), color=CortColor[2], size=.75) +
  
  geom_line(aes(x=SalivaTimes,y=CortNonRespmeans,group=1),color=CortColor[1],size=1.5) +
  geom_line(aes(x=SalivaTimes,y=CortRespmeans,group=1),color=CortColor[2],size=1.5) +
  
  geom_point(aes(x=SalivaTimes,y=CortNonRespmeans,color=CortColor[1]), size=3.5) +
  geom_point(aes(x=SalivaTimes,y=CortRespmeans,color=CortColor[2]), size=3.5) +
  
  scale_color_manual(values=c("#04b050","#91e1d6")) +
  theme_classic(base_size = 15) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(breaks=seq(-.4,.8,.4)) +
  ggtitle("Salivary Cortisol (u/dL)") +
  xlab("Time Point") + ylab("Salivary Cortisol (u/dL)") +
  theme(text =element_text(face="bold", color="black",family="Arial"),
        axis.text.x = element_text(face="bold", color="black"),
        axis.text.y = element_text(face="bold", color="black"),
        axis.ticks.x = element_blank(),
        legend.position = "none")
ggsave("CortLine.png", width = 8, height = 4)

#Reads the output data from MISTStats.R for the 69 subjects we're using
df69 = read_excel("MISTReformatedDataStatsRandom.xlsx")

#Plots bar graphs of dlPFC activation for cortisol responders and non-responders (no major outliers)
df_dlPFC_R = df69[which(df69$ModelUsing_dlPFC_R>0), ]
ggplot(data=df_dlPFC_R,aes(x=CortResponder,y=dlPFC_R)) +
  geom_hline(yintercept=0,color = "gray") +
  stat_boxplot(geom = "errorbar", width = 0.2, color=c("#5cb7ac","#04b050")) +
  geom_boxplot(color=c("#5cb7ac","#04b050"),fill=c("#91e1d6","#76c89c")) +
  scale_x_discrete(labels =c("Non-Responders","Responders")) + 
  theme_classic(base_size = 15) +
  ggtitle("Cortisol Responders dlPFC (R)") +
  xlab("Cortisol Responder Status") + ylab("dlPFC (R) Mean Beta") +
  theme(text =element_text(face="bold", color="black",family="Arial"),
        axis.text.x = element_text(face="bold", color="black"),
        axis.text.y = element_text(face="bold", color="black"),
        axis.ticks.x = element_blank(),
        axis.title.x=element_blank(),
        legend.position = "none")
ggsave("CortdlPFC.png", width = 4, height = 4)

#Plots bar graphs of Putamen activation for males and females (no major outliers)
df_Putamen_R = df69[which(df69$ModelUsing_Putamen_R>0), ]
ggplot(data=df69,aes(x=Sex,y=Putamen_R)) +
  geom_hline(yintercept=0,color = "gray") +
  stat_boxplot(geom = "errorbar", width = 0.2, color=c("#335bf3","#ea2628")) +
  geom_boxplot(color=c("#335bf3","#ea2628"),fill=c("#6288e7","#ea6c5f")) +
  scale_x_discrete(labels =c("Male","Female")) + 
  theme_classic(base_size = 15) +
  ggtitle("Sex vs Putamen (R)") +
  xlab("Sex") + ylab("Putamen (R) Mean Beta") +
  theme(text =element_text(face="bold", color="black",family="Arial"),
        axis.text.x = element_text(face="bold", color="black"),
        axis.text.y = element_text(face="bold", color="black"),
        axis.ticks.x = element_blank(),
        legend.position = "none")
ggsave("SexPutamen.png", width = 4, height = 4)

#Creates component+residual plots for significant relationships as indicated by MISTStats.R
ContinousSigPairs = c('vmPFC_L'='HRVDifMIST','Hippocampus_L'='STAIT', 'Putamen_R'='STAIT', 'VS_R'='Age') 
for (yvar in names(ContinousSigPairs)) {
  modelusing = paste0('ModelUsing_',yvar) 
  crPlot_MarkOutliers(df69,"Sex+Age+HRVDifMIST+CortResponder+STAIT",yvar,ContinousSigPairs[yvar],modelusing)
}

#Create person graphs for showing % of psychiatric disorders for presentation
png(file="people50.png",width=1400, height=300)
personograph(list(first=0.5, second=0.5), colors=list(first="#63AC4B", second="#BFBFBF"),n.icons=10, dimensions=c(1,10),draw.legend = FALSE,)
dev.off()
png(file="people80_need75.png",width=1400, height=300)
personograph(list(first=0.5, second=0.25,third=0.25), colors=list(first="#63AC4B", second="#99E073",third="#BFBFBF"),n.icons=10, dimensions=c(1,10),draw.legend = FALSE,)
dev.off()
