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

# residuals plot comparing sigvar and yvar according to model (an LM model)
crPlot_All <-function(df, model, yvar, sigvar, yname, xname, color="blue") {
  x1 <- df[sigvar]
  x1beta = x1*model$coefficients[sigvar]
  x1beta_resid = x1beta + model$residuals
  df_x1 <- as.data.frame((c(x1,x1beta_resid)),col.names=c('x1_col','x1beta_resid_col'))
  
  graphmodel = lm(formula = x1beta_resid_col ~ x1_col, data = df_x1)  
  
  xminmax = data.frame(x1_col=c((min(x1)-.3),(max(x1)+.3)))
  yminmax = predict(graphmodel,newdata=xminmax)
  
  graphplot = ggplot(data=df_x1,aes(x=x1_col, y=x1beta_resid_col)) +
    theme_classic(base_size = 10)  +
    geom_point(alpha=.7,size=1.5) +
    geom_segment(aes(x=xminmax[1,1], y=yminmax[1],xend=xminmax[2,1],yend=yminmax[2]), linetype=1, color=color,size=1) +
    #ggtitle(paste(yname,"vs",sigvar)) +
    xlab(xname) + ylab(yname) +
    theme(text =element_text(face="bold", color="black",family="Arial"),
          axis.text.x = element_text(face="bold", color="black"),
          axis.text.y = element_text(face="bold", color="black"),
          axis.ticks.x = element_blank(),
          legend.position = "none",
          plot.margin = margin(.5,.5,.5,.5, "cm"))
  if (xname == "Polyvictimization") {
    graphplot = graphplot + scale_x_continuous(breaks=seq(0,5,1))
  }
  
  return(graphplot)
  #ggsave(paste0(sigvar,"_",yvar[1],"_LM.png"), width = 5, height = 4)
}


# loads data and drops NAs
setwd('/Users/rcorr/Documents/Belger/Analysis/CodeToUpload/AcuteStressDataAnalysis/PrivateData/Chapter4')
df <- read_excel("Resub_Results_Using.xlsx", sheet="PV_Extract")
df_graph = drop_na(df)


# linear model controlling for sex, age, medication use, and diagnosis
model1 = lm(PCC_Insula~skewPV+Sex+Age+Medication+SCID, df_graph)

# graphs polyvictimization vs PCC to insula connectivity
graph_PCC = crPlot_All(df_graph, model1, 'PCC_Insula', 'skewPV', 'PCC → Insula (L) Connectivity', 'Polyvictimization', color='#7030A0')

#reformats and saves PCC graph
figure3_skew <- ggarrange(graph_PCC,
                     font.label = list(size = 11, color = "black", face = "bold", family="Arial"),
                     ncol = 1, nrow = 1,  align = "hv",
                     #labels="AUTO",
                     #common.legend = TRUE, legend = "bottom",
                     widths = c(1.2), heights = c(1.2))
#ggsave("Figure3_Graph.eps", device=cairo_ps, units="mm", width=100, height=200, dpi=7480)
ggsave("Figure3_Graph_Skew.png", units="mm", width=110, height=100)

