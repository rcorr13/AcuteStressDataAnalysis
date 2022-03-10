options(stringsAsFactors=FALSE);

library(readxl)
library(xlsx)

# List of all ROIs for each of the 4 copes 
# Cope 1 = control condition
# Cope 2 = experimental condition
# Cope 3 = control > experimental condition
# Cope 4 = experimental > control condition
allROIs = c('Cope1_vmPFC_L', 'Cope2_vmPFC_L', 'Cope3_vmPFC_L', 'Cope4_vmPFC_L', 'Cope1_VS_L', 'Cope2_VS_L', 'Cope3_VS_L', 'Cope4_VS_L', 'Cope1_VS_R', 'Cope2_VS_R', 'Cope3_VS_R', 'Cope4_VS_R', 'Cope1_PCC_L', 'Cope2_PCC_L', 'Cope3_PCC_L', 'Cope4_PCC_L', 'Cope1_Precentral_R', 'Cope2_Precentral_R', 'Cope3_Precentral_R', 'Cope4_Precentral_R', 'Cope1_OFC_L', 'Cope2_OFC_L', 'Cope3_OFC_L', 'Cope4_OFC_L', 'Cope1_Postcentral_R', 'Cope2_Postcentral_R', 'Cope3_Postcentral_R', 'Cope4_Postcentral_R', 'Cope1_Putamen_R', 'Cope2_Putamen_R', 'Cope3_Putamen_R', 'Cope4_Putamen_R', 'Cope1_Hippocampus_L', 'Cope2_Hippocampus_L', 'Cope3_Hippocampus_L', 'Cope4_Hippocampus_L', 'Cope1_STG_R', 'Cope2_STG_R', 'Cope3_STG_R', 'Cope4_STG_R', 'Cope1_STG_L', 'Cope2_STG_L', 'Cope3_STG_L', 'Cope4_STG_L', 'Cope1_Precuneus_R', 'Cope2_Precuneus_R', 'Cope3_Precuneus_R', 'Cope4_Precuneus_R', 'Cope1_SFG_R', 'Cope2_SFG_R', 'Cope3_SFG_R', 'Cope4_SFG_R', 'Cope1_Precentral2_R', 'Cope2_Precentral2_R', 'Cope3_Precentral2_R', 'Cope4_Precentral2_R', 'Cope1_Fusiform_R', 'Cope2_Fusiform_R', 'Cope3_Fusiform_R', 'Cope4_Fusiform_R', 'Cope1_PAG_R', 'Cope2_PAG_R', 'Cope3_PAG_R', 'Cope4_PAG_R', 'Cope1_ACC_R', 'Cope2_ACC_R', 'Cope3_ACC_R', 'Cope4_ACC_R', 'Cope1_AntInsula_R', 'Cope2_AntInsula_R', 'Cope3_AntInsula_R', 'Cope4_AntInsula_R', 'Cope1_IPS_L', 'Cope2_IPS_L', 'Cope3_IPS_L', 'Cope4_IPS_L', 'Cope1_IPS_R', 'Cope2_IPS_R', 'Cope3_IPS_R', 'Cope4_IPS_R', 'Cope1_vlPFC_R', 'Cope2_vlPFC_R', 'Cope3_vlPFC_R', 'Cope4_vlPFC_R', 'Cope1_SPL_L', 'Cope2_SPL_L', 'Cope3_SPL_L', 'Cope4_SPL_L', 'Cope1_dlPFC_R', 'Cope2_dlPFC_R', 'Cope3_dlPFC_R', 'Cope4_dlPFC_R', 'Cope1_MOG_L', 'Cope2_MOG_L', 'Cope3_MOG_L', 'Cope4_MOG_L', 'Cope1_Precuneus_L', 'Cope2_Precuneus_L', 'Cope3_Precuneus_L', 'Cope4_Precuneus_L', 'Cope1_Precentral_L', 'Cope2_Precentral_L', 'Cope3_Precentral_L', 'Cope4_Precentral_L', 'Cope1_dlPFC_L', 'Cope2_dlPFC_L', 'Cope3_dlPFC_L', 'Cope4_dlPFC_L')

#List of subjects being used in the analysis
subjectusing = c('cnt_101_bl','cnt_104_bl','cnt_107_bl','cnt_108_bl','cnt_109_bl','cnt_110_bl','cnt_111_bl','cnt_112_bl','cnt_113_bl','cnt_114_bl','cnt_115_bl','cnt_117_bl','cnt_118_bl','cnt_121_bl','cnt_122_bl','cnt_123_bl','cnt_124_bl','cnt_125_bl','cnt_126_bl','cnt_128_bl','cnt_129_bl','cnt_130_bl','cnt_131_bl','cnt_132_bl','cnt_136_bl','cnt_137_bl','cnt_139_bl','cnt_140_bl','cnt_143_bl','cnt_144_bl','cnt_148_bl','cnt_150_bl','cnt_152_bl','cnt_155_bl','cnt_157_bl','cnt_161_bl','cnt_162_bl','cnt_163_bl','cnt_166_bl','cnt_168_bl','cnt_169_bl','cnt_170_bl','cnt_171_bl','cnt_172_bl','cnt_175_bl','cnt_180_bl','cnt_182_bl','cnt_183_bl','cnt_187_bl','cnt_188_bl','cnt_191_bl','cnt_193_bl','cnt_197_bl','cnt_198_bl','cnt_199_bl','cnt_201_bl','cnt_202_bl','cnt_204_bl','cnt_205_bl','cnt_206_bl','cnt_207_bl','cnt_208_bl','cnt_210_bl','cnt_211_bl','cnt_212_bl','cnt_214_bl','cnt_215_bl','cnt_216_bl','cnt_217_bl','cnt_219_bl','cnt_225_bl','cnt_226_bl','cnt_227_bl','cnt_231_bl','cnt_234_bl','cnt_235_bl','cnt_236_bl','cnt_237_bl','cnt_239_bl','cnt_241_bl','cnt_244_bl','cnt_246_bl','cnt_247_bl','cnt_248_bl','cnt_250_bl','cnt_251_bl','cnt_252_bl','cnt_253_bl','cnt_254_bl','cnt_255_bl','cnt_256_bl','cnt_257_bl','cnt_261_bl','cnt_262_bl','cnt_263_bl','cnt_266_bl','cnt_267_bl','cnt_268_bl','cnt_269_bl','cnt_274_bl','cnt_275_bl')

#setwd('/nas/longleaf/home/rcorr/neuroanalytics/neuroanalytics/FinalProject')

#Uploads collected psych/demographic/heart rate/cortisol data from redcap
redcap <- read_excel("2ndLevelRedCap.xlsx",sheet="Using")
redcapheader = names(redcap)

#Uploads original MIST data file and adjusts strings to make more readable
voxeltable = read.csv('cognit_mist_02072020_beta_functional_featquery_results.txt',sep=' ')
df = voxeltable[is.element(voxeltable$Subject, subjectusing),]
df$Cope <- gsub('.feat', '', df$Cope)
df$Cope <- gsub('cope', 'Cope', df$Cope)
df$ROI <- gsub('_S05', '', df$ROI)
df$CopeRegion = paste0(df$Cope,"_",df$ROI)

#Creates final dataframe to fill
columns=c(redcapheader[2:length(redcapheader)],allROIs)
finaldf = data.frame(matrix(ncol=length(columns),nrow=length(subjectusing), dimnames=list(subjectusing, columns)))

#For each subject, updates their redcap variables and neural mean beta 
#activity in the final dataframe
for (subject in subjectusing) {
  subjdf = df[which(df$Subject==subject),]
  subjredcap = redcap[which(redcap$Subject==subject),]
  subjrow = c(subjredcap,matrix(NA,nrow=length(allROIs),ncol=1))
  for (variable in redcapheader[2:length(redcapheader)]){
    finaldf[subject,variable]=subjredcap[variable]
  }
  for (copeoption in allROIs){
    finaldf[subject,copeoption]=subjdf[which(subjdf$CopeRegion == copeoption),]$Mean
  }
}

#Saves the final dataframe
finaldf <- cbind(Subject = rownames(finaldf), finaldf)
write.xlsx(finaldf, "MISTReformatedData.xlsx", col.names = TRUE, row.names = FALSE)