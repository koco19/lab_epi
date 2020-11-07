rm(list=ls(all=TRUE))

# specify location to read files
setwd("C:/COVID Sentinel Kohorte Munich/KoCo19 Data/Paper 2")
library(rmarkdown)
library(readxl)
library(magrittr)
library(summarytools)
library(formattable)
library(ggplot2)
library(zoo)
library(FSA)
library(reshape2)
library(dplyr)
library(lubridate)
library(ggpubr)
library(arsenal)
library(stringi)
library(stringr)
library(tidyr)
library(foreign)
library(stats4)
library(glmmTMB)
library(bbmle)
library(countreg)
library(cowplot) 
library(lme4) 
library(sjPlot) 
library(sjmisc) 
library(effects)
library(sjstats) 
library(tidyquant)
library(binom)
library(geepack)
library(gee)
library(BAS)
library(BMS)
library(DTComPair)
library(irr)
library(ggpmisc)
library(RColorBrewer)
library(readr)
library(tidyr)
library(ggplot2)
library(Hmisc)
library(PupillometryR)
library(scales)
ggplot2::theme_set(ggplot2::theme_bw())


dat1<-read.csv("True_positives.csv", header = T)
dat<- read.csv("Final_Lab_Single_20200904.csv", header = T)
dat$X<-NULL
names(dat)<-c("ind_id"              ,       "PCR_TestDate"      ,        "PCR_Result"        ,       "model_outcome"        ,      "Blood_ID"    ,               
               "BloodSample_Date" ,"cohort"          ,     "IgA_1_Date"     ,   "IgA_2_Date"    ,    "IgA_3_Date"  ,     
               "IgG_1_Date"    ,    "IgG_2_Date"   ,     "IgG_3_Date"  ,      "IgA_1_quant"     ,    "IgA_2_quant"  ,      
               "IgA_3_quant"      ,   "IgG_1_quant"     ,    "IgG_2_quant"     ,    "IgG_3_quant"     ,    "IgA_1_result" ,
               "IgA_2_result" , "IgA_3_result" , "IgG_1_result" , "IgG_2_result" , "IgG_3_result" ,
               "machine_1"         ,         "machine_2"        ,          "Roche_quant11"       ,       "Roche_quant21"       ,       "Roche_quant31"    ,         
               "Roche_quant41"        ,      "Roche_quant12"         ,     "Roche_quant22"        ,      "Roche_quant32"         ,     "Roche_quant42" ,            
               "Roche_result11" ,  "Roche_result21"  , "Roche_result31" ,  "Roche_result41"  , "Roche_result12"  ,
               "Roche_result22" ,  "Roche_result32" ,  "Roche_result42" ,  "Roche_date11"     ,   "Roche_date21"  ,     
               "Roche_date31"    ,    "Roche_date41"    ,    "Roche_date12"    ,    "Roche_date22"    ,    "Roche_date32"  ,     
               "Roche_date42"    ,    "cPass"           ,           "NT"          ,               "VC_S1_IgA"        ,          "VC_S2_IgA"     ,            
               "VC_N_IgA"             ,      "VC_S1_IgM"          ,        "VC_S2_IgM"         ,         "VC_N_IgM"          ,         "VC_S1_IgG"    ,             
               "VC_S2_IgG"        ,          "VC_N_IgG"           ,        "LineBlot_NP_SARS_2"   ,      "LineBlot_RBD_SARS_2"    ,    "LineBlot_S1_SARS_2"    ,    
               "Schnupfen_NP_229E"    ,      "Schnupfen_NP_NL63"     ,     "Schnupfen_NP_OC43"     ,     "Schnupfen_NP_HKU1"     ,     "NT_result"      ,           
               "cPass_result"          ,     "VC_S1_IgA_result1"     ,     "VC_S1_IgA_result2"      ,    "VC_S2_IgA_result1"    ,      "VC_S2_IgA_result2"   ,      
               "VC_N_IgA_result1"     ,      "VC_N_IgA_result2"     ,      "VC_S1_IgM_result1"     ,     "VC_S1_IgM_result2"    ,      "VC_S2_IgM_result1"   ,      
               "VC_S2_IgM_result2"     ,     "VC_N_IgM_result1"      ,     "VC_N_IgM_result2"       ,    "VC_S1_IgG_result1"     ,     "VC_S1_IgG_result2"    ,     
               "VC_S2_IgG_result1"      ,    "VC_S2_IgG_result2"     ,     "VC_N_IgG_result1"       ,    "VC_N_IgG_result2"          )


#names<- c("ind_id","PCR_TestDate",
#          "PCR_Result_Date",
#          "PCR_Result","model_outcome" ,"Blood_id","BloodSample_Date", "study_perID"
#                  "IgA_1_Date","IgA_2_Date","IgA_3_Date",
#                  "IgG_1_Date","IgG_2_Date","IgG_3_Date",
#                  "IgA_1_quant","IgA_2_quant","IgA_3_quant",
#                  "IgG_1_quant","IgG_2_quant","IgG_3_quant",
#                  "IgA_1_result","IgA_2_result","IgA_3_result",
#                  "IgG_1_result","IgG_2_result","IgG_3_result",
#                  "machine_1" , "machine_2",
#                  "Roche_quant11","Roche_quant21","Roche_quant31","Roche_quant41",
#                  "Roche_quant12","Roche_quant22","Roche_quant32","Roche_quant42",
#                  "Roche_result11","Roche_result21","Roche_result31","Roche_result41",
#                  "Roche_result12","Roche_result22","Roche_result32","Roche_result42",
#                  "Roche_date11","Roche_date21","Roche_date31","Roche_date41",
#                 "Roche_date12","Roche_date22","Roche_date32","Roche_date42",
#                  "cPass",                      "NT",                        
#                  "VC_S1_IgA",                  "VC_S2_IgA",                  "VC_N_IgA",                   "VC_S1_IgM",                 
#                  "VC_S2_IgM",                  "VC_N_IgM",                   "VC_S1_IgG",                  "VC_S2_IgG",                 
#                  "VC_N_IgG",                   "NT_result",                  "cPass_result",               "VC_S1_IgA_result1",         
#                  "VC_S1_IgA_result2",          "VC_S2_IgA_result1",          "VC_S2_IgA_result2",          "VC_N_IgA_result1",          
#                  "VC_N_IgA_result2",           "VC_S1_IgM_result1",          "VC_S1_IgM_result2",          "VC_S2_IgM_result1",         
#                  "VC_S2_IgM_result2",          "VC_N_IgM_result1",           "VC_N_IgM_result2",           "VC_S1_IgG_result1",         
#                  "VC_S1_IgG_result2",          "VC_S2_IgG_result1",          "VC_S2_IgG_result2",          "VC_N_IgG_result1",          
#                  "VC_N_IgG_result2",           "cohort",                     "Sympt_begin",                "severity_sympt")



dat$study<-dat$cohort

# Calculate time difference between blood draw and PCR testing

date1<-strptime(dat$BloodSample_Date,format="%Y-%m-%d")
date2<-strptime(dat$PCR_TestDate,format="%Y-%m-%d")
dat$timeelapsed<- ceiling(difftime(date1,date2,units="days"))




# Subset data with PCR positives and having a valid time elapsed duration


dat_ana<- dat[which(dat$PCR_Result==1 & !is.na(dat$timeelapsed)),]

dat_ana$timeelapsed<-as.numeric(dat_ana$timeelapsed)

# Make the ROCHE zero's to NA
dat_ana[, 28:35][dat_ana[, 28:35] == 0] <- NA
summary(dat_ana$Roche_quant22)

# for Euro Immune and Roche we select the latest data measured

l<- length(dat_ana$ind_id)
IgA_quant<- rep(0,times=l)
IgG_quant<- rep(0, times=l)
Roche_quant<- rep(0, times=l)

for( i in 1:l)
{ x1<-dat_ana[i,c(14,15,16)]
  x2<-dat_ana[i,c(17,18,19)]
  x3<-dat_ana[i,c(28:35)]
  
  y1<- x1[which(!is.na(x1))]
  y2<- x2[which(!is.na(x2))]
  y3<- x3[which(!is.na(x3))]
  
  s1<- length(y1)
  s2<- length(y2)
  s3<- length(y3)
  
  IgA_quant[i]<- unlist(y1[s1])
  IgG_quant[i]<- unlist(y2[s2])
  Roche_quant[i]<-unlist(y3[s3])
  
}


dat_ana<- cbind(dat_ana,IgA_quant,IgG_quant,Roche_quant)

# select important data for analysis

dat_train<- subset(dat_ana, select = c(ind_id,timeelapsed,IgA_quant,IgG_quant,Roche_quant, cohort))

dat_train_long<- reshape2::melt(dat_train, id=c("ind_id","timeelapsed","cohort"))

colnames(dat_train_long)<-c("ind_id","timeelapsed","cohort","Lab_measurement","Lab_value")


dat_train$TimeInterval<- ifelse(dat_train$timeelapsed<=30,"Up to 30 days",
                                ifelse(dat_train$timeelapsed>30 & dat_train$timeelapsed<90,"Between 30-90 days",
                                       "After 90 days"))
dat_train$TimeInterval<- factor(dat_train$TimeInterval,
                                levels = c("Up to 30 days","Between 30-90 days","After 90 days"))
dat_train_long$TimeInterval<- ifelse(dat_train_long$timeelapsed<=30,"Up to 30 days",
                                     ifelse(dat_train_long$timeelapsed>30 & dat_train_long$timeelapsed<90,"Between 30-90 days",
                                            "After 90 days"))
dat_train_long$TimeInterval<- factor(dat_train_long$TimeInterval,
                                levels = c("Up to 30 days","Between 30-90 days","After 90 days"))


# Box plot

##################################################################################################
###################################################################################################
dat_train_colour<-dat_train


stat_box_data_Roche <- function(y, upper_limit = max(log10(dat_train$Roche_quant)) * 2.85) {
  
  return(
    
    data.frame(
      
      y = 1 * upper_limit,
      
      label = paste(
        
        'median =', round(median(y), 2), '\n',
        
        'mean =',round(mean(y), 2))
      
    )
    
  )
  
}

stat_box_data_IgG <- function(y, upper_limit = max(log10(dat_train$IgG_quant)) * 3.4) {
  
  return(
    
    data.frame(
      
      y = 1 * upper_limit,
      
      label = paste(
        
        'median =', round(median(y), 2), '\n',
        
        'mean =',round(mean(y), 2))
      
    )
    
  )
  
}


stat_box_data_IgA <- function(y, upper_limit = max(log(dat_train$IgA_quant)) * 3.5) {
  
  return(
    
    data.frame(
      
      y = 1 * upper_limit,
      
      label = paste(
        
        'median =', round(median(y), 2), '\n',
        
        'mean =',round(mean(y), 2))
      
    )
    
  )
  
}


theme_set(theme_bw())
ggplot2::theme_set(ggplot2::theme_classic())

######

DT = dunnTest(log10(Roche_quant) ~ TimeInterval,
              data=dat_train,
              method="by") 
DT

###

ggplot(dat_train, aes(y=log(Roche_quant),x= TimeInterval)) +
  geom_boxplot()+geom_jitter(position=position_jitter(0.2))+
  stat_compare_means()

ggsave("Rocheboxplot_log.jpeg",dpi=720, height=5, width = 7, units="in") 
dat_train$TimeInterval<- factor(dat_train$TimeInterval)

dat_train$TimeInterval_1<- as.numeric(dat_train$TimeInterval)
var<-FSA::Summarize(log10(dat_train$Roche_quant)~dat_train$TimeInterval)

dat_train$Roche_new<- ifelse(dat_train$Roche_quant>=0.4218,"Positive","Negative")
dat_train$Roche_old<- ifelse(dat_train$Roche_quant>=1,"Positive","Negative")
tab1<- FSA::Summarize(log10(dat_train$Roche_quant)~dat_train$TimeInterval+dat_train$Roche_new)
tab2<- FSA::Summarize(log10(dat_train$Roche_quant)~dat_train$TimeInterval+dat_train$Roche_old)
###

DT_R = dunnTest(log10(Roche_quant) ~ TimeInterval,
                data=dat_train,
                method="by") 
DT_R
x<-round(DT_R$res[[4]],4)

vr<-ggplot(dat_train, aes(y=log10(Roche_quant),x= factor(TimeInterval_1))) +

  geom_flat_violin( position = position_nudge(x = 0, y = 0),alpha = .3,color="#D55E00", fill="#D55E00",
                    adjust =2, trim = F)+
  #geom_point(position = position_nudge(x=-0.025), shape = 21, size = 2)+
  geom_boxplot(width=0.1, alpha=0.2,outlier.shape = NA)+
  #geom_jitter(position=position_jitter(0.25), shape = 21, fill = "#D55E00",
  #             color = "D55E00", size = 1)+
  geom_jitter(aes(x = TimeInterval_1 - 0.1,
                  y = log10(Roche_quant)),
              width = 0.1 - 0.25 * 0.2,
              height = 0,shape = 21, fill = "#D55E00",
              color = "#D55E00", size = 1,alpha=0.5) +
 
  #geom_violin(trim = FALSE, color="darkgrey")+
  #geom_boxplot(width=0.1, alpha=0.2, outlier.shape = NA)+
  stat_summary(fun=mean, geom='point', shape=21, size=2, color="black", fill="black")+
  #geom_jitter(position=position_jitter(0.2), shape = 21, fill = "#D55E00",
  #            color = "#D55E00", size = 1)+
  stat_compare_means(aes(label = sprintf("p = %5.4f", as.numeric(..p.format..))),label.y=2.2*max(log10(Roche_quant),na.rm = T),size=3)+ 
  
  stat_summary(
    
    fun.data = stat_box_data_Roche,
    
    geom = "text",
    
    hjust = 0.5,
    
    vjust = 0.9, size=3
    
  ) +
  scale_x_discrete(labels=c(paste('Up to 30 days\n (n =',var$n[1],')'),paste('Between 30-90 days\n (n =',var$n[2],')'),paste('After 90 days\n (n =',var$n[3],')')))+
  geom_hline(yintercept = 0, linetype=3,colour="grey30")+
  geom_hline(yintercept = log10(0.4218), linetype=2)+
  #geom_vline(xintercept = c(1.5,2.5))+
  annotate("text",x=0.75,y=-2.15,label=paste("n' = ",tab1$n[1]), colour="#D55E00")+
  annotate("text",x=1.75,y=-2.15,label=paste("n' = ",tab1$n[2]), colour="#D55E00")+
  annotate("text",x=2.75,y=-2.15,label=paste("n' = ",tab1$n[3]), colour="#D55E00")+
  annotate("text",x=0.75,y=2.45,label=paste("n' = ",tab1$n[4]), colour="#D55E00")+
  annotate("text",x=1.75,y=2.45,label=paste("n' = ",tab1$n[5]), colour="#D55E00")+
  annotate("text",x=2.75,y=2.45,label=paste("n' = ",tab1$n[6]), colour="#D55E00")+
  
  annotate("text",x=0.75,y=-1.6,label=paste("n = ",tab2$n[1]), colour="#D55E00", alpha=0.5)+
  annotate("text",x=1.75,y=-1.6,label=paste("n = ",tab2$n[2]), colour="#D55E00",alpha=0.5)+
  annotate("text",x=2.75,y=-1.6,label=paste("n = ",tab2$n[3]), colour="#D55E00",alpha=0.5)+
  annotate("text",x=0.75,y=3.1,label=paste("n = ",tab2$n[4]), colour="#D55E00",alpha=0.5)+
  annotate("text",x=1.75,y=3.1,label=paste("n = ",tab2$n[5]), colour="#D55E00",alpha=0.5)+
  annotate("text",x=2.75,y=3.1,label=paste("n = ",tab2$n[6]), colour="#D55E00",alpha=0.5)+
  ylab("Ro-N-Ig (log10 scaled)")+
  xlab(NULL)+
  scale_y_continuous(breaks= pretty_breaks(n=5))+
  annotate("label", x = 3.4, y = 3.95, label = paste("n = ",tab2$n[4]+tab2$n[5]+tab2$n[6]),
           colour="grey30",fill=NA) +
  annotate("label", x = 3.4, y = -0.9, label = paste("n = ",tab2$n[1]+tab2$n[2]+tab2$n[3]),
           colour="grey30",fill=NA) +
  annotate("label", x = 3.4, y = 3, fill=NA,label = paste("n' = ",tab1$n[4]+tab1$n[5]+tab1$n[6])) +
  annotate("label", x = 3.4, y = -1.95, fill=NA,label = paste("n' = ",tab1$n[1]+tab1$n[2]+tab1$n[3]))+
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.01, color="#CCCCCC"))+
  geom_segment(aes(x = 1, y = 3.9, xend = 1.9, yend = 3.9))+
   geom_segment(aes(x = 2, y = 3.9, xend = 2.9, yend = 3.9))+
  geom_segment(aes(x = 1, y = 4.5, xend = 3, yend = 4.5))+
  geom_text(x=1.5, y=4.15, label=paste("p =",format(x[3],scientific=FALSE)),size=3)+
  geom_text(x=2.5, y=4.15, label=paste("p =",format(x[1],scientific=FALSE)),size=3)+
  geom_text(x=2, y=4.75, label=paste("p =",format(x[2],scientific=FALSE)),size=3)
  
#3.75 3.15 -0.85 -1.45
#2 1.4 -0.8 -1.4
vr

ggsave("Roche_violinplot_log.jpeg",dpi=720, height=6, width = 8, units="in") 



###



###

ggplot(dat_train, aes(y=log10(IgG_quant),x= TimeInterval)) +
  geom_boxplot()+geom_jitter(position=position_jitter(0.2))+
  stat_compare_means()

ggsave("IgGboxplot_log.jpeg",dpi=720, height=5, width = 7, units="in") 


var<-FSA::Summarize(log10(dat_train$IgG_quant)~dat_train$TimeInterval)
dat_train$IgG_new<- ifelse(dat_train$IgG_quant>=1.015,"Positive","Negative")
dat_train$IgG_old<- ifelse(dat_train$IgG_quant>=1.1,"Positive","Negative")
tab1<- FSA::Summarize(log10(dat_train$IgG_quant)~dat_train$TimeInterval+dat_train$IgG_new)
tab2<- FSA::Summarize(log10(dat_train$IgG_quant)~dat_train$TimeInterval+dat_train$IgG_old)

DT_G = dunnTest(log10(IgG_quant) ~ TimeInterval,
                data=dat_train,
                method="by") 
DT_G
x<-round(DT_G$res[[4]],4)

vigg<-ggplot(dat_train, aes(y=log10(IgG_quant),x= factor(TimeInterval_1))) +
  
  geom_flat_violin( position = position_nudge(x = 0, y = 0),alpha = .3,color="#D55E00", fill="#D55E00",
                    adjust =2, trim = F)+
  #geom_point(position = position_nudge(x=-0.025), shape = 21, size = 2)+
  geom_boxplot(width=0.1, alpha=0.2,outlier.shape = NA)+
  #geom_jitter(position=position_jitter(0.25), shape = 21, fill = "D55E00",
  #             color = "D55E00", size = 1)+
  geom_jitter(aes(x = TimeInterval_1 - 0.1,
                  y = log10(IgG_quant)),
              width = 0.1 - 0.25 * 0.2,
              height = 0,shape = 21, fill = "#D55E00",
              color = "#D55E00", size = 1,alpha=0.5) +
  #geom_violin(trim = FALSE, color="darkgrey")+
  #geom_boxplot(width=0.1, alpha=0.2, outlier.shape = NA)+
  stat_summary(fun=mean, geom='point', shape=21, size=2, color="black", fill="black")+
  #geom_jitter(position=position_jitter(0.2), shape = 21, fill = "#D55E00",
  #            color = "#D55E00", size = 1)+
  scale_y_continuous(breaks= pretty_breaks(n=5))+
  stat_compare_means(aes(label = sprintf("p = %5.4f", as.numeric(..p.format..))),label.y=2.75*max(log10(IgG_quant),na.rm = T),size=3)+ 
  
  stat_summary(
    
    fun.data = stat_box_data_IgG,
    
    geom = "text",
    
    hjust = 0.5,
    
    vjust = 0.9, size=3
    
  ) +
  scale_x_discrete(labels=c(paste('Up to 30 days\n (n =',var$n[1],')'),paste('Between 30-90 days\n (n =',var$n[2],')'),paste('After 90 days\n (n =',var$n[3],')')))+
  geom_hline(yintercept = log10(1.1), linetype=3,colour="grey30")+
  geom_hline(yintercept = log10(1.015), linetype=2)+
  #geom_vline(xintercept = c(1.5,2.5))+
  annotate("text",x=0.75,y=-1.5,label=paste("n' = ",tab1$n[1]), colour="#D55E00")+
  annotate("text",x=1.75,y=-1.5,label=paste("n' = ",tab1$n[2]), colour="#D55E00")+
  annotate("text",x=2.75,y=-1.5,label=paste("n' = ",tab1$n[3]), colour="#D55E00")+
  annotate("text",x=0.75,y=1.5,label=paste("n' = ",tab1$n[4]), colour="#D55E00")+
  annotate("text",x=1.75,y=1.5,label=paste("n' = ",tab1$n[5]), colour="#D55E00")+
  annotate("text",x=2.75,y=1.5,label=paste("n' = ",tab1$n[6]), colour="#D55E00")+
  
  annotate("text",x=0.75,y=-1.2,label=paste("n = ",tab2$n[1]), colour="#D55E00", alpha=0.5)+
  annotate("text",x=1.75,y=-1.2,label=paste("n = ",tab2$n[2]), colour="#D55E00",alpha=0.5)+
  annotate("text",x=2.75,y=-1.2,label=paste("n = ",tab2$n[3]), colour="#D55E00",alpha=0.5)+
  annotate("text",x=0.75,y=1.8,label=paste("n = ",tab2$n[4]), colour="#D55E00",alpha=0.5)+
  annotate("text",x=1.75,y=1.8,label=paste("n = ",tab2$n[5]), colour="#D55E00",alpha=0.5)+
  annotate("text",x=2.75,y=1.8,label=paste("n = ",tab2$n[6]), colour="#D55E00",alpha=0.5)+
  ylab("EI-S1-IgG (log10 scaled)")+
  xlab(NULL)+
  annotate("label", x = 3.4, y = 2, label = paste("n = ",tab2$n[4]+tab2$n[5]+tab2$n[6]),
           colour="grey30", fill=NA) +
  annotate("label", x = 3.4, y = -0.8, label = paste("n = ",tab2$n[1]+tab2$n[2]+tab2$n[3]),
           colour="grey30",fill=NA) +
  annotate("label", x = 3.4, y = 1.4,fill=NA, label = paste("n' = ",tab1$n[4]+tab1$n[5]+tab1$n[6])) +
  annotate("label", x = 3.4, y = -1.4, fill=NA,label = paste("n' = ",tab1$n[1]+tab1$n[2]+tab1$n[3]))+
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.01, color="#CCCCCC"))+
  geom_segment(aes(x = 1, y = 2.2, xend = 1.9, yend = 2.2))+
  geom_segment(aes(x = 2, y = 2.2, xend = 2.9, yend = 2.2))+
  geom_segment(aes(x = 1, y = 2.8, xend = 3, yend = 2.8))+
  geom_text(x=1.5, y=2.35, label=paste("p =",format(x[3],scientific=FALSE)),size=3)+
  geom_text(x=2.5, y=2.35, label=paste("p =",format(x[1],scientific=FALSE)),size=3)+
  geom_text(x=2, y=3.05, label=paste("p =",format(x[2],scientific=FALSE)),size=3)
#3.75 3.15 -0.85 -1.45
#2 1.4 -0.8 -1.4

vigg
ggsave("IgG_violinplot_log.jpeg",dpi=720, height=6, width = 8, units="in") 

DT_G = dunnTest(log10(IgG_quant) ~ TimeInterval,
                data=dat_train,
                method="by") 
DT_G


###


ggplot(dat_train, aes(y=log(IgA_quant),x= TimeInterval)) +
  geom_boxplot()+geom_jitter(position=position_jitter(0.2))+
  stat_compare_means()

ggsave("IgAboxplot_log.jpeg",dpi=720, height=5, width = 7, units="in") 

var<-FSA::Summarize(log(dat_train$IgA_quant)~dat_train$TimeInterval)
dat_train$IgA_new<- ifelse(dat_train$IgA_quant>=1.085,"Positive","Negative")
dat_train$IgA_old<- ifelse(dat_train$IgA_quant>=1.1,"Positive","Negative")
tab1<- FSA::Summarize(log10(dat_train$IgA_quant)~dat_train$TimeInterval+dat_train$IgA_new)
tab2<- FSA::Summarize(log10(dat_train$IgA_quant)~dat_train$TimeInterval+dat_train$IgA_old)

DT_IgA = dunnTest(log10(IgA_quant) ~ TimeInterval,
                  data=dat_train,
                  method="by") 
DT_IgA
x<-round(DT_IgA$res[[4]],4)

viga<-ggplot(dat_train, aes(y=log10(IgA_quant),x= factor(TimeInterval_1))) +
  geom_flat_violin( position = position_nudge(x = 0, y = 0),alpha = .3,color="#D55E00", fill="#D55E00",
                    adjust =2, trim = F)+
  #geom_point(position = position_nudge(x=-0.025), shape = 21, size = 2)+
  geom_boxplot(width=0.1, alpha=0.2,outlier.shape = NA)+
  #geom_jitter(position=position_jitter(0.25), shape = 21, fill = "#D55E00",
  #             color = "#D55E00", size = 1)+
  geom_jitter(aes(x = TimeInterval_1 - 0.1,
                  y = log10(IgA_quant)),
              width = 0.1 - 0.25 * 0.2,
              height = 0,shape = 21, fill = "#D55E00",
              color = "#D55E00", size = 1,alpha=0.5) +
  
  #geom_violin(trim = FALSE, color="darkgrey")+
  #geom_boxplot(width=0.1, alpha=0.2, outlier.shape = NA)+
  stat_summary(fun=mean, geom='point', shape=21, size=2, color="black", fill="black")+
  #geom_jitter(position=position_jitter(0.2), shape = 21, fill = "#D55E00",
  #            color = "#D55E00", size = 1)+
  stat_compare_means(aes(label = sprintf("p = %5.4f", as.numeric(..p.format..))),label.y=2.8*max(log10(IgA_quant),na.rm = T),size=3)+ 
  
  stat_summary(
    
    fun.data = stat_box_data_IgG,
    
    geom = "text",
    
    hjust = 0.5,
    
    vjust = 0.9, size=3
    
  ) +
  scale_y_continuous(breaks= pretty_breaks(n=5))+
  scale_x_discrete(labels=c(paste('Up to 30 days\n (n =',var$n[1],')'),paste('Between 30-90 days\n (n =',var$n[2],')'),paste('After 90 days\n (n =',var$n[3],')')))+
  geom_hline(yintercept = log10(1.1), linetype=3,colour="grey30")+
  geom_hline(yintercept = log10(1.085), linetype=2)+
  #geom_vline(xintercept = c(1.5,2.5))+
  annotate("text",x=0.75,y=-1.7,label=paste("n' = ",tab1$n[1]), colour="#D55E00")+
  annotate("text",x=1.75,y=-1.7,label=paste("n' = ",tab1$n[2]), colour="#D55E00")+
  annotate("text",x=2.75,y=-1.7,label=paste("n' = ",tab1$n[3]), colour="#D55E00")+
  annotate("text",x=0.75,y=1.5,label=paste("n' = ",tab1$n[4]), colour="#D55E00")+
  annotate("text",x=1.75,y=1.5,label=paste("n' = ",tab1$n[5]), colour="#D55E00")+
  annotate("text",x=2.75,y=1.5,label=paste("n' = ",tab1$n[6]), colour="#D55E00")+
  
  annotate("text",x=0.75,y=-1.4,label=paste("n = ",tab2$n[1]), colour="#D55E00", alpha=0.5)+
  annotate("text",x=1.75,y=-1.4,label=paste("n = ",tab2$n[2]), colour="#D55E00",alpha=0.5)+
  annotate("text",x=2.75,y=-1.4,label=paste("n = ",tab2$n[3]), colour="#D55E00",alpha=0.5)+
  annotate("text",x=0.75,y=1.8,label=paste("n = ",tab2$n[4]), colour="#D55E00",alpha=0.5)+
  annotate("text",x=1.75,y=1.8,label=paste("n = ",tab2$n[5]), colour="#D55E00",alpha=0.5)+
  annotate("text",x=2.75,y=1.8,label=paste("n = ",tab2$n[6]), colour="#D55E00",alpha=0.5)+
  ylab("EI-S1-IgA (log10 scaled)")+
  xlab(NULL)+
  annotate("label", x = 3.4, y = 1.9, label = paste("n = ",tab2$n[4]+tab2$n[5]+tab2$n[6]),
           colour="grey30",fill=NA) +
  annotate("label", x = 3.4, y = -0.9, label = paste("n = ",tab2$n[1]+tab2$n[2]+tab2$n[3]),
           colour="grey30",fill=NA) +
  annotate("label", x = 3.4, y = 1.3, fill=NA,label = paste("n' = ",tab1$n[4]+tab1$n[5]+tab1$n[6])) +
  annotate("label", x = 3.4, y = -1.5, fill=NA,label = paste("n' = ",tab1$n[1]+tab1$n[2]+tab1$n[3]))+
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.01, color="#CCCCCC"))+
  geom_segment(aes(x = 1, y = 2.2, xend = 1.9, yend = 2.2))+
  geom_segment(aes(x = 2, y = 2.2, xend = 2.9, yend = 2.2))+
  geom_segment(aes(x = 1, y = 2.8, xend = 3, yend = 2.8))+
  geom_text(x=1.5, y=2.35, label=paste("p =",format(x[3],scientific=FALSE)),size=3)+
  geom_text(x=2.5, y=2.35, label=paste("p =",format(x[1],scientific=FALSE)),size=3)+
  geom_text(x=2, y=3.05, label=paste("p =",format(x[2],scientific=FALSE)),size=3)
viga
ggsave("IgA_violinplot_log.jpeg",dpi=720, height=6, width = 8, units="in") 

#3.75 3.15 -0.85 -1.45
#2 1.4 -0.8 -1.4
###

DT_IgA = dunnTest(log10(IgA_quant) ~ TimeInterval,
                  data=dat_train,
                  method="by") 
DT_IgA


#

library(patchwork)
(viga  + 
    vigg) / (plot_spacer()+vr + plot_spacer()+plot_layout(nrow=1,widths=c(1,2,1)))+
  plot_annotation(tag_levels = 'A',
                  title = 'Temporal analysis of primary tests for PCR positives') & 
  theme(plot.tag = element_text(size = 14))
library(svglite)
ggsave("antibodyovertime.svg",dpi=72, height=8, width = 13, units="in")
ggsave("antibodyovertime.pdf",dpi=72, height=8, width = 13, units="in")
ggsave("antibodyovertime.jpeg",dpi=72, height=8, width = 13, units="in")
ggsave("antibodyovertime.png", height=8, width = 13, units="in")
# only among positives


###

ggplot(dat_train[which(dat_train$Roche_quant>=1),], aes(y=log(Roche_quant),x= TimeInterval)) +
  geom_boxplot()+geom_jitter(position=position_jitter(0.2))+
  stat_compare_means()

ggsave("Rocheboxplot_logP.jpeg",dpi=720, height=5, width = 7, units="in")  

var<-FSA::Summarize(log(dat_train[which(dat_train$Roche_quant>=1),]$Roche_quant)~dat_train[which(dat_train$Roche_quant>=1),]$TimeInterval)
ggplot(dat_train[which(dat_train$Roche_quant>=1),], aes(y=log(Roche_quant),x= TimeInterval)) +
  geom_violin(trim = FALSE, color="darkgrey")+
  geom_boxplot(width=0.1, alpha=0.2)+
  stat_summary(fun=mean, geom='point', shape=21, size=2, color="darkred", fill="darkred")+
  geom_jitter(position=position_jitter(0.2), shape = 21, fill = "lightgray",
              color = "darkgrey",alpha=0.5, size = 1)+
  stat_compare_means(aes(label = sprintf("p = %5.4f", as.numeric(..p.format..))),label.y=1.65*max(log(Roche_quant)),size=3)+ 
  
  stat_summary(
    
    fun.data = stat_box_data_Roche,
    
    geom = "text",
    
    hjust = 0.5,
    
    vjust = 0.9, size=3
    
  ) +
  scale_x_discrete(labels=c(paste('Up to 30 days\n (n=',var$n[1],')'),paste('Between 30-90 days\n (n=',var$n[2],')'),paste('After 90 days\n (n=',var$n[3],')')))


ggsave("Roche_violinplot_logP.jpeg",dpi=720, height=5, width = 7, units="in") 




###

DT_RP = dunnTest(log(Roche_quant) ~ TimeInterval,
                 data=dat_train[which(dat_train$Roche_quant>=1),],
                 method="bh") 
DT_RP

###



DT_IgGP = dunnTest(IgG_quant ~ TimeInterval,
                   data=dat_train[which(dat_train$IgG_quant>=1.1),],
                   method="bh") 
DT_IgGP

###

ggplot(dat_train[which(dat_train$IgG_quant>=1.1),], aes(y=log(IgG_quant),x= TimeInterval)) +
  geom_boxplot()+geom_jitter(position=position_jitter(0.2))+
  stat_compare_means()

ggsave("IgGboxplot_logP.jpeg",dpi=720, height=5, width = 7, units="in") 

var<-FSA::Summarize(log(dat_train[which(dat_train$IgG_quant>=1.1),]$IgG_quant)~dat_train[which(dat_train$IgG_quant>=1.1),]$TimeInterval)
ggplot(dat_train[which(dat_train$IgG_quant>=1.1),], aes(y=log(IgG_quant),x= TimeInterval)) +
  geom_violin(trim = FALSE, color="darkgrey")+
  geom_boxplot(width=0.1, alpha=0.2)+
  stat_summary(fun=mean, geom='point', shape=21, size=2, color="darkred", fill="darkred")+
  geom_jitter(position=position_jitter(0.2), shape = 21, fill = "lightgray",
              color = "darkgrey",alpha=0.5, size = 1)+
  stat_compare_means(aes(label = sprintf("p = %5.4f", as.numeric(..p.format..))),label.y=1.65*max(log(IgG_quant)),size=3)+ 
  
  stat_summary(
    
    fun.data = stat_box_data_IgG,
    
    geom = "text",
    
    hjust = 0.5,
    
    vjust = 0.9, size=3
    
  ) +
  scale_x_discrete(labels=c(paste('Up to 30 days\n (n=',var$n[1],')'),paste('Between 30-90 days\n (n=',var$n[2],')'),paste('After 90 days\n (n=',var$n[3],')')))


ggsave("IgG_violinplot_logP.jpeg",dpi=720, height=5, width = 7, units="in") 

###

ggplot(dat_train[which(dat_train$IgA_quant>=1.1),], aes(y=log(IgA_quant),x= TimeInterval)) +
  geom_boxplot()+geom_jitter(position=position_jitter(0.2))+
  stat_compare_means()

ggsave("IgAboxplot_logP.jpeg",dpi=720, height=5, width = 7, units="in") 

var<-FSA::Summarize(log(dat_train[which(dat_train$IgA_quant>=1.1),]$IgA_quant)~dat_train[which(dat_train$IgA_quant>=1.1),]$TimeInterval)
ggplot(dat_train[which(dat_train$IgA_quant>=1.1),], aes(y=log(IgA_quant),x= TimeInterval)) +
  geom_violin(trim = FALSE, color="darkgrey")+
  geom_boxplot(width=0.1, alpha=0.2)+
  stat_summary(fun=mean, geom='point', shape=21, size=2, color="darkred", fill="darkred")+
  geom_jitter(position=position_jitter(0.2), shape = 21, fill = "lightgray",
              color = "darkgrey",alpha=0.5, size = 1)+
  stat_compare_means(label.y=1.65*max(log(IgA_quant)),size=3)+ 
  
  stat_summary(
    
    fun.data = stat_box_data_IgA,
    
    geom = "text",
    
    hjust = 0.5,
    
    vjust = 0.9, size=3
    
  ) +
  scale_x_discrete(labels=c(paste('Up to 30 days\n (n=',var$n[1],')'),paste('Between 30-90 days\n (n=',var$n[2],')'),paste('After 90 days\n (n=',var$n[3],')')))


ggsave("IgA_violinplot_logP.jpeg",dpi=720, height=5, width = 7, units="in") 



###

DT_IgAP = dunnTest(IgA_quant ~ TimeInterval,
                   data=dat_train[which(dat_train$IgA_quant>=1.1),],
                   method="bh") 
DT_IgAP



# Blood Donors
d<- dat[c(1,4,5,6,7,
          14,15,16,17,18,19,
          28,29,30,31,32,33,34,35)]

l<- length(d$ind_id)
IgA_quant<- rep(0,times=l)
IgG_quant<- rep(0, times=l)
Roche_quant<- rep(0, times=l)

for( i in 1:l)
{ x1<-d[i,c(6:8)]
x2<-d[i,c(9:11)]
x3<-d[i,c(12:19)]

y1<- x1[which(!is.na(x1))]
y2<- x2[which(!is.na(x2))]
y3<- x3[which(!is.na(x3))]

s1<- length(y1)
s2<- length(y2)
s3<- length(y3)

IgA_quant[i]<- ifelse(s1>0,unlist(y1[s1]),NA)
IgG_quant[i]<- ifelse(s2>0,unlist(y2[s2]),NA)
Roche_quant[i]<-ifelse(s3>0,unlist(y3[s3]),NA)

}


d<- cbind(d,IgA_quant,IgG_quant,Roche_quant)


dat_blooddonors<- d[which(d$model_outcome=="true_negatives_Blood_donors"),]

stat_box_data_Roche_bd <- function(y, upper_limit =0.6) {
  
  return(
    
    data.frame(
      
      y =  1*upper_limit,
      
      label = paste(
        
        'median =', round(median(y), 2), '\n',
        
        'mean =',round(mean(y, na.rm=T), 2))
      
    )
    
  )
  
}

stat_box_data_IgG_bd <- function(y, upper_limit =1.5* max(log10(dat_blooddonors$IgG_quant))) {
  
  return(
    
    data.frame(
      
      y = 1 * upper_limit,
      
      label = paste(
        
        'median =', round(median(y), 2), '\n',
        
        'mean =',round(mean(y,na.rm=T), 2))
      
    )
    
  )
  
}


stat_box_data_IgA_bd <- function(y, upper_limit = 1.5*max(log10(dat_blooddonors$IgA_quant))) {
  
  return(
    
    data.frame(
      
      y = 1 * upper_limit,
      
      label = paste(
        
        'median =', round(median(y), 2), '\n',
        
        'mean =',round(mean(y,na.rm=T), 2))
      
    )
    
  )
  
}

dat_blooddonors$cohort<- factor(dat_blooddonors$cohort, levels = c( "BloodDonor_Oct19",
                                                                    "BloodDonor_Mar20"))
dat_blooddonors$cohort_1<- as.numeric(dat_blooddonors$cohort)


var<-FSA::Summarize(log10(dat_blooddonors$IgA_quant)~dat_blooddonors$cohort)
dat_blooddonors$IgA_new<- ifelse(dat_blooddonors$IgA_quant>=1.085,"Positive","Negative")
dat_blooddonors$IgA_old<- ifelse(dat_blooddonors$IgA_quant>=1.1,"Positive","Negative")
tab1<- FSA::Summarize(log10(dat_blooddonors$IgA_quant)~dat_blooddonors$cohort+dat_blooddonors$IgA_new)
tab2<- FSA::Summarize(log10(dat_blooddonors$IgA_quant)~dat_blooddonors$cohort+dat_blooddonors$IgA_old)



bda<-ggplot(dat_blooddonors, aes(y=log10(IgA_quant),x= factor(cohort_1))) +
  geom_flat_violin( position = position_nudge(x = 0, y = 0),alpha = .3,color="#56B4E9", fill="#56B4E9",
                    adjust =2, trim = F)+
  #geom_point(position = position_nudge(x=-0.025), shape = 21, size = 2)+
  geom_boxplot(width=0.1, alpha=0.2,outlier.shape = NA)+
  #geom_jitter(position=position_jitter(0.25), shape = 21, fill = "#56B4E9",
  #             color = "#56B4E9", size = 1)+
  geom_jitter(aes(x = cohort_1 - 0.1,
                  y = log10(IgA_quant)),
              width = 0.1 - 0.25 * 0.2,
              height = 0,shape = 21, fill = "#56B4E9",
              color = "#56B4E9", size = 1,alpha=0.5) +
  stat_summary(fun=mean, geom='point', shape=21, size=2, color="darkred", fill="darkred")+
 
  stat_compare_means(aes(label = sprintf("p = %5.4f", as.numeric(..p.format..))))+ 
  xlab(NULL)+
  
  stat_summary(
    
    fun.data = stat_box_data_IgA_bd,
    
    geom = "text",
    
    hjust = 0.5,
    
    vjust = 0.9, size=3
    
  ) +
  scale_x_discrete(labels=c(paste('Blooddonors October2019\n (n=',var$n[1],')'),paste('Blooddonors March2020\n (n=',var$n[2],')')))+
  geom_hline(yintercept = log10(1.1),linetype=3,colour="grey30")+
  geom_hline(yintercept = log10(1.085),linetype=2,colour="black")+
  ylab("EI-S1-IgA (log10 scaled)")+
  #geom_vline(xintercept = 1.5)+
  annotate("text",x=0.65,y=-0.5,label=paste("n' = ",tab1$n[1]), colour="#56B4E9")+
  annotate("text",x=1.65,y=-0.5,label=paste("n' = ",tab1$n[2]), colour="#56B4E9")+
  annotate("text",x=0.65,y=0.25,label=paste("n' = ",tab1$n[3]), colour="#56B4E9")+
  annotate("text",x=1.65,y=0.25,label=paste("n' = ",tab1$n[4]), colour="#56B4E9")+
  annotate("text",x=0.65,y=-0.25,label=paste("n = ",tab2$n[1]), colour="#56B4E9", alpha=0.5)+
  annotate("text",x=1.65,y=-0.25,label=paste("n = ",tab2$n[2]), colour="#56B4E9",alpha=0.5)+
  annotate("text",x=0.65,y=0.50,label=paste("n = ",tab2$n[3]), colour="#56B4E9",alpha=0.5)+
  annotate("text",x=1.65,y=0.50,label=paste("n = ",tab2$n[4]), colour="#56B4E9",alpha=0.5)+
  annotate("label", x = 2.4, y = 1, label = paste("n = ",tab2$n[3]+tab2$n[4]),
           colour="grey30", fill=NA) +
  annotate("label", x = 2.4, y = -0.95, label = paste("n = ",tab2$n[1]+tab2$n[2]),
           colour="grey30",fill=NA) +
  annotate("label", x = 2.4 , y =  0.65, fill=NA,label = paste("n' = ",tab1$n[3]+tab1$n[4])) +
  annotate("label", x = 2.4 , y = -1.3, fill=NA,label = paste("n' = ",tab1$n[1]+tab1$n[2]))+
  scale_y_continuous(breaks= pretty_breaks(n=5))+
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.01, color="#CCCCCC"))
  
bda

ggsave("IgA_violinplot_Bloodonors.jpeg",dpi=720, height=5, width = 7, units="in") 


var<-FSA::Summarize(log10(dat_blooddonors$IgG_quant)~dat_blooddonors$cohort)
dat_blooddonors$IgG_new<- ifelse(dat_blooddonors$IgG_quant>=1.015,"Positive","Negative")
dat_blooddonors$IgG_old<- ifelse(dat_blooddonors$IgG_quant>=1.1,"Positive","Negative")
tab1<- FSA::Summarize(log10(dat_blooddonors$IgG_quant)~dat_blooddonors$cohort+dat_blooddonors$IgG_new)
tab2<- FSA::Summarize(log10(dat_blooddonors$IgG_quant)~dat_blooddonors$cohort+dat_blooddonors$IgG_old)

bdg<-ggplot(dat_blooddonors, aes(y=log10(IgG_quant),x= factor(cohort_1))) +
  geom_flat_violin( position = position_nudge(x = 0, y = 0),alpha = .3,color="#56B4E9", fill="#56B4E9",
                    adjust =2, trim = F)+
  #geom_point(position = position_nudge(x=-0.025), shape = 21, size = 2)+
  geom_boxplot(width=0.1, alpha=0.2,outlier.shape = NA)+
  #geom_jitter(position=position_jitter(0.25), shape = 21, fill = "#56B4E9",
  #             color = "#56B4E9", size = 1)+
  geom_jitter(aes(x = cohort_1 - 0.1,
                  y = log10(IgG_quant)),
              width = 0.1 - 0.25 * 0.2,
              height = 0,shape = 21, fill = "#56B4E9",
              color = "#56B4E9", size = 1, alpha=0.5) +
  stat_summary(fun=mean, geom='point', shape=21, size=2, color="darkred", fill="darkred")+
 
  stat_compare_means(aes(label = sprintf("p = %5.4f", as.numeric(..p.format..))),label.y = 0.7)+ 
  xlab(NULL)+
  
  stat_summary(
    
    fun.data = stat_box_data_IgG_bd,
    
    geom = "text",
    
    hjust = 0.5,
    
    vjust = 0.9, size=3
    
  ) +
  scale_x_discrete(labels=c(paste('Blooddonors October2019\n (n=',var$n[1],')'),paste('Blooddonors March2020\n (n=',var$n[2],')')))+
  geom_hline(yintercept = log10(1.1),linetype=3,colour="grey30")+
  geom_hline(yintercept = log10(1.015),linetype=2, colour="black")+
  ylab("EI-S1-IgG (log10 scaled)")+
  scale_y_continuous(breaks= pretty_breaks(n=5))+
  #geom_vline(xintercept = 1.5)+
  annotate("text",x=0.65,y=-.5,label=paste("n' = ",tab1$n[1]), colour="#56B4E9")+
  annotate("text",x=1.65,y=-.5,label=paste("n' = ",tab1$n[2]), colour="#56B4E9")+
  annotate("text",x=0.65,y=0.25,label=paste("n' = ",tab1$n[3]), colour="#56B4E9")+
  annotate("text",x=1.65,y=0.25,label=paste("n' = ",tab1$n[4]), colour="#56B4E9")+
  annotate("text",x=0.65,y=-0.25,label=paste("n = ",tab2$n[1]), colour="#56B4E9", alpha=0.5)+
  annotate("text",x=1.65,y=-0.25,label=paste("n = ",tab2$n[2]), colour="#56B4E9",alpha=0.5)+
  annotate("text",x=0.65,y=0.50,label=paste("n = ",tab2$n[3]), colour="#56B4E9",alpha=0.5)+
  annotate("text",x=1.65,y=0.50,label=paste("n = ",tab2$n[4]), colour="#56B4E9",alpha=0.5)+
  annotate("label", x = 2.4, y = 0.8, label = paste("n = ",tab2$n[3]+tab2$n[4]),
           colour="grey30",fill=NA) +
  annotate("label", x = 2.4, y = -0.9, label = paste("n = ",tab2$n[1]+tab2$n[2]),
           colour="grey30",fill=NA) +
  annotate("label", x = 2.4 , y =  0.5, fill=NA,label = paste("n' = ",tab1$n[3]+tab1$n[4])) +
  annotate("label", x = 2.4 , y = -1.2, fill=NA,label = paste("n' = ",tab1$n[1]+tab1$n[2]))+
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.01, color="#CCCCCC"))
#0.5 0.2 -0.5 -0.8  
bdg
ggsave("IgG_violinplot_Bloodonors.jpeg",dpi=720, height=5, width = 7, units="in") 

var<-FSA::Summarize(log10(dat_blooddonors$Roche_quant)~dat_blooddonors$cohort)
dat_blooddonors$Roche_new<- ifelse(dat_blooddonors$Roche_quant>=0.4218,"Positive","Negative")
dat_blooddonors$Roche_old<- ifelse(dat_blooddonors$Roche_quant>=1,"Positive","Negative")
tab1<- FSA::Summarize(log10(dat_blooddonors$Roche_quant)~dat_blooddonors$cohort+dat_blooddonors$Roche_new)
tab2<- FSA::Summarize(log10(dat_blooddonors$Roche_quant)~dat_blooddonors$cohort+dat_blooddonors$Roche_old)

bdr<-ggplot(dat_blooddonors[which(!is.na(dat_blooddonors$Roche_quant)),], aes(y=log10(Roche_quant),x= factor(cohort_1))) +
  #geom_violin(trim = FALSE, color="darkgrey")+
  geom_flat_violin( position = position_nudge(x = 0, y = 0),alpha = .3,color="#56B4E9", fill="#56B4E9",
                    adjust =2, trim = F)+
  #geom_point(position = position_nudge(x=-0.025), shape = 21, size = 2)+
  geom_boxplot(width=0.1, alpha=0.2,outlier.shape = NA)+
  #geom_jitter(position=position_jitter(0.25), shape = 21, fill = "#56B4E9",
  #             color = "#56B4E9", size = 1)+
  geom_jitter(aes(x = cohort_1 - 0.1,
                  y = log10(Roche_quant)),
              width = 0.1 - 0.25 * 0.2,
              height = 0,shape = 21, fill = "#56B4E9",
              color = "#56B4E9", size = 1, alpha=0.5) +
  
  
  stat_summary(fun=mean, geom='point', shape=21, size=2, color="darkred", fill="darkred")+
  
  stat_compare_means(aes(label = sprintf("p = %5.4f", as.numeric(..p.format..))),label.y = 0.32)+ 
  xlab(NULL)+
  
  stat_summary(
    
    fun.data = stat_box_data_Roche_bd,
    
    geom = "text",
    
    hjust = 0.5,
    
    vjust = 0.9, size=3
    
  ) +
  scale_x_discrete(labels=c(paste('Blooddonors October2019\n (n=',var$nvalid[1],')'),paste('Blooddonors March2020\n (n=',var$nvalid[2],')')))+
  geom_hline(yintercept = log10(1),linetype=3,colour="grey30")+
  geom_hline(yintercept = log10(0.4218),linetype=2, colour="black")+
  geom_segment( aes(x = 2.12, y = -1.10, xend = 2.09, yend = -1.15),
                size=0.75,arrow=arrow(length = unit(0.025, "npc")))+
  ylab("Ro-N-Ig (log10 scaled)")+
  #geom_vline(xintercept = 1.5)+
  scale_y_continuous(breaks= pretty_breaks(n=5))+
  annotate("text",x=0.65,y=-1,label=paste("n' = ",tab1$n[1]), colour="#56B4E9")+
  annotate("text",x=1.65,y=-1,label=paste("n' = ",tab1$n[2]), colour="#56B4E9")+
  annotate("text",x=1.65,y=0.15,label=paste("n' = ",tab1$n[3]), colour="#56B4E9")+
  
  annotate("text",x=0.65,y=-0.75,label=paste("n = ",tab2$n[1]), colour="#56B4E9", alpha=0.5)+
  annotate("text",x=1.65,y=-0.75,label=paste("n = ",tab2$n[2]), colour="#56B4E9",alpha=0.5)+
  annotate("text",x=1.65,y=0.30,label=paste("n = ",tab2$n[3]), colour="#56B4E9",alpha=0.5)+
  annotate("label", x = 2.4, y = 0.45, label = paste("n = ",tab2$n[3]),
           colour="grey30", fill=NA) +
  annotate("label", x = 2.4, y = -0.75, label = paste("n = ",tab2$n[1]+tab2$n[2]),
           colour="grey30",fill=NA) +
  annotate("label", x = 2.4 , y =  0.2, fill=NA,label = paste("n' = ",tab1$n[3])) +
  annotate("label", x = 2.4 , y = -1, fill=NA,label = paste("n' = ",tab1$n[1]+tab1$n[2]))+
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.01, color="#CCCCCC"))
 
#0.5 0.2 -0.5 -0.8

bdr
ggsave("Roche_violinplot_Bloodonors.jpeg",dpi=720, height=5, width = 7, units="in") 

library(patchwork)
(bda  + 
    bdg) / (plot_spacer()+bdr + plot_spacer()+plot_layout(nrow=1,widths=c(1,2,1)))+
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 14))

ggsave("Combined_Bloodonors.jpeg",dpi=720, height=8, width = 12, units="in") 
ggsave("Combined_Bloodonors.pdf",dpi=720, height=8, width = 12, units="in") 
ggsave("Combined_Bloodonors.svg",dpi=720, height=8, width = 12, units="in") 
ggsave("Combined_Bloodonors.png", height=8, width = 12, units="in") 
#rmarkdown::render("par_mod.R",quiet = TRUE)
