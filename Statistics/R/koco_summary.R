rm(list=ls(all=TRUE))

library(ggplot2)
library(rmarkdown)
library(readxl)
library(magrittr)
library(summarytools)
library(formattable)
library(ggplot2)
library(zoo)
library(FSA)
library(reshape2)
library(plyr)
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
library(ggpmisc)
library(pscl)
library(hurdlr)
library(PupillometryR)
library(broom.mixed)
library(emmeans)
library(epitools)
library(patchwork)
library(svglite)
library(visdat)
library(JointAI)

###############################################################################
# Setup

# Script files
here_r = function (...) here::here("Statistics", "R", ...)
# Data sets
here_data = function (...) here::here(
  "KoCo19_Datasets", "Analysis Data Sets", ...)
# Outputs
here_out = function (...) here::here("Epi_out", ...)

# create folders if not existent
dir.create(here_out(), showWarnings=F)

# Three datasets
#  ind_long_data - lab data as of now has the baseline measurements
#  will grow into a longitudinal dataset

# reading the csv files
ind_long_data<-read.csv(here_data("ind_lab_baseline_new.csv"),
                        header = T)
ind_characteristics<-read.csv(here_data("ind_characteristics_new.csv"),
                              header = T)
hh_characteristics<-read.csv(here_data("hh_characteristics_new.csv"),
                             header = T)

# Plotting style
#ggplot2::theme_set(ggplot2::theme_classic())
ggplot2::theme_set(ggplot2::theme_bw())

###############################################################################

#ind_long_data<- ind_long_data[which(ind_long_data$VisitDate<= as.Date("2020-06-12",format="%Y-%m-%d")),]
# Summary statistics of the tests

# Basic Tables
my_controls <- tableby.control(
  test = T,
  total = T,
  numeric.test = "kwt", 
  ordered.test = "trend",
  numeric.stats = c("meansd", "medianq1q3", "range", "Nmiss2"),
  cat.stats = c("countpct", "Nmiss2"),
  stats.labels = list(
    meansd = "Mean (SD)",
    medianq1q3 = "Median (Q1, Q3)",
    range = "Min - Max",
    Nmiss2 = "Missing"
  )
)

# Remove children below 14 years
ind_characteristics<- ind_characteristics[which(ind_characteristics$Age>=14),]

# Summary Table 1
summary(ind_characteristics$Age)
ind_characteristics$Age<- ifelse(ind_characteristics$ind_id %in% ind_long_data$ind_id==TRUE,
                                 ind_long_data$Age,ind_characteristics$Age)

ind_characteristics$Agegroup<- ifelse(ind_characteristics$Age<20,"0-19",
                                      ifelse(ind_characteristics$Age>=20 & ind_characteristics$Age<35,"20-34",
                                             ifelse(ind_characteristics$Age>=35 & ind_characteristics$Age<50,"35-49",
                                                    ifelse(ind_characteristics$Age>=50 & ind_characteristics$Age<65,"50-64",
                                                           ifelse(ind_characteristics$Age>=65 & ind_characteristics$Age<80,"65-79","80+")))))
ind_characteristics$Birth_Country<- ifelse(ind_characteristics$Birth_Country=="Deutschland","Germany","Other")
ind_characteristics$Birth_Country<- factor(ind_characteristics$Birth_Country)
ind_characteristics$EducationLevel<- ifelse(ind_characteristics$Education_ind %in% 
                                              c("Type1",  "Type2",   
                                                "Type7","Type8")==TRUE,"<12 years schooling",
                                            ifelse(ind_characteristics$Education_ind=="Type10","Unknown",
                                                   ifelse(ind_characteristics$Education_ind=="Type9","In school",">=12 years schooling")))
ind_characteristics$EmploymentStatus<- ifelse(ind_characteristics$Employment_ind %in% 
                                                c("Type1",  "Type2",  "Type8")==TRUE,"Unemployed",
                                              ifelse(ind_characteristics$Employment_ind %in% c("Type3","Type4","Type5","Type9")==TRUE,
                                                     "Employed",
                                              ifelse(ind_characteristics$Employment_ind %in% c("Type6","Type7")==TRUE,"Self Employed",
                                                     ifelse(ind_characteristics$Employment_ind %in% c("Type10","Type11","Type12","Type13")==TRUE,"Others",NA))))
ind_characteristics$Smoking_Ever_Min_1_Year<-ind_characteristics$Smoking_Ever_min1year
ind_characteristics$Smoking_Recently_Pastmonth<-ind_characteristics$Smoking_pastmonth

ind_characteristics$Employment_ind_lastyear<- factor(ind_characteristics$Employment_ind_lastyear,
                                                     levels=c("No","Not sure","Yes"),
                                                     labels = c("No","No","Yes"))

ind_characteristics$participation<- ifelse(is.na(ind_characteristics$Web.form.entry.date)==TRUE,"nonresponder","responder")
l<- length(ind_characteristics$ind_id)
Employment_HealthCare<-rep(0,times=l)

for( i in 1:l) {
  t<-ind_characteristics[i,c(87,88)]

  s<- length(t[which(t=="Yes")])
  s1<- length(t[which(is.na(t))])

  Employment_HealthCare[i]<- ifelse(s>0 & s1<2,"Yes", ifelse(s==0 & s1<2,"No",NA))
}  
ind_characteristics<-cbind(ind_characteristics,Employment_HealthCare)  


# include changes and new categories
employment_dat_corr<- read_xlsx(here_data("ArbeitinanderenBereichen_KR.xlsx"),col_names = T)

colnames(employment_dat_corr)[1]<-"ind_id"

colnames(employment_dat_corr)[6]<- "Employment_Education"
colnames(employment_dat_corr)[7]<- "Employment_SocialWork"

ind_characteristics<-merge.data.frame(ind_characteristics,employment_dat_corr[c(1,6,7)], by="ind_id",
                                      all.x = TRUE)

ind_characteristics$Employment_Education<- factor(ind_characteristics$Employment_Education,
                                                  levels = c("ja","nein"),
                                                  labels=c("Yes","No"))

ind_characteristics$Employment_SocialWork<- factor(ind_characteristics$Employment_SocialWork,
                                                  levels = c("ja","nein"),
                                                  labels=c("Yes","No"))

d1<-subset(ind_characteristics[which(ind_characteristics$Participation_Type!="Non participant"),],
           select=c(Sex,Agegroup,Birth_Country,EducationLevel,EmploymentStatus,
                                        Smoking_Ever_Min_1_Year,Smoking_Recently_Pastmonth,
                                        Employment_Airport,Employment_DoctorsClinic,
                                        Employment_EmergencyServices, Employment_Hospital,
                                        Employment_Sales,Employment_SeniorHome,
                                        Employment_Transportation,Employment_OtherRiskType,
                                        Employment_Education,Employment_SocialWork,Employment_HealthCare,
                                        Employment_Nolistedoption,Employment_ind_lastyear,Employment_ind,
                                        Participation_Type,participation,ind_id,
                                        Contact_past2w_covpos,Contact_past2w_covsusp,
                                        inclusion_criteria))



d1.1<-d1[which(is.na(d1$EmploymentStatus)),]

reshape2::dcast(d1,Participation_Type~.)
reshape2::dcast(d1,Participation_Type~participation)

d1$smokestatus <-  ifelse(d1$Smoking_Ever_Min_1_Year =="No" ,"Non smoker",
                                        ifelse(d1$Smoking_Ever_Min_1_Year =="Yes" &
                                                 d1$Smoking_Recently_Pastmonth=="Yes","Current smoker",
                                               ifelse((d1$Smoking_Ever_Min_1_Year=="Yes" &
                                                         d1$Smoking_Recently_Pastmonth=="No")|
                                                        d1$Smoking_Ever_Min_1_Year=="Not sure","Past smoker",NA)))
d1$smokestatus<-factor(d1$smokestatus,
                                      levels = c("Non smoker","Past smoker","Current smoker"))

reshape2::dcast(d1[which(d1$Participation_Type!="Non participant"),], Sex~., margins = T)

reshape2::dcast(d1[which(d1$Participation_Type!="Non participant"),], Birth_Country~., margins = T)

reshape2::dcast(d1[which(d1$Participation_Type!="Non participant"),], Agegroup~., margins = T)

reshape2::dcast(d1[which(d1$Participation_Type!="Non participant"),], EducationLevel~., margins = T)

reshape2::dcast(d1[which(d1$Participation_Type!="Non participant"),], EmploymentStatus~., margins = T)


reshape2::dcast(d1[which(d1$Participation_Type!="Non participant"),], Smoking_Ever_Min_1_Year~Smoking_Recently_Pastmonth, margins = T)

reshape2::dcast(d1[which(d1$Participation_Type!="Non participant"),], smokestatus~., margins = T)
l<-length(d1$Sex)
any_risk_emp<-rep(0, times=l)

for( i in 1:l) {
  t<- d1[i,c(8,10,12,13,14,15,16,17,18)]
  t1<- d1[i,c(5,20)]
  s1<- length(t[which(t=="Yes")])
  s2<- length(t[which(is.na(t))])
  
  any_risk_emp[i]<- ifelse(s2<9
                           & s1>0,"Yes",
                           ifelse(s2<9 & s1==0,"No","Missing"))
  
}

d1<- cbind(d1,any_risk_emp)

d1$any_risk_emp<- ifelse(d1$any_risk_emp=="Missing",NA,paste(d1$any_risk_emp))

d1$any_risk_emp_1<- ifelse(d1$EmploymentStatus=="Unemployed","No",
                         ifelse(d1$EmploymentStatus !="Unemployed" &
                                  d1$Employment_ind_lastyear!="Yes","No",
                                 ifelse(d1$Employment_ind_lastyear=="Yes"  & 
                                          d1$EmploymentStatus!="Unemployed" &
                                          d1$any_risk_emp=="Yes","Yes",
                                        ifelse(d1$Employment_ind_lastyear=="Yes"  & 
                                                 d1$EmploymentStatus!="Unemployed" &
                                                 d1$any_risk_emp=="No","No",
                                               NA))))



d1$any_risk_emp_1<- ifelse(is.na(d1$any_risk_emp_1)==FALSE,paste(d1$any_risk_emp_1),ifelse(is.na(d1$any_risk_emp_1)==TRUE & (is.na(d1$Contact_past2w_covpos)==FALSE |
                                                               is.na(d1$Contact_past2w_covsusp)==FALSE),"No",NA))
d1$any_risk_emp<-d1$any_risk_emp_1

d1$any_risk_emp_1<-d1$Contact_past2w_covpos<-d1$Contact_past2w_covsusp<-NULL

reshape2::dcast(d1[which(d1$Participation_Type!="Non participant"),], Employment_ind_lastyear+EmploymentStatus
                  ~any_risk_emp, margins = T)
reshape2::dcast(d1[which(d1$Participation_Type!="Non participant" &
                           d1$EmploymentStatus%in% c("Self Employed","Employed","Others")==TRUE &
                           d1$Employment_ind_lastyear=="Yes"),], EmploymentStatus~Employment_Airport, margins = T)
reshape2::dcast(d1[which(d1$Participation_Type!="Non participant" &
                           d1$EmploymentStatus%in% c("Self Employed","Employed","Others")==TRUE &
                           d1$Employment_ind_lastyear=="Yes"),], EmploymentStatus~Employment_DoctorsClinic, margins = T)
reshape2::dcast(d1[which(d1$Participation_Type!="Non participant" &
                           d1$EmploymentStatus%in% c("Self Employed","Employed","Others")==TRUE &
                           d1$Employment_ind_lastyear=="Yes"),], EmploymentStatus~Employment_EmergencyServices, margins = T)


t1<-tableby(inclusion_criteria~ fe(Sex)+Agegroup+fe(Birth_Country)+ EducationLevel+
              EmploymentStatus+ smokestatus+fe(Employment_ind_lastyear)
            ,
            data =d1,
            control = my_controls, simulate.p.value = TRUE, B=10000)

write2html(summary(t1, pfootnote=TRUE),here_out("Table1.html"))

d1$any_risk_emp<- ifelse(d1$Employment_ind_lastyear=="No","No",
                         ifelse(d1$Employment_ind_lastyear=="Yes" &
                                  !is.na(d1$any_risk_emp),paste(d1$any_risk_emp),NA))

t2<-tableby(inclusion_criteria~ 
              fe(any_risk_emp)
            ,
            data =d1,
            control = my_controls, simulate.p.value = TRUE, B=10000)

write2html(summary(t2, pfootnote=TRUE), here_out("Table2.html"))

d1[which(d1$any_risk_emp=="No"| d1$Employment_ind_lastyear=="No"),]$Employment_Education<-"No"
d1[which(d1$any_risk_emp=="No"| d1$Employment_ind_lastyear=="No"),]$Employment_SocialWork<-"No"

d1[which(d1$any_risk_emp=="No"| d1$Employment_ind_lastyear=="No"),]$Employment_Education<-"No"
d1[which(d1$any_risk_emp=="No"| d1$Employment_ind_lastyear=="No"),]$Employment_SocialWork<-"No"
d1[which(d1$any_risk_emp=="No"| d1$Employment_ind_lastyear=="No"),]$Employment_HealthCare<-"No"
d1[which(d1$any_risk_emp=="No"| d1$Employment_ind_lastyear=="No"),]$Employment_EmergencyServices<-"No"
d1[which(d1$any_risk_emp=="No"| d1$Employment_ind_lastyear=="No"),]$Employment_Sales<-"No"
d1[which(d1$any_risk_emp=="No"| d1$Employment_ind_lastyear=="No"),]$Employment_SeniorHome<-"No"

d1[which(d1$any_risk_emp=="No"| d1$Employment_ind_lastyear=="No"),]$Employment_Transportation<-"No"
d1[which(d1$any_risk_emp=="No"| d1$Employment_ind_lastyear=="No"),]$Employment_OtherRiskType<-"No"
d1[which(d1$any_risk_emp=="No"| d1$Employment_ind_lastyear=="No"),]$Employment_Airport<-"No"
d1[which(d1$any_risk_emp=="No"| d1$Employment_ind_lastyear=="No"),]$Employment_OtherRiskType<-"No"


d1[which(is.na(d1$any_risk_emp)==TRUE),]$Employment_Education<-NA
d1[which(is.na(d1$any_risk_emp)==TRUE),]$Employment_SocialWork<-NA
d1[which(is.na(d1$any_risk_emp)==TRUE),]$Employment_HealthCare<-NA
d1[which(is.na(d1$any_risk_emp)==TRUE),]$Employment_EmergencyServices<-NA
d1[which(is.na(d1$any_risk_emp)==TRUE),]$Employment_Sales<-NA
d1[which(is.na(d1$any_risk_emp)==TRUE),]$Employment_SeniorHome<-NA

d1[which(is.na(d1$any_risk_emp)==TRUE),]$Employment_Transportation<-NA
d1[which(is.na(d1$any_risk_emp)==TRUE),]$Employment_OtherRiskType<-NA
d1[which(is.na(d1$any_risk_emp)==TRUE),]$Employment_Airport<-NA
d1[which(is.na(d1$any_risk_emp)==TRUE),]$Employment_OtherRiskType<-NA


d1[which(d1$any_risk_emp=="Yes" & is.na(d1$Employment_Education)==TRUE),]$Employment_Education<-"No"
d1[which(d1$any_risk_emp=="Yes" & is.na(d1$Employment_SocialWork)==TRUE),]$Employment_SocialWork<-"No"

t3<-tableby(inclusion_criteria~ 
              fe(Employment_Airport)+fe(Employment_HealthCare)+
              fe(Employment_EmergencyServices)+
              fe(Employment_Sales)+fe(Employment_SeniorHome)+
              fe(Employment_Transportation)+
              fe(Employment_Education)+fe(Employment_SocialWork)+fe(Employment_OtherRiskType)
            ,
            data =d1,
            control = my_controls, simulate.p.value = TRUE, B=10000)

write2html(summary(t3, pfootnote=TRUE), here_out("Table3.html"))




hh_characteristics$Housing_Type<-factor(hh_characteristics$HousingType,
                                        levels = c("Type1","Type2","Type3","Type4","Type5","Type6","Type7"),
                                        labels = c("Buildings with 1-2 apartments","Buildings with 1-2 apartments",
                                                   "Buildings with 3-4 apartments","Buildings with >=5 apartments",
                                                   "Buildings with >=5 apartments","Buildings with >=5 apartments",
                                                   "Others"))


hh_characteristics$Household_Type<- ifelse(hh_characteristics$livingstyle=="Type1","Single/Couple",
                                        ifelse(hh_characteristics$livingstyle=="Type2","Family","Others"))

hh_characteristics$Household_Inhabitants<- ifelse(hh_characteristics$HouseholdSize==1,"1",
                                                  ifelse(hh_characteristics$HouseholdSize==2,"2",
                                                         ifelse(hh_characteristics$HouseholdSize==3 |
                                                                hh_characteristics$HouseholdSize==4,"3-4",
                                                                       "5+")))
hh_characteristics$LivingArea_perInhabitant<- round(hh_characteristics$livingarea_sqm/hh_characteristics$HouseholdSize,2)

hh_characteristics$NetIncome_monthly<-hh_characteristics$NetIncome_month_hh

d2<- subset(hh_characteristics,select=c(hh_id,NetIncome_monthly,Housing_Type,Household_Type,Household_Inhabitants,
                                        LivingArea_perInhabitant,livingstyle, inclusion_criteria))



reshape2::dcast(d2,NetIncome_monthly~.,margins = T)



d2$LivingArea_perInhabitant_cat<- ifelse(d2$LivingArea_perInhabitant<=30,"<= 30sqm/individual(< Quartile1)",
                                         ifelse(d2$LivingArea_perInhabitant>30 & d2$LivingArea_perInhabitant<=40,
                                                "30-40sqm/individual(Quartile1-Median)",
                                                ifelse(d2$LivingArea_perInhabitant>40 & d2$LivingArea_perInhabitant<=55,
                                                       "40-55sqm/individual(Median-Quartile3)",">55sqm/individual(>Quartile3)")))
d2$LivingArea_perInhabitant_cat<-factor(d2$LivingArea_perInhabitant_cat,
                                        levels = c("<= 30sqm/individual(< Quartile1)",
                                                   "30-40sqm/individual(Quartile1-Median)",
                                                   "40-55sqm/individual(Median-Quartile3)",
                                                   ">55sqm/individual(>Quartile3)"))
reshape2::dcast(d2,LivingArea_perInhabitant_cat~.,margins = T)



# 2*2 tables with Roche Positive

# Roche Positives Individual analysis

lab_dat<- subset(ind_long_data,select=c( ind_id,BloodID,VisitDate,hh_id,                    
                                         VisitDate_Baseline,IgG_result,R_Result))


d1<-merge.data.frame(d1,lab_dat,by="ind_id",all.x = TRUE)

d1.1<-d1[which(d1$inclusion_criteria=="Included"),]

d1$inclusion_criteria<-NULL

reshape2::dcast(d1[which(d1$Participation_Type!="Non participant"),], Sex~R_Result, margins = T)

reshape2::dcast(d1[which(d1$Participation_Type!="Non participant"),], Birth_Country~R_Result, margins = T)

reshape2::dcast(d1[which(d1$Participation_Type!="Non participant"),], Agegroup~R_Result, margins = T)

reshape2::dcast(d1[which(d1$Participation_Type!="Non participant"),], EducationLevel~R_Result, margins = T)

reshape2::dcast(d1[which(d1$Participation_Type!="Non participant"),], EmploymentStatus~R_Result, margins = T)


reshape2::dcast(d1[which(d1$Participation_Type!="Non participant"),], smokestatus~R_Result, margins = T)

d1.1$Employment_ind_lastyear<-factor(d1.1$Employment_ind_lastyear, levels=c("No","Not sure","Yes"),
                                     labels = c("No","No","Yes"))

d1.1$any_risk_emp<- ifelse(d1.1$EmploymentStatus!="Unemployed" &d1.1$Employment_ind_lastyear=="Yes" &  d1.1$any_risk_emp=="Yes","Yes","No")


# Summary tables


# Sex
ctable(d1.1$R_Result, d1.1$Sex, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$Sex)

#Age group

ctable(d1.1$R_Result, d1.1$Agegroup, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
chisq.test(d1.1$R_Result, d1.1$Agegroup, correct = TRUE,
           simulate.p.value =T, B = 2000)
#Birth country

ctable(d1.1$R_Result, d1.1$Birth_Country, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$Birth_Country)

# Education level
ctable(d1.1$R_Result, d1.1$EducationLevel, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$EducationLevel)

#employment status

ctable(d1.1$R_Result, d1.1$EmploymentStatus, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
chisq.test(d1.1$R_Result, d1.1$EmploymentStatus, correct = TRUE,
           simulate.p.value =T, B = 2000)

#smokestatus
ctable(d1.1$R_Result, d1.1$smokestatus, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
chisq.test(d1.1$R_Result, d1.1$smokestatus, correct = TRUE,
           simulate.p.value =T, B = 2000)
# Employment last year
ctable(d1.1$R_Result, d1.1$Employment_ind_lastyear, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$Employment_ind_lastyear)

# Employment in any risk job
ctable(d1.1$R_Result, d1.1$any_risk_emp, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$any_risk_emp)

# Employment_Airport
ctable(d1.1$R_Result, d1.1$Employment_Airport, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$Employment_Airport)

# Employment_HealthCare
ctable(d1.1$R_Result, d1.1$Employment_HealthCare, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$Employment_HealthCare)

# Employment_EmergencyServices
ctable(d1.1$R_Result, d1.1$Employment_EmergencyServices, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$Employment_EmergencyServices)

# Employment_Sales
ctable(d1.1$R_Result, d1.1$Employment_Sales, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$Employment_Sales)

# Employment_SeniorHome
ctable(d1.1$R_Result, d1.1$Employment_SeniorHome, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$Employment_SeniorHome)

# Employment_Transportation
ctable(d1.1$R_Result, d1.1$Employment_Transportation, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$Employment_Transportation)

# Employment_Education
ctable(d1.1$R_Result, d1.1$Employment_Education, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$Employment_Education)
# Employment_SocialWork
ctable(d1.1$R_Result, d1.1$Employment_SocialWork, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$Employment_SocialWork)
# Employment_Transportation
ctable(d1.1$R_Result, d1.1$Employment_OtherRiskType, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$Employment_OtherRiskType)

# Count of positives per household
hh_pos<- reshape2::dcast(lab_dat,hh_id~R_Result)

d2<- merge.data.frame(d2,hh_pos,by="hh_id",all.x=T)
l<-length(d2$hh_id)
Roche_pos<-rep(0,times=l)

for(i in 1:l)
{ t<-d2[i,c(9,10,11)]
  s1<-length(t[which(is.na(t))])
  s2<-t[2]
  Roche_pos[i]<- ifelse(s2>0,"Positive",
                        ifelse(s1<3 & s2==0,"Negative",NA))
}
d2<- cbind(d2,Roche_pos)

d2$Roche_pos<-factor(d2$Roche_pos,levels = c("Positive","Negative"),labels = c("Min 1 positive","No positives"))

d2$NetIncome_monthly_1<- factor(d2$NetIncome_monthly,levels = c("<500",">6000", "1000-1500", "1500-2000", "2000-2500", "2500-3000", "3000-4000", "4000-5000","500-750", 
                                                              "5000-6000",  "750-1000", "No Answer" ),
                              labels = c("<=2500","6000+","<=2500","<=2500","<=2500","2500-4000","2500-4000","4000-6000","<=2500","4000-6000","<=2500",NA))

d2$NetIncome_monthly_1<-factor(d2$NetIncome_monthly_1,levels=c("<=2500","2500-4000",      "4000-6000","6000+"))
reshape2::dcast(d2,NetIncome_monthly_1~.,margins = T)

reshape2::dcast(d2,Housing_Type~.,margins = T)
reshape2::dcast(d2,Household_Type~.,margins = T)
reshape2::dcast(d2,Household_Inhabitants~.,margins = T)

reshape2::dcast(d2,Household_Type+livingstyle~Household_Inhabitants,)

d2$Household_Type_1<- ifelse(d2$livingstyle=="Type1" & d2$Household_Inhabitants==1,"Single",
                             ifelse(d2$livingstyle=="Type1" & d2$Household_Inhabitants==2,"Couple",
                                    ifelse(d2$livingstyle=="Type2","Family","Others")))

reshape2::dcast(d2,Household_Type_1~.,margins = T)


summary(d2$LivingArea_perInhabitant)
reshape2::dcast(d2,LivingArea_perInhabitant_cat~.,margins = T)

d2$Household_Inhabitants_1<- ifelse(d2$Household_Inhabitants==1,"1",
                                    ifelse(d2$Household_Inhabitants==2,"2",
                                           ifelse(d2$Household_Inhabitants==3 | d2$Household_Inhabitants==4,"3-4",
                                                  "5+")))

t4<-tableby(inclusion_criteria~ 
              NetIncome_monthly_1+Housing_Type+Household_Type_1+Household_Inhabitants+
              LivingArea_perInhabitant_cat
            ,
            data =d2,
            control = my_controls, simulate.p.value = TRUE, B=10000)

write2html(summary(t4, pfootnote=TRUE), here_out("Table4.html"))

d2<- d2[which(d2$inclusion_criteria== "Included"),]
d2$inclusion_criteria<-NULL


# Household Income
ctable(d2$Roche_pos, d2$NetIncome_monthly_1, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
chisq.test(d2$Roche_pos, d2$NetIncome_monthly_1, correct = TRUE,
           simulate.p.value =T, B = 2000)

# Housing Type

ctable(d2$Roche_pos, d2$Housing_Type, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
chisq.test(d2$Roche_pos, d2$Housing_Type, correct = TRUE,
           simulate.p.value =T, B = 2000)

# Household type
ctable(d2$Roche_pos, d2$Household_Type_1, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
chisq.test(d2$Roche_pos, d2$Household_Type_1, correct = TRUE,
           simulate.p.value =T, B = 2000)

#Number of inhabitants 
ctable(d2$Roche_pos, d2$Household_Inhabitants, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
chisq.test(d2$Roche_pos, d2$Household_Inhabitants, correct = TRUE,
           simulate.p.value =T, B = 2000)

# area per inhabitant
ctable( d2$LivingArea_perInhabitant_cat, d2$Roche_pos,totals =T, headings = T, prop = "r",
       OR = TRUE, RR = TRUE)
chisq.test(d2$Roche_pos,d2$LivingArea_perInhabitant_cat, correct = TRUE,
           simulate.p.value =T, B = 2000)


# Complete Data
lab_dat<- read.csv(here_data("Complete_baseline.csv"), header = T)

lab_dat<-lab_dat[which(lab_dat$ind_id %in% ind_long_data$ind_id==TRUE),]

lab_dat[which(lab_dat$ind_id=="2609S01"),]$Age<-62
lab_dat[which(lab_dat$ind_id=="3115A01"),]$Age<-41
lab_dat[which(lab_dat$ind_id=="3221T01"),]$Age<-65

d1<- d1[which(d1$ind_id %in% ind_long_data[which(ind_long_data$inclusion_criteria=="Included"),]$ind_id==TRUE),]
d1.2<- d1[c(1,3,4,5,6,19,18,17,24,25,26,29)]

d2.2<-d2[c(1,13,14,5,3,8)]

d<- merge.data.frame(d1.2,lab_dat,by=c("ind_id","hh_id"))
d<- merge.data.frame(d,d2.2,by="hh_id",all.x = TRUE)

d<- merge.data.frame(d, subset(ind_characteristics,select=c("ind_id","pcpt_type", "Web.form.entry.date")),all.x = TRUE)
d<- d[which(!is.na(d$R_quant)),]
d$Roche_Result_old<- ifelse(d$R_quant>=1,"Positive","Negative")
d$IgG_Result_old<- ifelse(d$IgG_quant>=1.1,"Positive","Negative")
d$Roche_Result_new<- ifelse(d$R_quant>=0.4218,"Positive","Negative")
d$IgG_result_new<- ifelse(d$IgG_quant >= 1.015,"Positive","Negative")

# Note: Here we write to the data folder
write.csv(d, here_data("Koco_baseline.csv"))

ctable(d$IgG_result_new,d$IgG_Result_old)
ctable(d$IgG_result_new,d$Roche_Result_new)
ctable(d$Roche_Result_old,d$Roche_Result_new)
ctable(d$Roche_Result_old,d$IgG_Result_old)

# Roche Data Positivity 

add_data<- subset(d, select=c(ind_id,Age,
                                    Height,Weight, HouseholdSize, 
                                    Symptoms_past2w_Fever,                       
                                    Symptoms_past2w_Chills, Symptoms_past2w_Limbpain,                    
                                    Symptoms_past2w_Runningnose,Symptoms_past2w_SenseLoss.Smell.Taste.,      
                                    Symptoms_past2w_Sorethroat, Symptoms_past2w_Headache,                    
                                    Symptoms_past2w_Drycough, Symptoms_past2w_Breathlessness,              
                                    Symptoms_past2w_Fatigue,Symptoms_past2w_diarrhoea,any_symptoms,num_symptoms_pos,
                                    HealthStatus_Self_Overall_1,
                                    HealthStatus_Self_Diabetes,                  
                                    HealthStatus_Self_LungDisease,HealthStatus_Self_CardiovascularDisease,    
                                    HealthStatus_Self_Cancer,HealthStatus_Self_Obesity,                   
                                    HealthStatus_Self_Allergies_Skin,HealthStatus_Self_Allergies_Respiratory,     
                                    HealthStatus_Self_Autoimmune,  HealthStatus_Self_HIV,                       
                                    HealthStatus_Self_OtherChronic, HealthStatus_Self_OtherChronic_Specification,
                                    Medication_Immunosuppressive,Medication_Cortisone,                        
                                    Medication_Bloodpressure,Vaccination_Influenza2019,
                                    Pregnant_1,NetIncome_monthly_1,
                                    Household_Type_1,Housing_Type ,Household_Inhabitants,
                                    LivingArea_perInhabitant_cat))

d1.1<- merge.data.frame(d1.1,add_data, by="ind_id",all.x= T)



# 2*2 tables for symptoms and number of symptoms

# Symptoms_past2w_Fever
ctable(d1.1$R_Result, d1.1$Symptoms_past2w_Fever, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$Symptoms_past2w_Fever)

#Symptoms_past2w_Chills,
ctable(d1.1$R_Result, d1.1$Symptoms_past2w_Chills, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$Symptoms_past2w_Chills)
#Symptoms_past2w_Limbpain,     
ctable(d1.1$R_Result, d1.1$Symptoms_past2w_Limbpain, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$Symptoms_past2w_Limbpain)

#Symptoms_past2w_Runningnose,
ctable(d1.1$R_Result, d1.1$Symptoms_past2w_Runningnose, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$Symptoms_past2w_Runningnose)
#Symptoms_past2w_SenseLoss.Smell.Taste.,      
ctable(d1.1$R_Result, d1.1$Symptoms_past2w_SenseLoss.Smell.Taste., totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$Symptoms_past2w_SenseLoss.Smell.Taste.)
#Symptoms_past2w_Sorethroat,
ctable(d1.1$R_Result, d1.1$Symptoms_past2w_Sorethroat, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$Symptoms_past2w_Sorethroat)
#Symptoms_past2w_Headache,                    
ctable(d1.1$R_Result, d1.1$Symptoms_past2w_Headache, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$Symptoms_past2w_Headache)
#Symptoms_past2w_Drycough,
ctable(d1.1$R_Result, d1.1$Symptoms_past2w_Drycough, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$Symptoms_past2w_Drycough)
#Symptoms_past2w_Breathlessness,              
ctable(d1.1$R_Result, d1.1$Symptoms_past2w_Breathlessness, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$Symptoms_past2w_Breathlessness)
#Symptoms_past2w_Fatigue,
ctable(d1.1$R_Result, d1.1$Symptoms_past2w_Fatigue, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$Symptoms_past2w_Fatigue)
#Symptoms_past2w_diarrhoea,
ctable(d1.1$R_Result, d1.1$Symptoms_past2w_diarrhoea, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$Symptoms_past2w_diarrhoea)
#any_symptoms,
ctable(d1.1$R_Result, d1.1$any_symptoms, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$any_symptoms)
#num_symptoms_pos,
# Categorize them to 1,2,3+
d1.1$num_symptoms_pos_cat<- ifelse(d1.1$num_symptoms_pos==0,"0",ifelse(d1.1$num_symptoms_pos==1,"1",
                                   ifelse(d1.1$num_symptoms_pos==2,"2",
                                          "3+")))
ctable(d1.1$R_Result, d1.1$num_symptoms_pos_cat, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
chisq.test(d1.1$R_Result, d1.1$num_symptoms_pos_cat, correct = TRUE,
           simulate.p.value =T, B = 2000)

#HealthStatus_Self_Overall_1,
ctable(d1.1$R_Result, d1.1$HealthStatus_Self_Overall_1, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
chisq.test(d1.1$R_Result, d1.1$HealthStatus_Self_Overall_1, correct = TRUE,
           simulate.p.value =T, B = 2000)

#HealthStatus_Self_Diabetes,     
d1.1$HealthStatus_Self_Diabetes<- factor(d1.1$HealthStatus_Self_Diabetes,levels = c("No", "Not sure" ,"Yes"),
                                         labels=c("No","No","Yes"))

ctable(d1.1$R_Result, d1.1$HealthStatus_Self_Diabetes, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$HealthStatus_Self_Diabetes)
#HealthStatus_Self_LungDisease,
ctable(d1.1$R_Result, d1.1$HealthStatus_Self_LungDisease, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$HealthStatus_Self_LungDisease)
#HealthStatus_Self_CardiovascularDisease,    
ctable(d1.1$R_Result, d1.1$HealthStatus_Self_CardiovascularDisease, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$HealthStatus_Self_CardiovascularDisease)
#HealthStatus_Self_Cancer,
ctable(d1.1$R_Result, d1.1$HealthStatus_Self_Cancer, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$HealthStatus_Self_Cancer)
#HealthStatus_Self_Obesity,                   
ctable(d1.1$R_Result, d1.1$HealthStatus_Self_Obesity, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$HealthStatus_Self_Obesity)
#HealthStatus_Self_Allergies_Skin,
ctable(d1.1$R_Result, d1.1$HealthStatus_Self_Allergies_Skin, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$HealthStatus_Self_Allergies_Skin)
#HealthStatus_Self_Allergies_Respiratory,     
ctable(d1.1$R_Result, d1.1$HealthStatus_Self_Allergies_Respiratory, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$HealthStatus_Self_Allergies_Respiratory)
#HealthStatus_Self_Autoimmune,  
ctable(d1.1$R_Result, d1.1$HealthStatus_Self_Autoimmune, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$HealthStatus_Self_Autoimmune)
#HealthStatus_Self_HIV,                       
ctable(d1.1$R_Result, d1.1$HealthStatus_Self_HIV, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$HealthStatus_Self_HIV)
#HealthStatus_Self_OtherChronic
ctable(d1.1$R_Result, d1.1$HealthStatus_Self_OtherChronic, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)

fisher.test(d1.1$R_Result, d1.1$HealthStatus_Self_OtherChronic)
#Medication_Immunosuppressive,
ctable(d1.1$R_Result, d1.1$Medication_Immunosuppressive, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$Medication_Immunosuppressive)
#Medication_Cortisone,                        
ctable(d1.1$R_Result, d1.1$Medication_Cortisone, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$Medication_Cortisone)
#Medication_Bloodpressure,
ctable(d1.1$R_Result, d1.1$Medication_Bloodpressure, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$Medication_Bloodpressure)
#Vaccination_Influenza2019,
ctable(d1.1$R_Result, d1.1$Vaccination_Influenza2019, totals =T, headings = T, prop = "c",
       OR = TRUE, RR = TRUE)
fisher.test(d1.1$R_Result, d1.1$Vaccination_Influenza2019)

# Summary of the continous outcomes across multiple covariates
quant_result<-subset(lab_dat, select = c(ind_id,R_quant,IgA_quant,IgG_quant))
d1.1<- merge.data.frame(d1.1, quant_result,by="ind_id",all.x = T)

# Roche Values
# Sex
Summarize(R_quant~ Sex, data = d1.1)
wilcox.test(d1.1$R_quant~d1.1$Sex)

# Age group
Summarize(R_quant~ Agegroup, data = d1.1)
kruskal.test (d1.1$R_quant~d1.1$Agegroup)

# Country of birth
Summarize(R_quant~ Birth_Country, data = d1.1)
kruskal.test (d1.1$R_quant~d1.1$Birth_Country)

# Education Level
Summarize(d1.1$R_quant~ d1.1$EducationLevel, data = d1.1)
kruskal.test (d1.1$R_quant~d1.1$EducationLevel)

# Employment Status
Summarize(d1.1$R_quant~ d1.1$EmploymentStatus, data = d1.1)
kruskal.test (d1.1$R_quant~d1.1$EmploymentStatus)

# Smoke Status
Summarize(d1.1$R_quant~ d1.1$smokestatus, data = d1.1)
kruskal.test (d1.1$R_quant~d1.1$smokestatus)

# Employment last year
Summarize(d1.1$R_quant~d1.1$Employment_ind_lastyear, data = d1.1)
wilcox.test (d1.1$R_quant~d1.1$Employment_ind_lastyear)

# Employment in any risk job
Summarize(d1.1$R_quant~d1.1$any_risk_emp, data = d1.1)
wilcox.test (d1.1$R_quant~d1.1$any_risk_emp)

# Employment_Airport
Summarize(d1.1$R_quant~d1.1$Employment_Airport, data = d1.1)
wilcox.test (d1.1$R_quant~d1.1$Employment_Airport)

# Employment_HealthCare
Summarize(d1.1$R_quant~d1.1$Employment_HealthCare, data = d1.1)
wilcox.test (d1.1$R_quant~d1.1$Employment_HealthCare)

# Employment_EmergencyServices
Summarize(d1.1$R_quant~d1.1$Employment_EmergencyServices, data = d1.1)
wilcox.test (d1.1$R_quant~d1.1$Employment_EmergencyServices)

# Employment_Sales
Summarize(d1.1$R_quant~d1.1$Employment_Sales, data = d1.1)
wilcox.test (d1.1$R_quant~d1.1$Employment_Sales)

# Employment_SeniorHome
Summarize(d1.1$R_quant~d1.1$Employment_SeniorHome, data = d1.1)
wilcox.test (d1.1$R_quant~d1.1$Employment_SeniorHome)

# Employment_Transportation
Summarize(d1.1$R_quant~d1.1$Employment_Transportation, data = d1.1)
wilcox.test (d1.1$R_quant~d1.1$Employment_Transportation)

# Employment_Education
Summarize(d1.1$R_quant~d1.1$Employment_Education, data = d1.1)
wilcox.test (d1.1$R_quant~d1.1$Employment_Education)

# Employment_SocialWork
Summarize(d1.1$R_quant~d1.1$Employment_SocialWork, data = d1.1)
wilcox.test (d1.1$R_quant~d1.1$Employment_SocialWork)

# Employment_Other risk type
Summarize(d1.1$R_quant~ d1.1$Employment_OtherRiskType, data = d1.1)
wilcox.test (d1.1$R_quant~ d1.1$Employment_OtherRiskType)

# Symptoms_past2w_Fever
Summarize(d1.1$R_quant~ d1.1$Symptoms_past2w_Fever, data = d1.1)
wilcox.test (d1.1$R_quant~ d1.1$Symptoms_past2w_Fever)

#Symptoms_past2w_Chills
Summarize(d1.1$R_quant~ d1.1$Symptoms_past2w_Chills, data = d1.1)
wilcox.test (d1.1$R_quant~ d1.1$Symptoms_past2w_Chills)

#Symptoms_past2w_Limbpain
Summarize(d1.1$R_quant~ d1.1$Symptoms_past2w_Limbpain, data = d1.1)
wilcox.test (d1.1$R_quant~ d1.1$Symptoms_past2w_Limbpain)

#Symptoms_past2w_Runningnose
Summarize(d1.1$R_quant~ d1.1$Symptoms_past2w_Runningnose, data = d1.1)
wilcox.test (d1.1$R_quant~ d1.1$Symptoms_past2w_Runningnose)

#Symptoms_past2w_SenseLoss.Smell.Taste.
Summarize(d1.1$R_quant~ d1.1$Symptoms_past2w_SenseLoss.Smell.Taste., data = d1.1)
wilcox.test (d1.1$R_quant~ d1.1$Symptoms_past2w_SenseLoss.Smell.Taste.)

#Symptoms_past2w_Sorethroat
Summarize(d1.1$R_quant~ d1.1$Symptoms_past2w_Sorethroat, data = d1.1)
wilcox.test (d1.1$R_quant~ d1.1$Symptoms_past2w_Sorethroat)

#Symptoms_past2w_Headache
Summarize(d1.1$R_quant~ d1.1$Symptoms_past2w_Headache, data = d1.1)
wilcox.test (d1.1$R_quant~ d1.1$Symptoms_past2w_Headache)


#Symptoms_past2w_Drycough
Summarize(d1.1$R_quant~ d1.1$Symptoms_past2w_Drycough, data = d1.1)
wilcox.test (d1.1$R_quant~ d1.1$Symptoms_past2w_Drycough)

#Symptoms_past2w_Breathlessness
Summarize(d1.1$R_quant~ d1.1$Symptoms_past2w_Breathlessness, data = d1.1)
wilcox.test (d1.1$R_quant~ d1.1$Symptoms_past2w_Breathlessness)

#Symptoms_past2w_Fatigue
Summarize(d1.1$R_quant~ d1.1$Symptoms_past2w_Fatigue, data = d1.1)
wilcox.test (d1.1$R_quant~ d1.1$Symptoms_past2w_Fatigue)

#Symptoms_past2w_diarrhoea
Summarize(d1.1$R_quant~  d1.1$Symptoms_past2w_diarrhoea, data = d1.1)
wilcox.test (d1.1$R_quant~  d1.1$Symptoms_past2w_diarrhoea)

#any_symptoms
Summarize(d1.1$R_quant~  d1.1$any_symptoms, data = d1.1)
wilcox.test (d1.1$R_quant~  d1.1$any_symptoms)

#num_symptoms_pos,
#Categorize them to 1,2,3+
Summarize(d1.1$R_quant~  d1.1$num_symptoms_pos_cat, data = d1.1)
kruskal.test (d1.1$R_quant~  d1.1$num_symptoms_pos_cat)

#HealthStatus_Self_Overall
Summarize(d1.1$R_quant~  d1.1$HealthStatus_Self_Overall_1, data = d1.1)
kruskal.test (d1.1$R_quant~  d1.1$HealthStatus_Self_Overall_1)

#HealthStatus_Self_Diabetes,     
Summarize(d1.1$R_quant~ d1.1$HealthStatus_Self_Diabetes, data = d1.1)
wilcox.test (d1.1$R_quant~  d1.1$HealthStatus_Self_Diabetes)

#HealthStatus_Self_LungDisease  
Summarize(d1.1$R_quant~ d1.1$HealthStatus_Self_LungDisease, data = d1.1)
wilcox.test (d1.1$R_quant~  d1.1$HealthStatus_Self_LungDisease)

#HealthStatus_Self_CardiovascularDisease 
Summarize(d1.1$R_quant~ d1.1$HealthStatus_Self_CardiovascularDisease, data = d1.1)
wilcox.test (d1.1$R_quant~ d1.1$HealthStatus_Self_CardiovascularDisease)

#HealthStatus_Self_Cancer
Summarize(d1.1$R_quant~ d1.1$HealthStatus_Self_Cancer, data = d1.1)
wilcox.test (d1.1$R_quant~ d1.1$HealthStatus_Self_Cancer)

#HealthStatus_Self_Obesity
Summarize(d1.1$R_quant~ d1.1$HealthStatus_Self_Obesity, data = d1.1)
wilcox.test (d1.1$R_quant~ d1.1$HealthStatus_Self_Obesity)

#HealthStatus_Self_Allergies_Skin
Summarize(d1.1$R_quant~ d1.1$HealthStatus_Self_Allergies_Skin, data = d1.1)
wilcox.test (d1.1$R_quant~ d1.1$HealthStatus_Self_Allergies_Skin)

#HealthStatus_Self_Allergies_Respiratory
Summarize(d1.1$R_quant~ d1.1$HealthStatus_Self_Allergies_Respiratory, data = d1.1)
wilcox.test (d1.1$R_quant~ d1.1$HealthStatus_Self_Allergies_Respiratory)

#HealthStatus_Self_HIV
Summarize(d1.1$R_quant~ d1.1$HealthStatus_Self_HIV, data = d1.1)
wilcox.test (d1.1$R_quant~ d1.1$HealthStatus_Self_HIV)

#HealthStatus_Self_OtherChronic
Summarize(d1.1$R_quant~ d1.1$HealthStatus_Self_OtherChronic, data = d1.1)
wilcox.test (d1.1$R_quant~ d1.1$HealthStatus_Self_OtherChronic)

#Medication_Immunosuppressive
Summarize(d1.1$R_quant~  d1.1$Medication_Immunosuppressive, data = d1.1)
wilcox.test (d1.1$R_quant~  d1.1$Medication_Immunosuppressive)

#Medication_Cortisone

Summarize(d1.1$R_quant~ d1.1$Medication_Cortisone, data = d1.1)
wilcox.test (d1.1$R_quant~  d1.1$Medication_Cortisone)

#Medication_Bloodpressure

Summarize(d1.1$R_quant~ d1.1$Medication_Bloodpressure, data = d1.1)
wilcox.test (d1.1$R_quant~ d1.1$Medication_Bloodpressure)

#Vaccination_Influenza2019
Summarize(d1.1$R_quant~ d1.1$Vaccination_Influenza2019, data = d1.1)
wilcox.test (d1.1$R_quant~ d1.1$Vaccination_Influenza2019)


# IgG Values
# Sex
Summarize(IgG_quant~ Sex, data = d1.1)
wilcox.test(d1.1$IgG_quant~d1.1$Sex)

# Age group
Summarize(IgG_quant~ Agegroup, data = d1.1)
kruskal.test (d1.1$IgG_quant~d1.1$Agegroup)

# Country of birth
Summarize(IgG_quant~ Birth_Country, data = d1.1)
kruskal.test (d1.1$IgG_quant~d1.1$Birth_Country)

# Education Level
Summarize(d1.1$IgG_quant~ d1.1$EducationLevel, data = d1.1)
kruskal.test (d1.1$IgG_quant~d1.1$EducationLevel)

# Employment Status
Summarize(d1.1$IgG_quant~ d1.1$EmploymentStatus, data = d1.1)
kruskal.test (d1.1$IgG_quant~d1.1$EmploymentStatus)

# Smoke Status
Summarize(d1.1$IgG_quant~ d1.1$smokestatus, data = d1.1)
kruskal.test (d1.1$IgG_quant~d1.1$smokestatus)

# Employment last year
Summarize(d1.1$IgG_quant~d1.1$Employment_ind_lastyear, data = d1.1)
wilcox.test (d1.1$IgG_quant~d1.1$Employment_ind_lastyear)

# Employment in any risk job
Summarize(d1.1$IgG_quant~d1.1$any_risk_emp, data = d1.1)
wilcox.test (d1.1$IgG_quant~d1.1$any_risk_emp)

# Employment_Airport
Summarize(d1.1$IgG_quant~d1.1$Employment_Airport, data = d1.1)
wilcox.test (d1.1$IgG_quant~d1.1$Employment_Airport)

# Employment_HealthCare
Summarize(d1.1$IgG_quant~d1.1$Employment_HealthCare, data = d1.1)
wilcox.test (d1.1$IgG_quant~d1.1$Employment_HealthCare)

# Employment_EmergencyServices
Summarize(d1.1$IgG_quant~d1.1$Employment_EmergencyServices, data = d1.1)
wilcox.test (d1.1$IgG_quant~d1.1$Employment_EmergencyServices)

# Employment_Sales
Summarize(d1.1$IgG_quant~d1.1$Employment_Sales, data = d1.1)
wilcox.test (d1.1$IgG_quant~d1.1$Employment_Sales)

# Employment_SeniorHome
Summarize(d1.1$IgG_quant~d1.1$Employment_SeniorHome, data = d1.1)
wilcox.test (d1.1$IgG_quant~d1.1$Employment_SeniorHome)

# Employment_Transportation
Summarize(d1.1$IgG_quant~d1.1$Employment_Transportation, data = d1.1)
wilcox.test (d1.1$IgG_quant~d1.1$Employment_Transportation)

# Employment_Education
Summarize(d1.1$IgG_quant~d1.1$Employment_Education, data = d1.1)
wilcox.test (d1.1$IgG_quant~d1.1$Employment_Education)

# Employment_SocialWork
Summarize(d1.1$IgG_quant~d1.1$Employment_SocialWork, data = d1.1)
wilcox.test (d1.1$IgG_quant~d1.1$Employment_SocialWork)

# Employment_Other risk type
Summarize(d1.1$IgG_quant~ d1.1$Employment_OtherRiskType, data = d1.1)
wilcox.test (d1.1$IgG_quant~ d1.1$Employment_OtherRiskType)

# Symptoms_past2w_Fever
Summarize(d1.1$IgG_quant~ d1.1$Symptoms_past2w_Fever, data = d1.1)
wilcox.test (d1.1$IgG_quant~ d1.1$Symptoms_past2w_Fever)

#Symptoms_past2w_Chills
Summarize(d1.1$IgG_quant~ d1.1$Symptoms_past2w_Chills, data = d1.1)
wilcox.test (d1.1$IgG_quant~ d1.1$Symptoms_past2w_Chills)

#Symptoms_past2w_Limbpain
Summarize(d1.1$IgG_quant~ d1.1$Symptoms_past2w_Limbpain, data = d1.1)
wilcox.test (d1.1$IgG_quant~ d1.1$Symptoms_past2w_Limbpain)

#Symptoms_past2w_Runningnose
Summarize(d1.1$IgG_quant~ d1.1$Symptoms_past2w_Runningnose, data = d1.1)
wilcox.test (d1.1$IgG_quant~ d1.1$Symptoms_past2w_Runningnose)

#Symptoms_past2w_SenseLoss.Smell.Taste.
Summarize(d1.1$IgG_quant~ d1.1$Symptoms_past2w_SenseLoss.Smell.Taste., data = d1.1)
wilcox.test (d1.1$IgG_quant~ d1.1$Symptoms_past2w_SenseLoss.Smell.Taste.)

#Symptoms_past2w_Sorethroat
Summarize(d1.1$IgG_quant~ d1.1$Symptoms_past2w_Sorethroat, data = d1.1)
wilcox.test (d1.1$IgG_quant~ d1.1$Symptoms_past2w_Sorethroat)

#Symptoms_past2w_Headache
Summarize(d1.1$IgG_quant~ d1.1$Symptoms_past2w_Headache, data = d1.1)
wilcox.test (d1.1$IgG_quant~ d1.1$Symptoms_past2w_Headache)


#Symptoms_past2w_Drycough
Summarize(d1.1$IgG_quant~ d1.1$Symptoms_past2w_Drycough, data = d1.1)
wilcox.test (d1.1$IgG_quant~ d1.1$Symptoms_past2w_Drycough)

#Symptoms_past2w_Breathlessness
Summarize(d1.1$IgG_quant~ d1.1$Symptoms_past2w_Breathlessness, data = d1.1)
wilcox.test (d1.1$IgG_quant~ d1.1$Symptoms_past2w_Breathlessness)

#Symptoms_past2w_Fatigue
Summarize(d1.1$IgG_quant~ d1.1$Symptoms_past2w_Fatigue, data = d1.1)
wilcox.test (d1.1$IgG_quant~ d1.1$Symptoms_past2w_Fatigue)

#Symptoms_past2w_diarrhoea
Summarize(d1.1$IgG_quant~  d1.1$Symptoms_past2w_diarrhoea, data = d1.1)
wilcox.test (d1.1$IgG_quant~  d1.1$Symptoms_past2w_diarrhoea)

#any_symptoms
Summarize(d1.1$IgG_quant~  d1.1$any_symptoms, data = d1.1)
wilcox.test (d1.1$IgG_quant~  d1.1$any_symptoms)

#num_symptoms_pos,
#Categorize them to 1,2,3+
Summarize(d1.1$IgG_quant~  d1.1$num_symptoms_pos_cat, data = d1.1)
kruskal.test (d1.1$IgG_quant~  d1.1$num_symptoms_pos_cat)

#HealthStatus_Self_Overall
Summarize(d1.1$IgG_quant~  d1.1$HealthStatus_Self_Overall_1, data = d1.1)
kruskal.test (d1.1$IgG_quant~  d1.1$HealthStatus_Self_Overall_1)

#HealthStatus_Self_Diabetes,     
Summarize(d1.1$IgG_quant~ d1.1$HealthStatus_Self_Diabetes, data = d1.1)
wilcox.test (d1.1$IgG_quant~  d1.1$HealthStatus_Self_Diabetes)

#HealthStatus_Self_LungDisease  
Summarize(d1.1$IgG_quant~ d1.1$HealthStatus_Self_LungDisease, data = d1.1)
wilcox.test (d1.1$IgG_quant~  d1.1$HealthStatus_Self_LungDisease)

#HealthStatus_Self_CardiovascularDisease 
Summarize(d1.1$IgG_quant~ d1.1$HealthStatus_Self_CardiovascularDisease, data = d1.1)
wilcox.test (d1.1$IgG_quant~ d1.1$HealthStatus_Self_CardiovascularDisease)

#HealthStatus_Self_Cancer
Summarize(d1.1$IgG_quant~ d1.1$HealthStatus_Self_Cancer, data = d1.1)
wilcox.test (d1.1$IgG_quant~ d1.1$HealthStatus_Self_Cancer)

#HealthStatus_Self_Obesity
Summarize(d1.1$IgG_quant~ d1.1$HealthStatus_Self_Obesity, data = d1.1)
wilcox.test (d1.1$IgG_quant~ d1.1$HealthStatus_Self_Obesity)

#HealthStatus_Self_Allergies_Skin
Summarize(d1.1$IgG_quant~ d1.1$HealthStatus_Self_Allergies_Skin, data = d1.1)
wilcox.test (d1.1$IgG_quant~ d1.1$HealthStatus_Self_Allergies_Skin)

#HealthStatus_Self_Allergies_Respiratory
Summarize(d1.1$IgG_quant~ d1.1$HealthStatus_Self_Allergies_Respiratory, data = d1.1)
wilcox.test (d1.1$IgG_quant~ d1.1$HealthStatus_Self_Allergies_Respiratory)

#HealthStatus_Self_HIV
Summarize(d1.1$IgG_quant~ d1.1$HealthStatus_Self_HIV, data = d1.1)
wilcox.test (d1.1$IgG_quant~ d1.1$HealthStatus_Self_HIV)

#HealthStatus_Self_OtherChronic
Summarize(d1.1$IgG_quant~ d1.1$HealthStatus_Self_OtherChronic, data = d1.1)
wilcox.test (d1.1$IgG_quant~ d1.1$HealthStatus_Self_OtherChronic)

#Medication_Immunosuppressive
Summarize(d1.1$IgG_quant~  d1.1$Medication_Immunosuppressive, data = d1.1)
wilcox.test (d1.1$IgG_quant~  d1.1$Medication_Immunosuppressive)

#Medication_Cortisone

Summarize(d1.1$IgG_quant~ d1.1$Medication_Cortisone, data = d1.1)
wilcox.test (d1.1$IgG_quant~  d1.1$Medication_Cortisone)

#Medication_Bloodpressure

Summarize(d1.1$IgG_quant~ d1.1$Medication_Bloodpressure, data = d1.1)
wilcox.test (d1.1$IgG_quant~ d1.1$Medication_Bloodpressure)

#Vaccination_Influenza2019
Summarize(d1.1$IgG_quant~ d1.1$Vaccination_Influenza2019, data = d1.1)
wilcox.test (d1.1$IgG_quant~ d1.1$Vaccination_Influenza2019)


# IgA Values
# Sex
Summarize(IgA_quant~ Sex, data = d1.1)
wilcox.test(d1.1$IgA_quant~d1.1$Sex)

# Age group
Summarize(IgA_quant~ Agegroup, data = d1.1)
kruskal.test (d1.1$IgA_quant~d1.1$Agegroup)

# Country of birth
Summarize(IgA_quant~ Birth_Country, data = d1.1)
kruskal.test (d1.1$IgA_quant~d1.1$Birth_Country)

# Education Level
Summarize(d1.1$IgA_quant~ d1.1$EducationLevel, data = d1.1)
kruskal.test (d1.1$IgA_quant~d1.1$EducationLevel)

# Employment Status
Summarize(d1.1$IgA_quant~ d1.1$EmploymentStatus, data = d1.1)
kruskal.test (d1.1$IgA_quant~d1.1$EmploymentStatus)

# Smoke Status
Summarize(d1.1$IgA_quant~ d1.1$smokestatus, data = d1.1)
kruskal.test (d1.1$IgA_quant~d1.1$smokestatus)

# Employment last year
Summarize(d1.1$IgA_quant~d1.1$Employment_ind_lastyear, data = d1.1)
wilcox.test (d1.1$IgA_quant~d1.1$Employment_ind_lastyear)

# Employment in any risk job
Summarize(d1.1$IgA_quant~d1.1$any_risk_emp, data = d1.1)
wilcox.test (d1.1$IgA_quant~d1.1$any_risk_emp)

# Employment_Airport
Summarize(d1.1$IgA_quant~d1.1$Employment_Airport, data = d1.1)
wilcox.test (d1.1$IgA_quant~d1.1$Employment_Airport)

# Employment_HealthCare
Summarize(d1.1$IgA_quant~d1.1$Employment_HealthCare, data = d1.1)
wilcox.test (d1.1$IgA_quant~d1.1$Employment_HealthCare)

# Employment_EmergencyServices
Summarize(d1.1$IgA_quant~d1.1$Employment_EmergencyServices, data = d1.1)
wilcox.test (d1.1$IgA_quant~d1.1$Employment_EmergencyServices)

# Employment_Sales
Summarize(d1.1$IgA_quant~d1.1$Employment_Sales, data = d1.1)
wilcox.test (d1.1$IgA_quant~d1.1$Employment_Sales)

# Employment_SeniorHome
Summarize(d1.1$IgA_quant~d1.1$Employment_SeniorHome, data = d1.1)
wilcox.test (d1.1$IgA_quant~d1.1$Employment_SeniorHome)

# Employment_Transportation
Summarize(d1.1$IgA_quant~d1.1$Employment_Transportation, data = d1.1)
wilcox.test (d1.1$IgA_quant~d1.1$Employment_Transportation)

# Employment_Education
Summarize(d1.1$IgA_quant~d1.1$Employment_Education, data = d1.1)
wilcox.test (d1.1$IgA_quant~d1.1$Employment_Education)

# Employment_SocialWork
Summarize(d1.1$IgA_quant~d1.1$Employment_SocialWork, data = d1.1)
wilcox.test (d1.1$IgA_quant~d1.1$Employment_SocialWork)

# Employment_Other risk type
Summarize(d1.1$IgA_quant~ d1.1$Employment_OtherRiskType, data = d1.1)
wilcox.test (d1.1$IgA_quant~ d1.1$Employment_OtherRiskType)

# Symptoms_past2w_Fever
Summarize(d1.1$IgA_quant~ d1.1$Symptoms_past2w_Fever, data = d1.1)
wilcox.test (d1.1$IgA_quant~ d1.1$Symptoms_past2w_Fever)

#Symptoms_past2w_Chills
Summarize(d1.1$IgA_quant~ d1.1$Symptoms_past2w_Chills, data = d1.1)
wilcox.test (d1.1$IgA_quant~ d1.1$Symptoms_past2w_Chills)

#Symptoms_past2w_Limbpain
Summarize(d1.1$IgA_quant~ d1.1$Symptoms_past2w_Limbpain, data = d1.1)
wilcox.test (d1.1$IgA_quant~ d1.1$Symptoms_past2w_Limbpain)

#Symptoms_past2w_Runningnose
Summarize(d1.1$IgA_quant~ d1.1$Symptoms_past2w_Runningnose, data = d1.1)
wilcox.test (d1.1$IgA_quant~ d1.1$Symptoms_past2w_Runningnose)

#Symptoms_past2w_SenseLoss.Smell.Taste.
Summarize(d1.1$IgA_quant~ d1.1$Symptoms_past2w_SenseLoss.Smell.Taste., data = d1.1)
wilcox.test (d1.1$IgA_quant~ d1.1$Symptoms_past2w_SenseLoss.Smell.Taste.)

#Symptoms_past2w_Sorethroat
Summarize(d1.1$IgA_quant~ d1.1$Symptoms_past2w_Sorethroat, data = d1.1)
wilcox.test (d1.1$IgA_quant~ d1.1$Symptoms_past2w_Sorethroat)

#Symptoms_past2w_Headache
Summarize(d1.1$IgA_quant~ d1.1$Symptoms_past2w_Headache, data = d1.1)
wilcox.test (d1.1$IgA_quant~ d1.1$Symptoms_past2w_Headache)


#Symptoms_past2w_Drycough
Summarize(d1.1$IgA_quant~ d1.1$Symptoms_past2w_Drycough, data = d1.1)
wilcox.test (d1.1$IgA_quant~ d1.1$Symptoms_past2w_Drycough)

#Symptoms_past2w_Breathlessness
Summarize(d1.1$IgA_quant~ d1.1$Symptoms_past2w_Breathlessness, data = d1.1)
wilcox.test (d1.1$IgA_quant~ d1.1$Symptoms_past2w_Breathlessness)

#Symptoms_past2w_Fatigue
Summarize(d1.1$IgA_quant~ d1.1$Symptoms_past2w_Fatigue, data = d1.1)
wilcox.test (d1.1$IgA_quant~ d1.1$Symptoms_past2w_Fatigue)

#Symptoms_past2w_diarrhoea
Summarize(d1.1$IgA_quant~  d1.1$Symptoms_past2w_diarrhoea, data = d1.1)
wilcox.test (d1.1$IgA_quant~  d1.1$Symptoms_past2w_diarrhoea)

#any_symptoms
Summarize(d1.1$IgA_quant~  d1.1$any_symptoms, data = d1.1)
wilcox.test (d1.1$IgA_quant~  d1.1$any_symptoms)

#num_symptoms_pos,
#Categorize them to 1,2,3+
Summarize(d1.1$IgA_quant~  d1.1$num_symptoms_pos_cat, data = d1.1)
kruskal.test (d1.1$IgA_quant~  d1.1$num_symptoms_pos_cat)

#HealthStatus_Self_Overall
Summarize(d1.1$IgA_quant~  d1.1$HealthStatus_Self_Overall_1, data = d1.1)
kruskal.test (d1.1$IgA_quant~  d1.1$HealthStatus_Self_Overall_1)

#HealthStatus_Self_Diabetes,     
Summarize(d1.1$IgA_quant~ d1.1$HealthStatus_Self_Diabetes, data = d1.1)
wilcox.test (d1.1$IgA_quant~  d1.1$HealthStatus_Self_Diabetes)

#HealthStatus_Self_LungDisease  
Summarize(d1.1$IgA_quant~ d1.1$HealthStatus_Self_LungDisease, data = d1.1)
wilcox.test (d1.1$IgA_quant~  d1.1$HealthStatus_Self_LungDisease)

#HealthStatus_Self_CardiovascularDisease 
Summarize(d1.1$IgA_quant~ d1.1$HealthStatus_Self_CardiovascularDisease, data = d1.1)
wilcox.test (d1.1$IgA_quant~ d1.1$HealthStatus_Self_CardiovascularDisease)

#HealthStatus_Self_Cancer
Summarize(d1.1$IgA_quant~ d1.1$HealthStatus_Self_Cancer, data = d1.1)
wilcox.test (d1.1$IgA_quant~ d1.1$HealthStatus_Self_Cancer)

#HealthStatus_Self_Obesity
Summarize(d1.1$IgA_quant~ d1.1$HealthStatus_Self_Obesity, data = d1.1)
wilcox.test (d1.1$IgA_quant~ d1.1$HealthStatus_Self_Obesity)

#HealthStatus_Self_Allergies_Skin
Summarize(d1.1$IgA_quant~ d1.1$HealthStatus_Self_Allergies_Skin, data = d1.1)
wilcox.test (d1.1$IgA_quant~ d1.1$HealthStatus_Self_Allergies_Skin)

#HealthStatus_Self_Allergies_Respiratory
Summarize(d1.1$IgA_quant~ d1.1$HealthStatus_Self_Allergies_Respiratory, data = d1.1)
wilcox.test (d1.1$IgA_quant~ d1.1$HealthStatus_Self_Allergies_Respiratory)

#HealthStatus_Self_HIV
Summarize(d1.1$IgA_quant~ d1.1$HealthStatus_Self_HIV, data = d1.1)
wilcox.test (d1.1$IgA_quant~ d1.1$HealthStatus_Self_HIV)

#HealthStatus_Self_OtherChronic
Summarize(d1.1$IgA_quant~ d1.1$HealthStatus_Self_OtherChronic, data = d1.1)
wilcox.test (d1.1$IgA_quant~ d1.1$HealthStatus_Self_OtherChronic)

#Medication_Immunosuppressive
Summarize(d1.1$IgA_quant~  d1.1$Medication_Immunosuppressive, data = d1.1)
wilcox.test (d1.1$IgA_quant~  d1.1$Medication_Immunosuppressive)

#Medication_Cortisone

Summarize(d1.1$IgA_quant~ d1.1$Medication_Cortisone, data = d1.1)
wilcox.test (d1.1$IgA_quant~  d1.1$Medication_Cortisone)

#Medication_Bloodpressure

Summarize(d1.1$IgA_quant~ d1.1$Medication_Bloodpressure, data = d1.1)
wilcox.test (d1.1$IgA_quant~ d1.1$Medication_Bloodpressure)

#Vaccination_Influenza2019
Summarize(d1.1$IgA_quant~ d1.1$Vaccination_Influenza2019, data = d1.1)
wilcox.test (d1.1$IgA_quant~ d1.1$Vaccination_Influenza2019)


# Plot over time

# Roche Positivies
library(epitools)
d$Week_BloodDraw<-paste("Week-",1+floor(difftime(as.Date(d$VisitDate, format = "%Y-%m-%d"),
                                                         as.Date("2020-01-06", format = "%Y-%m-%d"),units="weeks")+1),sep="")

table(d$Week_BloodDraw)
d$Week_BloodDraw<- factor(d$Week_BloodDraw,
                                      levels = c("Week-15","Week-16", "Week-17" ,"Week-18", "Week-19", "Week-20", "Week-21", "Week-22", "Week-23" ,"Week-24",
                                                 "Week-26", "Week-27", "Week-28", "Week-35"),
                                      labels = c("Week-15","Week-16", "Week-17" ,"Week-18", "Week-19", "Week-20", "Week-21", "Week-22", "Week-23+","Week-23+",
                                                 "Week-23+","Week-23+","Week-23+","Week-23+"))

tr<-reshape2::dcast(d,Week_BloodDraw~Roche_Result_old)
tr$Sum<- tr$Negative+tr$Positive

l<- length(tr$Week_BloodDraw)
rate<-rep(0, times=l)
lci<- rep(0,times=l)
uci<- rep(0, times=l)
for(i in 1:l) {
  t<- tr[i,]
  x<- pois.exact(t$Positive,t$Sum,conf.level = 0.95)
  rate[i]<-unlist(x[3])
  lci[i]<-unlist(x[4])
  uci[i]<-unlist(x[5])
}

tr<-cbind(tr,rate,lci,uci)

tg<-reshape2::dcast(d,Week_BloodDraw~IgG_Result_old)
tg$Sum<-tg$Negative+ tg$Positive+ tg$'NA'

l<- length(tg$Week_BloodDraw)
rate<-rep(0, times=l)
lci<- rep(0,times=l)
uci<- rep(0, times=l)
for(i in 1:l) {
  t<- tg[i,]
  x<- pois.exact(t$Positive,t$Sum,conf.level = 0.95)
  rate[i]<-unlist(x[3])
  lci[i]<-unlist(x[4])
  uci[i]<-unlist(x[5])
}

tg<- cbind(tg,rate,lci,uci)

ta<-reshape2::dcast(d,Week_BloodDraw~IgA_result)
ta$Sum<- ta$Intermediate+ta$Negative+ ta$Positive+ ta$'NA'

l<- length(ta$Week_BloodDraw)
rate<-rep(0, times=l)
lci<- rep(0,times=l)
uci<- rep(0, times=l)
for(i in 1:l) {
  t<- ta[i,]
  x<- pois.exact(t$Positive,t$Sum,conf.level = 0.95)
  rate[i]<-unlist(x[3])
  lci[i]<-unlist(x[4])
  uci[i]<-unlist(x[5])
}

ta<- cbind(ta,rate,lci,uci)

ta$Test<- "Euroimmun S1 IgA"
tg$Test<- "Euroimmun S1 IgG"
tr$Test<- "Roche N"

tr$Total<- tr$Sum
ta$Total<- ta$Sum
tg$Total<- tg$Sum
#tr$Positive<- tr$reactive


testing_positivity<- rbind.data.frame(subset(ta, select = c(Test,Week_BloodDraw,Positive,Total,
                                                 rate,lci,uci)),
                           subset(tg, select = c(Test,Week_BloodDraw,Positive,Total,
                                                 rate,lci,uci)),
                           subset(tr, select = c(Test,Week_BloodDraw,Positive,Total,
                                                 rate,lci,uci)))
testing_positivity$rate<- round(testing_positivity$rate,3)
testing_positivity$lci<- round(testing_positivity$lci,3)
testing_positivity$uci<- round(testing_positivity$uci,3)



tr1<-reshape2::dcast(d,Week_BloodDraw~Roche_Result_new)
tr1$Sum<- tr1$Negative+tr1$Positive

l<- length(tr1$Week_BloodDraw)
rate<-rep(0, times=l)
lci<- rep(0,times=l)
uci<- rep(0, times=l)
for(i in 1:l) {
  t<- tr1[i,]
  x<- pois.exact(t$Positive,t$Sum,conf.level = 0.95)
  rate[i]<-unlist(x[3])
  lci[i]<-unlist(x[4])
  uci[i]<-unlist(x[5])
}

tr1<-cbind(tr1,rate,lci,uci)

tg1<-reshape2::dcast(d,Week_BloodDraw~IgG_result_new)
tg1$Sum<-tg1$Negative+ tg1$Positive+ tg1$'NA'

l<- length(tg1$Week_BloodDraw)
rate<-rep(0, times=l)
lci<- rep(0,times=l)
uci<- rep(0, times=l)
for(i in 1:l) {
  t<- tg1[i,]
  x<- pois.exact(t$Positive,t$Sum,conf.level = 0.95)
  rate[i]<-unlist(x[3])
  lci[i]<-unlist(x[4])
  uci[i]<-unlist(x[5])
}

tg1<- cbind(tg1,rate,lci,uci)

d$IgA_Result_new<- ifelse(d$IgA_quant>=1.085,"Positive","Negative")
ta1<-reshape2::dcast(d,Week_BloodDraw~IgA_Result_new)
ta1$Sum<- ta1$Negative+ ta1$Positive+ ta1$'NA'

l<- length(ta1$Week_BloodDraw)
rate<-rep(0, times=l)
lci<- rep(0,times=l)
uci<- rep(0, times=l)
for(i in 1:l) {
  t<- ta1[i,]
  x<- pois.exact(t$Positive,t$Sum,conf.level = 0.95)
  rate[i]<-unlist(x[3])
  lci[i]<-unlist(x[4])
  uci[i]<-unlist(x[5])
}

ta1<- cbind(ta1,rate,lci,uci)

ta1$Test<- "Euroimmun S1 IgA"
tg1$Test<- "Euroimmun S1 IgG"
tr1$Test<- "Roche N"

tr1$Total<- tr$Sum
ta1$Total<- ta$Sum
tg1$Total<- tg$Sum
#tr$Positive<- tr$reactive


testing_positivity1<- rbind.data.frame(subset(ta1, select = c(Test,Week_BloodDraw,Positive,Total,
                                                            rate,lci,uci)),
                                      subset(tg1, select = c(Test,Week_BloodDraw,Positive,Total,
                                                            rate,lci,uci)),
                                      subset(tr1, select = c(Test,Week_BloodDraw,Positive,Total,
                                                            rate,lci,uci)))
testing_positivity1$rate<- round(testing_positivity1$rate,3)
testing_positivity1$lci<- round(testing_positivity1$lci,3)
testing_positivity1$uci<- round(testing_positivity1$uci,3) 

testing_positivity1$Cutoff<- "Optimized cutoff"
testing_positivity$Cutoff<- "Manufacturer cutoff"
testing_positivity<-rbind(testing_positivity,testing_positivity1)
testing_positivity$labels<- paste(testing_positivity$Week_BloodDraw,"\n(n=",testing_positivity$Total,")",sep="")

# seroprevalence estimates
seropest<- read.csv(here_data("seroprevalence estimates.csv"), header = T)
seropest<- seropest[which(seropest$calculation=="weighted"),]

testing_positivity<- merge.data.frame(testing_positivity,seropest, by=c("Cutoff","Test"), all = T)


ggplot(testing_positivity, aes(x=labels,group=Test))+ 
  geom_line(aes(y=rate), colour="blue")+ geom_point(aes(y=rate),colour="blue")+
  geom_line(aes(y=lci), colour="red",linetype=2)+
  geom_line(aes(y=uci), colour="red",linetype=2)+ facet_grid(Test~Cutoff,scales = "free")+
  theme(axis.text.x = element_text(angle = 90))+
  geom_ribbon(aes(ymin = pplci, ymax = ppuci),fill = "grey70", alpha=0.5)+
  geom_line(aes(y=ppest),size=1.2)+
  xlab("Week of Blooddraw")+ ylab("Test positivity rate")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L))

ggsave(here_out("testing_positivity_old_new.jpeg"), height=8, width=10, dpi=720,units="in")
write.csv(testing_positivity, here_out("positivity_perweek.csv")) 

# Roche new Positivity
rrp<- testing_positivity[which(testing_positivity$Test=="Roche N" &
                                 testing_positivity$Cutoff=="Optimized cutoff"),]

rrp<- rbind(rrp,rrp,rrp,rrp)

rrp$group1<- rep(c("Sampling Weighted",
                  "Sampling Weighted",
                  "Unweighted",
                  "Unweighted"),each=9)
rrp$group2<- rep(c("Unadjusted",
                   "Adjusted",
                   "Unadjusted",
                   "Adjusted"),each=9)

roche_sero_estimates<-read.csv(here_data("Roche_sero_estimates.csv"),header = T)

rrp$ppest<- rrp$pplci<-rrp$ppuci<-NULL

rrp<- merge.data.frame(rrp, roche_sero_estimates,by=c("group1","group2"))

rrp$ppest<-rrp$ppest/100
rrp$pplci<-rrp$pplci/100
rrp$ppuci<-rrp$ppuci/100


rrp$group1<-factor(rrp$group1, levels = c("Unweighted","Sampling Weighted"),
                   labels=c("Unweighted","Weighted"))
rrp$group2<- factor(rrp$group2, levels = c("Unadjusted",
                                           "Adjusted"))

before_time<- c("Week-10","Week-11","Week-12","Week-13","Week-14")
before_time<- as.data.frame(before_time)
colnames(before_time)[1]<- "Week_BloodDraw"
before_time$labels<- paste(before_time$Week_BloodDraw,"\n(n=0)", sep="")

rrp$labels1<- str_remove(rrp$labels,"Week-")

rpps<-ggplot(rrp, aes(x=labels1, group=Test))+ 
         
         geom_ribbon(aes(ymin = pplci, ymax = ppuci,fill = "95% CI for overall seroprevalence"), alpha=0.4)+
         geom_point(aes(y=rate, colour="Weekly seroprevalence"))+
         geom_line(aes(y=rate, colour="Weekly seroprevalence"))+ 
         #geom_line(aes(y=lci, colour="red"),linetype=2)+
         #geom_line(aes(y=uci), colour="red",linetype=2)+ 
         geom_ribbon(aes(ymin =lci, ymax = uci,fill = "95% CI for weekly seroprevalence"), alpha=0.4)+
         #scale_x_discrete(labels = c("15","16","17","18","19","20","21","22","23+"))+
         #theme(axis.text.x = element_text(angle = 65))+
         xlab("Week of Blooddraw")+ ylab("Estimated seroprevalence")+
         facet_grid(group1~group2)+
         geom_line(aes(y=ppest, colour="Overall seroprevalence", group=1),size=1.2)+
         scale_y_continuous(n.breaks = 5,labels = scales::percent_format(accuracy = 1L))+
              ggtitle("Seropositivity with Ro-N-Ig (KoCo-19 cohort)")+
         scale_colour_manual(name='',values=c("Overall seroprevalence" = "#000000",
                                              "Weekly seroprevalence" = "#0072B2"))+
         scale_fill_manual(name='', values=c("95% CI for overall seroprevalence" = "grey50",
                                             "95% CI for weekly seroprevalence"= "grey80"))+
         theme(legend.position="bottom")+
          guides(fill=guide_legend(nrow=2,byrow=TRUE),
                 colour=guide_legend(nrow=2,byrow=TRUE))+
  theme(axis.ticks = element_line(colour = "black", size = 2))+
  theme(axis.text =  element_text(colour = "black"))+
  theme(
    strip.text.x = element_text(
      size = 10, color = "black"),
    strip.text.y = element_text(
      size = 10, color = "black")
  )+
  theme(legend.text=element_text(size=12))
         
rpps
ggsave(here_out("rocheseropositivity.jpeg"), height=7, width=10, dpi=720,units="in")

# Final graphic main paper
# Add Manufacturer cut off estimates
rrp1<- rrp[which(rrp$group2=="Adjusted" & rrp$group1=="Weighted"),]

rrp1<- rbind.fill(before_time,rrp1)
rrp1$Test<-"Roche N"
rrp1$Cutoff<-"Optimized cutoff"

rrp2<- testing_positivity[which(testing_positivity$Test=="Roche N" &
                                  testing_positivity$Cutoff=="Manufacturer cutoff"),]



rrp2$ppest<-0.01502033684	
rrp2$pplci<-0.00881812779
rrp2$ppuci<-0.0212225459
rrp2<-rbind.fill(before_time,rrp2)
rrp2$Cutoff<-"Manufacturer cutoff"

# Merge manufacturer and Optimized cut off
#rrp1<-rbind.fill(rrp1,rrp2)

rpps1<-ggplot(rrp1, aes(x=labels, group=Test)) +
  geom_ribbon(aes(ymin = pplci, ymax = ppuci),fill = "grey70", alpha=0.5)+
  geom_point(aes(y=rate), colour="blue")+
  geom_line(aes(y=rate), colour="blue")+ 
  geom_line(aes(y=lci), colour="red",linetype=2)+
  geom_line(aes(y=uci), colour="red",linetype=2)+ 
  theme(axis.text.x = element_text(angle = 90))+
  xlab("Week of Blooddraw")+ ylab("Test positivity rate")+
  #facet_grid(group1~group2)+
  geom_line(aes(y=ppest),size=1.2)+
  #facet_grid(Cutoff~.)+
  scale_y_continuous(n.breaks = 5,labels = scales::percent_format(accuracy = 1L))+
  ggtitle("Seropositivity with Roche N (KOCO19 cohort)")

rpps1
# Symptom prevalence

d$symptom_cat1<- ifelse(d$num_symptoms_pos>=3,"3+ Symptoms","<3 Symptoms")
d$symptom_cat2<- ifelse(d$Symptoms_past2w_SenseLoss.Smell.Taste.=="Yes","Loss of Sense - Smell","No loss of Sense - Smell")
d$symptom_cat3<- ifelse(d$num_symptoms_pos>=1,"Atleast one symptom","No symptoms")

sc1<- reshape2::dcast(d,Week_BloodDraw~ symptom_cat1)
sc2<- reshape2::dcast(d,Week_BloodDraw~ symptom_cat2)
sc3<- reshape2::dcast(d,Week_BloodDraw~ symptom_cat3)

sc1$total<-sc1$`3+ Symptoms`+sc1$`<3 Symptoms`
sc2$total<- sc2$`Loss of Sense - Smell`+ sc2$`No loss of Sense - Smell`+ sc2$'NA'
sc3$total<- sc3$`Atleast one symptom`+ sc3$`No symptoms`

sc1$category<-"Atleast three symptoms"
sc2$category<-"Loss of sense of smell"
sc3$category<-"Having atleast one symptoms"

colnames(sc1)[3]<-"Positives"
colnames(sc2)[2]<-"Positives"
colnames(sc3)[2]<-"Positives"

sc<- rbind(subset(sc1,select = c(Week_BloodDraw,Positives,total,category)),
           subset(sc2,select = c(Week_BloodDraw,Positives,total,category)),
           subset(sc3,select = c(Week_BloodDraw,Positives,total,category)))

l<- length(sc$Week_BloodDraw)
rate<-rep(0, times=l)
lci<- rep(0,times=l)
uci<- rep(0, times=l)
for(i in 1:l) {
  t<- sc[i,]
  x<- pois.exact(t$Positives,t$total,conf.level = 0.95)
  rate[i]<-unlist(x[3])
  lci[i]<-unlist(x[4])
  uci[i]<-unlist(x[5])
}
sc<- cbind(sc,rate,lci,uci)
sc$label<- paste(sc$Week_BloodDraw,"\n(n=",sc$total,")",sep="")


ggplot(sc, aes(x=label,group=category))+ 
  geom_line(aes(y=rate), colour="black")+ geom_point(aes(y=rate))+
  geom_line(aes(y=lci), colour="red",linetype=2)+
  geom_line(aes(y=uci), colour="red",linetype=2)+ facet_grid(.~category,scales = "free")+
  theme(axis.text.x = element_text(angle = 90))+
  xlab("Week of Blooddraw")+ ylab("Symptom positivity rate")+
  scale_y_continuous(labels = scales::percent)

ggsave(here_out("symptom_positivity.jpeg"), height=8, width=11, dpi=720,units="in")
write.csv(sc, here_out("positivity_perweek_symptoms.csv")) 

# Facemask usage

d$web_form_week<-paste("Week-",1+floor(difftime(as.Date(d$Web.form.entry.date, format = "%Y-%m-%d"),
                                                as.Date("2020-01-06", format = "%Y-%m-%d"),units="weeks")+1),sep="")

d$web_form_week<- ifelse(d$web_form_week=="Week-NA",NA, d$web_form_week)

d$web_form_week<- factor(d$web_form_week,
                         levels = c("Week-15", "Week-16", 
                                    "Week-17", "Week-18", "Week-19", "Week-20", "Week-21", "Week-22",
                                    "Week-23", "Week-24", "Week-25", "Week-26", "Week-27", "Week-28" ,
                                    "Week-29", "Week-30", "Week-31", "Week-32", "Week-33", "Week-34"),
                         labels = c("Week-15", "Week-16", 
                                    "Week-17", "Week-18", "Week-19", "Week-20", "Week-21", "Week-22",
                                    "Week-23+", "Week-23+", "Week-23+", "Week-23+", "Week-23+", "Week-23+" ,
                                    "Week-23+", "Week-23+", "Week-23+", "Week-23+", "Week-23+", "Week-23+"))

fp<-reshape2::dcast(d,web_form_week~Facemask_public)
fp<- fp[complete.cases(fp),]

fp$total<- fp$`Almost surely`+fp$Freuqently+ fp$`Rarely or never`+ fp$'NA'

fp1<- reshape2::melt(fp, id=c("web_form_week","Rarely or never","NA","total"))

fp1$label<- paste(fp1$web_form_week,"\n(n=",fp1$total,")",sep="")

l<- length(fp1$web_form_week)
rate<-rep(0, times=l)
lci<- rep(0,times=l)
uci<- rep(0, times=l)
for(i in 1:l) {
  t<- fp1[i,]
  x<- pois.exact(t$value,t$total,conf.level = 0.95)
  rate[i]<-unlist(x[3])
  lci[i]<-unlist(x[4])
  uci[i]<-unlist(x[5])
}
fp1<- cbind(fp1,rate,lci,uci)
before_time_fp<- before_time
colnames(before_time_fp)<-c("web_form_week","label")
before_time_fp$variable<-"Almost surely"
fp1<- rbind.fill(before_time_fp,fp1)

fmask_use<-ggplot(fp1[which(fp1$variable=="Almost surely"),], aes(x=label,group=variable))+ 
  geom_line(aes(y=rate), colour="black")+ geom_point(aes(y=rate))+
  geom_line(aes(y=lci), colour="red",linetype=2)+
  geom_line(aes(y=uci), colour="red",linetype=2)+ 
  #facet_grid(.~variable,scales = "free")+
  theme(axis.text.x = element_text(angle = 90))+
  xlab("Week of data entry in Webform")+ ylab("Rate")+
  scale_y_continuous(breaks = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels = scales::percent)+
  ggtitle("Facemask use at public places (KOCO19 cohort)")
fmask_use
ggsave(here_out("facemaskusage.jpeg"), height=8, width=11, dpi=720,units="in")
write.csv(fp1, here_out("facemask usage_perweek.csv"))


# With age category >70
d$age_cat70<- ifelse(d$Age>70,">70 years","<=70 years")


tr<-reshape2::dcast(d,Week_BloodDraw+age_cat70~Roche_Result_old)
tr$Sum<- tr$Negative+tr$Positive

l<- length(tr$Week_BloodDraw)
rate<-rep(0, times=l)
lci<- rep(0,times=l)
uci<- rep(0, times=l)
for(i in 1:l) {
  t<- tr[i,]
  x<- pois.exact(t$Positive,t$Sum,conf.level = 0.95)
  rate[i]<-unlist(x[3])
  lci[i]<-unlist(x[4])
  uci[i]<-unlist(x[5])
}

tr<-cbind(tr,rate,lci,uci)

tg<-reshape2::dcast(d,Week_BloodDraw+age_cat70~IgG_Result_old)
tg$Sum<-tg$Negative+ tg$Positive+ tg$'NA'

l<- length(tg$Week_BloodDraw)
rate<-rep(0, times=l)
lci<- rep(0,times=l)
uci<- rep(0, times=l)
for(i in 1:l) {
  t<- tg[i,]
  x<- pois.exact(t$Positive,t$Sum,conf.level = 0.95)
  rate[i]<-unlist(x[3])
  lci[i]<-unlist(x[4])
  uci[i]<-unlist(x[5])
}

tg<- cbind(tg,rate,lci,uci)

ta<-reshape2::dcast(d,Week_BloodDraw+age_cat70~IgA_result)
ta$Sum<- ta$Intermediate+ta$Negative+ ta$Positive+ ta$'NA'

l<- length(ta$Week_BloodDraw)
rate<-rep(0, times=l)
lci<- rep(0,times=l)
uci<- rep(0, times=l)
for(i in 1:l) {
  t<- ta[i,]
  x<- pois.exact(t$Positive,t$Sum,conf.level = 0.95)
  rate[i]<-unlist(x[3])
  lci[i]<-unlist(x[4])
  uci[i]<-unlist(x[5])
}

ta<- cbind(ta,rate,lci,uci)

ta$Test<- "Euroimmun S1 IgA"
tg$Test<- "Euroimmun S1 IgG"
tr$Test<- "Roche N"

tr$Total<- tr$Sum
ta$Total<- ta$Sum
tg$Total<- tg$Sum
#tr$Positive<- tr$reactive


testing_positivity<- rbind.data.frame(subset(ta, select = c(Test,age_cat70,Week_BloodDraw,Positive,Total,
                                                            rate,lci,uci)),
                                      subset(tg, select = c(Test,age_cat70,Week_BloodDraw,Positive,Total,
                                                            rate,lci,uci)),
                                      subset(tr, select = c(Test,age_cat70,Week_BloodDraw,Positive,Total,
                                                            rate,lci,uci)))
testing_positivity$rate<- round(testing_positivity$rate,3)
testing_positivity$lci<- round(testing_positivity$lci,3)
testing_positivity$uci<- round(testing_positivity$uci,3)



tr1<-reshape2::dcast(d,Week_BloodDraw+age_cat70~Roche_Result_new)
tr1$Sum<- tr1$Negative+tr1$Positive

l<- length(tr1$Week_BloodDraw)
rate<-rep(0, times=l)
lci<- rep(0,times=l)
uci<- rep(0, times=l)
for(i in 1:l) {
  t<- tr1[i,]
  x<- pois.exact(t$Positive,t$Sum,conf.level = 0.95)
  rate[i]<-unlist(x[3])
  lci[i]<-unlist(x[4])
  uci[i]<-unlist(x[5])
}

tr1<-cbind(tr1,rate,lci,uci)

tg1<-reshape2::dcast(d,Week_BloodDraw+age_cat70~IgG_result_new)
tg1$Sum<-tg1$Negative+ tg1$Positive+ tg1$'NA'

l<- length(tg1$Week_BloodDraw)
rate<-rep(0, times=l)
lci<- rep(0,times=l)
uci<- rep(0, times=l)
for(i in 1:l) {
  t<- tg1[i,]
  x<- pois.exact(t$Positive,t$Sum,conf.level = 0.95)
  rate[i]<-unlist(x[3])
  lci[i]<-unlist(x[4])
  uci[i]<-unlist(x[5])
}

tg1<- cbind(tg1,rate,lci,uci)

d$IgA_Result_new<- ifelse(d$IgA_quant>=1.085,"Positive","Negative")
ta1<-reshape2::dcast(d,Week_BloodDraw+age_cat70~IgA_Result_new)
ta1$Sum<- ta1$Negative+ ta1$Positive+ ta1$'NA'

l<- length(ta1$Week_BloodDraw)
rate<-rep(0, times=l)
lci<- rep(0,times=l)
uci<- rep(0, times=l)
for(i in 1:l) {
  t<- ta1[i,]
  x<- pois.exact(t$Positive,t$Sum,conf.level = 0.95)
  rate[i]<-unlist(x[3])
  lci[i]<-unlist(x[4])
  uci[i]<-unlist(x[5])
}

ta1<- cbind(ta1,rate,lci,uci)

ta1$Test<- "Euroimmun S1 IgA"
tg1$Test<- "Euroimmun S1 IgG"
tr1$Test<- "Roche N"

tr1$Total<- tr$Sum
ta1$Total<- ta$Sum
tg1$Total<- tg$Sum
#tr$Positive<- tr$reactive


testing_positivity1<- rbind.data.frame(subset(ta1, select = c(Test,age_cat70,Week_BloodDraw,Positive,Total,
                                                              rate,lci,uci)),
                                       subset(tg1, select = c(Test,age_cat70,Week_BloodDraw,Positive,Total,
                                                              rate,lci,uci)),
                                       subset(tr1, select = c(Test,age_cat70,Week_BloodDraw,Positive,Total,
                                                              rate,lci,uci)))
testing_positivity1$rate<- round(testing_positivity1$rate,3)
testing_positivity1$lci<- round(testing_positivity1$lci,3)
testing_positivity1$uci<- round(testing_positivity1$uci,3) 

testing_positivity1$Cutoff<- "Optimized cutoff"
testing_positivity$Cutoff<- "Manufacturer cutoff"
testing_positivity<-rbind(testing_positivity,testing_positivity1)
testing_positivity$labels<- paste(testing_positivity$Week_BloodDraw,"\n(n=",testing_positivity$Total,")",sep="")

ggplot(testing_positivity, aes(x=labels,group=Test))+ 
  geom_line(aes(y=rate), colour="black")+ geom_point(aes(y=rate))+
  geom_line(aes(y=lci), colour="red",linetype=2)+
  geom_line(aes(y=uci), colour="red",linetype=2)+ facet_grid(Test~Cutoff+age_cat70,scales = "free")+
  theme(axis.text.x = element_text(angle = 90))+
  xlab("Week of Blooddraw")+ ylab("Test positivity rate")+
  scale_y_continuous(labels = scales::percent)

ggsave(here_out("testing_positivity_agecat.jpeg"), height=8, width=10, dpi=720,units="in")
#write.csv(testing_positivity, here_out("positivity_perweek.csv"))


# Symptom prevalence

d$symptom_cat1<- ifelse(d$num_symptoms_pos>=3,"3+ Symptoms","<3 Symptoms")
d$symptom_cat2<- ifelse(d$Symptoms_past2w_SenseLoss.Smell.Taste.=="Yes","Loss of Sense - Smell","No loss of Sense - Smell")
d$symptom_cat3<- ifelse(d$num_symptoms_pos>=1,"Atleast one symptom","No symptoms")

sc1<- reshape2::dcast(d,Week_BloodDraw+age_cat70~ symptom_cat1)
sc2<- reshape2::dcast(d,Week_BloodDraw+age_cat70~ symptom_cat2)
sc3<- reshape2::dcast(d,Week_BloodDraw+age_cat70~ symptom_cat3)

sc1$total<-sc1$`3+ Symptoms`+sc1$`<3 Symptoms`
sc2$total<- sc2$`Loss of Sense - Smell`+ sc2$`No loss of Sense - Smell`+ sc2$'NA'
sc3$total<- sc3$`Atleast one symptom`+ sc3$`No symptoms`

sc1$category<-"Atleast three symptoms"
sc2$category<-"Loss of sense of smell"
sc3$category<-"Having atleast one symptoms"

colnames(sc1)[4]<-"Positives"
colnames(sc2)[3]<-"Positives"
colnames(sc3)[3]<-"Positives"

sc<- rbind(subset(sc1,select = c(Week_BloodDraw,age_cat70,Positives,total,category)),
           subset(sc2,select = c(Week_BloodDraw,age_cat70,Positives,total,category)),
           subset(sc3,select = c(Week_BloodDraw,age_cat70,Positives,total,category)))

l<- length(sc$Week_BloodDraw)
rate<-rep(0, times=l)
lci<- rep(0,times=l)
uci<- rep(0, times=l)
for(i in 1:l) {
  t<- sc[i,]
  x<- pois.exact(t$Positives,t$total,conf.level = 0.95)
  rate[i]<-unlist(x[3])
  lci[i]<-unlist(x[4])
  uci[i]<-unlist(x[5])
}
sc<- cbind(sc,rate,lci,uci)
sc$label<- paste(sc$Week_BloodDraw,"\n(n=",sc$total,")",sep="")


ggplot(sc, aes(x=label,group=category))+ 
  geom_line(aes(y=rate), colour="black")+ geom_point(aes(y=rate))+
  geom_line(aes(y=lci), colour="red",linetype=2)+
  geom_line(aes(y=uci), colour="red",linetype=2)+ facet_grid(category~age_cat70,scales = "free")+
  theme(axis.text.x = element_text(angle = 90))+
  xlab("Week of Blooddraw")+ ylab("Symptom positivity rate")+
  scale_y_continuous(labels = scales::percent)

ggsave(here_out("symptom_positivity_agecat.jpeg"), height=8, width=11, dpi=720,units="in")
#write.csv(sc, here_out("positivity_perweek_symptoms.csv"))

# Facemask usage
fp<- reshape2::dcast(d,web_form_week+age_cat70~Facemask_public)

fp<-fp[complete.cases(fp),]

fp$total<- fp$`Almost surely`+fp$Freuqently+ fp$`Rarely or never`+ fp$'NA'

fp1<- melt(fp, id=c("web_form_week","age_cat70","Rarely or never","NA","total"))

fp1$label<- paste(fp1$web_form_week,"\n(n=",fp1$total,")",sep="")

l<- length(fp1$web_form_week)
rate<-rep(0, times=l)
lci<- rep(0,times=l)
uci<- rep(0, times=l)
for(i in 1:l) {
  t<- fp1[i,]
  x<- pois.exact(t$value,t$total,conf.level = 0.95)
  rate[i]<-unlist(x[3])
  lci[i]<-unlist(x[4])
  uci[i]<-unlist(x[5])
}
fp1<- cbind(fp1,rate,lci,uci)

fmaskuse<-ggplot(fp1[which(fp1$variable=="Almost surely"),], aes(x=label,group=variable))+ 
  geom_line(aes(y=rate), colour="black")+ geom_point(aes(y=rate))+
  geom_line(aes(y=lci), colour="red",linetype=2)+
  geom_line(aes(y=uci), colour="red",linetype=2)+ facet_grid(variable~age_cat70,scales = "free")+
  theme(axis.text.x = element_text(angle = 90))+
  xlab("Week of Blooddraw")+ ylab("Rate of facemask\nusage in public")+
  scale_y_continuous(labels = scales::percent)
fmaskuse
ggsave(here_out("facemaskusage_agecat.jpeg"), height=8, width=11, dpi=720,units="in")
#write.csv(fp1, here_out("facemask usage_perweek.csv"))


# Munich Cases
muc_cases<- read.csv(here_data("muc_data_cases.csv"), header = T, na.strings=c("","NA"))
muc_cases[2,1]<-"08-Mar"
muc_cases[15,1]<-"22-Mar"
muc_cases[1,3]<-0
muc_cases[is.na(muc_cases$Verstorben),]$Verstorben<-0

muc_cases$Datum.<- paste(muc_cases$Datum.,"-2020", sep="")
muc_cases$Date<- as.Date(as.character(muc_cases$Datum.),format = c("%d-%b-%Y"))
muc_cases<- muc_cases[which(muc_cases$Date< "2020-06-06"),]
muc_cases$year_week<-paste("Week-",1+floor(difftime(as.Date(muc_cases$Date, format = "%Y-%m-%d"),
                                                    as.Date("2020-01-06", format = "%Y-%m-%d"),units="weeks")+1),sep="")
muc_cases$Genesen<- na.locf(muc_cases$Genesen)
muc_cases$Gesamt<- na.locf(muc_cases$Gesamt)
muc_cases$Aktuell.infiziert<- na.locf(muc_cases$Aktuell.infiziert)

l<-unique(muc_cases$year_week)
muc_week<-NULL
for ( i in l) {
  t<- muc_cases[which(muc_cases$year_week==i),]
  cum_deaths<- max(t$Verstorben, na.rm = T)
  cum_cases<- max(t$Gesamt,na.rm = T)
  x<- cbind(i,cum_deaths, cum_cases)
  muc_week<- rbind.data.frame(muc_week,x)
}

colnames(muc_week)[1]<- "week"

muc_week$week<-factor(muc_week$week)
muc_week$cum_deaths<-as.numeric(muc_week$cum_deaths)
muc_week$cum_cases<- as.numeric(muc_week$cum_cases)

l<- length(muc_week$week)
new_inf<-rep(0, times=l)
new_deaths<- rep(0, times=l)
for (i in 1:l) {
  if(i>1) {
    new_deaths[i]<- muc_week$cum_deaths[i] - muc_week$cum_deaths[i-1]
    new_inf[i]<- muc_week$cum_cases[i]- muc_week$cum_cases[i-1]
  }
}

muc_week<- cbind(muc_week,new_deaths,new_inf)

muc_week$inc_week<- (muc_week$new_inf/1456039)*100000
muc_week$cuminc_week<- (muc_week$cum_cases/1456039)*100000

muc_week$week<- factor(muc_week$week,
                       labels = c("Week-10", "Week-11", "Week-12", "Week-13", "Week-14", "Week-15",
                                  "Week-16", "Week-17", "Week-18", "Week-19", "Week-20", "Week-21",
                                  "Week-22" ,"Week-23+"))

# recruitment data
rec_dat<-reshape2::dcast(d,Week_BloodDraw~.)
rec_dat$cum_recruitment<- CUMULATIVE_SUM(rec_dat$.)
names(rec_dat)<-c("week","ind","cum_ind")

muc_week<- merge.data.frame(muc_week,rec_dat, by="week", all = T)

coeff<- round(max(muc_week$cuminc_week)/max(muc_week$new_inf),2)
inc_muc<-ggplot(muc_week, aes(x=week))+ 
  geom_bar (aes(y=new_inf,fill='#56B4E9'),stat="identity",alpha=0.65)+ 
  geom_bar (aes(y=ind, fill='#D55E00'),stat="identity",alpha=0.75)+
  theme(axis.text.x = element_text(angle = 90))+
  xlab("Week of 2020")+
  scale_fill_discrete(name = "Covid Scenario in Munich", labels = c( "Incidence in Munich",
                                                                     "KOCO19 Recruitment"))+
  geom_line( aes(y=cuminc_week /coeff, group=1), size=2, color="#D55E00") +
  scale_y_continuous(
    
    # Features of the first axis
    name = "# Individuals",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Cumulative incidence in Munich per\n 100000 person week (dark red)")
  ) +
  theme(legend.position="bottom")+
  ggtitle("COVID incidence vs cohort recruitment")+
  guides(colour=guide_legend(nrow = 2,byrow=TRUE))+
  theme(
    legend.title = element_text( size = 8),
    legend.text = element_text( size = 8)
  )
 # guides(fill=guide_legend(nrow = 2,byrow=TRUE))
inc_muc
ggsave(here_out("incidencemuc_koco.jpeg"),units = "in",width = 7, height=7, dpi=720)
# Mortality Graphics

mort_dat<- read.csv(here_data("mor_muc.csv"), header = T)

mort_dat<- mort_dat[which(mort_dat$X<24),] # restrict it to week 23
mort_dat$week<- ifelse(mort_dat$X==23,"Week-23+",paste("Week-",mort_dat$X, sep=""))
mort_dat$excess_deaths<- mort_dat$Y2020- mort_dat$Mean



mort_dat<- merge.data.frame(mort_dat,muc_week[c(1,4,5)], by="week")
mort_dat$week<- factor(mort_dat$week)
mort_dat$y1<- mort_dat$Y2020
mort_dat$y2<-mort_dat$Mean
mort_long<- reshape2::melt(mort_dat, id=c("week","X","Mean_2016_20",
                                 "y1","y2","excess_deaths","new_inf"))

mort_long$variable<- factor(mort_long$variable, levels = c("Y2016","Y2017","Y2018","Y2019","Y2020","Mean", "new_deaths"),
                            labels = c("Reported deaths 2016","Reported deaths 2017","Reported deaths 2018","Reported deaths 2019",
                                       "Reported deaths 2020","Mean deaths 2016-19",
                                       "Reported deaths COVID"))
cols <- c("Reported deaths COVID"="#999999","Mean deaths 2016-19"="#000000",
          "Reported deaths 2020"= "#E69F00",
          "Reported deaths 2019"="#56B4E9",
          "Reported deaths 2018"="#009E73",
          "Reported deaths 2017"="#F0E442",
          "Reported deaths 2016"="#0072B2",
          "COVID Incidence"="#D55E00")
coeff1<- round(max(mort_long$new_inf)/max(mort_long$value),2)
mor_muc<-ggplot(mort_long, aes(x=X))+
  geom_ribbon(aes(ymin=y1, ymax=y2),fill="grey90")+
  geom_line(aes(y=value,colour=variable),linetype=1, size=1.2)+
  scale_colour_manual(values = cols)+
  geom_line( aes(y=new_inf /coeff1, group=1),linetype=2, size=.9,alpha=1, color="#D55E00")+
  xlab("Week of 2020")+
  scale_y_continuous(
    
    # Features of the first axis
    name = "# Deaths",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff1, name="# COVID cases\n(dashed red)")
  )+
  geom_hline(yintercept = 0,linetype=2)+
    scale_x_continuous(breaks = c(10,11,12,13,14,15,16,17,18,19,20,21,22,23), labels = mort_dat$week)+
    theme(axis.text.x = element_text(angle = 90))+
  theme(legend.position="bottom")+
  ggtitle("Weekly situation in Munich")+labs(colour=NULL)+
  labs(fill="Excess deaths 2020")+
 guides(colour=guide_legend(nrow = 2,byrow=TRUE))+
  theme(
    legend.title = element_text( size = 8),
    legend.text = element_text( size = 8)
  )
mor_muc
ggsave(here_out("mortality_muc_koco.jpeg"),units = "in",width = 7, height=7, dpi=720)



library(patchwork)
(mor_muc+inc_muc) /(fmask_use+ rpps1) + plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 14))
ggsave(here_out("combined_epigraphs.jpeg"),units = "in",width = 13, height=9, dpi=720)
library(svglite)
ggsave(here_out("combined_epigraphs.svg"),units = "in",width = 13, height=9, dpi=720)
ggsave(here_out("combined_epigraphs.pdf"),units = "in",width = 13, height=9, dpi=720)


ggplot(lab_dat[which(!is.na(lab_dat$IgA_result_m)),],
       aes(y= IgA_quant, x=Testing_positive))+geom_boxplot()+
  xlab("Past PCR Testing")+facet_grid(.~IgA_result_m)+theme(axis.text.x = element_text(angle = 90))+
  geom_hline(yintercept = 1.1, linetype=2)
       
ggsave(here_out("labpaper_q7_IgA.jpeg"),units = "in",width = 9, height=9, dpi=720)

ggplot(lab_dat[which(!is.na(lab_dat$IgG_result_m)),],
       aes(y= IgG_quant, x=Testing_positive))+geom_boxplot()+
  xlab("Past PCR Testing")+facet_grid(.~IgG_result_m)+theme(axis.text.x = element_text(angle = 90))+
  geom_hline(yintercept = 1.1, linetype=2)

ggsave(here_out("labpaper_q7_IgG.jpeg"),units = "in",width = 9, height=9, dpi=720)

ggplot(lab_dat[which(!is.na(lab_dat$R_Result)),],
       aes(y= R_quant, x=Testing_positive))+geom_boxplot()+
  xlab("Past PCR Testing")+facet_grid(.~R_Result)+theme(axis.text.x = element_text(angle = 90))+
  geom_hline(yintercept = 1, linetype=2)

ggsave(here_out("labpaper_q7_Roche.jpeg"),units = "in",width = 9, height=9, dpi=720)
       
table(lab_dat$IgA_result)


# Box plot of the Roche, IgG, IgA values over weeks
stat_box_data_R <- function(y, upper_limit = max(log10(d$R_quant)) *1.2) {
  return( 
    data.frame(
      y = 1 * upper_limit,
      label = paste('n =', length(y), '\n',
                    'M =', round(median(y), 1), '\n',
                    'Mu =',round(mean(y), 1))
    )
  )
}

stat_box_data_IgG <- function(y, upper_limit = max(log10(d$IgG_quant),na.rm = T) * 1.2) {
  return( 
    data.frame(
      y = 1 * upper_limit,
      label = paste('n =', length(y), '\n',
                    'M =', round(median(y), 1), '\n',
                    'Mu =',round(mean(y), 1))
    )
  )
}

stat_box_data_IgA <- function(y, upper_limit = max(log10(d$IgA_quant), na.rm = T) *1.7) {
  return( 
    data.frame(
      y = 1 * upper_limit,
      label = paste('n =', length(y), '\n',
                    'M =', round(median(y), 1), '\n',
                    'Mu =',round(mean(y), 1))
    )
  )
}

d$Roche_Result_old <- factor(d$Roche_Result_old, levels=c( "Positive","Negative"))
d$Roche_Result_new<- factor(d$Roche_Result_new, levels=c("Positive","Negative"))
d$IgG_Result_old<- factor(d$IgG_Result_old, levels=c("Positive","Negative"))
d$IgG_result_new<- factor(d$IgG_result_new, levels=c("Positive","Negative"))

r1p<-ggplot(d[which(!is.na(d$R_quant)),],
       aes(y= log(R_quant), x=Week_BloodDraw))+
  geom_violin(trim = FALSE, color="darkgrey")+
  geom_boxplot(aes(colour=Roche_Result_old),
                                                            width=0.1, alpha=0.2)+ 
  geom_jitter(aes(colour=Roche_Result_old),
              position=position_jitter(0.2), shape = 21, 
              alpha=0.5, size = 1)+
  stat_summary(fun=mean, geom='point', shape=21, size=2, color="black")+
  xlab("Week of Blooddraw")+theme(axis.text.x = element_text(angle = 90))+
  geom_hline(yintercept = 0, linetype=2)+ facet_grid(Roche_Result_old~., scales = "free")+
  scale_color_manual(labels=c("Positive","Negative"),values=c("Negative"="#009E73",  "Positive"="#CC6666"), na.value="grey")+
  stat_summary(
    fun.data = stat_box_data_R, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9, size=3
  ) +
  theme(legend.position="bottom")+labs(colour="Roche Result (Manufacturer cutoff)")+
  ylab("Roche N (natural log scaled)")
r1p
ggsave(here_out("Roche_boxplot_time_oldcutoff.jpeg"),units = "in",width = 9, height=9, dpi=720)


igg1p<-ggplot(d[which(!is.na(d$IgG_quant)),],
       aes(y=log(IgG_quant), x=Week_BloodDraw))+geom_violin(trim = FALSE, color="darkgrey")+
  geom_boxplot(aes(colour=IgG_Result_old),width=0.1, alpha=0.2)+
  geom_jitter(aes(colour=IgG_Result_old),position=position_jitter(0.2), shape = 21, 
              alpha=0.5, size = 1)+
  stat_summary(fun=mean, geom='point', shape=21, size=2, color="black")+
  xlab("Week of Blooddraw")+theme(axis.text.x = element_text(angle = 90))+
  geom_hline(yintercept = log(1.1), linetype=2)+ facet_grid(IgG_Result_old~., scales = "free")+
  scale_color_manual(labels=c("Positive","Negative"),values=c("Negative"="#009E73",  "Positive"="#CC6666"), na.value="grey")+
  stat_summary(
    fun.data = stat_box_data_IgG, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9, size=3
  ) +
  theme(legend.position="bottom")+labs(colour="Euroimmun IgG Result (Manufacturer cutoff)")+
  ylab("Euroimmun S1 IgG (natural log scaled)")

igg1p
ggsave(here_out("IgG_boxplot_time_oldcutoff.jpeg"),units = "in",width = 9, height=9, dpi=720)

d$IgA_result_m<- factor(d$IgA_result_m,levels = c("Positive","Not Positive"),
                        labels = c("Positive","Negative"))

iga1p<-ggplot(d[which(!is.na(d$IgA_quant)),],
       aes(y= log(IgA_quant), x=Week_BloodDraw))+geom_violin(trim = FALSE, color="darkgrey")+
  geom_boxplot(aes(colour=IgA_result_m),width=0.1, alpha=0.2)+ 
  geom_jitter(aes(colour=IgA_result_m),position=position_jitter(0.2), shape = 21, 
              alpha=0.5, size = 1)+
  stat_summary(fun=mean, geom='point', shape=21, size=2, color="black")+
  xlab("Week of Blooddraw")+theme(axis.text.x = element_text(angle = 90))+
  geom_hline(yintercept = log(1.1), linetype=2)+ facet_grid(IgA_result_m~., scales = "free")+
  scale_color_manual(labels=c("Positive","Negative"),values=c("Negative"="#009E73",  "Positive"="#CC6666"), na.value="grey")+
  stat_summary(
    fun.data = stat_box_data_IgA, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9, size=3
  ) +
  theme(legend.position="bottom")+labs(colour="Euroimmun IgA Result (Manufacturer cutoff)")+
  ylab("Euroimmun S1 IgA (natural log scaled)")
iga1p
ggsave(here_out("IgA_boxplot_time_oldcutoff.jpeg"),units = "in",width = 9, height=9, dpi=720)


r1<-ggplot(d[which(!is.na(d$R_quant)),],
       aes(y= log10(R_quant), x=Week_BloodDraw))+
  geom_flat_violin( position = position_nudge(x = 0, y = 0),alpha = .3,color="darkgrey", fill="darkgrey",
                    adjust =2, trim = F, scale = "area")+
  #geom_point(position = position_nudge(x=-0.025), shape = 21, size = 2)+
  geom_boxplot(width=0.1, alpha=0.2,outlier.shape = NA)+
  #geom_jitter(position=position_jitter(0.25), shape = 21, fill = "#56B4E9",
  #             color = "#56B4E9", size = 1)+
  stat_compare_means(aes(label = ifelse( p < 2.e-16, "p < 0.0001", 
                                         ifelse( p < 1.e-2,
                                                 sprintf("p = %2.1e", as.numeric(..p.format..)), 
                                                 sprintf("p = %5.4f", as.numeric(..p.format..))))),
                     label.y = 2,size=3)+
  
  geom_jitter(aes(x = as.numeric(Week_BloodDraw) - 0.1,
                  y = log10(R_quant)),
              width = 0.1 - 0.25 * 0.2,
              height = 0,shape = 21, fill = "lightgray",
              color = "darkgrey", size = 1,alpha=0.5) +
  
  
  #geom_violin(trim = FALSE, color="darkgrey")+
  #geom_boxplot(width=0.1, alpha=0.2)+
  #geom_jitter(position=position_jitter(0.2), shape = 21, fill = "lightgray",
  #            color = "darkgrey",alpha=0.5, size = 1)+
  stat_summary(fun=mean, geom='point', shape=21, size=2, color="darkred", fill="darkred")+
  xlab("Week of Blooddraw")+theme(axis.text.x = element_text(angle = 90))+
  stat_summary(
    fun.data = stat_box_data_R, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9, size=3
  ) +
  theme(legend.position="bottom")+
  ylab("Ro-N-Ig (log10 scaled)")+
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.01, color="#CCCCCC"))
r1
ggsave(here_out("Roche_boxplot_time_all.jpeg"),units = "in",width = 9, height=9, dpi=720)


igg1<-ggplot(d[which(!is.na(d$IgG_quant)),],
       aes(y= log10(IgG_quant), x=Week_BloodDraw))+
  geom_flat_violin( position = position_nudge(x = 0, y = 0),alpha = .3,color="darkgrey", fill="darkgrey",
                    adjust =2, trim = F, scale = "area")+
  #geom_point(position = position_nudge(x=-0.025), shape = 21, size = 2)+
  geom_boxplot(width=0.1, alpha=0.2,outlier.shape = NA)+
  #geom_jitter(position=position_jitter(0.25), shape = 21, fill = "#56B4E9",
  #             color = "#56B4E9", size = 1)+
  geom_jitter(aes(x = as.numeric(Week_BloodDraw) - 0.1,
                  y = log10(IgG_quant)),
              width = 0.1 - 0.25 * 0.2,
              height = 0,shape = 21, fill = "lightgray",
              color = "darkgrey", size = 1,alpha=0.5) +
  stat_compare_means(aes(label = ifelse( p < 2.e-16, "p < 0.0001", 
                                         ifelse( p < 1.e-2,
                                                 sprintf("p = %2.1e", as.numeric(..p.format..)), 
                                                 sprintf("p = %5.4f", as.numeric(..p.format..))))),label.y = 1.05,size=3)+
  
  #geom_violin(trim = FALSE, color="darkgrey")+
  #geom_boxplot(width=0.1, alpha=0.2)+
  #geom_jitter(position=position_jitter(0.2), shape = 21, fill = "lightgray",
  #            color = "darkgrey",alpha=0.5, size = 1)+
  stat_summary(fun=mean, geom='point', shape=21, size=2, color="darkred", fill="darkred")+
  xlab("Week of Blooddraw")+theme(axis.text.x = element_text(angle = 90))+
  stat_summary(
    fun.data = stat_box_data_IgG, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9, size=3
  ) +
  theme(legend.position="bottom")+
  ylab("EI-S1-IgG (log10 scaled)")+
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.01, color="#CCCCCC"))
igg1
ggsave(here_out("IgG_boxplot_time_all.jpeg"),units = "in",width = 9, height=9, dpi=720)


iga1<-ggplot(d[which(!is.na(d$IgA_quant)),],
       aes(y= log10(IgA_quant), x=Week_BloodDraw))+
  geom_flat_violin( position = position_nudge(x = 0, y = 0),alpha = .3,color="darkgrey", fill="darkgrey",
                    adjust =2, trim = F, scale = "area")+
  #geom_point(position = position_nudge(x=-0.025), shape = 21, size = 2)+
  geom_boxplot(width=0.1, alpha=0.2,outlier.shape = NA)+
  #geom_jitter(position=position_jitter(0.25), shape = 21, fill = "#56B4E9",
  #             color = "#56B4E9", size = 1)+
  geom_jitter(aes(x = as.numeric(Week_BloodDraw) - 0.1,
                  y = log10(IgA_quant)),
              width = 0.1 - 0.25 * 0.2,
              height = 0,shape = 21, fill = "lightgray",
              color = "darkgrey", size = 1,alpha=0.5) +
  
  #geom_violin(trim = FALSE, color="darkgrey")+
  #geom_boxplot(width=0.1, alpha=0.2)+
  #geom_jitter(position=position_jitter(0.2), shape = 21, fill = "lightgray",
  #            color = "darkgrey",alpha=0.5, size = 1)+
  stat_compare_means(aes(label = ifelse( p < 2.e-16, "p < 0.0001", 
                                         ifelse( p < 1.e-2,
                                                 sprintf("p = %2.1e", as.numeric(..p.format..)), 
                                                 sprintf("p = %5.4f", as.numeric(..p.format..))))),label.y = 1.4,size=3)+
  stat_summary(fun=mean, geom='point', shape=21, size=2, color="darkred", fill="darkred")+
  stat_summary(
    fun.data = stat_box_data_IgA, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9, size=3
  ) +
  theme(legend.position="bottom")+
  ylab("EI-S1-IgA (log10 scaled)")+
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.01, color="#CCCCCC"))
iga1
ggsave(here_out("IgA_boxplot_time_all.jpeg"),units = "in",width = 9, height=9, dpi=720)

ind_long_data$R_Result_new<- ifelse(ind_long_data$R_quant>=0.4218,"Positive","Not Positive")
ind_long_data$IgG_result_new<- ifelse(ind_long_data$IgG_quant>=1.015,"Positive","Not Positive")
ind_long_data$IgA_result_new<- ifelse(ind_long_data$IgA_quant>=1.085,"Positive","Not Positive")
d$IgA_result_new<- ifelse(d$IgA_quant>=1.085,"Positive","Negative")
d$IgA_result_new<- factor(d$IgA_Result_new,levels=c("Positive","Negative"))
r2p<-ggplot(d[which(!is.na(d$R_quant)),],
       aes(y= log(R_quant), x=Week_BloodDraw))+geom_violin(trim = FALSE, color="darkgrey")+
  geom_boxplot(aes(colour=Roche_Result_new),width=0.1, alpha=0.2)+ 
  geom_jitter(aes(colour=Roche_Result_new),
              position=position_jitter(0.2), shape = 21, 
              alpha=0.5, size = 1)+
  stat_summary(fun=mean, geom='point', shape=21, size=2, color="black")+
  xlab("Week of Blooddraw")+theme(axis.text.x = element_text(angle = 90))+
  geom_hline(yintercept = log(0.4218), linetype=2)+ facet_grid(Roche_Result_new~., scales = "free")+
  scale_color_manual(labels=c("Positive","Negative"),values=c("Negative"="#009E73",  "Positive"="#CC6666"), na.value="grey")+
  stat_summary(
    fun.data = stat_box_data_R, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9, size=3
  ) +
  theme(legend.position="bottom")+labs(colour="Roche Result (Optimized cutoff)")+
  ylab("Roche N (log10 scaled)")
r2p
ggsave(here_out("Roche_boxplot_time_newcutoff.jpeg"),units = "in",width = 9, height=9, dpi=720)


igg2p<-ggplot(d[which(!is.na(d$IgG_quant)),],
       aes(y=log(IgG_quant), x=Week_BloodDraw))+geom_violin(trim = FALSE, color="darkgrey")+
  geom_boxplot(aes(colour=IgG_result_new),width=0.1, alpha=0.2)+
  geom_jitter(aes(colour=IgG_result_new),
              position=position_jitter(0.2), shape = 21, 
              alpha=0.5, size = 1)+
  xlab("Week of Blooddraw")+theme(axis.text.x = element_text(angle = 90))+
  stat_summary(fun=mean, geom='point', shape=21, size=2, color="black")+
  geom_hline(yintercept = log(1.015), linetype=2)+ facet_grid(IgG_result_new~., scales = "free")+
  scale_color_manual(labels=c("Positive","Negative"),values=c("Negative"="#009E73",  "Positive"="#CC6666"), na.value="grey")+
  stat_summary(
    fun.data = stat_box_data_IgG, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9, size=3
  ) +
  theme(legend.position="bottom")+labs(colour="Euroimmun IgG Result (Optimized cutoff)")+
  ylab("Euroimmun S1 IgG\n(natural log scaled)")

igg2p
ggsave(here_out("IgG_boxplot_time_newcutoff.jpeg"),units = "in",width = 9, height=9, dpi=720)

iga2p<-ggplot(d[which(!is.na(d$IgA_quant)),],
       aes(y= log(IgA_quant), x=Week_BloodDraw))+geom_violin(trim = FALSE, color="darkgrey")+
  geom_boxplot(aes(colour=IgA_result_new),width=0.1, alpha=0.2)+ 
  geom_jitter(aes(colour=IgA_result_new),position=position_jitter(0.2), shape = 21,
              alpha=0.5, size = 1)+
  xlab("Week of Blooddraw")+theme(axis.text.x = element_text(angle = 90))+
  geom_hline(yintercept = log(1.085), linetype=2)+ facet_grid(IgA_Result_new~., scales = "free")+
  stat_summary(fun=mean, geom='point', shape=21, size=2, color="black")+
  scale_color_manual(labels=c("Positive","Negative"),values=c("Negative"="#009E73",  "Positive"="#CC6666"), na.value="grey")+
  stat_summary(
    fun.data = stat_box_data_IgA, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9, size=3
  ) +
  theme(legend.position="bottom")+labs(colour="Euroimmun IgA Result (Optimized cutoff)")+
  ylab("Euroimmun S1 IgA\n(natural log scaled)")
iga2p
ggsave(here_out("IgA_boxplot_time_newcutoff.jpeg"),units = "in",width = 9, height=9, dpi=720)

# Box plots in grids


stat_box_data_R <- function(y, upper_limit = max(log10(d$R_quant),na.rm = T) *3.2) {
  return( 
    data.frame(
      y = 1 * upper_limit,
      label = paste('n =', length(y), '\n',
                    'M =', round(median(y), 1), '\n',
                    'Mu =',round(mean(y), 1))
    )
  )
}

stat_box_data_IgG <- function(y, upper_limit = max(log10(d$IgG_quant),na.rm = T) * 2.8) {
  return( 
    data.frame(
      y = 1 * upper_limit,
      label = paste('n =', length(y), '\n',
                    'M =', round(median(y), 1), '\n',
                    'Mu =',round(mean(y), 1))
    )
  )
}

stat_box_data_IgA <- function(y, upper_limit = max(log10(d$IgA_quant), na.rm = T) *2.7) {
  return( 
    data.frame(
      y = 1 * upper_limit,
      label = paste('n =', length(y), '\n',
                    'M =', round(median(y), 1), '\n',
                    'Mu =',round(mean(y), 1))
    )
  )
}



# Roche Long
r.dat.new<- subset(d, select = c(Week_BloodDraw,R_quant,Roche_Result_new))
r.dat.old<- subset(d, select = c(Week_BloodDraw,R_quant,Roche_Result_old))
colnames(r.dat.new)[3]<-colnames(r.dat.old)[3]<- "Result"
r.dat.new$cutoff<-"Optimized-Cutoff"
r.dat.old$cutoff<-"Manufacturer-Cutoff"

r.dat.new$cut<-log10(0.4218)
r.dat.old$cut<-0
r.long<- rbind.data.frame(r.dat.new,r.dat.old)

r.long$Result<- factor(r.long$Result,levels = c("Positive","Negative"))

rcp<-ggplot(r.long,
            aes(y= log10(R_quant), x=Week_BloodDraw))+
  
  #geom_point(position = position_nudge(x=-0.025), shape = 21, size = 2)+
  geom_boxplot(aes(colour=Result),width=0.1, alpha=0.2,outlier.shape = NA)+
  #geom_jitter(position=position_jitter(0.25), shape = 21, fill = "#56B4E9",
  #             color = "#56B4E9", size = 1)+
  geom_jitter(aes(x = as.numeric(Week_BloodDraw) - 0.1,
                  y = log10(R_quant),colour=Result,fill = Result),
              width = 0.1 - 0.25 * 0.2,
              height = 0,shape = 21, 
               size = 1,alpha=0.5) +
  facet_grid(cutoff+Result~., scales = "free_y")+
  
  #geom_violin(trim = FALSE, color="darkgrey")+
  #geom_boxplot(aes(colour=Result),width=0.1, alpha=0.2)+ 
  #geom_jitter(aes(colour=Result),
  #            position=position_jitter(0.2), shape = 21, 
  #            alpha=0.5, size = 1)+
  stat_summary(fun=mean, geom='point', shape=21, size=2, color="black")+
  xlab("Week of Blooddraw")+theme(axis.text.x = element_text(angle = 90))+
  stat_compare_means(aes(label = ifelse( p < 2.e-16, "p < 0.0001", 
                                         ifelse( p < 1.e-2,
                                                 sprintf("p = %2.1e", as.numeric(..p.format..)), 
                                                 sprintf("p = %5.4f", as.numeric(..p.format..))))),label.y = 3.4, size=3)+
  scale_color_manual(labels=c("Positive","Negative"),values=c("Negative"="#009E73",  "Positive"="#CC6666"), na.value="grey")+
  stat_summary(
    fun.data = stat_box_data_R, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9, size=3
  ) +
  theme(legend.position="bottom")+ theme(legend.position = "none")+
  ylab("Ro-N-Ig (log10 scaled)")+
  geom_line(aes(y = cut, group=cutoff), linetype=2)+
  geom_flat_violin( aes(color=Result, fill=Result),position = position_nudge(x = 0, y = 0),alpha = .3,
                    adjust =2, trim = F, scale = "area")+
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.01, color="#CCCCCC"))+
  theme(strip.background =element_rect(fill="white"))
rcp
ggsave(here_out("Roche_boxplot_diff_newcutoff.jpeg"),units = "in",width = 10, height=10, dpi=720)


# IgG Long
igg.dat.new<- subset(d, select = c(Week_BloodDraw,IgG_quant,IgG_result_new))
igg.dat.old<- subset(d, select = c(Week_BloodDraw,IgG_quant,IgG_Result_old))
colnames(igg.dat.new)[3]<-colnames(igg.dat.old)[3]<- "Result"
igg.dat.new$cutoff<-"Optimized-Cutoff"
igg.dat.old$cutoff<-"Manufacturer-Cutoff"

igg.dat.new$cut<-log10(1.015)
igg.dat.old$cut<-log10(1.1)
igg.long<- rbind.data.frame(igg.dat.new,igg.dat.old)
igg.long$Result<- factor(igg.long$Result,levels = c("Positive","Negative"))
igcp<-ggplot(igg.long[which(!is.na(igg.long$IgG_quant)),],
            aes(y= log10(IgG_quant), x=Week_BloodDraw))+
  geom_flat_violin( aes(color=Result, fill=Result),position = position_nudge(x = 0, y = 0),alpha = .3,
                    adjust =2, trim = F, scale = "area")+
  #geom_point(position = position_nudge(x=-0.025), shape = 21, size = 2)+
  geom_boxplot(aes(colour=Result),width=0.1, alpha=0.2,outlier.shape = NA)+
  #geom_jitter(position=position_jitter(0.25), shape = 21, fill = "#56B4E9",
  #             color = "#56B4E9", size = 1)+
  geom_jitter(aes(x = as.numeric(Week_BloodDraw) - 0.1,
                  y = log10(IgG_quant),colour=Result,fill = Result),
              width = 0.1 - 0.25 * 0.2,
              height = 0,shape = 21, 
              size = 1,alpha=0.5) +
  #geom_violin(trim = FALSE, color="darkgrey")+
  #geom_boxplot(aes(colour=Result),width=0.1, alpha=0.2)+ 
  #geom_jitter(aes(colour=Result),
  #            position=position_jitter(0.2), shape = 21, 
  #            alpha=0.5, size = 1)+
  stat_summary(fun=mean, geom='point', shape=21, size=2, color="black")+
  xlab("Week of Blooddraw")+theme(axis.text.x = element_text(angle = 90))+
  stat_compare_means(aes(label = ifelse( p < 2.e-16, "p < 0.0001", 
                                         ifelse( p < 1.e-2,
                                                 sprintf("p = %2.1e", as.numeric(..p.format..)), 
                                                 sprintf("p = %5.4f", as.numeric(..p.format..))))),label.y = 1.75, size=3)+
  scale_color_manual(labels=c("Positive","Negative"),values=c("Negative"="#009E73",  "Positive"="#CC6666"), na.value="grey")+
  stat_summary(
    fun.data = stat_box_data_IgG, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9, size=3
  ) +
  theme(legend.position="bottom")+theme(legend.position = "none")+
  ylab("EI-S1-IgG (log10 scaled)")+facet_grid(cutoff+Result~., scales = "free")+
  geom_line(aes(y = cut, group=cutoff), linetype=2)+
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.01, color="#CCCCCC"))+
  theme(strip.background =element_rect(fill="white"))
igcp
ggsave(here_out("IgG_boxplot_diff_newcutoff.jpeg"),units = "in",width = 10, height=10, dpi=720)


# IgA Long
iga.dat.new<- subset(d, select = c(Week_BloodDraw,IgA_quant,IgA_Result_new))
iga.dat.old<- subset(d, select = c(Week_BloodDraw,IgA_quant,IgA_result_m))
colnames(iga.dat.new)[3]<-colnames(iga.dat.old)[3]<- "Result"
iga.dat.new$cutoff<-"Optimized-Cutoff"
iga.dat.old$cutoff<-"Manufacturer-Cutoff"

iga.dat.new$cut<-log10(1.085)
iga.dat.old$cut<-log10(1.1)
iga.long<- rbind.data.frame(iga.dat.new,iga.dat.old)
iga.long$Result<-factor(iga.long$Result, levels = c("Positive","Negative"))
iacp<-ggplot(iga.long[which(!is.na(iga.long$IgA_quant)),],
             aes(y= log10(IgA_quant), x=Week_BloodDraw))+
  geom_flat_violin( aes(color=Result, fill=Result),position = position_nudge(x = 0, y = 0),alpha = .3,
                    adjust =2, trim = F, scale = "area")+
  #geom_point(position = position_nudge(x=-0.025), shape = 21, size = 2)+
  geom_boxplot(aes(colour=Result),width=0.1, alpha=0.2,outlier.shape = NA)+
  #geom_jitter(position=position_jitter(0.25), shape = 21, fill = "#56B4E9",
  #             color = "#56B4E9", size = 1)+
  geom_jitter(aes(x = as.numeric(Week_BloodDraw) - 0.1,
                  y = log10(IgA_quant),colour=Result,fill = Result),
              width = 0.1 - 0.25 * 0.2,
              height = 0,shape = 21, 
              size = 1,alpha=0.5) +
  
  #geom_violin(trim = FALSE, color="darkgrey")+
  #geom_boxplot(aes(colour=Result),width=0.1, alpha=0.2)+ 
  #geom_jitter(aes(colour=Result),
  #            position=position_jitter(0.2), shape = 21, 
  #            alpha=0.5, size = 1)+
  stat_summary(fun=mean, geom='point', shape=21, size=2, color="black")+
  stat_compare_means(aes(label = ifelse( p < 2.e-16, "p < 0.0001", 
                                         ifelse( p < 1.e-2,
                                                 sprintf("p = %2.1e", as.numeric(..p.format..)), 
                                                 sprintf("p = %5.4f", as.numeric(..p.format..))))),label.y = 1.45, size=3)+
  xlab("Week of Blooddraw")+theme(axis.text.x = element_text(angle = 90))+
  scale_color_manual(labels=c("Positive","Negative"),values=c("Negative"="#009E73",  "Positive"="#CC6666"), na.value="grey")+
  stat_summary(
    fun.data = stat_box_data_IgA, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9, size=3
  ) +
  theme(legend.position="bottom")+theme(legend.position = "none")+
  ylab("EI-S1-IgA (log10 scaled)")+facet_grid(cutoff+Result~., scales = "free")+
  geom_line(aes(y = cut, group=cutoff), linetype=2)+theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.01, color="#CCCCCC"))+
  theme(strip.background =element_rect(fill="white"))
iacp
ggsave(here_out("IgA_boxplot_diff_newcutoff.jpeg"),units = "in",width = 10, height=10, dpi=720)


r3p<- r1+rcp+plot_annotation(tag_levels = 'A')
r3p
ggsave(here_out("Roche_comb_KOCO19.png"),units = "in",width = 12, height=9)
ggsave(here_out("Roche_comb_KOCO19.jpeg"),units = "in",width = 12, height=9, dpi=720)
ggsave(here_out("Roche_comb_KOCO19.pdf"),units = "in",width = 12, height=9, dpi=720)
ggsave(here_out("Roche_comb_KOCO19.svg"),units = "in",width = 12, height=9, dpi=720)
igg3p<- igg1 + igcp+plot_annotation(tag_levels = 'A')
igg3p
ggsave(here_out("Igg_comb_KOCO19.png"),units = "in",width = 12, height=9)
ggsave(here_out("Igg_comb_KOCO19.jpeg"),units = "in",width = 12, height=9, dpi=720)
ggsave(here_out("Igg_comb_KOCO19.svg"),units = "in",width = 12, height=9, dpi=720)
ggsave(here_out("Igg_comb_KOCO19.pdf"),units = "in",width = 12, height=9, dpi=720)
iga3p<- iga1 + iacp+plot_annotation(tag_levels = 'A')
iga3p
ggsave(here_out("Iga_comb_KOCO19.png"),units = "in",width = 12, height=9)
ggsave(here_out("Iga_comb_KOCO19.jpeg"),units = "in",width = 12, height=9, dpi=720)
ggsave(here_out("Iga_comb_KOCO19.pdf"),units = "in",width = 12, height=9, dpi=720)
ggsave(here_out("Iga_comb_KOCO19.svg"),units = "in",width = 12, height=9, dpi=720)


# Pairwise adjusted comparisons
dunnTest(R_quant~Week_BloodDraw, d[which(d$Roche_Result_new=="Negative"),])
dunnTest(R_quant~Week_BloodDraw, d[which(d$Roche_Result_old=="Negative"),])


# Roche New

d$R_Result_1<- ifelse(d$Roche_Result_new=="Positive",1,0)
model_pois <- glmmTMB(R_Result_1 ~ Week_BloodDraw + (1|hh_id),
                      ziformula=~1,
                     family=poisson,d)
summary(model_pois)
sjPlot::plot_model(model_pois, digits = 4,
                   digits.p = 3,
                   axis.labels=c("Week-23+",
                                 "Week-22",
                                 "Week-21",
                                 "Week-20",
                                 "Week-19",
                                 "Week-18",
                                 "Week-17",
                                 "Week-16"),
                   show.values=TRUE, show.p=TRUE,
                   title="Effect on Roche positivity adjusting for Household Clustering")
sjPlot::tab_model(model_pois, digits = 4,
                  digits.p = 3,
                  show.re.var= TRUE, 
                  pred.labels =c("Intercept","Week-16",
                                 "Week-17",
                                 "Week-18",
                                 "Week-19",
                                 "Week-20",
                                 "Week-21",
                                 "Week-22",
                                 "Week-23+"),
                  dv.labels= "Effect on Roche positivity")

# Negative Binomial
model_nb <- glmmTMB(R_Result_1 ~ Week_BloodDraw + (1|hh_id),
                      family=nbinom1,d)
summary(model_nb)
sjPlot::plot_model(model_nb, digits = 4,
                   digits.p = 3,
                   axis.labels=c("Week-23+",
                                 "Week-22",
                                 "Week-21",
                                 "Week-20",
                                 "Week-19",
                                 "Week-18",
                                 "Week-17",
                                 "Week-16"),
                   show.values=TRUE, show.p=TRUE,
                   title="Effect on Roche positivity adjusting for Household Clustering")
sjPlot::tab_model(model_nb, digits = 4,
                  digits.p = 3,
                  show.re.var= TRUE, 
                  pred.labels =c("Intercept","Week-16",
                                 "Week-17",
                                 "Week-18",
                                 "Week-19",
                                 "Week-20",
                                 "Week-21",
                                 "Week-22",
                                 "Week-23+"),
                  dv.labels= "Effect on Roche positivity")
# Roche Old
d$R_Result_0<- ifelse(d$Roche_Result_old=="Positive",1,0)
model_pois <- glmmTMB(R_Result_0 ~ Week_BloodDraw + (1|hh_id),
                      ziformula=~1,
                      family=poisson,d)
summary(model_pois)
sjPlot::plot_model(model_pois, digits = 4,
                   digits.p = 3,
                   axis.labels=c("Week-23+",
                                 "Week-22",
                                 "Week-21",
                                 "Week-20",
                                 "Week-19",
                                 "Week-18",
                                 "Week-17",
                                 "Week-16"),
                   show.values=TRUE, show.p=TRUE,
                   title="Effect on Roche positivity adjusting for Household Clustering")
sjPlot::tab_model(model_pois, digits = 4,
                  digits.p = 3,
                  show.re.var= TRUE, 
                  pred.labels =c("Intercept","Week-16",
                                 "Week-17",
                                 "Week-18",
                                 "Week-19",
                                 "Week-20",
                                 "Week-21",
                                 "Week-22",
                                 "Week-23+"),
                  dv.labels= "Effect on Roche positivity")

# Negative Binomial
model_nb <- glmmTMB(R_Result_0 ~ Week_BloodDraw + (1|hh_id),
                    family=nbinom1,d)
summary(model_nb)
sjPlot::plot_model(model_nb, digits = 4,
                   digits.p = 3,
                   axis.labels=c("Week-23+",
                                 "Week-22",
                                 "Week-21",
                                 "Week-20",
                                 "Week-19",
                                 "Week-18",
                                 "Week-17",
                                 "Week-16"),
                   show.values=TRUE, show.p=TRUE,
                   title="Effect on Roche positivity adjusting for Household Clustering")
sjPlot::tab_model(model_nb, digits = 4,
                  digits.p = 3,
                  show.re.var= TRUE, 
                  pred.labels =c("Intercept","Week-16",
                                 "Week-17",
                                 "Week-18",
                                 "Week-19",
                                 "Week-20",
                                 "Week-21",
                                 "Week-22",
                                 "Week-23+"),
                  dv.labels= "Effect on Roche positivity")

# IgG new
d$IgG_Result_1<- ifelse(d$IgG_result_new=="Positive",1,0)
model_pois <- glmmTMB(IgG_Result_1 ~ Week_BloodDraw + (1|hh_id),
                      ziformula=~1,
                      family=poisson,d)
summary(model_pois)
sjPlot::plot_model(model_pois, digits = 4,
                   digits.p = 3,
                   axis.labels=c("Week-23+",
                                 "Week-22",
                                 "Week-21",
                                 "Week-20",
                                 "Week-19",
                                 "Week-18",
                                 "Week-17",
                                 "Week-16"),
                   show.values=TRUE, show.p=TRUE,
                   title="Effect on IgG positivity adjusting for Household Clustering")
sjPlot::tab_model(model_pois, digits = 4,
                  digits.p = 3,
                  show.re.var= TRUE, 
                  pred.labels =c("Intercept","Week-16",
                                 "Week-17",
                                 "Week-18",
                                 "Week-19",
                                 "Week-20",
                                 "Week-21",
                                 "Week-22",
                                 "Week-23+"),
                  dv.labels= "Effect on IgG positivity")

# Negative Binomial
model_nb <- glmmTMB(R_Result_1 ~ Week_BloodDraw + (1|hh_id),
                    family=nbinom1,d)
summary(model_nb)
sjPlot::plot_model(model_nb, digits = 4,
                   digits.p = 3,
                   axis.labels=c("Week-23+",
                                 "Week-22",
                                 "Week-21",
                                 "Week-20",
                                 "Week-19",
                                 "Week-18",
                                 "Week-17",
                                 "Week-16"),
                   show.values=TRUE, show.p=TRUE,
                   title="Effect on IgG positivity adjusting for Household Clustering")
sjPlot::tab_model(model_nb, digits = 4,
                  digits.p = 3,
                  show.re.var= TRUE, 
                  pred.labels =c("Intercept","Week-16",
                                 "Week-17",
                                 "Week-18",
                                 "Week-19",
                                 "Week-20",
                                 "Week-21",
                                 "Week-22",
                                 "Week-23+"),
                  dv.labels= "Effect on IgG positivity")




hw_Roche<- reshape2::dcast(ind_long_data, hh_id+Week_BloodDraw+HouseholdSize~R_Result)


fit_zipoisson <- glmmTMB(reactive~Week_BloodDraw+
                           offset(log(HouseholdSize)),
                         data=hw_Roche,
                         ziformula=~1,
                         family=poisson)
summary(fit_zipoisson)


fit_zip<-zeroinfl(reactive~
                     offset(log(HouseholdSize))| Week_BloodDraw, data = hw_Roche)
summary(fit_zip)

fit_zinb<-zeroinfl(reactive~
                    offset(log(HouseholdSize))| Week_BloodDraw, data = hw_Roche,dist = "negbin")
summary(fit_zinb)


# selected individuals mismatch IgG Roche
selected<- ind_long_data[which((ind_long_data$IgG_result=="Positive" & 
                                 ind_long_data$R_Result!="reactive") | 
                                 (ind_long_data$IgG_result!="Positive" & 
                                    ind_long_data$R_Result=="reactive")),] 

sel<- subset(selected,select=c(ind_id,hh_id,VisitDate,BloodID,IgA_quant,IgG_quant, R_quant,IgA_result,
                               IgG_result,R_Result))
sel$duplicated_hh<- duplicated(sel$hh_id)

r_pos<- reshape2:: dcast(ind_long_data, hh_id+HouseholdSize~R_Result)
r_pos<- r_pos[c(1,2,4)]


igg_pos<-reshape2:: dcast(ind_long_data, hh_id+HouseholdSize~IgG_result)
igg_pos<-igg_pos[c(1,2,5)]

sel<- merge.data.frame(sel, r_pos,by="hh_id")
sel<- merge.data.frame(sel, igg_pos,by=c("hh_id","HouseholdSize"))

colnames(sel)[13]<-"Roche_positive"
colnames(sel)[14]<-"IgG_Positive"

write.csv(sel, here_out("mismatch_info.csv"))

sel$hh_transmission<- ifelse(sel$Roche_positive>1 | sel$IgG_Positive>1,"Potential Transmission","Singular Event")
 sel[which(sel$hh_transmission=="Potential Transmission"),]$hh_id

 
 # Missing Data
library(visdat)
library(JointAI)
# Select important variables
dat_imp<- subset(d1.1[which(!is.na(d1.1$R_quant)),],select=c(hh_id,ind_id,Sex,Agegroup,Birth_Country,EducationLevel,EmploymentStatus,
                               smokestatus,Employment_ind_lastyear,
                               any_risk_emp,Employment_Airport,
                               Employment_Education,Employment_EmergencyServices,
                               Employment_HealthCare, Employment_Sales,Employment_SeniorHome,
                               Employment_SocialWork,Employment_Transportation,Employment_OtherRiskType,
                               NetIncome_monthly_1,
                               Household_Type_1,Housing_Type,
                               LivingArea_perInhabitant_cat,num_symptoms_pos_cat,
                               HealthStatus_Self_Overall_1,
                               R_quant,IgA_quant,IgG_quant)) 

dat_imp1<- subset(d1.1[which(!is.na(d1.1$R_quant)),],select=c(Sex,Agegroup,Birth_Country,EducationLevel,EmploymentStatus,
                                                                                     smokestatus,Employment_ind_lastyear,
                                                                                     any_risk_emp,
                                                                                     Household_Type_1,Housing_Type,
                                                                                     LivingArea_perInhabitant_cat,num_symptoms_pos_cat,
                                                                                     HealthStatus_Self_Overall_1,
                                                                                     R_quant,IgA_quant,IgG_quant)) 
dat_imp2<- subset(d1.1[which(!is.na(d1.1$R_quant)),],select=c(Sex,Agegroup,Birth_Country,EducationLevel,EmploymentStatus,
                                                              smokestatus,Employment_ind_lastyear,
                                                              any_risk_emp,
                                                              Household_Type_1,Housing_Type,
                                                              LivingArea_perInhabitant_cat,num_symptoms_pos_cat,
                                                              HealthStatus_Self_Overall_1,
                                                              R_quant,IgA_quant,IgG_quant,NetIncome_monthly_1)) 
reshape2::dcast(dat_imp, Sex~., margins = T)
reshape2::dcast(dat_imp, Agegroup~., margins = T)
reshape2::dcast(dat_imp, Birth_Country~., margins = T)
reshape2::dcast(dat_imp, EducationLevel~., margins = T)
reshape2::dcast(dat_imp, EmploymentStatus~., margins = T)
reshape2::dcast(dat_imp, smokestatus~., margins = T)

reshape2::dcast(dat_imp, Employment_ind_lastyear+EmploymentStatus~., margins = T)
reshape2::dcast(dat_imp, Employment_ind_lastyear+EmploymentStatus~any_risk_emp, margins = T)
reshape2::dcast(dat_imp, Employment_ind_lastyear+EmploymentStatus~Employment_Airport, margins = T)
reshape2::dcast(dat_imp, Employment_ind_lastyear+EmploymentStatus~Employment_HealthCare, margins = T)
reshape2::dcast(dat_imp, Employment_ind_lastyear+EmploymentStatus~Employment_EmergencyServices, margins = T)
reshape2::dcast(dat_imp, Employment_ind_lastyear+EmploymentStatus~Employment_Sales, margins = T)
reshape2::dcast(dat_imp, Employment_ind_lastyear+EmploymentStatus~Employment_SeniorHome, margins = T)
reshape2::dcast(dat_imp, Employment_ind_lastyear+EmploymentStatus~Employment_Transportation, margins = T)
reshape2::dcast(dat_imp, Employment_ind_lastyear+EmploymentStatus~Employment_Education, margins = T)
reshape2::dcast(dat_imp, Employment_ind_lastyear+EmploymentStatus~Employment_SocialWork, margins = T)
reshape2::dcast(dat_imp, Employment_ind_lastyear+EmploymentStatus~Employment_OtherRiskType, margins = T)

reshape2::dcast(d2.2[which((d2.2$hh_id%in% dat_imp$hh_id)==TRUE),] , NetIncome_monthly_1~., margins = T)
reshape2::dcast(d2.2[which((d2.2$hh_id%in% dat_imp$hh_id)==TRUE),] , Household_Type_1~., margins = T)
reshape2::dcast(d2.2[which((d2.2$hh_id%in% dat_imp$hh_id)==TRUE),] , Household_Inhabitants~., margins = T)
reshape2::dcast(d2.2[which((d2.2$hh_id%in% dat_imp$hh_id)==TRUE),] , Housing_Type~., margins = T)
reshape2::dcast(d2.2[which((d2.2$hh_id%in% dat_imp$hh_id)==TRUE),] , LivingArea_perInhabitant_cat~., margins = T)

colnames(dat_imp1)<-c("Sex" , "Age_Category","Birth_Country",            
                      "Education_Level","CurrentEmployment_Status","Smoking_Status" ,                
                      "Employment_Pastyear",      "Any_Risk_Employment","Household_Type",
                      "Housing_Type","LivingArea_perInhabitant", "NumofSymptoms_Positive",        
                      "HealthStatus_SelfReported",  "R_Quantitative","IgA_Quantitative",                   
                      "IgG_Quantitative")

colnames(dat_imp2)<-c("Sex" , "Age_Category","Birth_Country",            
                      "Education_Level","CurrentEmployment_Status","Smoking_Status" ,                
                      "Employment_Pastyear",      "Any_Risk_Employment","Household_Type",
                      "Housing_Type","LivingArea_perInhabitant", "NumofSymptoms_Positive",        
                      "HealthStatus_SelfReported",  "R_Quantitative","IgA_Quantitative",                   
                      "IgG_Quantitative","Net_MonthlyIncome")


# Missing pattern by cluster 
vis_miss(dat_imp1, cluster = TRUE)

ggsave(here_out("Missingpattern_cluster.jpeg"),height=8, width = 8, units = "in", dpi=720)

vis_miss(dat_imp1, sort_miss = TRUE)
ggsave(here_out("Missingpattern_sort.jpeg"),height=8, width = 8, units = "in", dpi=720)
md_pattern(dat_imp1,color = c(grDevices::grey(0.1), grDevices::grey(0.7)),
           border = grDevices::grey(0.5),pattern = FALSE)
ggsave(here_out("Missingpattern.jpeg"),height=8, width =8, units = "in", dpi=720)


md_pattern(dat_imp2,color = c(grDevices::grey(0.1), grDevices::grey(0.7)),
           border = grDevices::grey(0.5),pattern = FALSE)
ggsave(here_out("Missingpattern_incIncome.jpeg"),height=8, width =8, units = "in", dpi=720)


# Add health variables to the list
d1.1<- merge.data.frame(d1.1, subset(d,select = c(ind_id,Facemask_public)),
                        by="ind_id")
d1.1$Sex<-factor(d1.1$Sex)
d1.1$Sex<- relevel(d1.1$Sex,ref="Male")
d1.1$Facemask_public<-factor(d1.1$Facemask_public, levels = c("Almost surely","Freuqently","Rarely or never"),
                             labels = c("Almost surely","Frequently/Rarely","Frequently/Rarely"))

d1.1$Facemask_public<- relevel(d1.1$Facemask_public,ref="Almost surely")

# Multivariate Outcomes for IgG Quant
d1.1$R_Result<- factor(d1.1$R_Result)


d1.1$Sex<-factor(d1.1$Sex)
d1.1$Agegroup<-factor(d1.1$Agegroup)
d1.1$Birth_Country<-factor(d1.1$Birth_Country)
d1.1$EducationLevel<-factor(d1.1$EducationLevel)
d1.1$EmploymentStatus<-factor(d1.1$EmploymentStatus)
d1.1$any_risk_emp<-factor(d1.1$any_risk_emp)
d1.1$Household_Type_1<-factor(d1.1$Household_Type_1)
d1.1$num_symptoms_pos_cat<-factor(d1.1$num_symptoms_pos_cat)
d1.1$HealthStatus_Self_Overall_1<-factor(d1.1$HealthStatus_Self_Overall_1)
d1.1$Household_Inhabitants<-factor(d1.1$Household_Inhabitants)
d1.1$HealthStatus_Self_Overall_1<- factor(d1.1$HealthStatus_Self_Overall_1,levels = c("excellent","very good",
                                                                                      "good","not good"))

d1.1$NetIncome_monthly_1<-relevel(d1.1$NetIncome_monthly_1, ref = "2500-4000")
d1.1$Agegroup<-relevel(d1.1$Agegroup, ref = "20-34")

result<-subset(d,select = c(ind_id,Roche_Result_old,Roche_Result_new,
                            IgG_Result_old,IgG_result_new,
                            IgA_Result_new,IgA_result))

d1.1<- merge.data.frame(d1.1, result,by="ind_id" )


ind_characteristics$telephone_id<- as.numeric(substr(ind_characteristics$pers_id,1,4))
ind_characteristics$tel_res<- ifelse(ind_characteristics$telephone_id==8999,"Yes","No")

ind_tel<- ind_characteristics[which(ind_characteristics$tel_res=="Yes"),]

d1.1$telres<- ifelse(d1.1$ind_id %in% ind_tel$ind_id==TRUE,"Yes","No")
d1.1$discordant<- ifelse((d1.1$R_quant>=1 & d1.1$IgG_quant<1.1)|
                          (d1.1$R_quant<1 & d1.1$IgG_quant>=1.1),"Discordant","Concordant")
d1.1$discordant2<- ifelse(d1.1$Roche_Result_new!= d1.1$IgG_result_new,"Discordant","Concordant")

t1<- tableby( telres~ fe(Sex)+    chisq(Agegroup)+ fe(Birth_Country)+
                chisq(EducationLevel)+ chisq(EmploymentStatus)+
                chisq(smokestatus)+ fe(Employment_ind_lastyear)+
                fe(any_risk_emp)+ fe(Employment_Airport)+
                fe(Employment_HealthCare)+ fe(Employment_EmergencyServices)+
                fe(Employment_Sales)+ fe(Employment_SeniorHome)+
                fe(Employment_Transportation)+ fe(Employment_Education)+
                fe(Employment_SocialWork)+ fe(Employment_OtherRiskType)+
                chisq(Household_Type_1)+ chisq(NetIncome_monthly_1)+
                chisq(Housing_Type)+ chisq(LivingArea_perInhabitant_cat)+
                chisq(Household_Inhabitants)+
                chisq(HealthStatus_Self_Overall_1)+chisq(Symptoms_past2w_SenseLoss.Smell.Taste.)+
                fe(Facemask_public)+ fe(HealthStatus_Self_Allergies_Respiratory)
              , data = d1.1, control =  my_controls,simulate.p.value=TRUE, B=500)

write2html(summary(t1, pfootnote=TRUE),
           here_out("Table_telephonerespvsnon_1a.html"),
           title="Summary of Telephone Responder")

t1<- tableby( discordant~ fe(Sex)+    chisq(Agegroup)+ fe(Birth_Country)+
                chisq(EducationLevel)+ chisq(EmploymentStatus)+
                chisq(smokestatus)+ fe(Employment_ind_lastyear)+
                fe(any_risk_emp)+ fe(Employment_Airport)+
                fe(Employment_HealthCare)+ fe(Employment_EmergencyServices)+
                fe(Employment_Sales)+ fe(Employment_SeniorHome)+
                fe(Employment_Transportation)+ fe(Employment_Education)+
                fe(Employment_SocialWork)+ fe(Employment_OtherRiskType)+
                chisq(Household_Type_1)+ chisq(NetIncome_monthly_1)+
                chisq(Housing_Type)+ chisq(LivingArea_perInhabitant_cat)+
                chisq(Household_Inhabitants)+
                chisq(HealthStatus_Self_Overall_1)+chisq(Symptoms_past2w_SenseLoss.Smell.Taste.)+
                fe(Facemask_public)+ fe(HealthStatus_Self_Allergies_Respiratory)
              , data = d1.1, control =  my_controls,simulate.p.value=TRUE, B=500)

write2html(summary(t1, pfootnote=TRUE),
           here_out("Table_discordantlab.html"),
           title="Summary of Discordant vs Concordant")


t1<- tableby( discordant2~ fe(Sex)+    chisq(Agegroup)+ fe(Birth_Country)+
                chisq(EducationLevel)+ chisq(EmploymentStatus)+
                chisq(smokestatus)+ fe(Employment_ind_lastyear)+
                fe(any_risk_emp)+ fe(Employment_Airport)+
                fe(Employment_HealthCare)+ fe(Employment_EmergencyServices)+
                fe(Employment_Sales)+ fe(Employment_SeniorHome)+
                fe(Employment_Transportation)+ fe(Employment_Education)+
                fe(Employment_SocialWork)+ fe(Employment_OtherRiskType)+
                chisq(Household_Type_1)+ chisq(NetIncome_monthly_1)+
                chisq(Housing_Type)+ chisq(LivingArea_perInhabitant_cat)+
                chisq(Household_Inhabitants)+
                chisq(HealthStatus_Self_Overall_1)+chisq(Symptoms_past2w_SenseLoss.Smell.Taste.)+
                fe(Facemask_public)+ fe(HealthStatus_Self_Allergies_Respiratory)
              , data = d1.1, control =  my_controls,simulate.p.value=TRUE, B=500)

write2html(summary(t1, pfootnote=TRUE),
           here_out("Table_discordantlab_optimalcutoff.html"),
           title="Summary of Discordant vs Concordant")

# Updating Agegroup
 
d1.1$Agegroup<- ifelse(d1.1$Age<20,"0-19",
                       ifelse(d1.1$Age>=20 & d1.1$Age<35,"20-34",
                              ifelse(d1.1$Age>=35 & d1.1$Age<50,"35-49",
                                     ifelse(d1.1$Age>=50 & d1.1$Age<65,"50-64",
                                            ifelse(d1.1$Age>=65 & d1.1$Age<80,"65-79","80+")))))
d1.1$Agegroup<- factor(d1.1$Agegroup) 



# Imputing for Age and adjusting for Sex
d1.1$Roche_Result_new<- factor(d1.1$Roche_Result_new)
d1.1$Roche_Result_new<- relevel(d1.1$Roche_Result_new,ref = "Negative")
mod_R<-glmer_imp(Roche_Result_new ~Sex+Age, data=d1.1[which(!is.na(d1.1$R_Result)),], random=~(1|hh_id), n.chains = 3, n.adapt = 100, n.iter = 500,
                 thin = 1,seed=1234,family =binomial(link = "logit"))
traceplot(mod_R)
summary(mod_R)

cc_sex <- cbind(summary(mod_R)$res$Roche_Result_new$regcoef[, c('Mean')]
                     ,summary(mod_R)$res$Roche_Result_new$regcoef[, c('2.5%')],
                     summary(mod_R)$res$Roche_Result_new$regcoef[, c('97.5%')])  ## slow (~ 11 seconds)

exp(cc_sex)

# Age group only
mod_R<-glmer_imp(Roche_Result_new ~Agegroup, data=d1.1[which(!is.na(d1.1$Roche_Result_new)),], random=~(1|hh_id), n.chains = 3, n.adapt = 100, n.iter = 500,
                 thin = 1,seed=1234,family =binomial(link = "logit"))
traceplot(mod_R)
summary(mod_R)

cc_agegroup <- cbind(summary(mod_R)$res$Roche_Result_new$regcoef[, c('Mean')]
                     ,summary(mod_R)$res$Roche_Result_new$regcoef[, c('2.5%')],
                     summary(mod_R)$res$Roche_Result_new$regcoef[, c('97.5%')])  ## slow (~ 11 seconds)
exp(cc_agegroup)
# Sex only
mod_R<-glmer_imp(Roche_Result_new ~Sex, data=d1.1[which(!is.na(d1.1$Roche_Result_new)),], random=~(1|hh_id), n.chains = 3, n.adapt = 100, n.iter = 500,
                 thin = 1,seed=1234,family =binomial(link = "logit"))
traceplot(mod_R)
summary(mod_R)

cc_Sex<- cbind(summary(mod_R)$res$Roche_Result_new$regcoef[, c('Mean')]
                     ,summary(mod_R)$res$Roche_Result_new$regcoef[, c('2.5%')],
                     summary(mod_R)$res$Roche_Result_new$regcoef[, c('97.5%')])  ## slow (~ 11 seconds)
exp(cc_Sex)

# Imputing for Agegroup based on Sex
mod_R<-glmer_imp(Roche_Result_new ~Sex+Agegroup, data=d1.1[which(!is.na(d1.1$R_Result)),], random=~(1|hh_id), n.chains = 3, n.adapt = 100, n.iter = 500,
           thin = 1,seed=1234,family =binomial(link = "logit"))
traceplot(mod_R)
summary(mod_R)

cc_agegroup <- cbind(summary(mod_R)$res$Roche_Result_new$regcoef[, c('Mean')]
            ,summary(mod_R)$res$Roche_Result_new$regcoef[, c('2.5%')],
            summary(mod_R)$res$Roche_Result_new$regcoef[, c('97.5%')])  ## slow (~ 11 seconds)

exp(cc_agegroup)

# Imputing for Birth Country based on Sex Age
mod_R<-glmer_imp(Roche_Result_new ~Sex+Age+Birth_Country , data=d1.1[which(!is.na(d1.1$Roche_Result_new)),], random=~(1|hh_id), n.chains = 3, n.adapt = 100, n.iter = 500,
                 thin = 1,seed=1234,family =binomial(link = "logit"))
traceplot(mod_R)
summary(mod_R)

cc_birthcountry <- cbind(summary(mod_R)$res$Roche_Result_new$regcoef[, c('Mean')]
                ,summary(mod_R)$res$Roche_Result_new$regcoef[, c('2.5%')],
                summary(mod_R)$res$Roche_Result_new$regcoef[, c('97.5%')])  ## slow (~ 11 seconds)

exp(cc_birthcountry)

# Imputing for EducationLevel based on Sex Age
mod_R<-glmer_imp(Roche_Result_new ~Sex+Age+EducationLevel , data=d1.1[which(!is.na(d1.1$Roche_Result_new)),], random=~(1|hh_id), n.chains = 3, n.adapt = 100, n.iter = 500,
                 thin = 1,seed=1234,family =binomial(link = "logit"))
traceplot(mod_R)
summary(mod_R)

cc_EducationLevel<- cbind(summary(mod_R)$res$Roche_Result_new$regcoef[, c('Mean')]
                         ,summary(mod_R)$res$Roche_Result_new$regcoef[, c('2.5%')],
                         summary(mod_R)$res$Roche_Result_new$regcoef[, c('97.5%')])  ## slow (~ 11 seconds)

exp(cc_EducationLevel)


# Imputing for EmploymentStatus based on Sex Age
mod_R<-glmer_imp(Roche_Result_new ~Sex+Age+EmploymentStatus , data=d1.1[which(!is.na(d1.1$Roche_Result_new)),], random=~(1|hh_id), n.chains = 3, n.adapt = 100, n.iter = 500,
                 thin = 1,seed=1234,family =binomial(link = "logit"))
traceplot(mod_R)
summary(mod_R)

cc_EmploymentStatus<- cbind(summary(mod_R)$res$Roche_Result_new$regcoef[, c('Mean')]
                          ,summary(mod_R)$res$Roche_Result_new$regcoef[, c('2.5%')],
                          summary(mod_R)$res$Roche_Result_new$regcoef[, c('97.5%')])  ## slow (~ 11 seconds)

exp(cc_EmploymentStatus)

# Imputing for Employment Past year based on Sex Age
mod_R<-glmer_imp(Roche_Result_new ~Sex+Age+Employment_ind_lastyear, data=d1.1[which(!is.na(d1.1$Roche_Result_new)),], random=~(1|hh_id), n.chains = 3, n.adapt = 100, n.iter = 500,
                 thin = 1,seed=1234,family =binomial(link = "logit"))
traceplot(mod_R)
summary(mod_R)

cc_Employment_ind_lastyear<- cbind(summary(mod_R)$res$Roche_Result_new$regcoef[, c('Mean')]
                            ,summary(mod_R)$res$Roche_Result_new$regcoef[, c('2.5%')],
                            summary(mod_R)$res$Roche_Result_new$regcoef[, c('97.5%')])  ## slow (~ 11 seconds)

exp(cc_Employment_ind_lastyear)

# remove anylist employment
any_risk_emp<-NULL
# Imputing for Employment Ris job year based on Sex Age
mod_R<-glmer_imp(Roche_Result_new ~Sex+Age+ any_risk_emp, data=d1.1[which(!is.na(d1.1$Roche_Result_new)),], random=~(1|hh_id), n.chains = 3, n.adapt = 100, n.iter = 500,
                 thin = 1,seed=1234,family =binomial(link = "logit"))
traceplot(mod_R)
summary(mod_R)

cc_any_risk_emp<- cbind(summary(mod_R)$res$Roche_Result_new$regcoef[, c('Mean')]
                                   ,summary(mod_R)$res$Roche_Result_new$regcoef[, c('2.5%')],
                                   summary(mod_R)$res$Roche_Result_new$regcoef[, c('97.5%')])  ## slow (~ 11 seconds)

exp(cc_any_risk_emp)

# modify any risk employment
#d1.1$Employment_HealthCare<- ifelse(d1.1$any_risk_emp=="No","No",ifelse(d1.1$any_risk_emp=="Yes" &
#                                                                          d1.1$Employment_HealthCare=="Yes","Yes",NA))
#d1.1$Employment_Sales<- ifelse(d1.1$any_risk_emp=="No","No",ifelse(d1.1$any_risk_emp=="Yes" &
#                                                                          d1.1$Employment_Sales=="Yes","Yes",NA))

d1.1$Employment_HealthCare<- factor(d1.1$Employment_HealthCare)
d1.1$Employment_Sales<- factor(d1.1$Employment_Sales)
Employment_HealthCare<-NULL
# Imputing for Employment Health care based on Sex Age
mod_R<-glmer_imp(Roche_Result_new ~Sex+Age+ Employment_HealthCare, data=d1.1[which(!is.na(d1.1$Roche_Result_new)),], random=~(1|hh_id), n.chains = 3, n.adapt = 100, n.iter = 500,
                 thin = 1,seed=1234,family =binomial(link = "logit"))
traceplot(mod_R)
summary(mod_R)

cc_Employment_HealthCare<- cbind(summary(mod_R)$res$Roche_Result_new$regcoef[, c('Mean')]
                        ,summary(mod_R)$res$Roche_Result_new$regcoef[, c('2.5%')],
                        summary(mod_R)$res$Roche_Result_new$regcoef[, c('97.5%')])  ## slow (~ 11 seconds)

exp(cc_Employment_HealthCare)


# Imputing for Employment Sales based on Sex Age
mod<-glmmTMB(Roche_Result_new ~Sex+Age+ Employment_Sales+(1|hh_id), data=d1.1[which(!is.na(d1.1$Roche_Result_new)),],family =binomial(link = "logit"))

summary(mod)


mod_R<-glmer_imp(Roche_Result_new ~Sex+Age+ Employment_Sales, data=d1.1[which(!is.na(d1.1$Roche_Result_new)),], random=~(1|hh_id), n.chains = 3, n.adapt = 100, n.iter = 500,
                 thin = 1,seed=1234,family =binomial(link = "logit"))
traceplot(mod_R)
summary(mod_R)

cc_Employment_Sales<- cbind(summary(mod_R)$res$Roche_Result_new$regcoef[, c('Mean')]
                                 ,summary(mod_R)$res$Roche_Result_new$regcoef[, c('2.5%')],
                                 summary(mod_R)$res$Roche_Result_new$regcoef[, c('97.5%')])  ## slow (~ 11 seconds)

exp(cc_Employment_Sales)

# Imputing for Smoking based on Sex Age
mod_R<-glmer_imp(Roche_Result_new ~Sex+Age+ smokestatus, data=d1.1[which(!is.na(d1.1$Roche_Result_new)),], random=~(1|hh_id), n.chains = 3, n.adapt = 100, n.iter = 500,
                 thin = 1,seed=1234,family =binomial(link = "logit"))
traceplot(mod_R)
summary(mod_R)

cc_smokestatus<- cbind(summary(mod_R)$res$Roche_Result_new$regcoef[, c('Mean')]
                            ,summary(mod_R)$res$Roche_Result_new$regcoef[, c('2.5%')],
                            summary(mod_R)$res$Roche_Result_new$regcoef[, c('97.5%')])  ## slow (~ 11 seconds)

exp(cc_smokestatus)


# Imputing for Income based on Sex Age
mod_R<-glmer_imp(Roche_Result_new ~Sex+Age+ NetIncome_monthly_1, data=d1.1[which(!is.na(d1.1$Roche_Result_new)),], random=~(1|hh_id), n.chains = 3, n.adapt = 100, n.iter = 500,
                 thin = 1,seed=1234,family =binomial(link = "logit"))
traceplot(mod_R)
summary(mod_R)

cc_income<- cbind(summary(mod_R)$res$Roche_Result_new$regcoef[, c('Mean')]
                       ,summary(mod_R)$res$Roche_Result_new$regcoef[, c('2.5%')],
                       summary(mod_R)$res$Roche_Result_new$regcoef[, c('97.5%')])  ## slow (~ 11 seconds)

exp(cc_income)


# Imputing for Household Type based on Sex Age
mod_R<-glmer_imp(Roche_Result_new ~Sex+Age+ Household_Type_1, data=d1.1[which(!is.na(d1.1$Roche_Result_new)),], random=~(1|hh_id), n.chains = 3, n.adapt = 100, n.iter = 500,
                 thin = 1,seed=1234,family =binomial(link = "logit"))
traceplot(mod_R)
summary(mod_R)

cc_householdtype<- cbind(summary(mod_R)$res$Roche_Result_new$regcoef[, c('Mean')]
                  ,summary(mod_R)$res$Roche_Result_new$regcoef[, c('2.5%')],
                  summary(mod_R)$res$Roche_Result_new$regcoef[, c('97.5%')])  ## slow (~ 11 seconds)

exp(cc_householdtype)

# Imputing for Household Size based on Sex Age
mod_R<-glmer_imp(Roche_Result_new ~Sex+Age+ Household_Inhabitants, data=d1.1[which(!is.na(d1.1$Roche_Result_new)),], random=~(1|hh_id), n.chains = 3, n.adapt = 100, n.iter = 500,
                 thin = 1,seed=1234,family =binomial(link = "logit"))
traceplot(mod_R)
summary(mod_R)

cc_householdsize<- cbind(summary(mod_R)$res$Roche_Result_new$regcoef[, c('Mean')]
                         ,summary(mod_R)$res$Roche_Result_new$regcoef[, c('2.5%')],
                         summary(mod_R)$res$Roche_Result_new$regcoef[, c('97.5%')])  ## slow (~ 11 seconds)

exp(cc_householdsize)


# Imputing for Housing Type based on Sex Age
mod_R<-glmer_imp(Roche_Result_new ~Sex+Age+ Housing_Type, data=d1.1[which(!is.na(d1.1$Roche_Result_new)),], random=~(1|hh_id), n.chains = 3, n.adapt = 100, n.iter = 500,
                 thin = 1,seed=1234,family =binomial(link = "logit"))
traceplot(mod_R)
summary(mod_R)

cc_housingtype<- cbind(summary(mod_R)$res$Roche_Result_new$regcoef[, c('Mean')]
                         ,summary(mod_R)$res$Roche_Result_new$regcoef[, c('2.5%')],
                         summary(mod_R)$res$Roche_Result_new$regcoef[, c('97.5%')])  ## slow (~ 11 seconds)

exp(cc_housingtype)

# Imputing for Area per inhabitant based on Sex Age
mod_R<-glmer_imp(Roche_Result_new ~Sex+Age+ LivingArea_perInhabitant_cat, data=d1.1[which(!is.na(d1.1$Roche_Result_new)),], random=~(1|hh_id), n.chains = 3, n.adapt = 100, n.iter = 500,
                 thin = 1,seed=1234,family =binomial(link = "logit"))
traceplot(mod_R)
summary(mod_R)

cc_LivingArea_perInhabitant_cat<- cbind(summary(mod_R)$res$Roche_Result_new$regcoef[, c('Mean')]
                       ,summary(mod_R)$res$Roche_Result_new$regcoef[, c('2.5%')],
                       summary(mod_R)$res$Roche_Result_new$regcoef[, c('97.5%')])  ## slow (~ 11 seconds)

exp(cc_LivingArea_perInhabitant_cat)

# Imputing for Facemask usage based on Sex Age
mod_R<-glmer_imp(Roche_Result_new ~Sex+Age+ Facemask_public, data=d1.1[which(!is.na(d1.1$Roche_Result_new)),], random=~(1|hh_id), n.chains = 3, n.adapt = 100, n.iter = 500,
                 thin = 1,seed=1234,family =binomial(link = "logit"))
traceplot(mod_R)
summary(mod_R)

cc_Facemask_public<- cbind(summary(mod_R)$res$Roche_Result_new$regcoef[, c('Mean')]
                                        ,summary(mod_R)$res$Roche_Result_new$regcoef[, c('2.5%')],
                                        summary(mod_R)$res$Roche_Result_new$regcoef[, c('97.5%')])  ## slow (~ 11 seconds)

exp(cc_Facemask_public)

# Imputing for Loss of sense of smell/taste based on Sex Age
d1.1$Symptoms_past2w_SenseLoss.Smell.Taste.<- factor(d1.1$Symptoms_past2w_SenseLoss.Smell.Taste.)
mod_R<-glmer_imp(Roche_Result_new ~Sex+Age+Symptoms_past2w_SenseLoss.Smell.Taste., data=d1.1[which(!is.na(d1.1$Roche_Result_new)),], random=~(1|hh_id), n.chains = 3, n.adapt = 100, n.iter = 500,
                 thin = 1,seed=1234,family =binomial(link = "logit"))
traceplot(mod_R)
summary(mod_R)

cc_senseloss<- cbind(summary(mod_R)$res$Roche_Result_new$regcoef[, c('Mean')]
                           ,summary(mod_R)$res$Roche_Result_new$regcoef[, c('2.5%')],
                           summary(mod_R)$res$Roche_Result_new$regcoef[, c('97.5%')])  ## slow (~ 11 seconds)

exp(cc_senseloss)


# Imputing for Overall health based on Sex Age

mod_R<-glmer_imp(Roche_Result_new ~Sex+Age+HealthStatus_Self_Overall_1, data=d1.1[which(!is.na(d1.1$Roche_Result_new)),], random=~(1|hh_id), n.chains = 3, n.adapt = 100, n.iter = 500,
                 thin = 1,seed=1234,family =binomial(link = "logit"))
traceplot(mod_R)
summary(mod_R)

cc_healthstatus<- cbind(summary(mod_R)$res$Roche_Result_new$regcoef[, c('Mean')]
                     ,summary(mod_R)$res$Roche_Result_new$regcoef[, c('2.5%')],
                     summary(mod_R)$res$Roche_Result_new$regcoef[, c('97.5%')])  ## slow (~ 11 seconds)

exp(cc_healthstatus)


# Imputing for Respiratory Allergies on Sex Age
d1.1$HealthStatus_Self_Allergies_Respiratory<-as.factor(d1.1$HealthStatus_Self_Allergies_Respiratory)
mod_R<-glmer_imp(Roche_Result_new ~Sex+Age+HealthStatus_Self_Allergies_Respiratory, data=d1.1[which(!is.na(d1.1$Roche_Result_new)),], random=~(1|hh_id), n.chains = 3, n.adapt = 100, n.iter = 500,
                 thin = 1,seed=1234,family =binomial(link = "logit"))
traceplot(mod_R)
summary(mod_R)

cc_allergies_resp<- cbind(summary(mod_R)$res$Roche_Result_new$regcoef[, c('Mean')]
                        ,summary(mod_R)$res$Roche_Result_new$regcoef[, c('2.5%')],
                        summary(mod_R)$res$Roche_Result_new$regcoef[, c('97.5%')])  ## slow (~ 11 seconds)

exp(cc_allergies_resp)

# Imputing for Skin Allergies on Sex Age
d1.1$HealthStatus_Self_Allergies_Skin<-as.factor(d1.1$HealthStatus_Self_Allergies_Skin)
mod_R<-glmer_imp(Roche_Result_new ~Sex+Age+HealthStatus_Self_Allergies_Skin, data=d1.1[which(!is.na(d1.1$Roche_Result_new)),], random=~(1|hh_id), n.chains = 3, n.adapt = 100, n.iter = 500,
                 thin = 1,seed=1234,family =binomial(link = "logit"))
traceplot(mod_R)
summary(mod_R)

cc_skinallergies<- cbind(summary(mod_R)$res$Roche_Result_new$regcoef[, c('Mean')]
                        ,summary(mod_R)$res$Roche_Result_new$regcoef[, c('2.5%')],
                        summary(mod_R)$res$Roche_Result_new$regcoef[, c('97.5%')])  ## slow (~ 11 seconds)

exp(cc_skinallergies)


# Imputing for Autoimmune on Sex Age
d1.1$HealthStatus_Self_Autoimmune<-as.factor(d1.1$HealthStatus_Self_Autoimmune)
mod_R<-glmer_imp(Roche_Result_new ~Sex+Age+HealthStatus_Self_Autoimmune, data=d1.1[which(!is.na(d1.1$Roche_Result_new)),], random=~(1|hh_id), n.chains = 3, n.adapt = 100, n.iter = 500,
                 thin = 1,seed=1234,family =binomial(link = "logit"))
traceplot(mod_R)
summary(mod_R)

cc_autoimmune<- cbind(summary(mod_R)$res$Roche_Result_new$regcoef[, c('Mean')]
                        ,summary(mod_R)$res$Roche_Result_new$regcoef[, c('2.5%')],
                        summary(mod_R)$res$Roche_Result_new$regcoef[, c('97.5%')])  ## slow (~ 11 seconds)

exp(cc_autoimmune)


# Imputing for Cancer on Sex Age
d1.1$HealthStatus_Self_Cancer<-as.factor(d1.1$HealthStatus_Self_Cancer)
mod_R<-glmer_imp(Roche_Result_new ~Sex+Age+HealthStatus_Self_Cancer, data=d1.1[which(!is.na(d1.1$Roche_Result_new)),], random=~(1|hh_id), n.chains = 3, n.adapt = 100, n.iter = 500,
                 thin = 1,seed=1234,family =binomial(link = "logit"))
traceplot(mod_R)
summary(mod_R)

cc_cancer<- cbind(summary(mod_R)$res$Roche_Result_new$regcoef[, c('Mean')]
                        ,summary(mod_R)$res$Roche_Result_new$regcoef[, c('2.5%')],
                        summary(mod_R)$res$Roche_Result_new$regcoef[, c('97.5%')])  ## slow (~ 11 seconds)

exp(cc_cancer)

# Imputing forCardiovascular on Sex Age
d1.1$HealthStatus_Self_CardiovascularDisease<- as.factor(d1.1$HealthStatus_Self_CardiovascularDisease)
mod_R<-glmer_imp(Roche_Result_new ~Sex+Age+HealthStatus_Self_CardiovascularDisease, data=d1.1[which(!is.na(d1.1$Roche_Result_new)),], random=~(1|hh_id), n.chains = 3, n.adapt = 100, n.iter = 500,
                 thin = 1,seed=1234,family =binomial(link = "logit"))
traceplot(mod_R)
summary(mod_R)

cc_cardiovascular<- cbind(summary(mod_R)$res$Roche_Result_new$regcoef[, c('Mean')]
                        ,summary(mod_R)$res$Roche_Result_new$regcoef[, c('2.5%')],
                        summary(mod_R)$res$Roche_Result_new$regcoef[, c('97.5%')])  ## slow (~ 11 seconds)

exp(cc_cardiovascular)


# Imputing for Diabetes  on Sex Age
d1.1$HealthStatus_Self_Diabetes<- as.factor(d1.1$HealthStatus_Self_Diabetes)
mod_R<-glmer_imp(Roche_Result_new ~Sex+Age+HealthStatus_Self_Diabetes, data=d1.1[which(!is.na(d1.1$Roche_Result_new)),], random=~(1|hh_id), n.chains = 3, n.adapt = 100, n.iter = 500,
                 thin = 1,seed=1234,family =binomial(link = "logit"))
traceplot(mod_R)
summary(mod_R)

cc_diabetes<- cbind(summary(mod_R)$res$Roche_Result_new$regcoef[, c('Mean')]
                        ,summary(mod_R)$res$Roche_Result_new$regcoef[, c('2.5%')],
                        summary(mod_R)$res$Roche_Result_new$regcoef[, c('97.5%')])  ## slow (~ 11 seconds)

exp(cc_diabetes)

# Imputing for Lungdisease  on Sex Age
d1.1$HealthStatus_Self_LungDisease<-as.factor(d1.1$HealthStatus_Self_LungDisease)
mod_R<-glmer_imp(Roche_Result_new ~Sex+Age+HealthStatus_Self_LungDisease, data=d1.1[which(!is.na(d1.1$Roche_Result_new)),], random=~(1|hh_id), n.chains = 3, n.adapt = 100, n.iter = 500,
                 thin = 1,seed=1234,family =binomial(link = "logit"))
traceplot(mod_R)
summary(mod_R)

cc_lungdisease<- cbind(summary(mod_R)$res$Roche_Result_new$regcoef[, c('Mean')]
                        ,summary(mod_R)$res$Roche_Result_new$regcoef[, c('2.5%')],
                        summary(mod_R)$res$Roche_Result_new$regcoef[, c('97.5%')])  ## slow (~ 11 seconds)

exp(cc_lungdisease)

# Imputing for Obesity  on Sex Age
d1.1$HealthStatus_Self_Obesity<- as.factor(d1.1$HealthStatus_Self_Obesity)
mod_R<-glmer_imp(Roche_Result_new ~Sex+Age+HealthStatus_Self_Obesity, data=d1.1[which(!is.na(d1.1$Roche_Result_new)),], random=~(1|hh_id), n.chains = 3, n.adapt = 100, n.iter = 500,
                 thin = 1,seed=1234,family =binomial(link = "logit"))
traceplot(mod_R)
summary(mod_R)

cc_obesity<- cbind(summary(mod_R)$res$Roche_Result_new$regcoef[, c('Mean')]
                        ,summary(mod_R)$res$Roche_Result_new$regcoef[, c('2.5%')],
                        summary(mod_R)$res$Roche_Result_new$regcoef[, c('97.5%')])  ## slow (~ 11 seconds)

exp(cc_obesity)


# GLMM Risk factor analysis
# Roche new
library(broom.mixed)
library(emmeans)
d1.1$NetIncome_monthly_1<-relevel(d1.1$NetIncome_monthly_1, ref = "2500-4000")
d1.1$Roche_Result_new<-factor(d1.1$Roche_Result_new)
d1.1$Roche_Result_new <- relevel(d1.1$Roche_Result_new, ref = "Negative")
model_bin <- glmmTMB(Roche_Result_new ~Sex + (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)

tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_new ~Agegroup + (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)

tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_new ~Agegroup + Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_new ~  Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_new ~ Birth_Country + Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_new ~ EmploymentStatus+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_new ~ Employment_ind_lastyear+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_new ~ EducationLevel+Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_new ~ smokestatus+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")



model_bin <- glmmTMB(Roche_Result_new ~ any_risk_emp+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_new ~ Employment_HealthCare+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_new ~ Employment_Sales+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")


model_bin <- glmmTMB(Roche_Result_new ~ NetIncome_monthly_1+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")
model_bin <- glmmTMB(Roche_Result_new ~ Household_Type_1+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")
model_bin <- glmmTMB(Roche_Result_new ~ Housing_Type+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(d1.1$Housing_Type!="Others"),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_new ~ Household_Inhabitants+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")
model_bin <- glmmTMB(Roche_Result_new ~ LivingArea_perInhabitant_cat+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")




model_bin <- glmmTMB(Roche_Result_new ~ Facemask_public+ Age+ Sex+(1|hh_id),
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_new ~ Symptoms_past2w_SenseLoss.Smell.Taste.+ Age+ Sex+(1|hh_id),
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_new ~ HealthStatus_Self_Overall_1 + Age+ Sex+(1|hh_id),
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_new ~ HealthStatus_Self_Allergies_Respiratory + Age+ Sex+(1|hh_id),
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_new ~ HealthStatus_Self_Allergies_Skin + Age+ Sex+(1|hh_id),
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_new ~ HealthStatus_Self_Autoimmune + Age+ Sex+(1|hh_id),
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_new ~HealthStatus_Self_Cancer + Age+ Sex+(1|hh_id),
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_new ~HealthStatus_Self_CardiovascularDisease + Age+ Sex+(1|hh_id),
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_new ~HealthStatus_Self_Diabetes + Age+ Sex+(1|hh_id),
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_new ~HealthStatus_Self_LungDisease + Age+ Sex+(1|hh_id),
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_new ~HealthStatus_Self_Obesity + Age+ Sex+(1|hh_id),
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")



# Roche Old
d1.1$Roche_Result_old<-factor(d1.1$Roche_Result_old)
d1.1$Roche_Result_old <- relevel(d1.1$Roche_Result_old, ref = "Negative")
model_bin <- glmmTMB(Roche_Result_old ~Sex + (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_old)),])
summary(model_bin)

tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_old ~Agegroup + (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_old)),])
summary(model_bin)

tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_old ~Agegroup + Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_old ~  Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_old ~ Birth_Country + Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_old ~ EmploymentStatus+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_old ~ Employment_ind_lastyear+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_old ~ EducationLevel+Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_old ~ smokestatus+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")



model_bin <- glmmTMB(Roche_Result_old ~ any_risk_emp+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_old ~ Employment_HealthCare+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_old ~ Employment_Sales+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")


model_bin <- glmmTMB(Roche_Result_old ~ NetIncome_monthly_1+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")
model_bin <- glmmTMB(Roche_Result_old ~ Household_Type_1+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")
model_bin <- glmmTMB(Roche_Result_old ~ Housing_Type+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(d1.1$Housing_Type!="Others"),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_old ~ Household_Inhabitants+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")
model_bin <- glmmTMB(Roche_Result_old ~ LivingArea_perInhabitant_cat+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")




model_bin <- glmmTMB(Roche_Result_old ~ Facemask_public+ Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_old ~ Symptoms_past2w_SenseLoss.Smell.Taste.+ Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_old ~ HealthStatus_Self_Overall_1 + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_old ~ HealthStatus_Self_Allergies_Respiratory + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_old ~ HealthStatus_Self_Allergies_Skin + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_old ~ HealthStatus_Self_Autoimmune + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_old ~HealthStatus_Self_Cancer + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_old ~HealthStatus_Self_CardiovascularDisease + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_old ~HealthStatus_Self_Diabetes + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_old ~HealthStatus_Self_LungDisease + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(Roche_Result_old ~HealthStatus_Self_Obesity + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

# IgG New
d1.1$IgG_result_new <-factor(d1.1$IgG_result_new)
d1.1$IgG_result_new <- relevel(d1.1$IgG_result_new, ref = "Negative")
model_bin <- glmmTMB(IgG_result_new ~Sex + (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_result_new)),])
summary(model_bin)

tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_result_new ~Agegroup + (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_result_new)),])
summary(model_bin)

tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_result_new ~Agegroup + Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_result_new ~  Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_result_new ~ Birth_Country + Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_result_new ~ EmploymentStatus+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_result_new ~ Employment_ind_lastyear+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_result_new ~ EducationLevel+Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_result_new ~ smokestatus+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")



model_bin <- glmmTMB(IgG_result_new ~ any_risk_emp+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_result_new ~ Employment_HealthCare+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_result_new ~ Employment_Sales+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")


model_bin <- glmmTMB(IgG_result_new ~ NetIncome_monthly_1+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")
model_bin <- glmmTMB(IgG_result_new ~ Household_Type_1+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")
model_bin <- glmmTMB(IgG_result_new ~ Housing_Type+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(d1.1$Housing_Type!="Others"),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_result_new ~ Household_Inhabitants+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")
model_bin <- glmmTMB(IgG_result_new ~ LivingArea_perInhabitant_cat+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")




model_bin <- glmmTMB(IgG_result_new ~ Facemask_public+ Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_result_new ~ Symptoms_past2w_SenseLoss.Smell.Taste.+ Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_result_new ~ HealthStatus_Self_Overall_1 + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_result_new ~ HealthStatus_Self_Allergies_Respiratory + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_result_new ~ HealthStatus_Self_Allergies_Skin + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_result_new ~ HealthStatus_Self_Autoimmune + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_result_new ~HealthStatus_Self_Cancer + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_result_new ~HealthStatus_Self_CardiovascularDisease + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_result_new ~HealthStatus_Self_Diabetes + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_result_new ~HealthStatus_Self_LungDisease + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_result_new ~HealthStatus_Self_Obesity + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")


# IgG old
d1.1$IgG_Result_old <-factor(d1.1$IgG_Result_old)
d1.1$IgG_Result_old <- relevel(d1.1$IgG_Result_old, ref = "Negative")
model_bin <- glmmTMB(IgG_Result_old ~Sex + (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_Result_old)),])
summary(model_bin)

tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_Result_old ~Agegroup + (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_Result_old)),])
summary(model_bin)

tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_Result_old ~Agegroup + Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_Result_old ~  Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_Result_old ~ Birth_Country + Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_Result_old ~ EmploymentStatus+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_Result_old ~ Employment_ind_lastyear+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_Result_old ~ EducationLevel+Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_Result_old ~ smokestatus+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")



model_bin <- glmmTMB(IgG_Result_old ~ any_risk_emp+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_Result_old ~ Employment_HealthCare+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_Result_old ~ Employment_Sales+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")


model_bin <- glmmTMB(IgG_Result_old ~ NetIncome_monthly_1+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")
model_bin <- glmmTMB(IgG_Result_old ~ Household_Type_1+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")
model_bin <- glmmTMB(IgG_Result_old ~ Housing_Type+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(d1.1$Housing_Type!="Others"),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_Result_old ~ Household_Inhabitants+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")
model_bin <- glmmTMB(IgG_Result_old ~ LivingArea_perInhabitant_cat+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")




model_bin <- glmmTMB(IgG_Result_old ~ Facemask_public+ Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_Result_old ~ Symptoms_past2w_SenseLoss.Smell.Taste.+ Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_Result_old ~ HealthStatus_Self_Overall_1 + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_Result_old ~ HealthStatus_Self_Allergies_Respiratory + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_Result_old ~ HealthStatus_Self_Allergies_Skin + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_Result_old ~ HealthStatus_Self_Autoimmune + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_Result_old ~HealthStatus_Self_Cancer + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_Result_old ~HealthStatus_Self_CardiovascularDisease + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_Result_old ~HealthStatus_Self_Diabetes + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_Result_old ~HealthStatus_Self_LungDisease + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgG_Result_old ~HealthStatus_Self_Obesity + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgG_Result_old)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

###########
# IgA New
d1.1$IgA_Result_new <-factor(d1.1$IgA_Result_new)
d1.1$IgA_Result_new <- relevel(d1.1$IgA_Result_new, ref = "Negative")
model_bin <- glmmTMB(IgA_Result_new ~Sex + (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_Result_new)),])
summary(model_bin)

tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_Result_new ~Agegroup + (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_Result_new)),])
summary(model_bin)

tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_Result_new ~Agegroup + Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_Result_new ~  Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_Result_new ~ Birth_Country + Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_Result_new ~ EmploymentStatus+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_Result_new ~ Employment_ind_lastyear+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_Result_new ~ EducationLevel+Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_Result_new ~ smokestatus+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")



model_bin <- glmmTMB(IgA_Result_new ~ any_risk_emp+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_Result_new ~ Employment_HealthCare+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_Result_new ~ Employment_Sales+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")


model_bin <- glmmTMB(IgA_Result_new ~ NetIncome_monthly_1+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")
model_bin <- glmmTMB(IgA_Result_new ~ Household_Type_1+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")
model_bin <- glmmTMB(IgA_Result_new ~ Housing_Type+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(d1.1$Housing_Type!="Others"),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_Result_new ~ Household_Inhabitants+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")
model_bin <- glmmTMB(IgA_Result_new ~ LivingArea_perInhabitant_cat+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")




model_bin <- glmmTMB(IgA_Result_new ~ Facemask_public+ Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_Result_new ~ Symptoms_past2w_SenseLoss.Smell.Taste.+ Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_Result_new ~ HealthStatus_Self_Overall_1 + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_Result_new ~ HealthStatus_Self_Allergies_Respiratory + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_Result_new ~ HealthStatus_Self_Allergies_Skin + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_Result_new ~ HealthStatus_Self_Autoimmune + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_Result_new ~HealthStatus_Self_Cancer + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_Result_new ~HealthStatus_Self_CardiovascularDisease + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_Result_new ~HealthStatus_Self_Diabetes + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_Result_new ~HealthStatus_Self_LungDisease + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_Result_new ~HealthStatus_Self_Obesity + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")


# IgA old
d1.1<- merge.data.frame(d1.1,subset(d,select=c(ind_id,IgA_result_m)), by="ind_id")

d1.1$IgA_result_m <-factor(d1.1$IgA_result_m)
d1.1$IgA_result_m<- relevel(d1.1$IgA_result_m, ref = "Negative")
model_bin <- glmmTMB(IgA_result_m~Sex + (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_result_m)),])
summary(model_bin)

tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_result_m~Agegroup + (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_result_m)),])
summary(model_bin)

tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_result_m~Agegroup + Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_result_m)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_result_m~  Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_result_m)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_result_m~ Birth_Country + Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_result_m)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_result_m~ EmploymentStatus+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_result_m)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_result_m~ Employment_ind_lastyear+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_result_m)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_result_m~ EducationLevel+Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_result_m)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_result_m~ smokestatus+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_result_m)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")



model_bin <- glmmTMB(IgA_result_m~ any_risk_emp+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_result_m)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_result_m~ Employment_HealthCare+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_result_m)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_result_m~ Employment_Sales+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_result_m)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")


model_bin <- glmmTMB(IgA_result_m~ NetIncome_monthly_1+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_result_m)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")
model_bin <- glmmTMB(IgA_result_m~ Household_Type_1+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_result_m)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")
model_bin <- glmmTMB(IgA_result_m~ Housing_Type+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(d1.1$Housing_Type!="Others"),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_result_m~ Household_Inhabitants+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_result_m)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")
model_bin <- glmmTMB(IgA_result_m~ LivingArea_perInhabitant_cat+ Age+ Sex+ (1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_result_m)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")




model_bin <- glmmTMB(IgA_result_m~ Facemask_public+ Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_result_m)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_result_m~ Symptoms_past2w_SenseLoss.Smell.Taste.+ Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_result_m)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_result_m~ HealthStatus_Self_Overall_1 + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_result_m)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_result_m~ HealthStatus_Self_Allergies_Respiratory + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_result_m)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_result_m~ HealthStatus_Self_Allergies_Skin + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_result_m)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_result_m~ HealthStatus_Self_Autoimmune + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_result_m)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_result_m~HealthStatus_Self_Cancer + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_result_m)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_result_m~HealthStatus_Self_CardiovascularDisease + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_result_m)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_result_m~HealthStatus_Self_Diabetes + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_result_m)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_result_m~HealthStatus_Self_LungDisease + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_result_m)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glmmTMB(IgA_result_m~HealthStatus_Self_Obesity + Age+ Sex+(1|hh_id),
                     family=binomial,data=d1.1[which(!is.na(d1.1$IgA_result_m)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")


# Tables of the variables
ctable(d1.1$Sex,d1.1$Roche_Result_new)
ctable(d1.1$Agegroup,d1.1$Roche_Result_new)
ctable(d1.1$Birth_Country,d1.1$Roche_Result_new)
ctable(d1.1$EducationLevel,d1.1$Roche_Result_new)
ctable(d1.1$EmploymentStatus,d1.1$Roche_Result_new)
ctable(d1.1$Employment_Sales,d1.1$Roche_Result_new)
ctable(d1.1$Employment_HealthCare,d1.1$Roche_Result_new)
ctable(d1.1$Employment_ind_lastyear,d1.1$Roche_Result_new)
ctable(d1.1$any_risk_emp,d1.1$Roche_Result_new)
ctable(d1.1$smokestatus,d1.1$Roche_Result_new)
ctable(d1.1$NetIncome_monthly_1,d1.1$Roche_Result_new)
ctable(d1.1$Household_Type_1,d1.1$Roche_Result_new)
ctable(d1.1$Housing_Type,d1.1$Roche_Result_new)
ctable(d1.1$Household_Inhabitants,d1.1$Roche_Result_new)
ctable(d1.1$LivingArea_perInhabitant_cat,d1.1$Roche_Result_new)

ctable(d1.1$Facemask_public,d1.1$Roche_Result_new)
ctable(d1.1$Symptoms_past2w_SenseLoss.Smell.Taste.,d1.1$Roche_Result_new)
ctable(d1.1$HealthStatus_Self_Overall_1,d1.1$Roche_Result_new)
ctable(d1.1$HealthStatus_Self_Allergies_Respiratory,d1.1$Roche_Result_new)
ctable(d1.1$HealthStatus_Self_Allergies_Skin,d1.1$Roche_Result_new)
ctable(d1.1$HealthStatus_Self_Autoimmune,d1.1$Roche_Result_new)
ctable(d1.1$HealthStatus_Self_Cancer,d1.1$Roche_Result_new)
ctable(d1.1$HealthStatus_Self_CardiovascularDisease,d1.1$Roche_Result_new)
ctable(d1.1$HealthStatus_Self_Diabetes,d1.1$Roche_Result_new)
ctable(d1.1$HealthStatus_Self_LungDisease,d1.1$Roche_Result_new)
ctable(d1.1$HealthStatus_Self_Obesity,d1.1$Roche_Result_new)

# GLMS

model_bin <- glm(Roche_Result_new ~Sex,
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glm(Roche_Result_new ~Agegroup ,
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glm(Roche_Result_new ~Agegroup + Sex,
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glm(Roche_Result_new ~Age + Sex,
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")


model_bin <- glm(Roche_Result_new ~ Birth_Country + Age+ Sex,
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glm(Roche_Result_new ~ EmploymentStatus+Age+ Sex,
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glm(Roche_Result_new ~ Employment_ind_lastyear+ Age+ Sex,
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glm(Roche_Result_new ~ EducationLevel+ Age+ Sex,
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")


model_bin <- glm(Roche_Result_new ~ any_risk_emp+ Age+ Sex,
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glm(Roche_Result_new ~ Employment_HealthCare+ Age+ Sex,
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glm(Roche_Result_new ~ Employment_Sales+ Age+ Sex,
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")


model_bin <- glm(Roche_Result_new ~ smokestatus+ Age+ Sex,
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")


model_bin <- glm(Roche_Result_new ~ NetIncome_monthly_1+ Age+ Sex,
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")
model_bin <- glm(Roche_Result_new ~ Household_Type_1+ Age+ Sex,
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")
model_bin <- glm(Roche_Result_new ~ Housing_Type+ Age+ Sex,
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")
model_bin <- glm(Roche_Result_new ~ Household_Inhabitants+ Age+ Sex,
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")
model_bin <- glm(Roche_Result_new ~ LivingArea_perInhabitant_cat+ Age+ Sex,
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")


model_bin <- glm(Roche_Result_new ~ Facemask_public+ Age+ Sex,
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glm(Roche_Result_new ~ Symptoms_past2w_SenseLoss.Smell.Taste.+ Age+ Sex,
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glm(Roche_Result_new ~ HealthStatus_Self_Overall_1 + Age+ Sex,
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glm(Roche_Result_new ~ HealthStatus_Self_Allergies_Respiratory + Age+ Sex,
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glm(Roche_Result_new ~ HealthStatus_Self_Allergies_Skin + Age+ Sex,
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glm(Roche_Result_new ~ HealthStatus_Self_Autoimmune + Age+ Sex,
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glm(Roche_Result_new ~HealthStatus_Self_Cancer + Age+ Sex,
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glm(Roche_Result_new ~HealthStatus_Self_CardiovascularDisease + Age+ Sex,
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glm(Roche_Result_new ~HealthStatus_Self_Diabetes + Age+ Sex,
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glm(Roche_Result_new ~HealthStatus_Self_LungDisease + Age+ Sex,
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glm(Roche_Result_new ~HealthStatus_Self_Obesity + Age+ Sex,
                 family=binomial,data=d1.1[which(!is.na(d1.1$Roche_Result_new)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")


# Odds ratio Table 2
d2$NetIncome_monthly_1<-relevel(d2$NetIncome_monthly_1, ref = "2500-4000")

d2$Roche_pos<-factor(d2$Roche_pos)
d2$Roche_pos <- relevel(d2$Roche_pos, ref = "No positives")
model_bin <- glm(Roche_pos ~ NetIncome_monthly_1,
                 family=binomial,d2[which(!is.na(d2$reactive)),])
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glm(Roche_pos ~ Household_Type_1,
                 family=binomial,d2)
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")


model_bin <- glm(Roche_pos ~ Household_Inhabitants,
                 family=binomial,d2)
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glm(Roche_pos ~ Housing_Type,
                 family=binomial,d2)
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

model_bin <- glm(Roche_pos ~ LivingArea_perInhabitant_cat,
                 family=binomial,d2)
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")

t1<- tableby( Roche_Result_new~ fe(Sex)+    chisq(Agegroup)+ fe(Birth_Country)+
                fe(EducationLevel)+ chisq(EmploymentStatus)+
                chisq(smokestatus)+ fe(Employment_ind_lastyear)+
                fe(any_risk_emp)+ fe(Employment_Airport)+
                fe(Employment_HealthCare)+ fe(Employment_EmergencyServices)+
                fe(Employment_Sales)+ fe(Employment_SeniorHome)+
                fe(Employment_Transportation)+ fe(Employment_Education)+
                fe(Employment_SocialWork)+ fe(Employment_OtherRiskType)
              , data = d1.1, control =  my_controls,simulate.p.value=TRUE, B=500)

write2html(summary(t1, pfootnote=TRUE), here_out("Table_Roche_1a.html"),
           title="Summary of Roche Results")



model_bin_Roche <- glmmTMB(Roche_Result_new ~Sex+Age+
                             any_risk_emp+
                             LivingArea_perInhabitant_cat+
                             Household_Type_1+
                             HealthStatus_Self_Allergies_Respiratory+
                             (1|hh_id),
                           family=binomial,d1.1)

summary(model_bin_Roche)


tidy(model_bin_Roche ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")



mod_Roche<-glmer_imp(Roche_Result_new ~Sex+Age+
                       any_risk_emp+
                       LivingArea_perInhabitant_cat+
                       Household_Type_1+
                       HealthStatus_Self_Allergies_Respiratory, data=d1.1[which(!is.na(d1.1$Roche_Result_new)),],
                     random=~(1|hh_id), n.chains = 3, n.adapt = 3000, n.iter = 12000,
                 thin = 1,seed=1234,family =binomial(link = "logit"))
traceplot(mod_Roche)
summary(mod_Roche)

cc_mod_Roche<- cbind(summary(mod_Roche)$res$Roche_Result_new$regcoef[, c('Mean')]
                   ,summary(mod_Roche)$res$Roche_Result_new$regcoef[, c('2.5%')],
                   summary(mod_Roche)$res$Roche_Result_new$regcoef[, c('97.5%')])  ## slow (~ 11 seconds)

exp(cc_mod_Roche)



# sensitivity for without the loss of sense of smell and taste

# Individuals without loss of sense in smell and taste
model_bin_Roche_2 <- glmmTMB(Roche_Result_new ~Sex+Age+
                             any_risk_emp+
                             LivingArea_perInhabitant_cat+
                             Household_Type_1+
                             HealthStatus_Self_Allergies_Respiratory+
                             (1|hh_id),
                           family=binomial,d1.1[which(d1.1$Symptoms_past2w_SenseLoss.Smell.Taste.!="Yes"),])

summary(model_bin_Roche_2)


tidy(model_bin_Roche_2 ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")



mod_Roche_2<-glmer_imp(Roche_Result_new ~Sex+Age+
                       any_risk_emp+
                       LivingArea_perInhabitant_cat+
                       Household_Type_1+
                       HealthStatus_Self_Allergies_Respiratory, data=d1.1[which(d1.1$Symptoms_past2w_SenseLoss.Smell.Taste.!="Yes"),],
                     random=~(1|hh_id), n.chains = 3, n.adapt = 3000, n.iter = 12000,
                     thin = 1,seed=1234,family =binomial(link = "logit"))
traceplot(mod_Roche_2)
summary(mod_Roche_2)

cc_mod_Roche<- cbind(summary(mod_Roche_2)$res$Roche_Result_new$regcoef[, c('Mean')]
                     ,summary(mod_Roche_2)$res$Roche_Result_new$regcoef[, c('2.5%')],
                     summary(mod_Roche_2)$res$Roche_Result_new$regcoef[, c('97.5%')])  ## slow (~ 11 seconds)

exp(cc_mod_Roche)

# Individuals with loss of sense in smell and taste
# GLM only not GLMM due to model fitting issues
model_bin_Roche_3 <- glm(Roche_Result_new ~Sex+Age+
                               any_risk_emp+
                               LivingArea_perInhabitant_cat+
                               Household_Type_1+
                               HealthStatus_Self_Allergies_Respiratory,
                             family=binomial,d1.1[which(d1.1$Symptoms_past2w_SenseLoss.Smell.Taste.!="No" &
                                                          d1.1$Household_Type_1!="Single"),])

summary(model_bin_Roche_3)

confint(model_bin_Roche_3,method="wald")

tidy(model_bin_Roche_3 ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="profile")



mod_Roche_3<-glm_imp(Roche_Result_new ~Sex+Age+
                         any_risk_emp+
                         LivingArea_perInhabitant_cat+
                         Household_Type_1+
                         HealthStatus_Self_Allergies_Respiratory,
                       data=d1.1[which(d1.1$Symptoms_past2w_SenseLoss.Smell.Taste.=="Yes" &
                                         d1.1$Household_Type_1!="Single"),],
                        n.chains = 3, n.adapt = 3000, n.iter = 12000,
                       thin = 1,seed=1234,family =binomial(link = "logit"))
traceplot(mod_Roche_3)
summary(mod_Roche_3)

cc_mod_Roche<- cbind(summary(mod_Roche_3)$res$Roche_Result_new$regcoef[, c('Mean')]
                     ,summary(mod_Roche_3)$res$Roche_Result_new$regcoef[, c('2.5%')],
                     summary(mod_Roche_3)$res$Roche_Result_new$regcoef[, c('97.5%')])  ## slow (~ 11 seconds)

exp(cc_mod_Roche)


# Concordance of loss of sense of smell/taste vs respiratory allergies

ctable(d1.1$Symptoms_past2w_SenseLoss.Smell.Taste.,d1.1$HealthStatus_Self_Allergies_Respiratory,
       OR = TRUE,
       RR = TRUE)


# End of sensitivity

# Number of infections in the household

d2$NetIncome_monthly_1<-factor(d2$NetIncome_monthly_1)
d2$Household_Type_1<-factor(d2$Household_Type_1)
d2$Household_Inhabitants_1<-factor(d2$Household_Inhabitants_1)
d2$Housing_Type<-factor(d2$Housing_Type)
d2$LivingArea_perInhabitant_cat<- factor(d2$LivingArea_perInhabitant_cat)


positive_hh<- reshape2::dcast(d1.1, hh_id ~Roche_Result_new)

positive_hh<- merge.data.frame(positive_hh,d2, by="hh_id", all.x = TRUE)

positive_hh$NetIncome_monthly_1<-relevel(positive_hh$NetIncome_monthly_1, ref = "2500-4000")

positive_hh<- merge.data.frame(positive_hh, hh_characteristics[c(2,5)], by="hh_id" )
# Zero Inflated Regression Model for household size adjusted number of positives  no other adjustment

model_bin <- glmmTMB(Positive~ Household_Inhabitants + offset(HouseholdSize)
                       ,
                     data=positive_hh,
                     ziformula=~.,
                     family=poisson)
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="wald")


# adjusting for household type in addition
model_bin <- glmmTMB(Positive~ Household_Inhabitants + positive_hh$Household_Type_1
                     ,
                     data=positive_hh,
                     ziformula=~Household_Inhabitants ,
                     family=poisson)
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="wald")


# adjusting for housingtype in addition
model_bin <- glmmTMB(Positive~ Household_Inhabitants + Housing_Type
                     ,
                     data=positive_hh,
                     ziformula=~Household_Inhabitants ,
                     family=poisson)
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="wald")


# adjusting for household income in addition
model_bin <- glmmTMB(Positive~ Household_Inhabitants +NetIncome_monthly_1
                     ,
                     data=positive_hh,
                     ziformula=~Household_Inhabitants ,
                     family=poisson)
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="wald")

# adjusting for household size
model_bin <- glmmTMB(Positive~ Household_Inhabitants + LivingArea_perInhabitant_cat
                     ,
                     data=positive_hh,
                     ziformula=~Household_Inhabitants ,
                     family=poisson)
summary(model_bin)
tidy(model_bin ,conf.int=TRUE,exponentiate=TRUE,effects="fixed",conf.method="wald")

ctable(positive_hh$Household_Inhabitants,positive_hh$Positive)



#rmarkdown::render("koco_summary.R",quiet = TRUE)
