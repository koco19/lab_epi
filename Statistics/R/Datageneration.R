rm(list=ls(all=TRUE))

# specify location to save files
# specify folders for script/data/output loading and saving
here_r = function (...)
  here::here("Statistics", "R", ...)
here_lab_data = function (...)
  here::here("KoCo19_Datasets", ...)

# Load settings and functions
source(here_r("setup.R"))
source(here_r("functions.R"))

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
# go to web address to download data
utils::browseURL("https://kumbox.med.uni-muenchen.de/public/?folder=398f3a0a&path=R")
Sys.sleep(60)

#read all R files which are saved in the source directory defined by the user
dat2<-readRDS("KoCo19_hhtInfo_labeled.rds")
dat3<-readRDS("KoCo19_tlnInfo_labeled.rds")
dat4<-readRDS("KoCo19_hhtQuest_labeled.rds")
dat5<-readRDS("KoCo19_tlnQuest_labeled.rds")
dat6<-readRDS("KoCo19_sympt_diary_labeled.rds")
dat7e<-readRDS("KoCo19_Lab_Euroimmune_labeled.rds")
dat7r<-readRDS("KoCo19_Lab_Roche_labeled.rds")
dat7r_dbs<-readRDS("KoCo19_Lab_RocheDBS_labeled.rds")
dat8<-readRDS("KoCo19_Lab_NT_Array_cPass_labeled.rds")
dat9a<-readRDS("KoCo19_FUQuestAdults_labeled.rds")
dat9b<-readRDS("KoCo19_FUQuestKids_labeled.rds")
dat10<-readRDS("KoCo19_FreeTimeQuest_labeled.rds")
# Household level data
hh_dat<-subset(dat2, select = c(hht_ID,Q1_b_Datum_Besuch,
                                Q5_a_Gebaeudetyp,Q5_c_WievieleMenschen))
colnames(hh_dat)<-c("hh_id","VisitDate_Baseline","HousingType","HouseholdSize")
hh_dat$HousingType<-recode(hh_dat$HousingType,
                           "Freistehendes Ein-/Zweifamilienhaus" = "Type1",
                           "Ein- / Zweifamilienhaus als Reihenhaus oder Doppelhaus"= "Type2",
                           "Wohnhaus mit 3-4 Wohnungen"="Type3",
                           "Wohnhaus mit 5-8 Wohnungen"="Type4",
                           "Wohnhaus mit 9 und mehr Wohnungen, aber kein Hochhaus"="Type5",
                           "Hochhaus (9 und mehr Stockwerke)"="Type6",
                           "Andere (Wohnwagen, Zelt etc.)"="Type7")
hh_dat$HousingType<-factor(hh_dat$HousingType,
                           levels=c("Type1","Type2","Type3",
                                    "Type4","Type5","Type6",
                                    "Type7"))

# duplicates at household
hh_d<-reshape2::dcast(hh_dat,hh_id+HouseholdSize+VisitDate_Baseline~.,
                      fun.aggregate = length)
hh_dup<-hh_d[which(hh_d$.>1),] # subset the duplicated HHID's
if(length(hh_dup$hh_id)==0) {print("No duplicates")}
hh_dat$duplicated_hh<-duplicated(hh_dat$hh_id)
# Individual level data
ind_dat<-subset(dat3, select=c(tln_ID,hht_ID,
                               Q1_b_Datum_Besuch, Q6_e_Teilnahme,
                               Q6_d_Geschlecht,
                               der_alter,Q7_a_Groesse,
                               Q7_b_Gewicht,Q7_c_Bauchumfang,
                               Q9_a_Fingerprick_geschult,
                               Q9_b_Fingerprick_selbst,
                               Q9_c_Einwilligung_Genetik,
                               blut_ID_1,Q1_b_Datum_Besuch_blut_1,    
                               blut_ID_2,Q1_b_Datum_Besuch_blut_2,    
                               prick_ID_1,Q1_b_Datum_Besuch_prick_1,   
                               prick_ID_2,Q1_b_Datum_Besuch_prick_2,   
                               abstrich_ID_1,Q1_b_Datum_Besuch_abstrich_1))
colnames(ind_dat)<-c("ind_id","hh_id","VisitDate_Baseline",
                     "Participation_Type",
                     "Sex","Age","Height","Weight","Waist_length",
                     "Fingerprick_trainedperson","Fingerprick_self",
                     "Geneticinfo_consent","BloodtestID_V1",
                     "Bloodtest_Date_V1","BloodtestID_V2",
                     "Bloodtest_Date_V2","PricktestID_V1",
                     "Pricktest_Date_V1","PricktestID_V2",
                     "Pricktest_Date_V2",
                     "NasalswabID_V1","Nasalswab_Date_V1")


# find duplicated IDs
ind_d<-reshape2::dcast(ind_dat,hh_id+ind_id+Sex+Age+VisitDate_Baseline~.,
                       fun.aggregate = length)
ind_dup<-ind_d[which(ind_d$.>1),]

if(length(ind_dup$hh_id)==0){print("No duplicated individuals")}
ind_dat$duplicated_ind<-duplicated(ind_dat$ind_id)

ind_dat<- ind_dat[which(ind_dat$duplicated_ind==FALSE),]

ind_dat$Participation_Type<-recode(ind_dat$Participation_Type,
                                  "Nimmt nicht teil"= "Non participant",
                                  "Ja, nimmt an Blutproben und Interviews teil"="Participant-Interview/Bloodtest",
                                  "Nimmt nur an Interviews teil"="Participant-Interview")
ind_dat$Participation_Type<-factor(ind_dat$Participation_Type,
                                   levels=c("Participant-Interview/Bloodtest",
                                            "Participant-Interview",
                                            "Non participant"))
ind_dat$Blood_Testing<-ifelse(!is.na(ind_dat$BloodtestID_V1),"Yes",
                              "No")
  
ind_dat$Sex1<- ifelse(ind_dat$Sex=="Weiblich","Female",
                      ifelse(ind_dat$Sex=="Divers","Other","Male"))
ind_dat$Sex<-ind_dat$Sex1
ind_dat$Sex1<-NULL

ind_dat[which(ind_dat$ind_id=="2609S01"),]$Age<-62
ind_dat[which(ind_dat$ind_id=="3115A01"),]$Age<-41
ind_dat[which(ind_dat$ind_id=="3221T01"),]$Age<-65


ind_dat$Age_Cat_WHO<-ifelse(ind_dat$Age<=19,"<=19years",
                            ifelse(ind_dat$Age>=20 & ind_dat$Age<=29,"20-29years",
                            ifelse(ind_dat$Age>=30 & ind_dat$Age<=39,"30-39years",
                            ifelse(ind_dat$Age>=40 & ind_dat$Age<=49,"40-49years",
                            ifelse(ind_dat$Age>=50 & ind_dat$Age<=59,"50-59years",
                            ifelse(ind_dat$Age>=60 & ind_dat$Age<=69,"60-69years",
                            ifelse(ind_dat$Age>=70 & ind_dat$Age<=79,"70-79years",
                            ifelse(ind_dat$Age>=80 & ind_dat$Age<=89,"80-89years",
                                   ">=90years"))))))))
ind_dat$Age_Cat_WHO<-factor(ind_dat$Age_Cat_WHO,
                            levels=c("<=19years","20-29years","30-39years","40-49years",
                                     "50-59years","60-69years","70-79years","80-89years",
                                     ">=90years"))

ind_dat$Age_Cat_RKI<-ifelse(ind_dat$Age <=4,"<=4years",
                           ifelse(ind_dat$Age >=5 & ind_dat$Age <=14,"5-14years",
                           ifelse(ind_dat$Age >=15 & ind_dat$Age <=34,"15-34years",
                           ifelse(ind_dat$Age >=35 & ind_dat$Age <=59,"35-59years",
                           ifelse(ind_dat$Age >=60 & ind_dat$Age <=79,"60-79years",
                                  ">=80years")))))
ind_dat$Age_Cat_RKI<- factor(ind_dat$Age_Cat_RKI,
                             levels = c("<=4years","5-14years","15-34years",
                                        "35-59years","60-79years",">=80years"))
ind_dat$Fingerprick_trainedperson<-factor(ind_dat$Fingerprick_trainedperson,
                                          levels=c("Nein","Ja",
                                                   "Wurde schon beantwortet bei Haushaltsbesuch oder früherem FU Follow-Up Besuch"),
                                          labels = c("No","Yes","Answeredpreviously"))
ind_dat$Fingerprick_self<-factor(ind_dat$Fingerprick_self,
                                          levels=c("Nein","Ja",
                                                   "Wurde schon beantwortet bei Haushaltsbesuch oder früherem FU Follow-Up Besuch"),
                                          labels = c("No","Yes","Answeredpreviously"))
ind_dat$Geneticinfo_consent<-factor(ind_dat$Geneticinfo_consent,
                                 levels=c("Nein","Ja",
                                          "Wurde schon beantwortet bei Haushaltsbesuch oder früherem FU Follow-Up Besuch"),
                                 labels = c("No","Yes","Answeredpreviously"))


ind_dat<- merge.data.frame(ind_dat, hh_dat,
                           by=c("hh_id","VisitDate_Baseline"),all.x = TRUE)

# participants per household
pp.hh<- reshape2::dcast(ind_dat,hh_id~.)
colnames(pp.hh)<-c("hh_id","obs_hh_members")
ind_dat<-merge.data.frame(ind_dat,pp.hh, by="hh_id",all.x = TRUE)

#ind_dat$daysinstudy<- difftime(ind_dat$VisitDate_Baseline,ind_dat$VisitDate_Baseline,
#                               units = "days")

# cumulative total of households recruited/individuals recruited 
# blood samples collected
hh_cum<-reshape2::dcast(hh_dat,VisitDate_Baseline~.)
colnames(hh_cum)<-c("VisitDate","Count")
hh_cum$cum_count<-cumsum(hh_cum$Count)
hh_cum$category<-"Recruited Households"

ind_cum<-reshape2::dcast(ind_dat,VisitDate_Baseline~.)
colnames(ind_cum)<-c("VisitDate","Count")
ind_cum$cum_count<-cumsum(ind_cum$Count)
ind_cum$category<-"Recruited Individuals"

ind_blood_cum<-reshape2::dcast(ind_dat[which(ind_dat$Blood_Testing=="Yes"),],
                         VisitDate_Baseline~.)
colnames(ind_blood_cum)<-c("VisitDate","Count")
ind_blood_cum$cum_count<-cumsum(ind_blood_cum$Count)
ind_blood_cum$category<-"Blood Sample Collected"

#Cumulative recruitment

recruitment_dat<-rbind.data.frame(hh_cum,ind_cum,ind_blood_cum)

ggplot(data = recruitment_dat,aes(x=VisitDate,y=cum_count))+
  geom_line(aes(group=category,colour=category))+
  geom_point(aes(colour=category))+
  xlab("Datum")+
  ylab("Kumulativ #")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_date(date_labels="%d-%m-%y",date_breaks  ="1 week")+
  labs(colour=NULL)+
  geom_hline(yintercept = 3000, linetype=2,colour="black")+
  scale_colour_discrete( labels = c("Blutproben", "Haushalte", "Bewohner"))+
  theme(legend.position="bottom")

ggsave("recruitment_data_deu.jpeg",height=5.5,width = 7.5,units="in", dpi=1080)

ggsave("recruitment_data_deu.pdf",height=5.5,width = 7.5, units="in",dpi=1080)

ggplot(data = recruitment_dat,aes(x=VisitDate,y=cum_count))+
  geom_line(aes(group=category,colour=category))+
  geom_point(aes(colour=category))+
  xlab("Study Date")+
  ylab("Cumulative Count")+
  geom_hline(yintercept = 3000, linetype=2,colour="black")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_date(date_labels="%d-%m-%y",date_breaks  ="1 week")+
  labs(colour=NULL)+
  scale_colour_discrete( labels = c("Blood Tests Performed",
                                    "Households Recruited",
                                    "Individuals Recruited"))+
  theme(legend.position="bottom")

ggsave("recruitment_data_eng.pdf",height=5.5,width = 7.5,units="in",dpi=720)


# Household specific additional characteristics
dat4<-dat4[c(-24)]
colnames(dat4)<-c("hh_id","WebForm_EntryDate","ResponsibleIndividual_ID",
                  "num_rooms","livingarea_sqm","livingstyle","livingstyle_others",
                  "daily_cigarette_count","Employment_SeniorHome",
                  "Employment_Hospital","Employment_DoctorsClinic",
                  "Employment_Sales","Employment_Transportation",
                  "Employment_Airport","Employment_EmergencyServices",
                  "Employment_OtherRiskType","Employment_OtherSpecification",
                  "Previoustesting_COVID","Num_memberstested",
                  "Num_memberspositive","HighestEducation_hh",
                  "Employmentstatus_hh","NetIncome_month_hh")


hh_characteristics<-merge.data.frame(hh_dat,dat4,by="hh_id",all.x = TRUE)
hh_characteristics$duplicated_hh<-duplicated(hh_characteristics$hh_id)

hh_characteristics<-hh_characteristics[which(hh_characteristics$duplicated_hh!=TRUE),]

hh_characteristics$livingstyle<-factor(hh_characteristics$livingstyle,
                                       levels=c("Einzelbewohner oder Paar",
                                                "Familie mit Kindern (inkl. Patchwork-Familien)",
                                                "Wohngemeinschaft",
                                                "Untervermietete Zimmer in Haus/Wohnung",
                                                "Betreutes Wohnen etc.",
                                                "Andere Wohnform"),
                                       labels=c("Type1","Type2","Type3","Type4","Type5","Type6"))

hh_characteristics$daily_cigarette_count<-factor(hh_characteristics$daily_cigarette_count,
                                                 levels=c("keine",         "1-5 Zigaretten",        "6-10 Zigaretten",
                                                          "11- 20 Zigaretten" ,"21-40 Zigaretten", "mehr als 40 Zigaretten"),
                                                 labels = c("Nill","1-5 cigarettes","6-10 cigarettes",
                                                            "11-20 cigarettes","21-40 cigarettes",">40 cigarettes"))

hh_characteristics$Employment_SeniorHome<-factor(hh_characteristics$Employment_SeniorHome,
                                                 levels = c("ja","nein"),
                                                 labels = c("Yes","No"))
hh_characteristics$Employment_Hospital<-factor(hh_characteristics$Employment_Hospital,
                                               levels = c("ja","nein"),
                                               labels = c("Yes","No"))
hh_characteristics$Employment_DoctorsClinic<-factor(hh_characteristics$Employment_DoctorsClinic,
                                                    levels = c("ja","nein"),
                                                    labels = c("Yes","No"))

hh_characteristics$Employment_Sales<-factor(hh_characteristics$Employment_Sales,
                                            levels = c("ja","nein"),
                                            labels = c("Yes","No"))
hh_characteristics$Employment_Transportation<-factor(hh_characteristics$Employment_Transportation,
                                                     levels = c("ja","nein"),
                                                     labels = c("Yes","No"))
hh_characteristics$Employment_Airport<-factor(hh_characteristics$Employment_Airport,
                                              levels = c("ja","nein"),
                                              labels = c("Yes","No"))
hh_characteristics$Employment_EmergencyServices<-factor(hh_characteristics$Employment_EmergencyServices,
                                                        levels = c("ja","nein"),
                                                        labels = c("Yes","No"))
hh_characteristics$Employment_OtherRiskType<-factor(hh_characteristics$Employment_OtherRiskType,
                                                    levels = c("ja","nein"),
                                                    labels = c("Yes","No"))
hh_characteristics$Previoustesting_COVID<-factor(hh_characteristics$Previoustesting_COVID,
                                                 levels = c("ja","nein"),
                                                 labels = c("Yes","No"))
hh_characteristics$HighestEducation_hh<-factor(hh_characteristics$HighestEducation_hh, 
                                               levels = c("Hauptschulabschluss/Volksschulabschluss (Mittelschule)",
                                                          "Realschulabschluss (mittlere Reife, Mittelschule)",
                                                          "Fachhochschulreife/fachgebundene Hochschulreife",
                                                          "Abitur / allgemeine Hochschulreife",
                                                          "Fachhochschulabschluss, auch Ingenieurschulabschluss ", 
                                                          "Abschluss einer Universität, wissenschaftlichen Hochschule, Kunsthochschule",
                                                          "anderer Schulabschluss",
                                                          "Schule beendet ohne Abschluss",
                                                          "noch kein Schulabschluss",
                                                          "Ich weiß nicht"),
                                               labels = c("Type1","Type2","Type3","Type4","Type5",
                                                          "Type6","Type7","Type8","Type9","Type10"))
hh_characteristics$Employmentstatus_hh<-factor(hh_characteristics$Employmentstatus_hh,
                                               levels = c("ja","nein"),
                                               labels = c("Yes","No"))
hh_characteristics$NetIncome_month_hh<-factor(hh_characteristics$NetIncome_month_hh,
                                              levels = c("unter 500 Euro",    "500 bis unter 750 Euro","750 bis unter 1.000 Euro",
                                                         "1.000 bis unter 1.500 Euro", "1.500 bis unter 2.000 Euro","2.000 bis unter 2.500 Euro",
                                                         "2.500 bis unter 3.000 Euro","3.000 bis unter 4.000 Euro","4.000 bis unter 5.000 Euro",
                                                         "5.000 bis unter 6.000 Euro","6.000 Euro und mehr","Keine Angabe"),
                                              labels = c("<500","500-750","750-1000","1000-1500","1500-2000","2000-2500",
                                                         "2500-3000","3000-4000","4000-5000","5000-6000",">6000","No Answer"))

hh_characteristics$HouseholdSize <- ifelse(hh_characteristics$HouseholdSize==9999,NA,hh_characteristics$HouseholdSize)
hh_characteristics<- hh_characteristics[which(hh_characteristics$hh_id %in% 
                                                c("2716W00", "3752F00", "3755B00", "3751F00","5458M00")==FALSE),]

hh_const<-read.csv("KoCo19_Haushalte4Modeler_20200909.csv",header = T)
hh_const<- hh_const[which(hh_const$hh_id %in% 
                    c("2716W00", "3752F00", "3755B00", "3751F00","5458M00")==FALSE),]

write.csv(hh_const,"Household_Constituencies.txt")
write.csv(hh_const,"Household_Constituencies.csv")
hh_const$X<-NULL
colnames(hh_const)[1]<-c("hh_id")
names(hh_const)

hh_characteristics<-merge.data.frame(hh_characteristics,hh_const,by="hh_id",all.x = TRUE)

obspp_hh<- reshape2::dcast(ind_dat, hh_id~. , value.var = 'obs_hh_members') # replace household size by observed hh members depending if hh size is smaller than observed members

hh_characteristics<- merge.data.frame(hh_characteristics,obspp_hh,by="hh_id")

hh_characteristics$HouseholdSize<- ifelse(hh_characteristics$HouseholdSize< hh_characteristics$.,hh_characteristics$.,hh_characteristics$HouseholdSize)

hh_characteristics$.<-NULL

write.csv(hh_characteristics,"KoCo19_hhcharacteristics.csv")
write.table(hh_characteristics, "KoCo19_hhcharacteristics.txt", append = FALSE, sep = "\t ", dec = ".",
            col.names = TRUE)
labs_hhchar<-c("Household ID",
               "Visit Date of Baseline data reference",
               "Housing type categories- refer to description",
               "Household size",
               "duplicated_household",
               "Web form entry date",
               "Individual filling up the household form",
               "number of rooms in the house",
               "living area in square meters",
               "living style- refer to description",
               "living style- others specification",
               "daily number of cigarettes smoked in the household",
               "Employed household member at Senior Home",
               "Employed household member at Hospital",
               "Employed household member at Doctors Clinic",
               "Employed household member at Sales position/Supermarket",
               "Employed household member at Public Transport services",
               "Employed household member at Airport",
               "Employed household member at Emergency services",
               "Employed household member at Others",
               "Specification of Others",
               "Covid testing of Household member in the past",
               "How many members tested?",
               "How many were tested as positive",
               "Highest Education level in the household",
               "Employment status in the household having fulltime or partime job outside home",
               "Monthly net income of the household",
               "address_id",                 "postcode",                  
               "city_district",              "suburb",                     "ConstIdResideStr",          
               "ID_RandomRoute",             "Random_Route_ID_korrigiert","constituency")
variablebook_hhchar<- cbind(names(hh_characteristics),labs_hhchar)


colnames(variablebook_hhchar)<-c("Variable name","Label description")
write.table(variablebook_hhchar,"variablebook_hhchar.txt",append = FALSE, sep = "\t ", dec = ".",
            col.names = TRUE, row.names = FALSE)

# Individual specific additional characteristics and risk factors
dat5<-dat5[c(-66)]
colnames(dat5)<-c("ind_id","Web form entry date",
                  "pcpt_type","pers_id","pers_DOB","pcpt_DOB",
                  "Birth_Country","Education_ind",
                  "Employment_ind","Employment_ind_lastyear",
                  "Symptoms_past2w_Fever","Symptoms_past2w_Chills",
                  "Symptoms_past2w_Limbpain","Symptoms_past2w_Runningnose",
                  "Symptoms_past2w_SenseLoss(Smell/Taste)","Symptoms_past2w_Sorethroat",
                  "Symptoms_past2w_Headache","Symptoms_past2w_Drycough",
                  "Symptoms_past2w_Breathlessness","Symptoms_past2w_Fatigue",
                  "Symptoms_past2w_diarrhoea",
                  "Testing_pastattempt","Testing_pastperformed",
                  "Testing_location_GP","Testing_location_WorkDoctor",
                  "Testing_location_Hospital","Testing_location_DriveThroughStation",
                  "Testing_location_OtherInstitution","Testing_OtherSpecification",
                  "Testing_positive",
                  "HealthStatus_Self_Overall","HealthStatus_Self_Diabetes",
                  "HealthStatus_Self_LungDisease","HealthStatus_Self_CardiovascularDisease",
                  "HealthStatus_Self_Cancer","HealthStatus_Self_Obesity",
                  "HealthStatus_Self_Allergies_Skin","HealthStatus_Self_Allergies_Respiratory",
                  "HealthStatus_Self_Autoimmune","HealthStatus_Self_HIV",
                  "HealthStatus_Self_OtherChronic","HealthStatus_Self_OtherChronic_Specification",
                  "Medication_Immunosuppressive","Medication_Cortisone",
                  "Medication_Bloodpressure","Vaccination_Influenza2019",
                  "Pregnant","Pregnancy_Weeks",
                  "Smoking_Ever_min1year","Smoking_pastmonth",
                  "Nicotine_past6months","Facemask_public",
                  "Contact_past2w_covpos","Contact_past2w_covsusp",
                  "Result_covsusp_case",
                  "Employment_SeniorHome",
                  "Employment_Hospital",
                  "Employment_DoctorsClinic",	
                  "Employment_Sales",
                  "Employment_Transportation",
                  "Employment_Airport",	
                  "Employment_EmergencyServices",
                  "Employment_Nolistedoption",
                  "Employment_OtherRiskType",
                  "Employment_OtherSpecification")




dat5$pcpt_type<-factor(dat5$pcpt_type,
                       levels = c("Für mich selbst","Für ein anderes Haushaltsmitglied"),
                       labels = c("self","other member")) 

dat5$Education_ind<-factor(dat5$Education_ind,
                           levels = c("Hauptschulabschluss/Volksschulabschluss (Mittelschule)",
                                      "Realschulabschluss (mittlere Reife, Mittelschule)",
                                      "Fachhochschulreife/fachgebundene Hochschulreife",
                                      "Abitur / allgemeine Hochschulreife",
                                      "Fachhochschulabschluss, auch Ingenieurschulabschluss ", 
                                      "Abschluss einer Universität, wissenschaftlichen Hochschule, Kunsthochschule",
                                      "anderer Schulabschluss",
                                      "Schule beendet ohne Abschluss",
                                      "noch kein Schulabschluss",
                                      "Ich weiß nicht"),
                           labels = c("Type1","Type2","Type3","Type4","Type5",
                                      "Type6","Type7","Type8","Type9","Type10")) 

dat5$Employment_ind<-factor(dat5$Employment_ind,
                           levels = c("Zur Zeit nicht berufstätig (Rentner, Student, Schüler)", 
                                      "Arbeitslos", 
                                      "Auszubildende/r", 
                                      "Angestellte/r", 
                                      "Arbeiter/-in", 
                                      "Selbstständige/-r ohne Beschäftigte (auch Honorarkräfte, Personen mit Werkvertrag)", 
                                      "Selbstständige/-r mit Beschäftigten", 
                                      "Mithelfende/-r Familienangehörige/-r(unbezahlte Tätigkeit)", 
                                      "Beamter/Beamtin, Richter/-in, Dienstordnungsangestellte/-r", 
                                      "Zeitsoldat/-in, Berufssoldat/-in", 
                                      "FSJler/-in / BFJler/-in", 
                                      "Nebenjobber/-in", 
                                      "Vorübergehende Freistellung / Kurzarbeit", 
                                      "keine Angabe"),
                           labels = c("Type1","Type2","Type3","Type4","Type5",
                                      "Type6","Type7","Type8","Type9","Type10",
                                      "Type11","Type12","Type13","Type14"))

dat5$Employment_ind_lastyear<- factor(dat5$Employment_ind_lastyear,
                                      levels=c( "nein","ja","Ich bin mir unsicher"),
                                      labels=c("No","Yes","Not sure"))

dat5$Symptoms_past2w_Fever<-factor(dat5$Symptoms_past2w_Fever,
                                   levels = c("nein","ja","weiß nicht"),
                                   labels = c("No","Yes","Not sure"))
dat5$Symptoms_past2w_Chills<-factor(dat5$Symptoms_past2w_Chills,
                                   levels = c("nein","ja","weiß nicht"),
                                   labels = c("No","Yes","Not sure"))
dat5$Symptoms_past2w_Limbpain<-factor(dat5$Symptoms_past2w_Limbpain,
                                   levels = c("nein","ja","weiß nicht"),
                                   labels = c("No","Yes","Not sure"))
dat5$Symptoms_past2w_Runningnose<-factor(dat5$Symptoms_past2w_Runningnose,
                                   levels = c("nein","ja","weiß nicht"),
                                   labels = c("No","Yes","Not sure"))
dat5$`Symptoms_past2w_SenseLoss(Smell/Taste)`<-factor(dat5$`Symptoms_past2w_SenseLoss(Smell/Taste)`,
                                   levels = c("nein","ja","weiß nicht"),
                                   labels = c("No","Yes","Not sure"))
dat5$Symptoms_past2w_Sorethroat<-factor(dat5$Symptoms_past2w_Sorethroat,
                                   levels = c("nein","ja","weiß nicht"),
                                   labels = c("No","Yes","Not sure"))
dat5$Symptoms_past2w_Headache<-factor(dat5$Symptoms_past2w_Headache,
                                   levels = c("nein","ja","weiß nicht"),
                                   labels = c("No","Yes","Not sure"))
dat5$Symptoms_past2w_Drycough<-factor(dat5$Symptoms_past2w_Drycough,
                                   levels = c("nein","ja","weiß nicht"),
                                   labels = c("No","Yes","Not sure"))
dat5$Symptoms_past2w_Breathlessness<-factor(dat5$Symptoms_past2w_Breathlessness,
                                   levels = c("nein","ja","weiß nicht"),
                                   labels = c("No","Yes","Not sure"))
dat5$Symptoms_past2w_Fatigue<-factor(dat5$Symptoms_past2w_Fatigue,
                                   levels = c("nein","ja","weiß nicht"),
                                   labels = c("No","Yes","Not sure"))
dat5$Symptoms_past2w_diarrhoea<-factor(dat5$Symptoms_past2w_diarrhoea,
                                     levels = c("nein","ja","weiß nicht"),
                                     labels = c("No","Yes","Not sure"))

dat5$Testing_pastattempt<-factor(dat5$Testing_pastattempt,
                                       levels = c("nein","ja"),
                                       labels = c("No","Yes"))

dat5$Testing_pastperformed<-factor(dat5$Testing_pastperformed,
                                 levels = c("nein","ja"),
                                 labels = c("No","Yes"))
dat5$Testing_location_GP<-factor(dat5$Testing_location_GP,
                                 levels = c("nein","ja"),
                                 labels = c("No","Yes"))
dat5$Testing_location_WorkDoctor<-factor(dat5$Testing_location_WorkDoctor,
                                 levels = c("nein","ja"),
                                 labels = c("No","Yes"))
dat5$Testing_location_Hospital<-factor(dat5$Testing_location_Hospital,
                                 levels = c("nein","ja"),
                                 labels = c("No","Yes"))
dat5$Testing_location_DriveThroughStation<-factor(dat5$Testing_location_DriveThroughStation,
                                 levels = c("nein","ja"),
                                 labels = c("No","Yes"))
dat5$Testing_location_OtherInstitution<-factor(dat5$Testing_location_OtherInstitution,
                                                  levels = c("nein","ja"),
                                                  labels = c("No","Yes"))


dat5$Testing_positive<-factor(dat5$Testing_positive,
                              levels = c("nein, kein positives Testergebnis",
                                         "ja, mindestens ein Testergebnis war positiv",
                                         "ich habe noch kein Testergebnis erhalten"),
                              labels = c("No positive",
                                         "Yes-1 positive atleast",
                                         "Result not available"))
dat5$HealthStatus_Self_Overall<-factor(dat5$HealthStatus_Self_Overall,
                                       levels = c("ausgezeichnet","sehr gut","gut","weniger gut","schlecht"),
                                       labels = c("excellent", "very good", "good", "less good", "bad"))


dat5$HealthStatus_Self_Diabetes<-factor(dat5$HealthStatus_Self_Diabetes,
                                       levels = c("nein","ja","weiß ich nicht"),
                                       labels = c("No","Yes","Not sure"))
dat5$HealthStatus_Self_LungDisease<-factor(dat5$HealthStatus_Self_LungDisease,
                                        levels = c("nein","ja","weiß ich nicht"),
                                        labels = c("No","Yes","Not sure"))

dat5$HealthStatus_Self_CardiovascularDisease<-factor(dat5$HealthStatus_Self_CardiovascularDisease,
                                        levels = c("nein","ja","weiß ich nicht"),
                                        labels = c("No","Yes","Not sure"))
dat5$HealthStatus_Self_Cancer<-factor(dat5$HealthStatus_Self_Cancer,
                                        levels = c("nein","ja","weiß ich nicht"),
                                        labels = c("No","Yes","Not sure"))
dat5$HealthStatus_Self_Obesity<-factor(dat5$HealthStatus_Self_Obesity,
                                        levels = c("nein","ja","weiß ich nicht"),
                                        labels = c("No","Yes","Not sure"))
dat5$HealthStatus_Self_Allergies_Skin<-factor(dat5$HealthStatus_Self_Allergies_Skin,
                                        levels = c("nein","ja","weiß ich nicht"),
                                        labels = c("No","Yes","Not sure"))
dat5$HealthStatus_Self_Allergies_Respiratory<-factor(dat5$HealthStatus_Self_Allergies_Respiratory,
                                        levels = c("nein","ja","weiß ich nicht"),
                                        labels = c("No","Yes","Not sure"))
dat5$HealthStatus_Self_Autoimmune<-factor(dat5$HealthStatus_Self_Autoimmune,
                                        levels = c("nein","ja","weiß ich nicht"),
                                        labels = c("No","Yes","Not sure"))
dat5$HealthStatus_Self_HIV<-factor(dat5$HealthStatus_Self_HIV,
                                        levels = c("nein","ja","weiß ich nicht"),
                                        labels = c("No","Yes","Not sure"))

dat5$HealthStatus_Self_OtherChronic<-factor(dat5$HealthStatus_Self_OtherChronic,
                                   levels = c("nein","ja","weiß ich nicht"),
                                   labels = c("No","Yes","Not sure"))

dat5$Medication_Immunosuppressive<-factor(dat5$Medication_Immunosuppressive,
                                            levels = c("nein","ja","weiß nicht"),
                                            labels = c("No","Yes","Not sure"))
dat5$Medication_Cortisone<-factor(dat5$Medication_Cortisone,
                                          levels = c("nein","ja","weiß nicht"),
                                          labels = c("No","Yes","Not sure"))
dat5$Medication_Bloodpressure<-factor(dat5$Medication_Bloodpressure,
                                          levels = c("nein","ja","weiß nicht"),
                                          labels = c("No","Yes","Not sure"))
dat5$Vaccination_Influenza2019<-factor(dat5$Vaccination_Influenza2019,
                                          levels = c("nein","ja","weiß nicht"),
                                          labels = c("No","Yes","Not sure"))
dat5$Pregnant<-factor(dat5$Pregnant,
                                          levels = c("nein","ja","weiß nicht"),
                                          labels = c("No","Yes","Not sure"))
dat5$Smoking_Ever_min1year<-factor(dat5$Smoking_Ever_min1year,
                                          levels = c("nein","ja","weiß nicht"),
                                          labels = c("No","Yes","Not sure"))
dat5$Smoking_pastmonth<-factor(dat5$Smoking_pastmonth,
                                          levels = c("nein","ja"),
                                          labels = c("No","Yes"))
dat5$Nicotine_past6months<-factor(dat5$Nicotine_past6months,
                               levels = c("nein","ja","weiß nicht"),
                               labels = c("No","Yes","Not sure"))
dat5$Contact_past2w_covpos<-factor(dat5$Contact_past2w_covpos,
                               levels = c("nein","ja","weiß nicht"),
                               labels = c("No","Yes","Not sure"))

dat5$Contact_past2w_covsusp<-factor(dat5$Contact_past2w_covsusp,
                                   levels = c("nein","ja","weiß nicht"),
                                   labels = c("No","Yes","Not sure"))
dat5$Result_covsusp_case<-factor(dat5$Result_covsusp_case,
                                    levels = c("nein","ja","weiß nicht"),
                                    labels = c("No","Yes","Not sure"))

dat5$Facemask_public<-factor(dat5$Facemask_public,
                             levels=c("nie","selten","gelegentlich","oft","immer",
                                      "ich vermeide Begegnungen mit Fremden vollkommen"),
                             labels = c("never", "rarely", "occasionally", "often", "always",
                                        "avoid contact with strangers"))
                             
dat5$Employment_SeniorHome<-factor(dat5$Employment_SeniorHome,
                               levels = c("nein","ja"),
                               labels = c("No","Yes"))
dat5$Employment_Hospital<-factor(dat5$Employment_Hospital,
                                   levels = c("nein","ja"),
                                   labels = c("No","Yes"))
dat5$Employment_DoctorsClinic<-factor(dat5$Employment_DoctorsClinic,
                                   levels = c("nein","ja"),
                                   labels = c("No","Yes"))
dat5$Employment_Sales<-factor(dat5$Employment_Sales,
                                   levels = c("nein","ja"),
                                   labels = c("No","Yes"))
dat5$Employment_Transportation<-factor(dat5$Employment_Transportation,
                                   levels = c("nein","ja"),
                                   labels = c("No","Yes"))
dat5$Employment_Airport<-factor(dat5$Employment_Airport,
                                       levels = c("nein","ja"),
                                       labels = c("No","Yes"))
dat5$Employment_EmergencyServices<-factor(dat5$Employment_EmergencyServices,
                                       levels = c("nein","ja"),
                                       labels = c("No","Yes"))
dat5$Employment_Nolistedoption<-factor(dat5$Employment_Nolistedoption,
                                       levels = c("nein","ja"),
                                       labels = c("No","Yes"))
dat5$Employment_OtherRiskType<-factor(dat5$Employment_OtherRiskType,
                                       levels = c("nein","ja"),
                                       labels = c("No","Yes"))


# Updated list from Katja on the employment others and corrected
# Correction data in Shared Drive

employment_dat_corr<- read_xlsx("ArbeitinanderenBereichen_KR.xlsx",col_names = T)

colnames(employment_dat_corr)[1]<-"ind_id"

# update employment others option where there is a correction
t<- employment_dat_corr[which(employment_dat_corr$ArbBerAnders!=employment_dat_corr$ArbBerAnders_corrected),]
dat5$Employment_OtherRiskType<- ifelse(dat5$ind_id %in% t$ind_id==TRUE & dat5$Employment_OtherRiskType=="Yes","No",
                                       ifelse(dat5$ind_id %in% t$ind_id==TRUE & dat5$Employment_OtherRiskType=="No",
                                              "Yes",ifelse(dat5$ind_id %in% t$ind_id==FALSE & is.na(dat5$Employment_OtherRiskType)==FALSE,
                                              paste(dat5$Employment_OtherRiskType),NA)))


# update employment doctors_clinic/ emergency services/ transportation/ sales
l<-length(dat5$ind_id)
for(i in 1:l)
{ t<- dat5[i,]
  
  t1<- employment_dat_corr[which(employment_dat_corr$ind_id==t$ind_id),]
  n1<-nrow(t1)
  if(n1==1)
  {x1<- ifelse(t1$Gesundheitswesen=="ja","Yes","No")
                                             
   x2<- ifelse(t1$`Office/Sale/Handwerk`=="ja" ,"Yes","No")
                                             
   
   x3<-ifelse(t1$`Police/Security` =="ja" ,"Yes","No")
                                     
  
   x4<-ifelse(is.na(t1$Comment) ==FALSE ,"Yes","No")
                                             
  
  
  dat5[i,]$Employment_DoctorsClinic<-x1
  dat5[i,]$Employment_Sales<-x2
  dat5[i,]$Employment_EmergencyServices <-x3
  dat5[i,]$Employment_Transportation<-x4
  }
  }

ind_characteristics<-merge.data.frame(ind_dat,dat5,by=c("ind_id"),all.x = TRUE)


ind_characteristics$HouseholdSize <- ifelse(ind_characteristics$HouseholdSize==9999,NA,ind_characteristics$HouseholdSize)
ind_characteristicsHeight<- ifelse(ind_characteristics$Height==999,NA,ind_characteristics$Height)
ind_characteristics$Weight<- ifelse(ind_characteristics$Weight==999.9,NA,ind_characteristics$Weight)
ind_characteristics$Waist_length<- ifelse(ind_characteristics$Waist_length==999,NA,ind_characteristics$Waist_length)


ind_characteristics$HouseholdSize<- ifelse(ind_characteristics$HouseholdSize< ind_characteristics$obs_hh_members,
                                           ind_characteristics$obs_hh_members,ind_characteristics$HouseholdSize)

pcr_new<-read_xlsx("KoCo19MismatchesPcrSerology_2020.09.04_modeller_a.xlsx",col_names = T)

# test not performed
pcr_new_np<- pcr_new[which(pcr_new$PCR_true=="No"),]
ind_characteristics$Testing_pastperformed
ind_characteristics[which(ind_characteristics$ind_id %in% pcr_new_np$tln_id==TRUE),]$Testing_pastperformed<-"No"

# Testing performed
pcr_new_p<-pcr_new[which(pcr_new$PCR_true=="Yes"),]
pcr_new_p$PcrRes<- factor(pcr_new_p$PcrRes,levels=c("???","neg.","pos."),
                          labels = c("Result not available","No positive", "Yes-1 positive atleast"))


ind_characteristics[which(ind_characteristics$ind_id %in% pcr_new_p$tln_id==TRUE),]$Testing_pastperformed<-"Yes"
colnames(pcr_new_p)[1]<-"ind_id"
l<- length(pcr_new_p$ind_id)

for( i in 1:l)
{  t<- pcr_new_p[i,]$ind_id
   r<- pcr_new_p[i,]$PcrRes
ind_characteristics[which(ind_characteristics$ind_id ==t),]$Testing_positive<-r
}

ind_characteristics<- ind_characteristics[which(ind_characteristics$hh_id %in% 
                                                  c("2716W00", "3752F00", "3755B00", "3751F00","5458M00")==FALSE),]
write.csv(ind_characteristics,"KoCo19_indcharacteristics.csv")
write.table(ind_characteristics, "KoCo19_indcharacteristics.txt", append = FALSE, sep = "\t ", dec = ".",
            col.names = TRUE)
# Lab results data

# Select the latest of the possible 3 observations for the lab measurements for a blood sample
# corresponds to latest observation

dat7e_update<-dat7e


l<-length(dat7e_update$tln_ID)

for( i in 1:l)
{ 

  # among IgA check the latest obs
  ta1<- dat7e_update[i,c(9,10,20,21,31,32)]
  
  
  x1<- ifelse(is.na(ta1$eur_quotient_2_IgA)==TRUE &
                                    is.na(ta1$eur_quotient_3_IgA)==TRUE &is.na(ta1$eur_quotient_1_IgA)==FALSE , ta1$eur_quotient_1_IgA,
                                  ifelse(is.na(ta1$eur_quotient_2_IgA)==FALSE &
                                           is.na(ta1$eur_quotient_3_IgA)==FALSE, ta1$eur_quotient_3_IgA,
                                         ifelse(is.na(ta1$eur_quotient_2_IgA)==FALSE &
                                                  is.na(ta1$eur_quotient_3_IgA)==TRUE,ta1$eur_quotient_2_IgA,NA)))
  x2<- ifelse(is.na(ta1$eur_datei_2_IgA)==TRUE &
                                    is.na(ta1$eur_datei_3_IgA)==TRUE & is.na(ta1$eur_datei_1_IgA)==FALSE, paste(ta1$eur_datei_1_IgA),
                                  ifelse(is.na(ta1$eur_datei_2_IgA)==FALSE &
                                           is.na(ta1$eur_datei_3_IgA)==FALSE, paste(ta1$eur_datei_3_IgA),
                                         ifelse(is.na(ta1$eur_datei_2_IgA)==FALSE &
                                                  is.na(ta1$eur_datei_3_IgA)==TRUE,paste(ta1$eur_datei_2_IgA),NA)))
  
   dat7e_update[i,]$eur_quotient_1_IgA<-   x1                          
   dat7e_update[i,]$eur_datei_1_IgA<-   x2 

   # among IgG check the latest obs
   tg1<- dat7e_update[i,c(42,43,53,54,64,65)]
   
   
   y1<- ifelse(is.na(tg1$eur_quotient_2_IgG)==TRUE &
                 is.na(tg1$eur_quotient_3_IgG)==TRUE &is.na(tg1$eur_quotient_1_IgG)==FALSE , tg1$eur_quotient_1_IgG,
               ifelse(is.na(tg1$eur_quotient_2_IgG)==FALSE &
                        is.na(tg1$eur_quotient_3_IgG)==FALSE, tg1$eur_quotient_3_IgG,
                      ifelse(is.na(tg1$eur_quotient_2_IgG)==FALSE &
                               is.na(tg1$eur_quotient_3_IgG)==TRUE,tg1$eur_quotient_2_IgG,NA)))
   y2<- ifelse(is.na(tg1$eur_datei_2_IgG)==TRUE &
                 is.na(tg1$eur_datei_3_IgG)==TRUE & is.na(tg1$eur_datei_1_IgG)==FALSE, paste(tg1$eur_datei_1_IgG),
               ifelse(is.na(tg1$eur_datei_2_IgG)==FALSE &
                        is.na(tg1$eur_datei_3_IgG)==FALSE, paste(tg1$eur_datei_3_IgG),
                      ifelse(is.na(tg1$eur_datei_2_IgG)==FALSE &
                               is.na(tg1$eur_datei_3_IgG)==TRUE,paste(tg1$eur_datei_2_IgG),NA)))
   
   dat7e_update[i,]$eur_quotient_1_IgG<-   y1                          
   dat7e_update[i,]$eur_datei_1_IgG<-   y2 
   
   
   }


lab_dat<- subset(dat7e_update, select=c(tln_ID,blut_ID,
                                 Q1_b_Datum_Besuch_blut_1_2,eur_quotient_1_IgA,eur_datei_1_IgA,
                                 eur_quotient_1_IgG,eur_datei_1_IgG))

colnames(lab_dat)<-c("ind_id","BloodID","VisitDate","IgA_quant",
                     "td_IgA","IgG_quant",
                     "td_IgG")

lab_dat<-lab_dat[which(!is.na(lab_dat$IgA_quant)),] # remove the condition when we have Roche Results
# obtain test dates for IgG and IgA 
lab_dat$dIgA<-paste(str_sub(lab_dat$td_IgA,1,4),"-",
                    str_sub(lab_dat$td_IgA,5,6),"-",
                    str_sub(lab_dat$td_IgA,7,8),sep="")
lab_dat$dIgG<-paste(str_sub(lab_dat$td_IgG,1,4),"-",
                    str_sub(lab_dat$td_IgG,5,6),"-",
                    str_sub(lab_dat$td_IgG,7,8),sep="")

lab_dat$IgA_testdate<-as.Date(lab_dat$dIgA,format = "%Y-%m-%d")
lab_dat$IgG_testdate<-as.Date(lab_dat$dIgG,format = "%Y-%m-%d")

# delete non essential test dates
lab_dat$td_IgA<-lab_dat$td_IgG<-lab_dat$dIgA<-lab_dat$dIgG<-NULL 

lab_dat$test_delay_IgA<- lab_dat$IgA_testdate - lab_dat$VisitDate
lab_dat$test_delay_IgG<- lab_dat$IgG_testdate - lab_dat$VisitDate



lab_dat$IgG_result<- ifelse(lab_dat$IgG_quant<0.8,"Negative",
                            ifelse(lab_dat$IgG_quant>=0.8 &
                                     lab_dat$IgG_quant<1.1,"Intermediate",
                                   "Positive"))
lab_dat$IgA_result<- ifelse(lab_dat$IgA_quant<0.8,"Negative",
                                 ifelse(lab_dat$IgA_quant>=0.8 &
                                          lab_dat$IgA_quant<1.1,"Intermediate",
                                        "Positive"))

lab_dat$IgA_result<-factor(lab_dat$IgA_result,
                                levels=c("Negative","Intermediate","Positive"))

lab_dat$IgG_result<-factor(lab_dat$IgG_result,
                                levels=c("Negative","Intermediate","Positive"))



# select individuals for which there is double or more information information of lab
lab_indv<- reshape2::dcast(lab_dat, ind_id+BloodID+VisitDate~.,fun.aggregate = length)
selected_ind<- lab_indv[which(lab_indv$.>1),] # check if length is zero or not



# Read the Roche Data

dat7r_update<- dat7r[c(1,2,3,
                       14,16,
                       26,28,
                       38,40,
                       50,52,
                       99,101,
                       111,113,
                       123,125,
                       135,137)]


l<-length(dat7r_update$tln_ID)

for( i in 1:l)
{ 
  
  # among Roche check the latest obs
  ta1<- dat7r_update[i,c(4,6,8,10,12,14,16,18)]
  ta2<- dat7r_update[i,c(5,7,9,11,13,15,17,19)]
  
  s1<- max(which(!is.na(ta1)))
  s2<- max(which(!is.na(ta2)))
  s3<- length(ta1[which(!is.na(ta1))])
  s4<- length(ta2[which(!is.na(ta2))])
  
  x1<- ifelse(s3>0, unlist(ta1[s1]),NA)
  x2<- ifelse(s4>0, unlist(ta2[s2]),NA)
  
  dat7r_update[i,]$roche_COI_1_1 <-   x1                          
  dat7r_update[i,]$roche_meas_date_1_1 <-   x2 
  
  
  
}


lab_roche<- subset(dat7r_update, select=c(tln_ID,blut_ID,
                                        Q1_b_Datum_Besuch_blut_1_2,roche_COI_1_1,roche_meas_date_1_1))

colnames(lab_roche)<-c("ind_id","BloodID","VisitDate","R_quant",
                     "R_testdate")

# obtain test dates for IgG and IgA 

lab_roche$R_testdate<-as.Date(lab_roche$R_testdate,format = "%d.%m.%Y")


lab_roche$test_delay_R<- lab_roche$R_testdate - lab_roche$VisitDate



lab_roche$R_Result<- ifelse(lab_roche$R_quant<1,"nonreactive",
                           "reactive")


# Merge Roche results into lab_dat

lab_dat<- merge.data.frame(lab_dat,lab_roche,by=c("ind_id","BloodID","VisitDate"),all = T)


# select individuals for which there is double or more information information of lab
lab_indv<- reshape2::dcast(lab_dat, ind_id+BloodID+VisitDate~.,fun.aggregate = length)
selected_ind<- lab_indv[which(lab_indv$.>1),] # check if length is zero or not

# Roche DBS

# Read the Roche DBS Data

dat7r_dbs_update<- dat7r_dbs[c(1,2,3,
                       11,12,21)]


lab_rocheDBS<-dat7r_dbs_update

colnames(lab_rocheDBS)<-c("PricktestID_V1","ind_id","VisitDate","RDBS_quant","RDBS_Result",
                       "RDBS_testdate")

# obtain test dates for IgG and IgA 

lab_rocheDBS$RDBS_testdate<-as.Date(as.character(lab_rocheDBS$RDBS_testdate), format="%d.%m.%Y")


lab_rocheDBS$test_delay_RDBS<- lab_rocheDBS$RDBS_testdate - lab_rocheDBS$VisitDate


# Merge Roche results into lab_dat

lab_dat<- merge.data.frame(lab_dat,lab_rocheDBS,by=c("ind_id","VisitDate"),all = T)


# select individuals for which there is double or more information information of lab
lab_indv<- reshape2::dcast(lab_dat, ind_id+BloodID+VisitDate~.,fun.aggregate = length)
selected_ind<- lab_indv[which(lab_indv$.>1),] # check if length is zero or not


# Add Neutralization Results and Cpass and Array Results
nt_dat<- dat8[c(1,2,3,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)]
names(nt_dat)<- c("BloodID","ind_id","VisitDate","CPass_quant","NT_quant",
                  "VC_S1_IgA","VC_S2_IgA","VC_N_IgA",
                  "VC_S1_IgM","VC_S2_IgM","VC_N_IgM",
                  "VC_S1_IgG" ,"VC_S2_IgG",        
                  "VC_N_IgG",
                  "LineBlot_NP_SARS_2",         "LineBlot_RBD_SARS_2",        "LineBlot_S1_SARS_2",        
                  "Schnupfen_NP_229E" ,         "Schnupfen_NP_NL63",          "Schnupfen_NP_OC43" ,        
                  "Schnupfen_NP_HKU1" ,         "NT_result",                  "cPass_result")

n1<- dat8[c(1,7:15)]
n1$VC_S1_IgA_1<- ifelse(n1$VC_S1_IgA=="<10",9,
                        as.numeric(n1$VC_S1_IgA))
n1$VC_S2_IgA_1<- ifelse(n1$VC_S2_IgA=="<10",9,
                        as.numeric(n1$VC_S1_IgA))
n1$VC_N_IgA_1<- ifelse(n1$VC_N_IgA=="<10",9,
                        as.numeric(n1$VC_S1_IgA))
n1$VC_S1_IgM_1<- ifelse(n1$VC_S1_IgM=="<10",9,
                        as.numeric(n1$VC_S1_IgA))
n1$VC_S2_IgM_1<- ifelse(n1$VC_S2_IgM=="<10",9,
                        as.numeric(n1$VC_S1_IgA))
n1$VC_N_IgM_1<- ifelse(n1$VC_N_IgM=="<10",9,
                        as.numeric(n1$VC_S1_IgA))
n1$VC_S1_IgG_1<- ifelse(n1$VC_S1_IgG=="<10",9,
                        as.numeric(n1$VC_S1_IgA))
n1$VC_S2_IgG_1<- ifelse(n1$VC_S2_IgG=="<10",9,
                        as.numeric(n1$VC_S1_IgA))
n1$VC_N_IgG_1<- ifelse(n1$VC_N_IgG=="<10",9,
                        as.numeric(n1$VC_S1_IgA))
n1$VC_S1_IgA_Result_c100<- ifelse(n1$VC_S1_IgA_1>100,"positive","negative")
n1$VC_S2_IgA_Result_c100<- ifelse(n1$VC_S2_IgA_1>100,"positive","negative")
n1$VC_N_IgA_Result_c100<-  ifelse(n1$VC_N_IgA_1>100,"positive","negative")
n1$VC_S1_IgM_Result_c100<- ifelse(n1$VC_S1_IgM_1>100,"positive","negative")
n1$VC_S2_IgM_Result_c100<- ifelse(n1$VC_S2_IgM_1>100,"positive","negative")
n1$VC_N_IgM_Result_c100<-  ifelse(n1$VC_N_IgM_1>100,"positive","negative")
n1$VC_S1_IgG_Result_c100<- ifelse(n1$VC_S1_IgG_1>100,"positive","negative")
n1$VC_S2_IgG_Result_c100<- ifelse(n1$VC_S2_IgG_1>100,"positive","negative")
n1$VC_N_IgG_Result_c100<-  ifelse(n1$VC_N_IgG_1>100,"positive","negative")

n1$VC_S1_IgA_Result_c70<-ifelse(n1$VC_S1_IgA_1>70,"positive","negative")
n1$VC_S2_IgA_Result_c70<-ifelse(n1$VC_S2_IgA_1>70,"positive","negative")
n1$VC_N_IgA_Result_c70<-ifelse(n1$VC_N_IgA_1>70,"positive","negative")
n1$VC_S1_IgM_Result_c70<-ifelse(n1$VC_S1_IgM_1>70,"positive","negative")
n1$VC_S2_IgM_Result_c70<-ifelse(n1$VC_S2_IgM_1>70,"positive","negative")
n1$VC_N_IgM_Result_c70<-ifelse(n1$VC_N_IgM_1>70,"positive","negative")
n1$VC_S1_IgG_Result_c70<-ifelse(n1$VC_S1_IgG_1>70,"positive","negative")
n1$VC_S2_IgG_Result_c70<-ifelse(n1$VC_S2_IgG_1>70,"positive","negative")
n1$VC_N_IgG_Result_c70<-ifelse(n1$VC_N_IgG_1>70,"positive","negative")


nt_dat$VC_S1_IgA_Result_c100<- n1[,21]
nt_dat$VC_S2_IgA_Result_c100<- n1[,22]
nt_dat$VC_N_IgA_Result_c100<-  n1[,23]
nt_dat$VC_S1_IgM_Result_c100<- n1[,24]
nt_dat$VC_S2_IgM_Result_c100<- n1[,25]
nt_dat$VC_N_IgM_Result_c100<-  n1[,26]
nt_dat$VC_S1_IgG_Result_c100<- n1[,27]
nt_dat$VC_S2_IgG_Result_c100<- n1[,28]
nt_dat$VC_N_IgG_Result_c100<-  n1[,29]
  
nt_dat$VC_S1_IgA_Result_c70<-n1[,30]
nt_dat$VC_S2_IgA_Result_c70<-n1[,31]
nt_dat$VC_N_IgA_Result_c70<-n1[,32]
nt_dat$VC_S1_IgM_Result_c70<-n1[,33]
nt_dat$VC_S2_IgM_Result_c70<-n1[,34]
nt_dat$VC_N_IgM_Result_c70<-n1[,35]
nt_dat$VC_S1_IgG_Result_c70<-n1[,36]
nt_dat$VC_S2_IgG_Result_c70<-n1[,37]
nt_dat$VC_N_IgG_Result_c70<-n1[,38]


# Merge NT results into lab_dat

lab_dat<- merge.data.frame(lab_dat,nt_dat,by=c("ind_id","BloodID","VisitDate"),all = T)


# select individuals for which there is double or more information information of lab
lab_indv<- reshape2::dcast(lab_dat, ind_id+BloodID+VisitDate~.,fun.aggregate = length)
selected_ind<- lab_indv[which(lab_indv$.>1),] # check if length is zero or not


# Merge lab data to individual data to have baseline data
ind_dat_base<-ind_dat[c(1:14,23:30)]

colnames(ind_dat_base)[c(13,14)]<-c("BloodID","VisitDate")

ind_dat_base$daysfromBaseline<-ind_dat_base$VisitDate_Baseline - ind_dat_base$VisitDate

ind_dat_base<- ind_dat_base[which(ind_dat_base$hh_id %in%
                                    c("2716W00", "3752F00", "3755B00", "3751F00","5458M00")==FALSE),]

ind_long_data<-merge.data.frame(ind_dat_base,lab_dat,
                               by=c("ind_id","BloodID","VisitDate"),all.x  = TRUE)


labs<-c("Individual ID","BloodID","Visit Date","Household ID",
        "VisitDate_Baseline",
        "Participation type",
        "Sex","Age",
        "Height","Weight",
        "Waist_length","Fingerprick_trainedperson",
        "Fingerprick_self","Geneticinfo_consent",
        "duplicated_individual","Blood Testing done?",
        "Age category WHO","Age category RKI",
        "Housing type categories- refer to description","Household size",
        "duplicated_household",
        "number of household members observed in the cohort",
        "daysfromBaseline",
        "IgA_quant",  "IgG_quant","IgA_testdate",             
        "IgG_testdate", "test_delay_IgA",            "test_delay_IgG",            "IgG_result",               
        "IgA_result","R_quant","R_testdate","R_testdelay","R_Result",
        "PricktestID_V1","RDBS_quant","RDBS_Result","RDBS_testdate","test_delay_RDBS",
        "CPass_quant","NT_quant","VC_S1_IgA","VC_S2_IgA","VC_N_IgA",                 
        "VC_S1_IgM",                 "VC_S2_IgM",                 "VC_N_IgM",                  "VC_S1_IgG",                 "VC_S2_IgG",                
        "VC_N_IgG",
       "LineBlot_NP_SARS_2",         "LineBlot_RBD_SARS_2",        "LineBlot_S1_SARS_2",        
        "Schnupfen_NP_229E",          "Schnupfen_NP_NL63",          "Schnupfen_NP_OC43",         
       "Schnupfen_NP_HKU1",     
        "NT_Result",                 "CPass_Result",              "VC_S1_IgA_Result_c100",     "VC_S2_IgA_Result_c100",    
        "VC_N_IgA_Result_c100",      "VC_S1_IgM_Result_c100",     "VC_S2_IgM_Result_c100",     "VC_N_IgM_Result_c100",      "VC_S1_IgG_Result_c100",    
        "VC_S2_IgG_Result_c100",     "VC_N_IgG_Result_c100" ,     "VC_S1_IgA_Result_c70",      "VC_S2_IgA_Result_c70",      "VC_N_IgA_Result_c70",      
        "VC_S1_IgM_Result_c70",      "VC_S2_IgM_Result_c70" ,     "VC_N_IgM_Result_c70",       "VC_S1_IgG_Result_c70",      "VC_S2_IgG_Result_c70",     
        "VC_N_IgG_Result_c70")
attr(ind_long_data, "var.labels") <- labs #data attributes are variable labels



# Important - This will be the longitudinal data with lab results which are the most useful
# ind_long_data - for individual longitudinal data of the lab test results
ind_long_data$HouseholdSize<- ifelse(ind_long_data$HouseholdSize==9999,NA,ind_long_data$HouseholdSize)
ind_long_data$Height<- ifelse(ind_long_data$Height==999,NA,ind_long_data$Height)
ind_long_data$Weight<- ifelse(ind_long_data$Weight==999.9,NA,ind_long_data$Weight)
ind_long_data$Waist_length<- ifelse(ind_long_data$Waist_length==999,NA,ind_long_data$Waist_length)

problems_hh_size<-ind_long_data[which(ind_long_data$HouseholdSize<ind_long_data$obs_hh_members),]
unique(problems_hh_size$hh_id)

ind_long_data$HouseholdSize<- ifelse(ind_long_data$HouseholdSize< ind_long_data$obs_hh_members,ind_long_data$obs_hh_members,ind_long_data$HouseholdSize)

write.dta(ind_long_data,file="KoCo19_BaselineLAB.dta",convert.dates = TRUE)
write.csv(ind_long_data,"KoCo19_BaselineLAB.csv")
write.table(ind_long_data, "KoCo19_BaselineLAB.txt", append = FALSE, sep = "\t ", dec = ".",
             col.names = TRUE)

variablebook<- cbind(names(ind_long_data),labs)
colnames(variablebook)<-c("Variable name","Label description")
write.table(variablebook,"variablebook.txt",append = FALSE, sep = "\t ", dec = ".",
            col.names = TRUE, row.names = FALSE)


problems_hh_size<-ind_long_data[which(ind_long_data$HouseholdSize<ind_long_data$obs_hh_members),]
unique(problems_hh_size$hh_id)

# Generate Roche Positives and IgG Positives for the Modeling Team



igg_P<- reshape2::dcast(ind_long_data[which(!is.na(ind_long_data$VisitDate)),], VisitDate~IgG_result)
Roche_P<- reshape2::dcast(ind_long_data[which(!is.na(ind_long_data$VisitDate)),], VisitDate~R_Result)

write.table(igg_P,"IgGPositivesperdate_KOCO19.txt")

write.table(Roche_P,"RochePositivesperdate_KOCO19.txt")


# Subset data for Houda

dat_igg<- subset(ind_long_data[which(!is.na(ind_long_data$IgG_result)),], select = c(ind_id,hh_id,VisitDate,HouseholdSize,Sex,Age,Age_Cat_RKI,Age_Cat_WHO,
                                                                                     IgG_result))
dat_roche<- subset(ind_long_data[which(!is.na(ind_long_data$R_Result)),], select = c(ind_id,hh_id,VisitDate,HouseholdSize,Sex,Age,Age_Cat_RKI,Age_Cat_WHO,
                                                                                     R_Result))

write.csv(dat_igg,"IgG_data.csv")
write.csv(dat_roche,"Roche_data.csv")

dat_igg$HouseholdSize_c<- ifelse(dat_igg$HouseholdSize==1,"1",
                                 ifelse(dat_igg$HouseholdSize==2,"2",
                                        ifelse(dat_igg$HouseholdSize==3,"3",
                                               ifelse(dat_igg$HouseholdSize==4,"4","5+"))))
dat_roche$HouseholdSize_c<- ifelse(dat_roche$HouseholdSize==1,"1",
                                 ifelse(dat_roche$HouseholdSize==2,"2",
                                        ifelse(dat_roche$HouseholdSize==3,"3",
                                               ifelse(dat_roche$HouseholdSize==4,"4","5+"))))

dat_igg_cat<- reshape2::dcast(dat_igg, VisitDate+ HouseholdSize_c+ Age_Cat_RKI~IgG_result)
dat_roche_cat<- reshape2::dcast(dat_roche, VisitDate+ HouseholdSize_c+ Age_Cat_RKI~R_Result)

write.csv(dat_igg_cat, "IgG_cat_data.csv")
write.csv(dat_roche_cat, "Roche_cat_data.csv")


# Second measurements for the households/individuals with mismatch Roche and Euroimmune
mismatch_id<- read.csv("mismatch_info.csv",header = T)
lab_2<- lab_dat[which(lab_dat$ind_id %in% mismatch_id$ind_id==TRUE),]
lab_2<-lab_2[which(!is.na(lab_2$R_quant)),]

lab_2_update<-NULL
l<- unique(lab_2$ind_id)
for(i in l)
{  t<- lab_2[which(lab_2$ind_id==i),]

   t1<- t[order(t$VisitDate),]
   l1<- length(t1$ind_id)
   t1$VisitNum<-0
   for( j in 1:l1)
   { t1$VisitNum[j]<- paste("V",j,sep = "")
     
   }
   lab_2_update<-rbind(lab_2_update,t1)
}

lab2_date<- reshape2::dcast(lab_2_update,ind_id~VisitNum, value.var = 'VisitDate')
lab2_iga<-reshape2::dcast(lab_2_update,ind_id~VisitNum, value.var = 'IgA_quant')
lab2_igg<-reshape2::dcast(lab_2_update,ind_id~VisitNum, value.var = 'IgG_quant')
lab2_r<-reshape2::dcast(lab_2_update,ind_id~VisitNum, value.var = 'R_quant')

lab_2_wide<- merge.data.frame(lab2_date,lab2_iga,by="ind_id")
lab_2_wide<- merge.data.frame(lab_2_wide,lab2_igg,by="ind_id")
lab_2_wide<- merge.data.frame(lab_2_wide,lab2_r,by="ind_id")
colnames(lab_2_wide)<-c("ind_id","Visit1","Visit2",
                        "IgA1","IgA2","IgG1","IgG2","Roche1","Roche2") 

lab_2_wide$Visit1<- as.Date(lab_2_wide$Visit1,format="%Y-%m-%d")
lab_2_wide$Visit2<- as.Date(lab_2_wide$Visit2,format="%Y-%m-%d")

lab_2_wide$Base_IgG<- ifelse(lab_2_wide$IgG1<1.1,"Not Positive","Positive")
lab_2_wide$V2_IgG<- ifelse(lab_2_wide$IgG2<1.1,"Not Positive","Positive")
lab_2_wide$Base_Roche<- ifelse(lab_2_wide$Roche1 <1,"Not Positive","Positive")
lab_2_wide$V2_Roche<- ifelse(lab_2_wide$Roche2 <1,"Not Positive","Positive")

write.csv(lab_2_wide,"labmismatch_Roche_IgG.csv")



rdata<-read.csv("Lab_Roche_multiple_measurements.csv",header = T)
rdbs.data<-read.csv("data_rocheDBS_final.csv",header = T)
euidata<-read.csv("Lab_Euroimmune_multiple_measurements.csv",header = T)
ntdata<-read.csv("Lab_NT_Array_co.csv",header = T)

rdata1<- rdata[which(rdata$tln_ID %in% lab_2_wide$ind_id==TRUE),]
rdbs.data1<- rdbs.data[which(rdbs.data$tln_ID %in% lab_2_wide$ind_id==TRUE),]
euidata1<- euidata[which(euidata$tln_ID%in% lab_2_wide$ind_id==TRUE),]
ntdata1<- ntdata[which(ntdata$tln_ID%in% lab_2_wide$ind_id==TRUE),]

lab_data_mismatch<- merge.data.frame(rdata1,euidata1,by=c("tln_ID","blut_ID","Q1_b_Datum_Besuch_blut_1_2","study_per_ID"), all = T)
lab_data_mismatch<- merge.data.frame(lab_data_mismatch,ntdata1,by=c("tln_ID","blut_ID","Q1_b_Datum_Besuch_blut_1_2","study_per_ID"), all = T)
#lab_data_mismatch<- merge.data.frame(lab_data_mismatch,rdbs.data1,by=c("tln_ID","study_per_ID"), all = T)

# Include PCR information
pcr_data<- subset(ind_characteristics[which(ind_characteristics$ind_id %in%
                                              lab_2_wide$ind_id==TRUE),],select=c(ind_id,Testing_pastattempt,Testing_pastperformed,
                                               Testing_positive))
colnames(pcr_data)<- c("tln_ID","PCRTesting_pastattempt","PCRTesting_pastperformed",
                                               "PCRTesting_positive")

lab_data_mismatch<- merge.data.frame(pcr_data,lab_data_mismatch,by="tln_ID",all = T)


write.csv(lab_data_mismatch,"lab_data_mismatch.csv")

pcr_pos<-ind_characteristics[which(ind_characteristics$Testing_positive=="Yes-1 positive atleast"),]

lab_dat_pcr_pos<- lab_dat[which(lab_dat$ind_id %in% pcr_pos$ind_id==TRUE),]

lab_dat_pcr_pos_sel<-lab_dat_pcr_pos[which(lab_dat_pcr_pos$IgG_quant<1.1 | lab_dat_pcr_pos$R_quant<1),]

rdata2<- rdata[which(rdata$tln_ID %in% lab_dat_pcr_pos_sel$ind_id==TRUE),]
euidata2<- euidata[which(euidata$tln_ID%in% lab_dat_pcr_pos_sel$ind_id==TRUE),]
ntdata2<- ntdata[which(ntdata$tln_ID%in% lab_dat_pcr_pos_sel$ind_id==TRUE),]

pcrpos_data_mismatch<- merge.data.frame(rdata2,euidata2,by=c("tln_ID","blut_ID","Q1_b_Datum_Besuch_blut_1_2","study_per_ID"), all = T)
pcrpos_data_mismatch<- merge.data.frame(pcrpos_data_mismatch,ntdata2,by=c("tln_ID","blut_ID","Q1_b_Datum_Besuch_blut_1_2","study_per_ID"), all = T)


write.csv(pcrpos_data_mismatch,"pcrpos_data_mismatch.csv")
#rmarkdown::render("Datageneration.R",quiet = TRUE)

baseline<- read.csv("Koco_baseline.csv",header=T)
baseline<- baseline[which(!is.na(baseline$R_quant)),]

ind_characteristics$inclusion_criteria<- ifelse(ind_characteristics$ind_id %in% baseline$ind_id==TRUE,"Included","Not included")
ind_long_data$inclusion_criteria<- ifelse(ind_long_data$ind_id %in% baseline$ind_id==TRUE,"Included","Not included")
hh_characteristics$inclusion_criteria<- ifelse(hh_characteristics$hh_id %in% baseline$hh_id==TRUE,"Included",
                                                                 "Not included")

write.csv(ind_characteristics,"ind_characteristics_new.csv")
write.csv(ind_long_data,"ind_lab_baseline_new.csv")
write.csv(hh_characteristics,"hh_characteristics_new.csv")
