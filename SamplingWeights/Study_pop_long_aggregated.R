#' This script calculates the sampling weights for confounding effects based 
#' on the sampling design (selection of the constituencies, the households,
#' and the individuals). The weights of the households are shared to account
#' for the random routes crossing the boundaries of the constituencies.
#' Finally it splits the data in sub datasets based on the weeks of visit.

here_koco_data = function (...) here::here("KoCo19_Datasets", ...)
here_algo_results = function (...) here::here("AlgorithmResults", ...)
here_weights = function(...) here::here("SamplingWeights", ...)

library(descr)
library(randomForest)
library(e1071)
library(reshape2)
library(sampling)
#############################
# Load data sets
#############################

###
# Individual data sets
###

# Reading the lab data set
KoCo_BLab <- read.csv(here_koco_data("Analysis Data Sets", "Koco_baseline.csv"), stringsAsFactors = TRUE) 

# Cutoffs
cutoffs <- readRDS(here_algo_results("cutoffs.RData"))

# Change the thresholds for the prevalence
# Calculate predictions for these optimized cutoffs
KoCo_BLab$R_Result <- ifelse(KoCo_BLab$R_quant < cutoffs["roche","Median"], "nonreactive", "reactive")

# Calculate predicions for manufacturers' cutoffs
KoCo_BLab$R_Result_manu_cutoff <- ifelse(KoCo_BLab$R_quant < 1, "nonreactive", "reactive")


# Keep only the variables of interest
KoCo_BLab <- KoCo_BLab[, c("ind_id", "hh_id", "Age", "Sex", "Birth_Country.x", "VisitDate_Baseline",
                           "obs_hh_members", "R_Result", "R_Result_manu_cutoff")]

# We need this other data set to calculate the number of ppl in the study population (age >= 14) in each hh
KoCo19 <- read.csv(here_koco_data("Analysis Data Sets", "ind_lab_baseline_new.csv"), stringsAsFactors = F)

# In each household, calculate the number of members aged 14 or more
member_study_hh <- as.data.frame(table(KoCo19$hh_id[KoCo19$Age >= 14]))

colnames(member_study_hh) <- c("hh_id", "obs_hh_members_study")

KoCo19 <- merge(KoCo19, member_study_hh)

# Identify households with missing ages
missing_age <- KoCo19[is.na(KoCo19$Age), ]

# In the households of size 2 with missing ages, change the number of members in 
# the study population to 2
KoCo19$obs_hh_members_study[KoCo19$obs_hh_members == 2 & KoCo19$hh_id %in% missing_age$hh_id] <- 2

# Add the variable obs_hh_members_study to the data set KoCo_BLab
obs_hh_members_study <- KoCo19[!duplicated(KoCo19$hh_id), c("hh_id", "obs_hh_members_study")]

KoCo_BLab <- merge(KoCo_BLab, obs_hh_members_study, all.x = TRUE)

###
# Data set with the constituencies
###

# reading the data set with with the consituencies info for each household
Const <- read.csv(here_koco_data("Analysis Data Sets", "KoCo19_Haushalte4Modeler_wRRstartConstituency_20200910.csv"), stringsAsFactors = TRUE) 

# Remove some hh that are not in the final study population
Const <- Const[Const$hht_ID %in% KoCo_BLab$hh_id, ]

# Recoding of the IDs of the ending constituencies
Const$const_end <- paste0(substr(Const$constituency, 1, 2), "0",
                          substr(Const$constituency, 3, 4))

Const$const_end[Const$constituency < 1000] <- 
  paste0("0", substr(Const$constituency[Const$constituency < 1000], 1, 1),
         "0", substr(Const$constituency[Const$constituency < 1000], 2, 3))

# Recoding of the IDs of the starting constituencies
Const$const_start <- paste0(substr(Const$constituency_RRstart, 1, 2), "0",
                            substr(Const$constituency_RRstart, 3, 4))

Const$const_start[Const$constituency_RRstart < 1000] <- 
  paste0("0", substr(Const$constituency_RRstart[Const$constituency_RRstart < 1000], 1, 1),
         "0", substr(Const$constituency_RRstart[Const$constituency_RRstart < 1000], 2, 3))

# Keep only the variables of interest
Const <- Const[, c("hht_ID", "const_end", "const_start")]

###
# Data set from the Statistisches Amt
###

### Number of households per constituency

# Information about number of hh in Munich depending on constituency based on stat.Amt
Munich_hh <- read.csv(here_koco_data("Analysis Data Sets", "muc_hh_structure_KoCo_bis.csv"), stringsAsFactors = TRUE)

# Recoding of the IDs of the constituencies
Munich_hh$const <- as.character(Munich_hh$Const_ID)

Munich_hh$const[Munich_hh$Const_ID < 10000] <- paste0("0", Munich_hh$const[Munich_hh$Const_ID < 10000])

# Keep only the variables of interest
Munich_hh <- Munich_hh[, c("const", "Nb_hh")]

###
# Contiguity matrices
###

# First order neighborhood of the constituencies
load(here_koco_data("Analysis Data Sets", "contiguity_matrices-munich-constituencies_200914.RData"))

# First and second order neighborhood of the constituencies (neighbors of the neighbors)
contiguity_Mat <- matrix(0, nrow = 755, ncol = 755)

for(i in 1:755){
  for(j in 1:755){
    if(M.binary[i, j] == 1){
      for(k in 1:755){
        if(M.binary[k, j] == 1){
          contiguity_Mat[i, k] <- 1
        }
      }
    }
  }
}

# Changing the rownames
rownames(contiguity_Mat) <- paste0(substr(rownames(M.binary), 1, 2), "0",
                                   substr(rownames(M.binary), 3, 4)) 

rownames(contiguity_Mat)[as.numeric(rownames(M.binary)) < 1000] <- 
  paste0("0", substr(rownames(M.binary)[as.numeric(rownames(M.binary)) < 1000], 1, 1),
         "0", substr(rownames(M.binary)[as.numeric(rownames(M.binary)) < 1000], 2, 3))

# Changing the colnames
colnames(contiguity_Mat) <- paste0(substr(colnames(M.binary), 1, 2), "0",
                                   substr(colnames(M.binary), 3, 4)) 

colnames(contiguity_Mat)[as.numeric(colnames(M.binary)) < 1000] <- 
  paste0("0", substr(colnames(M.binary)[as.numeric(colnames(M.binary)) < 1000], 1, 1),
         "0", substr(colnames(M.binary)[as.numeric(colnames(M.binary)) < 1000], 2, 3))

# Reorder the rows and the columns of the matrix
contiguity_Mat <- contiguity_Mat[order(rownames(contiguity_Mat)), order(colnames(contiguity_Mat))]

#############################
# Sampling weights of the constituencies
#############################

# Weight of constituency based an random sampling (nb of all constituencies / nb of sampled constituencies)
w_constituency <- length(unique(Munich_hh$const)) / length(unique(Const$const_start))

# Add this weight to the individual data
KoCo_BLab$w_constituency <- w_constituency

#############################
# Conditional sampling weights of the households
#############################

# Creating a table with, for each constituency, the number of households (hh) that have been surveyed
# and the number of hh that would have been surveyed is the constituency was selected (30)
w_hh <- merge(Munich_hh, as.data.frame(table(Const$const_start)),
              by.x = "const", by.y = "Var1", all.x=T)
names(w_hh)[names(w_hh)=="Freq"] <- "n_samp_hh"

# Indicator that the constituency was selected
w_hh$samp <- 0
w_hh$samp[!is.na(w_hh$n_samp_hh)] <- 1

# For the constituencies that were not drawn, we would have selected 30 households
w_hh$n_samp_hh[is.na(w_hh$n_samp_hh)] <- 30

# Weights of the households
w_hh$w_hh <- w_hh$Nb_hh/w_hh$n_samp_hh

#############################
# Sharing the weights of the households
#############################

# Nb of neighbors
nb <- rowSums(contiguity_Mat)

# Normalized matrix
contiguity_Mat <- contiguity_Mat / nb

# Reorder the table with the weights of each hh in each constituency
w_hh <- w_hh[order(w_hh$const), ]

# For each selected consituency (samp = 1), we will share the weights with the 
# neighboring ones (first and second order neighbors)
shared_w_hh <- contiguity_Mat %*% w_hh$w_hh 


# Add the initial weights of the households and shared weights to the hh data
shared_w_hh <- data.frame(const = rownames(shared_w_hh), w_household = shared_w_hh)
Const <- merge(Const, w_hh[, c("const", "w_hh")], by.x = "const_start", by.y = "const", all.x = TRUE)
Const <- merge(Const, shared_w_hh, by.x = "const_start", by.y = "const", all.x = TRUE)

# Add the shared weights to the individual data
KoCo_BLab <- merge(KoCo_BLab, Const[, c("hht_ID", "w_hh", "w_household")], by.x = "hh_id", by.y = "hht_ID", all.x = TRUE)

# Rename variable
names(KoCo_BLab)[names(KoCo_BLab) == "w_hh"] <- "w_hh_noshare"

#############################
# Conditional sampling weights at the individual level (some observations were removed due to missingness in the data)
#############################

# Creating a variable with the number of people (ppl) in the households that are taking part in the study
KoCo_BLab <- merge(KoCo_BLab, as.data.frame(table(KoCo_BLab$hh_id)), by.x = "hh_id", by.y = "Var1", all.x=T)
names(KoCo_BLab)[names(KoCo_BLab)=="Freq"] <- "n_ppl_part_hh"

# Calculating the weight for ppl in the household depending on other mm of the hh (not taking part in the study but in the study pop)
KoCo_BLab$w_ind <- KoCo_BLab$obs_hh_members_study / KoCo_BLab$n_ppl_part_hh 
# w_ind_study is the weight ppl have for their household (because of other mm not taking part in the study) 
# e.g the ppl with weight 6 represent additionally 5 others of their hh.

# Remove unnecessary variable
KoCo_BLab$n_ppl_part_hh <- NULL

#############################
# Final(non conditional) sampling weights at the individual level
#############################

KoCo_BLab$w_ind_samp <- KoCo_BLab$w_constituency * KoCo_BLab$w_household * KoCo_BLab$w_ind

# Estimation of the proportion with/without the sampling weights
freq(x = KoCo_BLab$R_Result, w = KoCo_BLab$w_ind_samp, plot=F)
freq(x = KoCo_BLab$R_Result, plot=F)

freq(x = KoCo_BLab$R_Result_manu_cutoff, w = KoCo_BLab$w_ind_samp, plot=F)
freq(x = KoCo_BLab$R_Result_manu_cutoff, plot=F)

#############################
# Longitudinal data
#############################

###
# Formatting the visit date
###
table(KoCo_BLab$VisitDate_Baseline, useNA = "always")

KoCo_BLab$date <- as.Date(KoCo_BLab$VisitDate_Baseline)

KoCo_BLab$week <- 15
KoCo_BLab$week[KoCo_BLab$date > "2020-04-12"] <- 16
KoCo_BLab$week[KoCo_BLab$date > "2020-04-19"] <- 17
KoCo_BLab$week[KoCo_BLab$date > "2020-04-26"] <- 18
KoCo_BLab$week[KoCo_BLab$date > "2020-05-03"] <- 19
KoCo_BLab$week[KoCo_BLab$date > "2020-05-10"] <- 20
KoCo_BLab$week[KoCo_BLab$date > "2020-05-17"] <- 21
KoCo_BLab$week[KoCo_BLab$date > "2020-05-24"] <- 22
KoCo_BLab$week[KoCo_BLab$date > "2020-05-31"] <- 23
KoCo_BLab$week[KoCo_BLab$date > "2020-06-07"] <- 24

###
# Households with different visit dates?
###

test <- KoCo_BLab[, c("hh_id", "week")]
test <- test[order(test$hh_id, test$week), ]
test <- unique(test)
test[test$hh_id %in% test$hh_id[duplicated(test$hh_id)], ]
sum(duplicated(test$hh_id))
# If hh were visited on different weeks, we keep the earliest date

week <- test[!duplicated(test$hh_id), ]
KoCo_BLab <- merge(KoCo_BLab[, setdiff(names(KoCo_BLab), "week")], week, by = "hh_id")

###
# Merge weeks
###

# Nb of participants per aggregated period of time
tab <- table(KoCo_BLab$week)

sum(tab[1:3])
sum(tab[4:5])
sum(tab[6:7])
sum(tab[8:10])

# Suggested cut-off points
KoCo_BLab$time_point <- cut(KoCo_BLab$week, breaks = c(15, 17, 19, 21, 24),
                            right = TRUE,
                            include.lowest = TRUE)

table(KoCo_BLab$time_point)

###
# Calculate the probability to be in the sub-samples
###

KoCo_BLab <- merge(KoCo_BLab, Const[ , c("hht_ID", "const_start")], by.x = "hh_id", by.y = "hht_ID",
                   all.x = TRUE)

nb_house <- unique(KoCo_BLab[, c("hh_id", "time_point", "const_start")])

# Calculate the number of hh surveyed in each constituency per time point
nb_house_time <- tapply(nb_house$const_start, nb_house$time_point, function(x) as.data.frame(table(x)))

# Aggregating the time intervals
nb_house_time[[2]] <- rbind(nb_house_time[[1]], nb_house_time[[2]])
nb_house_time[[3]] <- rbind(nb_house_time[[2]], nb_house_time[[3]])
nb_house_time[[4]] <- rbind(nb_house_time[[3]], nb_house_time[[4]])

for(i in 1:length(nb_house_time)){
nb_house_time[[i]] <- aggregate(nb_house_time[[i]]$Freq, by=list(nb_house_time[[i]]$x), FUN=sum)
colnames(nb_house_time[[i]]) <- c("x", "Freq")
}

# Rename the list
names(nb_house_time) <- c("[15,17]", "[15,19]", "[15,21]", "[15,24]")

# Calculate the total number of hh surveyed in each consituency
nb_house_tot <- as.data.frame(table(nb_house$const_start))
names(nb_house_tot) <- c("x", "n_samp")

# Merge the information and calculate the conditional weights
nb_house <- lapply(nb_house_time, function(x){
  res <- merge(x, nb_house_tot)
  res$w_time <- res$n_samp / res$Freq
  return(res)
})

###
# Create the subsamples
###

KoCo_BLab_split <- split(KoCo_BLab, KoCo_BLab$time_point)

# Aggregating the time intervals (since they are not overlapping, rbind is sufficient)
KoCo_BLab_split[[2]] <- rbind(KoCo_BLab_split[[1]], KoCo_BLab_split[[2]])
KoCo_BLab_split[[3]] <- rbind(KoCo_BLab_split[[2]], KoCo_BLab_split[[3]])
KoCo_BLab_split[[4]] <- rbind(KoCo_BLab_split[[3]], KoCo_BLab_split[[4]])

# Rename the list
names(KoCo_BLab_split) <- c("[15,17]", "[15,19]", "[15,21]", "[15,24]")

KoCo_BLab_split <- lapply(1:length(nb_house), function(x){
  res <- merge(KoCo_BLab_split[[x]], nb_house[[x]], by.x = "const_start", by.y = "x", all.x = TRUE)
  res$w_house_time <- res$w_household * res$w_time
  return(res)
})

KoCo_BLab_split_study_pop <- KoCo_BLab_split

#############################
# Tidy
#############################

rm(list = setdiff(ls(), "KoCo_BLab_split_study_pop"))

######################################################## CALIBRATION ###############################################################

###
# Complete sample of KoCo19 (with missing values)
###

# We need this data set to identify households with or without children
KoCo19 <- read.csv(here_koco_data("Analysis Data Sets", "ind_lab_baseline_new.csv"), stringsAsFactors = F)

# Defining ppl with missing age as children
KoCo19$Age[is.na(KoCo19$Age) & KoCo19$obs_hh_members > 2] <- 10

###
# Data sets from the Statistisches Amt
###

### Sex/Age distribution

margins_age_sex <- read.csv(here_koco_data("Analysis Data Sets", "muc_age_structure_KoCo_bis_study_pop.csv"), stringsAsFactors = F)   

# Rename variables to match the included information 
names(margins_age_sex)[names(margins_age_sex)=="Group.1"] <- "age_cat"
names(margins_age_sex)[names(margins_age_sex)=="Group.2"] <- "Sex"
names(margins_age_sex)[names(margins_age_sex)=="x"] <- "Freq"

# Changing the first letters of sex to match the information in the study data set
margins_age_sex$Sex[margins_age_sex$Sex=="male"] <- "Male"
margins_age_sex$Sex[margins_age_sex$Sex=="female"] <- "Female"

# Creating a variable including the information about the sex/age categories
margins_age_sex$sex_age <- paste(margins_age_sex$Sex, margins_age_sex$age_cat,
                                 sep = "_")

### Number of households per constituency

# Information about number of hh in Munich depending on constituency based on stat.Amt
Munich_hh <- read.csv(here_koco_data("Analysis Data Sets", "muc_hh_structure_KoCo_bis.csv"), stringsAsFactors = TRUE) 

# Recoding of the IDs of the constituencies
Munich_hh$const <- as.character(Munich_hh$Const_ID)

Munich_hh$const[Munich_hh$Const_ID < 10000] <- paste0("0", Munich_hh$const[Munich_hh$Const_ID < 10000])

# Keep only the variables of interest
Munich_hh <- Munich_hh[, c("const", "Nb_hh")]

#############################
# Collecting auxiliary data for calibration (definition of the margins)
#############################

###
# Age / Sex structure and country of birth in Munich (based on KoCo19, stat. Amt)
###

# Selecting the margins for sex/age categories 
totals <- margins_age_sex$Freq
names(totals) <- margins_age_sex$sex_age

# Selecting the margins for country of origin 
country_germany_unknow <- 1085145
country_other <- sum(totals) - country_germany_unknow

###
# Nb of single / multi-ppl hh and hh with / without children in Munich (based on KoCo19, stat. Amt)
###

# Selecting the margins for household size (single vs. multi) 
# only main residences
n_single_house <- 451655
n_multi_house <- 829392 - n_single_house

# Selecting the margins for household with or without children in Munich
n_house_0_child <- 683971
n_house_1plus_child <- 829392 - n_house_0_child

###
# Summarizing the information about Munich
###
totals <- c(totals, country_germany_unknow = country_germany_unknow, country_other = country_other,
            n_single_house = n_single_house, n_multi_house = n_multi_house,
            n_house_0_child = n_house_0_child, n_house_1plus_child = n_house_1plus_child)

# Removing temporary data
rm(margins_age_sex, country_germany_unknow, country_other, n_single_house, n_multi_house, n_house_0_child, n_house_1plus_child)

#############################
# Create in the sample the variables needed for the calibration
#############################

data_house_split <- lapply(KoCo_BLab_split_study_pop, function(subdata){
  
  ###
  # Age / Sex structure, country of origin 
  ###
  
  # Creating age categories matching the information about Munich 
  subdata$age_cat <- cut(subdata$Age, c(14, 19, 34, 49, 64, 80, 150),
                         include.lowest = TRUE,
                         right = TRUE,
                         labels = c("14-19", "20-34", "35-49", "50-64",
                                    "65-79", ">=80"))
  
  # Creating a variable including the information about the sex/age categories
  subdata$sex_age <- paste(subdata$Sex, subdata$age_cat, sep = "_")
  
  # Creating a variable including the information about hh_id and sex/age categories
  subdata$hh_sex_age <- paste(subdata$hh_id, subdata$sex_age, sep = "_")
  
  # Creating a variable including the information about the country of origin
  subdata$Birth_Country.x[is.na(subdata$Birth_Country.x)] <- "Germany"
  
  # Checking the nb of ppl by country of origin
  table(subdata$Birth_Country.x, useNA = "always")
  
  # Creating a variable including the information about hh_id and country categories
  subdata$hh_country <- paste(subdata$hh_id, subdata$Birth_Country.x, sep = "_")
  
  ### Sex/Age
  
  # Calculating the number of males, females in each age group in each household
  n_sex_age <- freq(subdata$hh_sex_age, w = subdata$w_ind, plot=F)
  
  # Removing the line for totals
  n_sex_age <- n_sex_age[-nrow(n_sex_age), ]
  
  # Splitting the information saved in the variable about hh_id and sex/age categories
  n_sex_age <- data.frame(hh_id = substr(rownames(n_sex_age), 1, 7),
                          sex_age = substr(rownames(n_sex_age), 9, 30),
                          freq = n_sex_age[, "Frequency"])
  
  # Reshaping the dataframe to have for each hh the number of ppl based on sex and age
  data_house <- reshape(n_sex_age, v.names = "freq",
                        timevar = "sex_age", idvar = "hh_id", direction = "wide")
  
  ### Country of origin
  
  # Calculating the number Germans / other in each household
  n_country <- freq(subdata$hh_country, w = subdata$w_ind, plot=F)
  
  # Removing the line for totals
  n_country <- n_country[-nrow(n_country), ]
  
  # Splitting the information saved in the variable about hh_id and country of origin
  n_country <- data.frame(hh_id = substr(rownames(n_country), 1, 7),
                          country = substr(rownames(n_country), 9, 15),
                          freq = n_country[, "Frequency"])
  
  # Reshaping the dataframe to have for each hh the number of ppl based on contry of origin
  data_house2 <- reshape(n_country, v.names = "freq",
                         timevar = "country", idvar = "hh_id", direction = "wide")
  
  # Merging of the two information
  data_house <- merge(data_house, data_house2, by = "hh_id")
  
  # Setting empty cells to 0
  data_house[is.na(data_house)] <- 0
  
  # Renaming columns and rows 
  colnames(data_house) <- gsub(x = colnames(data_house), pattern = "freq.", replacement = "")
  rownames(data_house) <- NULL
  
  # Reordering the columns
  data_house <- data_house[, c("hh_id", "Male_14-19", "Male_20-34", "Male_35-49", "Male_50-64", "Male_65-79", "Male_>=80",
                               "Female_14-19", "Female_20-34", "Female_35-49", "Female_50-64", "Female_65-79", "Female_>=80",
                               "Germany", "Other")]
  
  ###
  # Nb of members in the hh, children / no children in the hh 
  ###
  
  # Adding information about number of observed individuals in hh
  data_house <- merge(data_house, unique(subdata[, c("hh_id", "obs_hh_members")]), by="hh_id", all.x = T)
  
  # Adding information about type of housing (single vs. multi)
  data_house$house <- cut(data_house$obs_hh_members, breaks = c(0, 1, 100),
                          labels = c("single", "multi"))
  
  # Creating dummy variable(s) for type of housing
  data_house <- cbind(data_house, model.matrix(~ house - 1, data = data_house))
  
  # Adding information about children in the household (children vs. no children)
  # Nb of children in each household
  nb_child <- tapply(KoCo19$Age < 18, KoCo19$hh_id, function(x) sum(x, na.rm = T))
  nb_child <- data.frame(hh_id = names(nb_child), nb_child = nb_child)
  
  # Dummy variable: Child vs. no child
  nb_child$child <- ifelse(nb_child$nb_child == 0, "no", "yes")
  
  data_house <- merge(data_house, nb_child, by="hh_id", all.x = T)
  
  # Creating dummy variable(s) for Children
  data_house <- cbind(data_house, model.matrix(~ child - 1, data = data_house))
  
  # Adding rownames
  rownames(data_house) <- data_house$hh_id
  
  # Removing unneccessary variables
  data_house[, c("hh_id", "house", "obs_hh_members", "nb_child", "child")] <- NULL
  
  return(data_house)
})

#############################
# Calibration Roche
#############################

# Checking if the order of age / sex categories are the same
for (i in 1:length(data_house_split)){
  print(identical(colnames(data_house_split[[i]][1:12]),names(totals)[1:12]))
}

# Sampling weights at the household level
d_house <- lapply(1:length(KoCo_BLab_split_study_pop), function(x){
  res <- KoCo_BLab_split_study_pop[[x]]$w_constituency * KoCo_BLab_split_study_pop[[x]]$w_house_time
  names(res) <- KoCo_BLab_split_study_pop[[x]]$hh_id
  res <- res[!duplicated(names(res))]
  
  # Ordering weights in the same order than the household data
  res <- res[rownames(data_house_split[[x]])]
})

###
# Calculating the calibrated weights by means of different calibration methods.
###

### method "logit"

### Time point 1
g_logit_1 <- calib(data_house_split[[1]], d = d_house[[1]], totals, method = "logit",
                   bounds=c(0.3,3.01), max_iter = 2000)
w_hh_cal_logit_1 <- g_logit_1 * d_house[[1]]

# Checking if calibrated weights sum up to auxiliary data
sum(t(w_hh_cal_logit_1) %*% as.matrix(data_house_split[[1]]) - totals[])
summary(w_hh_cal_logit_1)
plot(density(g_logit_1))

### Time point 2
g_logit_2 <- calib(data_house_split[[2]], d = d_house[[2]], totals, method = "logit",
                   bounds=c(0.3,3.01), max_iter = 2000)
w_hh_cal_logit_2 <- g_logit_2 * d_house[[2]]

# Checking if calibrated weights sum up to auxiliary data
sum(t(w_hh_cal_logit_2) %*% as.matrix(data_house_split[[2]]) - totals[])
summary(w_hh_cal_logit_2)
plot(density(g_logit_2))
sd(w_hh_cal_logit_2)

d_house[[2]][which(w_hh_cal_logit_2 == max(w_hh_cal_logit_2))]

### Time point 3
g_logit_3 <- calib(data_house_split[[3]], d = d_house[[3]], totals, method = "logit",
                   bounds=c(0.3,3.01), max_iter = 2000)
w_hh_cal_logit_3 <- g_logit_3 * d_house[[3]]

# Checking if calibrated weights sum up to auxiliary data
sum(t(w_hh_cal_logit_3) %*% as.matrix(data_house_split[[3]]) - totals[])
summary(w_hh_cal_logit_3)
sd(w_hh_cal_logit_3)

### Time point 4 (same as the not-trend script)
g_logit_4 <- calib(data_house_split[[4]], d = d_house[[4]], totals, method="logit", 
                   bounds=c(0.3,3.01), max_iter = 2000)
w_hh_cal_logit_4 <- g_logit_4 * d_house[[4]]

# Checking if calibrated weights sum up to auxiliary data
sum(t(w_hh_cal_logit_4) %*% as.matrix(data_house_split[[4]]) - totals[])
summary(w_hh_cal_logit_4)
sd(w_hh_cal_logit_4)
plot(density(g_logit_4))

calib_weights <- list(w_hh_cal_logit_1, w_hh_cal_logit_2, w_hh_cal_logit_3, w_hh_cal_logit_4)
g_weights <- list(g_logit_1, g_logit_2, g_logit_3, g_logit_4)

calib_weights <- lapply(1:4, function(x){
  res <- data.frame(hh_id = names(d_house[[x]]), w_hh_cal = calib_weights[[x]],
                    g = g_weights[[x]])
})

KoCo_BLab_split_study_pop <- lapply(1:4, function(x){
  res <- merge(KoCo_BLab_split_study_pop[[x]], calib_weights[[x]], by = "hh_id")
  res$w_ind_cal <- res$w_hh_cal * res$w_ind
  return(res)
})

#############################
# Calibrated estimators
#############################

test <- lapply(KoCo_BLab_split_study_pop, function(x){
  unweighted <- freq(x = x$R_Result, plot=F)
  weighted <- freq(x = x$R_Result, w = x$w_ind_cal, plot=F)
  return(list(unweighted, weighted))
})

### Time trend unweighted

test[[1]][[1]][2,2]
test[[2]][[1]][2,2]
test[[3]][[1]][2,2]
test[[4]][[1]][2,2]

### Time trend weighted

test[[1]][[2]][2,2]
test[[2]][[2]][2,2]
test[[3]][[2]][2,2]
test[[4]][[2]][2,2]

#############################
# Variance of the calibrated estimator - only for Roche               
#############################

# reading the data set with with the consituencies info for each household
Const <- read.csv(here_koco_data("Analysis Data Sets", "KoCo19_Haushalte4Modeler_wRRstartConstituency_20200910.csv"), stringsAsFactors = TRUE)

spec_sens_classifier <- as.data.frame(readRDS(here_algo_results("specificity_sensitivity_classifier.RData")))

# Reorder the rows
spec_sens_classifier <- spec_sens_classifier[c("Roche N pan-Ig optimized cut-off",
                                               "Roche N pan-Ig manufacturer's cut-off"), ]

for(i in 1:length(KoCo_BLab_split_study_pop)){
  KoCo_BLab <- KoCo_BLab_split_study_pop[[i]]
  data_house <- data_house_split[[i]]
  Const_sub <- Const
  
  ###
  # calculate the linearized variable for a proportion
  ###
  
  ### Linearization at the individual level
  
  # Dummy variables for Roche
  KoCo_BLab$Roche_ind <- ifelse(KoCo_BLab$R_Result == "reactive", 1, 0)
  
  KoCo_BLab$Roche_ind_manu_cutoff <- ifelse(KoCo_BLab$R_Result_manu_cutoff == "reactive", 1, 0)
  
  # Check
  table(KoCo_BLab$Roche_ind, KoCo_BLab$R_Result)
  
  # Weighted proportion for Roche
  p_roche <- sum(KoCo_BLab$Roche_ind * KoCo_BLab$w_ind_cal) / sum(KoCo_BLab$w_ind_cal)
  
  # Weighted proportion for Roche manufacturer
  p_roche_manu <- sum(KoCo_BLab$Roche_ind_manu_cutoff * KoCo_BLab$w_ind_cal) / sum(KoCo_BLab$w_ind_cal)
  
  # Linearized variables
  KoCo_BLab$roche_u_k <- (KoCo_BLab$Roche_ind - p_roche) / sum(KoCo_BLab$w_ind_cal)
  KoCo_BLab$roche_manu_u_k <- (KoCo_BLab$Roche_ind_manu_cutoff - p_roche_manu) / sum(KoCo_BLab$w_ind_cal)
  
  ### Linearization at the household level
  
  split_hh <- split(KoCo_BLab[, c("hh_id", "roche_u_k", "w_ind", "roche_manu_u_k")], KoCo_BLab$hh_id)
  
  hh_u_k <- sapply(split_hh, function(z){
    hh_roche_u_k <- sum(z[, "roche_u_k"] * z[, "w_ind"])
    hh_roche_manu_u_k <- sum(z[, "roche_manu_u_k"] * z[, "w_ind"])
    return(c(hh_roche_u_k = hh_roche_u_k, 
             hh_roche_manu_u_k = hh_roche_manu_u_k))
  })
  
  hh_u_k <- data.frame(hh_id = colnames(hh_u_k), hh_roche_u_k = hh_u_k[1, ],
                       hh_roche_manu_u_k = hh_u_k[2, ])
  
  ###
  # Create a data frame at the hh level with the linearized variable,
  # the auxiliary information, the calibrated weights and the ratio
  # of the calibrated weights and the sampling weights g
  ###
  
  data_house$hh_id <- rownames(data_house)
  
  data_house <- merge(hh_u_k, data_house, by = "hh_id")
  
  data_house <- merge(data_house, KoCo_BLab[!duplicated(KoCo_BLab$hh_id), 
                                            c("hh_id", "w_hh_noshare", "w_household", "w_house_time", "w_hh_cal",
                                              "g")], by = "hh_id")
  
  ###
  # Run a linear model with the linearized variable as response variable and the
  # auxiliary information as covariates including the calibrated weights
  ###
  
  res.lm.roche <- lm(hh_roche_u_k ~ ., weights = data_house$w_hh_cal,
                     data = data_house[setdiff(colnames(data_house), 
                                               c("hh_id", "hh_roche_manu_u_k",
                                                 "w_constituency", "w_household","w_house_time",
                                                 "w_hh_cal", "g", "w_hh_noshare"))])
  
  summary(res.lm.roche)
  
  res.lm.roche.manu <- lm(hh_roche_manu_u_k ~ ., weights = data_house$w_hh_cal,
                          data = data_house[setdiff(colnames(data_house), 
                                                    c("hh_id", "hh_roche_u_k",
                                                      "w_constituency", "w_household","w_house_time",
                                                      "w_hh_cal", "g", "w_hh_noshare"))])
  
  summary(res.lm.roche.manu)
  
  ###
  # Data set with the constituencies
  ###
  
  # Remove some hh that are not in the final study population
  Const_sub <- Const_sub[Const_sub$hht_ID %in% KoCo_BLab$hh_id, ]
  
  # Recoding of the IDs of the starting constituencies
  Const_sub$const_start <- paste0(substr(Const_sub$constituency_RRstart, 1, 2), "0",
                                  substr(Const_sub$constituency_RRstart, 3, 4))
  
  Const_sub$const_start[Const_sub$constituency_RRstart < 1000] <-
    paste0("0", substr(Const_sub$constituency_RRstart[Const_sub$constituency_RRstart < 1000], 1, 1),
           "0", substr(Const_sub$constituency_RRstart[Const_sub$constituency_RRstart < 1000], 2, 3))
  
  # Keep only the variables of interest
  Const_sub <- Const_sub[, c("hht_ID", "const_start")]
  
  ###
  # Variance estimation
  ###
  
  # For the variance estimation, we will not directly use the residuals e_k but the
  # product of e_k and g_k, plus a term to account for the weight sharing
  
  data_res <- cbind(data_house, e_roche = res.lm.roche$residuals,
                    e_roche_manu = res.lm.roche.manu$residuals)
  data_res$ge_roche <- data_res$g * data_res$e_roche * data_res$w_household / data_res$w_hh_noshare
  data_res$ge_roche_manu <- data_res$g * data_res$e_roche_manu * data_res$w_household / data_res$w_hh_noshare
  
  # Keep useful variables
  data_res <- data_res[, c("hh_id", "ge_roche", "ge_roche_manu", "w_hh_noshare", "w_house_time")]
  
  # Add the constituency ID
  data_res <- merge(data_res, Const_sub, by.x = "hh_id", by.y = "hht_ID")
  
  ### First term of the variance estimation
  
  # Total number of constituencies
  N_const <- length(unique(Munich_hh$const))
  
  # theoretical number of constituencies surveyed
  n_const_th <- 100
  
  # Caculate the estimated totals of ge at the constituency level
  split_const <- split(data_res, data_res$const_start)
  
  tot_e_const <- sapply(split_const, function(z){
    t_e_roche <- sum(z[, "ge_roche"] * z[, "w_house_time"])
    t_e_roche_manu <- sum(z[, "ge_roche_manu"] * z[, "w_house_time"])
    return(c(t_e_roche = t_e_roche, 
             t_e_roche_manu = t_e_roche_manu))
  })
  
  # Calculate the dispersion of ge (running with the exact number of surveyed constituencies)
  s_e_roche <- var(tot_e_const[1, ])
  s_e_roche_manu <- var(tot_e_const[2, ])
  
  # First term of the variance estimation
  V1_roche <- N_const^2 * (1 / n_const_th - 1 / N_const) * s_e_roche
  V1_roche_manu <- N_const^2 * (1 / n_const_th - 1 / N_const) * s_e_roche_manu
  
  ### Second term of the variance estimation
  
  # In each constituency, variance of the error terms ge
  s_e_const_roche <- tapply(data_res$ge_roche, data_res$const_start, var)
  s_e_const_roche_manu <- tapply(data_res$ge_roche_manu, data_res$const_start, var)
  
  # Add the number of households surveyed and in the population for each constituency
  s_e_const_roche <- data.frame(hh_id = names(s_e_const_roche), s_e_roche = s_e_const_roche)
  s_e_const_roche_manu <- data.frame(hh_id = names(s_e_const_roche_manu), s_e_roche_manu = s_e_const_roche_manu)
  
  # For constituencies with freq==1, tapply(, , var) is giving NA, therefore we replace NA by 0
  s_e_const_roche[is.na(s_e_const_roche$s_e_roche)==T, "s_e_roche"] <- 0
  s_e_const_roche_manu[is.na(s_e_const_roche_manu$s_e_roche_manu)==T, "s_e_roche_manu"] <- 0
  
  # Nb hh surveyed
  nb_hh_s <- as.data.frame(table(Const_sub$const_start))
  
  # Merge all the information
  s_e_const <- merge(s_e_const_roche, s_e_const_roche_manu, by = "hh_id")   
  s_e_const <- merge(s_e_const, nb_hh_s, by.x = "hh_id", by.y = "Var1")
  s_e_const <- merge(s_e_const, Munich_hh, by.x = "hh_id", by.y = "const", all.x = TRUE)
  
  # Second term of the variance estimation
  V2_roche <- N_const / n_const_th * sum(s_e_const$Nb_hh^2 * (1 / s_e_const$Freq - 1 / s_e_const$Nb_hh) * s_e_const$s_e_roche)
  V2_roche_manu <- N_const / n_const_th * sum(s_e_const$Nb_hh^2 * (1 / s_e_const$Freq - 1 / s_e_const$Nb_hh) * s_e_const$s_e_roche_manu)
  
  ###
  # Variance estimation and confidence intervals
  ###
  
  ### Roche
  
  w_p_roche <- freq(x = KoCo_BLab$R_Result, w = KoCo_BLab$w_ind_cal, plot=F)[2, 2]
  
  V_roche <- V1_roche + V2_roche
  
  # Upper bound CI
  w_u_ci_roche <- w_p_roche + qnorm(0.975) * sqrt(V_roche) * 100
  
  # Lower bound CI
  w_l_ci_roche <- w_p_roche - qnorm(0.975) * sqrt(V_roche) * 100
  
  # shrinkage if CI_low < 0
  w_l_ci_roche <- max(w_l_ci_roche, 0)
  
  ### Roche manufacturer
  
  w_p_roche_manu <- freq(x = KoCo_BLab$R_Result_manu_cutoff, w = KoCo_BLab$w_ind_cal, plot=F)[2, 2]
  
  V_roche_manu <- V1_roche_manu + V2_roche_manu
  
  # Upper bound CI
  w_u_ci_roche_manu <- w_p_roche_manu + qnorm(0.975) * sqrt(V_roche_manu) * 100
  
  # Lower bound CI
  w_l_ci_roche_manu <- w_p_roche_manu - qnorm(0.975) * sqrt(V_roche_manu) * 100
  
  # shrinkage if CI_low < 0
  w_l_ci_roche_manu <- max(w_l_ci_roche_manu, 0)
  
  ###
  # Confidence intervals for the unweighted version
  ###
  
  ### Roche
  p_roche <- freq(x = KoCo_BLab$R_Result, plot=F)[2 , 2]
  
  # Upper bound CI
  u_ci_roche <- p_roche + qnorm(0.975)* sqrt(p_roche / 100*(1-p_roche / 100) / length(unique(KoCo_BLab$hh_id))
  )*100
  
  # Lower bound CI
  l_ci_roche <- p_roche - qnorm(0.975) * sqrt(p_roche / 100*(1-p_roche / 100) / length(unique(KoCo_BLab$hh_id))
  )*100
  
  # shrinkage if CI_low < 0
  l_ci_roche <- max(l_ci_roche, 0)
  
  ### Roche manufacturer
  p_roche_manu <- freq(x = KoCo_BLab$R_Result_manu_cutoff, plot=F)[2 , 2]
  
  # Upper bound CI
  u_ci_roche_manu <- p_roche_manu + qnorm(0.975)* sqrt(p_roche_manu / 100*(1-p_roche_manu / 100) / length(unique(KoCo_BLab$hh_id))
  )*100
  
  # Lower bound CI
  l_ci_roche_manu <- p_roche_manu - qnorm(0.975) * sqrt(p_roche_manu / 100*(1-p_roche_manu / 100) / length(unique(KoCo_BLab$hh_id))
  )*100
  
  # shrinkage if CI_low < 0
  l_ci_roche_manu <- max(l_ci_roche_manu, 0)
  
  #############################
  # Final table with the results
  #############################
  
  results <- data.frame(calculation = rep(c("weighted", "unweighted"), each = 2),
                        test = rep(c("Roche", "Roche_manu"), times = 2),
                        estimate = c(w_p_roche,  
                                     w_p_roche_manu,
                                     p_roche, 
                                     p_roche_manu),
                        lower_ci = c(w_l_ci_roche, 
                                     w_l_ci_roche_manu,
                                     l_ci_roche,
                                     l_ci_roche_manu),
                        upper_ci = c(w_u_ci_roche,
                                     w_u_ci_roche_manu, 
                                     u_ci_roche,
                                     u_ci_roche_manu))
  
  ### Export results
  write.csv(results, here_koco_data(paste("Estimates_study_pop_agg_t", paste(i, ".csv", sep=""), sep="")), row.names = FALSE)
  
  #############################
  # Adjust for sensitivity and specificity
  #############################
  
  # Adjust for sensitivity and specificity
  results_adj <- results
  
  results_adj[, c("estimate", "lower_ci", "upper_ci")] <- (results_adj[, c("estimate", "lower_ci", "upper_ci")] / 100 + # we need the crude prevalence
                                                             spec_sens_classifier$Specificity - 1) / (spec_sens_classifier$Sensitivity + spec_sens_classifier$Specificity - 1)*100 # back to percentage
  
  #shrinkage if CI_low < 0
  results_adj[which(results_adj$lower_ci < 0), "lower_ci"] <- 0
  
  ### Export results
  write.csv(results_adj, here_koco_data(paste("Estimates_study_pop_adjusted_agg_t", paste(i, ".csv", sep=""), sep="")), row.names = FALSE) 
}