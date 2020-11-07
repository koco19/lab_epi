#' This script runs both "long" scripts "ignoring" the first phase of the study design and decide that the 
#' household was selected randomly from all households in Munich. Household weight and Variance estimation 
#' is adapted to this decision.

here_koco_data = function (...) here::here("KoCo19_Datasets", ...)
here_algo_results = function (...) here::here("AlgorithmResults", ...)
here_weights = function(...) here::here("SamplingWeights", ...)

library(descr)
library(reshape2)
library(sampling)
library(randomForest)
library(e1071)

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

#############################
# Sampling weights of the households
#############################

# Weight of constituency based an random sampling (nb of all constituencies / nb of sampled constituencies)
w_household <- sum(Munich_hh$Nb_hh) / length(unique(KoCo_BLab$hh_id))

# Add this weight to the individual data
KoCo_BLab$w_household <- w_household

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

KoCo_BLab$w_ind_samp <- KoCo_BLab$w_household * KoCo_BLab$w_ind

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

nb_house <- unique(KoCo_BLab[, c("hh_id", "time_point")])

# Calculate the number of hh surveyed per time point
nb_house_time <- as.data.frame(table(nb_house$time_point))

# Calculate the conditional weights
nb_house_time$w_time <- sum(Munich_hh$Nb_hh) / nb_house_time$Freq

KoCo_BLab <- merge(KoCo_BLab, nb_house_time, by.x = "time_point", by.y = "Var1", all.x = TRUE)

###
# Create the subsamples
###

KoCo_BLab_split <- split(KoCo_BLab, KoCo_BLab$time_point)

KoCo_BLab_split <- lapply(1:length(KoCo_BLab_split), function(x){
  res <- KoCo_BLab_split[[x]]
  res$w_house_time <- res$w_household * res$w_time
  return(res)
})

KoCo_BLab_split_study_pop <- KoCo_BLab_split
#############################
# Tidy
#############################

rm(list = setdiff(ls(), "KoCo_BLab_split_study_pop"))


################################################################## CALIBRATION_WEIGHTS_LONG #######################################################


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
  res <- KoCo_BLab_split_study_pop[[x]]$w_house_time
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
                   bounds = c(0.001, 2), max_iter = 1000)
w_hh_cal_logit_1 <- g_logit_1 * d_house[[1]]

# Checking if calibrated weights sum up to auxiliary data
sum(t(w_hh_cal_logit_1) %*% as.matrix(data_house_split[[1]]) - totals[])
summary(w_hh_cal_logit_1)
plot(density(g_logit_1))

### Time point 2
g_logit_2 <- calib(data_house_split[[2]], d = d_house[[2]], totals, method = "logit",
                   bounds = c(0.001, 2), max_iter = 1000)
w_hh_cal_logit_2 <- g_logit_2 * d_house[[2]]

# Checking if calibrated weights sum up to auxiliary data
sum(t(w_hh_cal_logit_2) %*% as.matrix(data_house_split[[2]]) - totals[])
summary(w_hh_cal_logit_2)
plot(density(g_logit_2))
sd(w_hh_cal_logit_2)

d_house[[2]][which(w_hh_cal_logit_2 == max(w_hh_cal_logit_2))]

### Time point 3
g_logit_3 <- calib(data_house_split[[3]], d = d_house[[3]], totals, method = "logit",
                   bounds = c(0.001, 2), max_iter = 1000)
w_hh_cal_logit_3 <- g_logit_3 * d_house[[3]]

# Checking if calibrated weights sum up to auxiliary data
sum(t(w_hh_cal_logit_3) %*% as.matrix(data_house_split[[3]]) - totals[])
summary(w_hh_cal_logit_3)
sd(w_hh_cal_logit_3)

### Time point 4
g_logit_4 <- calib(data_house_split[[4]], d = d_house[[4]], totals, method = "logit",
                   bounds = c(0.001, 2), max_iter = 1000)
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

spec_sens_classifier <- as.data.frame(readRDS(here_algo_results("specificity_sensitivity_classifier.RData")))

# Reorder the rows
spec_sens_classifier <- spec_sens_classifier[c("Roche N pan-Ig optimized cut-off",
                                               "Roche N pan-Ig manufacturer's cut-off"), ]

for(i in 1:length(KoCo_BLab_split_study_pop)){
  KoCo_BLab <- KoCo_BLab_split_study_pop[[i]]
  data_house <- data_house_split[[i]]
  
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
                                            c("hh_id", "w_household", "w_house_time", "w_hh_cal",
                                              "g")], by = "hh_id")
  
  ###
  # Run a linear model with the linearized variable as response variable and the
  # auxiliary information as covariates including the calibrated weights
  ###
  
  res.lm.roche <- lm(hh_roche_u_k ~ ., weights = data_house$w_hh_cal,
                     data = data_house[setdiff(colnames(data_house), 
                                               c("hh_id", "hh_roche_manu_u_k",
                                                 "w_household","w_house_time",
                                                 "w_hh_cal", "g"))])
  
  summary(res.lm.roche)
  
  res.lm.roche.manu <- lm(hh_roche_manu_u_k ~ ., weights = data_house$w_hh_cal,
                          data = data_house[setdiff(colnames(data_house), 
                                                    c("hh_id", "hh_roche_u_k",
                                                     "w_household","w_house_time",
                                                      "w_hh_cal", "g"))])
  
  summary(res.lm.roche.manu)
  
  ###
  # Variance estimation
  ###

  data_res <- cbind(data_house, e_roche = res.lm.roche$residuals,
                    e_roche_manu = res.lm.roche.manu$residuals)
  
  ### First term of the variance estimation
  
  # Total number of constituencies
  N <- sum(Munich_hh$Nb_hh)
  
  # theoretical number of constituencies surveyed
  n <- length(unique(KoCo_BLab$hh_id))
  
  # Calculate the dispersion of ge (running with the exact number of surveyed constituencies)
  s_e_roche <- var(data_res$e_roche)
  s_e_roche_manu <- var(data_res$e_roche_manu)
  
  # First term of the variance estimation
  V_roche <- N^2 * (1 / n - 1 / N) * s_e_roche
  V_roche_manu <- N^2 * (1 / n - 1 / N) * s_e_roche_manu
  
  ###
  # Variance estimation and confidence intervals
  ###
  
  ### Roche
  
  w_p_roche <- freq(x = KoCo_BLab$R_Result, w = KoCo_BLab$w_ind_cal, plot=F)[2, 2]
  
  # Upper bound CI
  w_u_ci_roche <- w_p_roche + qnorm(0.975) * sqrt(V_roche) * 100
  
  # Lower bound CI
  w_l_ci_roche <- w_p_roche - qnorm(0.975) * sqrt(V_roche) * 100
  
  # shrinkage if CI_low < 0
  w_l_ci_roche <- max(w_l_ci_roche, 0)
  
  ### Roche manufacturer
  
  w_p_roche_manu <- freq(x = KoCo_BLab$R_Result_manu_cutoff, w = KoCo_BLab$w_ind_cal, plot=F)[2, 2]
  
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
  write.csv(results, here_koco_data(paste("Estimates_study_pop_simple_t", paste(i, ".csv", sep=""), sep="")), row.names = FALSE)
  
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
  write.csv(results_adj, here_koco_data(paste("Estimates_study_pop_simple_adjusted_t", paste(i, ".csv", sep=""), sep="")), row.names = FALSE) 
}