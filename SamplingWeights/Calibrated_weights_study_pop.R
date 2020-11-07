#' This script calibrates the sampling weights (from the script Sampling_weights.R)
#' and calculates the variance associated with the estimation of the proportion of ppl.
#' infected y the virus(positive tested).
#' 

rm(list = ls())

here_weights = function(...) here::here("SamplingWeights", ...)


# if (!exists("KoCo_BLab")) {                                         # since we have the rm(list = ls()) command, this if clause is useless. KoCo_BLab_split will never exist
  source(here_weights("Sampling_weights_study_pop.R"))
#}

here_statistics = function(...) here::here("Statistics", ...)
here_data = function(...) here_statistics("Data", ...)
here_algo_results = function (...) here::here("AlgorithmResults", ...)
here_weights = function(...) here::here("SamplingWeights", ...)

# CAUTION: For Calibration we need KoCo_BLab, you have to run the corresponding script first

library(descr)
library(reshape2)
library(sampling)


#############################
# Load data sets
#############################

###
# Complete sample of KoCo19 (with missing values)
###

# We need this data set to identify households with or without children
KoCo19 <- read.csv(here_data("ind_lab_baseline_new.csv"), stringsAsFactors = F)


# We have 134 missing ages in this data set. We consider that:
# - in households with 2 members, the missing age is another adult
# - in households with 3 or more members, the missing age(s) is/are a child(ren)


# In households with missing ages and with more than 2 members, we impute the missing ages with
# the value 10. It does not matter here, we will not use directly this value. We just want to say
# that these missing values are children.
KoCo19$Age[is.na(KoCo19$Age) & KoCo19$obs_hh_members > 2] <- 10


###
# Data set with the constituencies
###

# reading the data set with with the consituencies info for each household
Const <- read.csv(here_data("KoCo19_Haushalte4Modeler_wRRstartConstituency_20200910.csv"), stringsAsFactors = TRUE)
# 3007 hh

# Remove some hh that are not in the final study population
Const <- Const[Const$hht_ID %in% KoCo_BLab$hh_id, ]
# 2994 hh

# Recoding of the IDs of the starting constituencies
Const$const_start <- paste0(substr(Const$constituency_RRstart, 1, 2), "0",
                            substr(Const$constituency_RRstart, 3, 4))

Const$const_start[Const$constituency_RRstart < 1000] <- 
  paste0("0", substr(Const$constituency_RRstart[Const$constituency_RRstart < 1000], 1, 1),
         "0", substr(Const$constituency_RRstart[Const$constituency_RRstart < 1000], 2, 3))

# Keep only the variables of interest
Const <- Const[, c("hht_ID", "const_start")]


###
# Data sets from the Statistisches Amt
###


### Sex/Age distribution

margins_age_sex <- read.csv(here_data("muc_age_structure_KoCo_bis_study_pop.csv"), stringsAsFactors = F)

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
Munich_hh <- read.csv(here_data("muc_hh_structure_KoCo_bis.csv"), stringsAsFactors = TRUE)

# Recoding of the IDs of the constituencies
Munich_hh$const <- as.character(Munich_hh$Const_ID)

Munich_hh$const[Munich_hh$Const_ID < 10000] <- paste0("0", Munich_hh$const[Munich_hh$Const_ID < 10000])

# Keep only the variables of interest
Munich_hh <- Munich_hh[, c("const", "Nb_hh")]


#############################
# Changing the weight of the missing value for IgA/IgG
#############################

KoCo_BLab[is.na(KoCo_BLab$IgG_result), ]

KoCo_BLab[KoCo_BLab$hh_id == "2866P00", ]
# 2 members: 2866P01 for which we have a missing value, 2866P02 for which we have IgG/IgA results.

# We change the weight of 2866P02 for IgG/IgA results
KoCo_BLab$w_ind_igg <- KoCo_BLab$w_ind
KoCo_BLab$w_ind_igg[KoCo_BLab$ind_id == "2866P02"] <- KoCo_BLab$w_ind[KoCo_BLab$ind_id == "2866P02"] * 2
KoCo_BLab$w_ind_igg[KoCo_BLab$ind_id == "2866P01"] <- NA



#############################
# Collecting auxiliary data for calibration (definition of the margins)
#############################

###
# Age/Sex structure and country of birth in Munich (based on KoCo19, stat. Amt)
###

# Selecting the margins for sex/age categories 
totals <- margins_age_sex$Freq
names(totals) <- margins_age_sex$sex_age

# Selecting the margins for country of origin 
country_germany_unknow <- 1085145
country_other <- sum(totals) - country_germany_unknow

###
# Nb of single/multi-ppl hh and hh with/without children in Munich (based on KoCo19, stat. Amt)
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

###
# Age/Sex structure, country of origin 
###


# Creating age categories matching the information about Munich 
KoCo_BLab$age_cat <- cut(KoCo_BLab$Age, c(14, 19, 34, 49, 64, 80, 150),
                            include.lowest = TRUE,
                            right = TRUE,
                            labels = c("14-19", "20-34", "35-49", "50-64",
                                       "65-79", ">=80"))

# Creating a variable including the information about the sex/age categories
KoCo_BLab$sex_age <- paste(KoCo_BLab$Sex, KoCo_BLab$age_cat, sep = "_")

# Checking the nb of ppl in the sex/age categories
table(KoCo_BLab$sex_age, useNA = "always")

# Creating a variable including the information about hh_id and sex/age categories
KoCo_BLab$hh_sex_age <- paste(KoCo_BLab$hh_id, KoCo_BLab$sex_age, sep = "_")

table(KoCo_BLab$Birth_Country.x, useNA = "always")

# Creating a variable including the information about the country of origin
KoCo_BLab$Birth_Country.x[is.na(KoCo_BLab$Birth_Country.x)] <- "Germany"

# Checking the nb of ppl by country of origin
table(KoCo_BLab$Birth_Country.x, useNA = "always")

# Creating a variable including the information about hh_id and country categories
KoCo_BLab$hh_country <- paste(KoCo_BLab$hh_id, KoCo_BLab$Birth_Country.x, sep = "_")


###
# Roche
###

### Sex/Age

# Calculating the number of males, females in each age group in each household
n_sex_age <- freq(KoCo_BLab$hh_sex_age, w = KoCo_BLab$w_ind, plot=F)

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

# Calculating the number Germans/other in each household
n_country <- freq(KoCo_BLab$hh_country, w = KoCo_BLab$w_ind, plot=F)

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
# IgG/IgA
###

### Sex/Age

# Calculating the number of males, females in each age group in each household
n_sex_age <- freq(KoCo_BLab$hh_sex_age, w = KoCo_BLab$w_ind_igg, plot=F)

# Removing the line for totals
n_sex_age <- n_sex_age[-nrow(n_sex_age), ]

# Splitting the information saved in the variable about hh_id and sex/age categories
n_sex_age <- data.frame(hh_id = substr(rownames(n_sex_age), 1, 7),
                        sex_age = substr(rownames(n_sex_age), 9, 30),
                        freq = n_sex_age[, "Frequency"])

# Reshaping the dataframe to have for each hh the number of ppl based on sex and age
data_house_igg <- reshape(n_sex_age, v.names = "freq",
                          timevar = "sex_age", idvar = "hh_id", direction = "wide")


### Country of origin

# Calculating the number Germans/other in each household
n_country <- freq(KoCo_BLab$hh_country, w = KoCo_BLab$w_ind_igg, plot=F)

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
data_house_igg <- merge(data_house_igg, data_house2, by = "hh_id")

# Setting empty cells to 0
data_house_igg[is.na(data_house_igg)] <- 0

# Renaming columns and rows 
colnames(data_house_igg) <- gsub(x = colnames(data_house_igg), pattern = "freq.", replacement = "")
rownames(data_house_igg) <- NULL


# Reordering the columns
data_house_igg <- data_house_igg[, c("hh_id", "Male_14-19", "Male_20-34", "Male_35-49", "Male_50-64",
                                     "Male_65-79", "Male_>=80", "Female_14-19", "Female_20-34", 
                                     "Female_35-49", "Female_50-64", "Female_65-79", "Female_>=80",
                                     "Germany", "Other")]

###
# Nb of members in the hh, children/no children in the hh 
###

# Adding information about number of observed individuals in hh
data_house <- merge(data_house, unique(KoCo_BLab[, c("hh_id", "obs_hh_members")]), by="hh_id", all.x = T)


# Adding information about type of housing (single vs. multi)
# TODO y: Is it right to use obs_hh_members here? As those are not the real household sizes.
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

# Adding this information to data_house_igg
data_house_igg <- merge(data_house_igg, data_house[, c("hh_id", "housesingle", "housemulti", "childno", "childyes")],
                        by = "hh_id")

# Adding rownames
rownames(data_house) <- data_house$hh_id
rownames(data_house_igg) <- data_house_igg$hh_id


# Removing unnecessary variables
data_house[, c("hh_id", "house", "obs_hh_members", "nb_child", "child")] <- NULL
data_house_igg[, c("hh_id")] <- NULL


# removing temporary data
rm(n_sex_age, n_country, KoCo19, data_house2, nb_child)


#############################
# Calibration Roche
#############################

# Checking if the order of age/sex categories are the same
colnames(data_house)
names(totals)

# Sampling weights at the household level
d_house <- KoCo_BLab$w_constituency * KoCo_BLab$w_household
names(d_house) <- KoCo_BLab$hh_id
d_house <- d_house[!duplicated(names(d_house))]

# Ordering weights in the same order than the household data
d_house <- d_house[rownames(data_house)]

###
# Calculating the calibrated weights by means of different calibration methods.
###

### method "linear"
g <- calib(data_house, d = d_house, totals, method = "linear")
w_hh_cal_lin <- g * d_house

# Checking if calibrated weights sum up to auxiliary data
sum(t(w_hh_cal_lin) %*% as.matrix(data_house) - totals[])
summary(w_hh_cal_lin)
# Negative and big weigths -> We do not retain the linear method


### method "raking"
g <- calib(data_house, d = d_house, totals, method = "raking")
w_hh_cal_rak <- g * d_house

# Checking if calibrated weights sum up to auxiliary data
sum(t(w_hh_cal_rak) %*% as.matrix(data_house) - totals[])
summary(w_hh_cal_rak)
# Very big weigths -> We do not retain the raking ratio method


### method "truncated"
g_trunc <- calib(data_house, d = d_house, totals, method = "truncated",
           bounds = c(0.2, 4), max_iter = 2000)
# Not converging...
w_hh_cal_trunc <- g_trunc * d_house

# Checking if calibrated weights sum up to auxiliary data
# sum(t(w_hh_cal_trunc) %*% as.matrix(data_house) - totals[])
# summary(w_hh_cal_trunc)
# plot(density(g_trunc))



### method "logit"
g_logit <- calib(data_house, d = d_house, totals, method="logit",
           bounds=c(0.3,3.01), max_iter = 2000)  
w_hh_cal_logit <- g_logit * d_house
summary(d_house)
# Checking if calibrated weights sum up to auxiliary data
sum(t(w_hh_cal_logit) %*% as.matrix(data_house) - totals[])
summary(w_hh_cal_logit)
plot(density(g_logit))
# Ok

sd(w_hh_cal_logit)

calib_weights <- data.frame(hh_id = names(d_house), w_hh_cal = w_hh_cal_logit,
                            g = g_logit)

KoCo_BLab <- merge(KoCo_BLab, calib_weights, by = "hh_id")

KoCo_BLab$w_ind_cal <- KoCo_BLab$w_hh_cal * KoCo_BLab$w_ind


#############################
# Calibration IgG/IgA
#############################

# Checking if the order of age/sex categories are the same
colnames(data_house_igg)
names(totals)

# Sampling weights at the household level
d_house <- KoCo_BLab$w_constituency * KoCo_BLab$w_household
names(d_house) <- KoCo_BLab$hh_id
d_house <- d_house[!duplicated(names(d_house))]

# Ordering weights in the same order than the household data
d_house <- d_house[rownames(data_house)]

###
# Calculating the calibrated weights by means of different calibration methods.
###

### method "truncated"
g_trunc <- calib(data_house_igg, d = d_house, totals, method = "truncated",
                 bounds = c(0.2, 4), max_iter = 1000)
# Not converging...
w_hh_cal_trunc <- g_trunc * d_house

# Checking if calibrated weights sum up to auxiliary data
# sum(t(w_hh_cal_trunc) %*% as.matrix(data_house) - totals[])
# summary(w_hh_cal_trunc)
# plot(density(g_trunc))
# Ok


### method "logit"
g_logit <- calib(data_house_igg, d = d_house, totals, method="logit",
                 bounds=c(0.3,3.01), max_iter = 2000)  
w_hh_cal_logit <- g_logit * d_house

# Checking if calibrated weights sum up to auxiliary data
sum(t(w_hh_cal_logit) %*% as.matrix(data_house) - totals[])
summary(w_hh_cal_logit)
plot(density(g_logit))
# Ok


calib_weights <- data.frame(hh_id = names(d_house), w_hh_cal_igg = w_hh_cal_logit,
                            g_igg = g_logit)

KoCo_BLab <- merge(KoCo_BLab, calib_weights, by = "hh_id")

KoCo_BLab$w_ind_cal_igg <- KoCo_BLab$w_hh_cal_igg * KoCo_BLab$w_ind_igg


#############################
# Calibrated estimators
#############################

# Estimation of the proportion with/without the sampling/calibrated weights
freq(x = KoCo_BLab$IgA_result, plot=F)
freq(x = KoCo_BLab$IgA_result, w = KoCo_BLab$w_ind_samp, plot=F)
freq(x = KoCo_BLab$IgA_result, w = KoCo_BLab$w_ind_cal_igg, plot=F)

freq(x = KoCo_BLab$IgG_result, plot=F)
freq(x = KoCo_BLab$IgG_result, w = KoCo_BLab$w_ind_samp, plot=F)
freq(x = KoCo_BLab$IgG_result, w = KoCo_BLab$w_ind_cal_igg, plot=F)


freq(x = KoCo_BLab$R_Result, plot=F)
freq(x = KoCo_BLab$R_Result, w = KoCo_BLab$w_ind_samp, plot=F)
freq(x = KoCo_BLab$R_Result, w = KoCo_BLab$w_ind_cal, plot=F)


# Estimation of the proportion by age group
age <- split(KoCo_BLab, KoCo_BLab$age_cat)
sapply(age, function(z){
  round(freq(z[, "R_Result"], plot = F)[2, 2], 2)
})

sapply(age, function(z){
  round(freq(z[, "R_Result"], w = z[, "w_ind_samp"], plot = F)[2, 2], 2)
})

sapply(age, function(z){
  round(freq(z[, "R_Result"], w = z[, "w_ind_cal"], plot = F)[2, 2], 2)
})


# Estimation of the proportion by sex
sex <- split(KoCo_BLab, KoCo_BLab$Sex)
sapply(sex, function(z){
  round(freq(z[, "R_Result"], plot = F)[2, 2], 2)
})

sapply(sex, function(z){
  round(freq(z[, "R_Result"], w = z[, "w_ind_samp"], plot = F)[2, 2], 2)
})

sapply(sex, function(z){
  round(freq(z[, "R_Result"], w = z[, "w_ind_cal"], plot = F)[2, 2], 2)
})




#############################
# Exporting file with the weights
#############################

### Adding the weights that sum up to the size of the sample

KoCo_Weights <- KoCo_BLab

# At the household level

# Roche
tab_hh <- KoCo_Weights[!duplicated(KoCo_Weights$hh_id), "w_hh_cal" ]

KoCo_Weights$w_hh_samp <- KoCo_Weights$w_hh_cal * length(tab_hh) / sum(tab_hh)

# IgG
tab_hh <- KoCo_Weights[!duplicated(KoCo_Weights$hh_id), "w_hh_cal_igg" ]

KoCo_Weights$w_hh_samp_igg <- KoCo_Weights$w_hh_cal_igg * length(tab_hh) / sum(tab_hh)

# Check if the sum is correct
sum(KoCo_Weights[!duplicated(KoCo_Weights$hh_id), "w_hh_samp" ])
sum(KoCo_Weights[!duplicated(KoCo_Weights$hh_id), "w_hh_samp_igg" ])

# At the individual level
KoCo_Weights$w_ind_samp <- KoCo_Weights$w_ind_cal * nrow(KoCo_Weights) / sum(KoCo_Weights$w_ind_cal)
KoCo_Weights$w_ind_samp_igg <- KoCo_Weights$w_ind_cal_igg * sum(!is.na(KoCo_Weights$w_ind_igg)) / sum(KoCo_Weights$w_ind_cal_igg, na.rm = TRUE)

# Check if the sum is correct
sum(KoCo_Weights$w_ind_samp)
sum(KoCo_Weights$w_ind_samp_igg, na.rm = TRUE)


### Keep the variables of interest
KoCo_Weights <- KoCo_Weights[, c("hh_id", "ind_id", "Age", "Sex", "Birth_Country.x",
                                 "obs_hh_members", "IgG_result", "IgA_result", "R_Result",
                                 "R_Result_manu_cutoff", "IgA_result_manu_cutoff", "IgG_result_manu_cutoff",
                                 "w_hh_cal", "w_ind_cal", "w_hh_cal_igg", "w_ind_cal_igg",
                                 "w_hh_samp", "w_ind_samp","w_hh_samp_igg", "w_ind_samp_igg")]

### Export table
write.csv(KoCo_Weights, here_data("KoCo_weigths.csv"), row.names = FALSE)


# Removing temporary data
rm(list = setdiff(ls(), c("here_weights", "here_statistics", "here_algo_results", "KoCo_BLab", "data_house", "Const", "Munich_hh")))


#############################
# Variance of the calibrated estimator
#############################

###
# calculate the linearized variable for a proportion
###

### Linearization at the individual level

# Dummy variables for Roche, IgG nad IgA results
KoCo_BLab$Roche_ind <- ifelse(KoCo_BLab$R_Result == "reactive", 1, 0)
KoCo_BLab$IgG_ind <- ifelse(KoCo_BLab$IgG_result == "Positive", 1, 0)
KoCo_BLab$IgA_ind <- ifelse(KoCo_BLab$IgA_result == "Positive", 1, 0)

KoCo_BLab$Roche_ind_manu_cutoff <- ifelse(KoCo_BLab$R_Result_manu_cutoff == "reactive", 1, 0)
KoCo_BLab$IgG_ind_manu_cutoff <- ifelse(KoCo_BLab$IgG_result_manu_cutoff == "Positive", 1, 0)
KoCo_BLab$IgA_ind_manu_cutoff <- ifelse(KoCo_BLab$IgA_result_manu_cutoff == "Positive", 1, 0)


KoCo_BLab$rf_result_ind <- ifelse(KoCo_BLab$rf_result == "Positive", 1, 0)
KoCo_BLab$svm_result_ind <- ifelse(KoCo_BLab$svm_result == "Positive", 1, 0)


# Check
table(KoCo_BLab$Roche_ind, KoCo_BLab$R_Result)
table(KoCo_BLab$IgG_ind, KoCo_BLab$IgG_result)
table(KoCo_BLab$IgA_ind, KoCo_BLab$IgA_result)



# Weighted proportion for Roche
p_roche <- sum(KoCo_BLab$Roche_ind * KoCo_BLab$w_ind_cal)/sum(KoCo_BLab$w_ind_cal)

# Weighted proportion for IgG
p_igg <- sum(KoCo_BLab$IgG_ind * KoCo_BLab$w_ind_cal_igg, na.rm = TRUE)/sum(KoCo_BLab$w_ind_cal_igg, na.rm = TRUE)

# Weighted proportion for IgA
p_iga <- sum(KoCo_BLab$IgA_ind * KoCo_BLab$w_ind_cal_igg, na.rm = TRUE)/sum(KoCo_BLab$w_ind_cal_igg, na.rm = TRUE)

# Weighted proportion for Roche manufacturer
p_roche_manu <- sum(KoCo_BLab$Roche_ind_manu_cutoff * KoCo_BLab$w_ind_cal)/sum(KoCo_BLab$w_ind_cal)

# Weighted proportion for IgG manufacturer
p_igg_manu <- sum(KoCo_BLab$IgG_ind_manu_cutoff * KoCo_BLab$w_ind_cal_igg, na.rm = TRUE)/sum(KoCo_BLab$w_ind_cal_igg, na.rm = TRUE)

# Weighted proportion for IgA manufacturer
p_iga_manu <- sum(KoCo_BLab$IgA_ind_manu_cutoff * KoCo_BLab$w_ind_cal_igg, na.rm = TRUE)/sum(KoCo_BLab$w_ind_cal_igg, na.rm = TRUE)

# Weighted proportion for RF 
p_rf <- sum(KoCo_BLab$rf_result_ind * KoCo_BLab$w_ind_cal_igg, na.rm = TRUE)/sum(KoCo_BLab$w_ind_cal_igg, na.rm = TRUE)

# Weighted proportion for SVM 
p_svm <- sum(KoCo_BLab$svm_result_ind * KoCo_BLab$w_ind_cal_igg, na.rm = TRUE)/sum(KoCo_BLab$w_ind_cal_igg, na.rm = TRUE)


# Linearized variables
KoCo_BLab$roche_u_k <- (KoCo_BLab$Roche_ind - p_roche)/sum(KoCo_BLab$w_ind_cal)
KoCo_BLab$igg_u_k <- (KoCo_BLab$IgG_ind - p_igg)/sum(KoCo_BLab$w_ind_cal_igg, na.rm = TRUE)
KoCo_BLab$iga_u_k <- (KoCo_BLab$IgA_ind - p_iga)/sum(KoCo_BLab$w_ind_cal_igg, na.rm = TRUE)
KoCo_BLab$roche_manu_u_k <- (KoCo_BLab$Roche_ind_manu_cutoff - p_roche_manu)/sum(KoCo_BLab$w_ind_cal)
KoCo_BLab$igg_manu_u_k <- (KoCo_BLab$IgG_ind_manu_cutoff - p_igg_manu)/sum(KoCo_BLab$w_ind_cal_igg, na.rm = TRUE)
KoCo_BLab$iga_manu_u_k <- (KoCo_BLab$IgA_ind_manu_cutoff - p_iga_manu)/sum(KoCo_BLab$w_ind_cal_igg, na.rm = TRUE)
KoCo_BLab$rf_u_k <- (KoCo_BLab$rf_result_ind - p_rf)/sum(KoCo_BLab$w_ind_cal_igg, na.rm = TRUE)
KoCo_BLab$svm_u_k <- (KoCo_BLab$svm_result_ind - p_svm)/sum(KoCo_BLab$w_ind_cal_igg, na.rm = TRUE)


### Linearization at the household level

split_hh <- split(KoCo_BLab[, c("hh_id", "roche_u_k", "igg_u_k", "iga_u_k", "w_ind", "w_ind_igg",
                                "roche_manu_u_k", "igg_manu_u_k", "iga_manu_u_k",
                                "rf_u_k", "svm_u_k")], KoCo_BLab$hh_id)

hh_u_k <- sapply(split_hh, function(z){
  hh_roche_u_k <- sum(z[, "roche_u_k"] * z[, "w_ind"])
  hh_igg_u_k <- sum(z[, "igg_u_k"] * z[, "w_ind_igg"], na.rm = TRUE)
  hh_iga_u_k <- sum(z[, "iga_u_k"] * z[, "w_ind_igg"], na.rm = TRUE)
  hh_roche_manu_u_k <- sum(z[, "roche_manu_u_k"] * z[, "w_ind"])
  hh_igg_manu_u_k <- sum(z[, "igg_manu_u_k"] * z[, "w_ind_igg"], na.rm = TRUE)
  hh_iga_manu_u_k <- sum(z[, "iga_manu_u_k"] * z[, "w_ind_igg"], na.rm = TRUE)
  hh_rf_u_k <- sum(z[, "rf_u_k"] * z[, "w_ind_igg"], na.rm = TRUE)
  hh_svm_u_k <- sum(z[, "svm_u_k"] * z[, "w_ind_igg"], na.rm = TRUE)
  return(c(hh_roche_u_k = hh_roche_u_k, hh_igg_u_k = hh_igg_u_k, hh_iga_u_k = hh_iga_u_k,
           hh_roche_manu_u_k = hh_roche_manu_u_k, hh_igg_manu_u_k = hh_igg_manu_u_k, 
           hh_iga_manu_u_k = hh_iga_manu_u_k, hh_rf_u_k = hh_rf_u_k, hh_svm_u_k = hh_svm_u_k))
})


hh_u_k <- data.frame(hh_id = colnames(hh_u_k), hh_roche_u_k = hh_u_k[1, ],
                     hh_igg_u_k = hh_u_k[2, ], hh_iga_u_k = hh_u_k[3, ],
                     hh_roche_manu_u_k = hh_u_k[4, ],
                     hh_igg_manu_u_k = hh_u_k[5, ], hh_iga_manu_u_k = hh_u_k[6, ],
                     hh_rf_u_k = hh_u_k[7, ], hh_svm_u_k = hh_u_k[8, ])

###
# Create a data frame at the hh level with the linearized variable,
# the auxiliary information, the calibrated weights and the ratio
# of the calibrated weights and the sampling weights g
###

data_house$hh_id <- rownames(data_house)

data_house <- merge(hh_u_k, data_house, by = "hh_id")

data_house <- merge(data_house, KoCo_BLab[!duplicated(KoCo_BLab$hh_id), 
                                          c("hh_id", "w_hh_noshare", "w_household", "w_hh_cal", "w_hh_cal_igg",
                                            "g", "g_igg")],
                    by = "hh_id")


###
# Run a linear model with the linearized variable as response variable and the
# auxiliary information as covariates including the calibrated weights
###


res.lm.roche <- lm(hh_roche_u_k ~ ., weights = data_house$w_hh_cal,
                   data = data_house[setdiff(colnames(data_house), 
                                             c("hh_id", "hh_igg_u_k", "hh_iga_u_k",
                                               "hh_roche_manu_u_k", "hh_igg_manu_u_k", "hh_iga_manu_u_k",
                                               "hh_rf_u_k", "hh_svm_u_k",
                                               "w_constituency", "w_household",
                                               "w_hh_cal", "w_hh_cal_igg", "g", "g_igg", "w_hh_noshare"))])

summary(res.lm.roche)

res.lm.igg <- lm(hh_igg_u_k ~ ., weights = data_house$w_hh_cal_igg,
                   data = data_house[setdiff(colnames(data_house), 
                                             c("hh_id", "hh_roche_u_k", "hh_iga_u_k",
                                               "hh_roche_manu_u_k", "hh_igg_manu_u_k", "hh_iga_manu_u_k",
                                               "hh_rf_u_k", "hh_svm_u_k",
                                               "w_constituency", "w_household",
                                               "w_hh_cal", "w_hh_cal_igg", "g", "g_igg", "w_hh_noshare"))])

summary(res.lm.igg)

res.lm.iga <- lm(hh_iga_u_k ~ ., weights = data_house$w_hh_cal_igg,
                 data = data_house[setdiff(colnames(data_house), 
                                           c("hh_id", "hh_roche_u_k", "hh_igg_u_k",
                                             "hh_roche_manu_u_k", "hh_igg_manu_u_k", "hh_iga_manu_u_k",
                                             "hh_rf_u_k", "hh_svm_u_k",
                                             "w_constituency", "w_household",
                                             "w_hh_cal", "w_hh_cal_igg", "g", "g_igg", "w_hh_noshare"))])

summary(res.lm.iga)


res.lm.roche.manu <- lm(hh_roche_manu_u_k ~ ., weights = data_house$w_hh_cal,
                   data = data_house[setdiff(colnames(data_house), 
                                             c("hh_id", "hh_igg_u_k", "hh_iga_u_k",
                                               "hh_roche_u_k", "hh_igg_manu_u_k", "hh_iga_manu_u_k",
                                               "hh_rf_u_k", "hh_svm_u_k",
                                               "w_constituency", "w_household",
                                               "w_hh_cal", "w_hh_cal_igg", "g", "g_igg", "w_hh_noshare"))])

summary(res.lm.roche.manu)

res.lm.igg.manu <- lm(hh_igg_manu_u_k ~ ., weights = data_house$w_hh_cal_igg,
                 data = data_house[setdiff(colnames(data_house), 
                                           c("hh_id", "hh_roche_u_k", "hh_iga_u_k",
                                             "hh_roche_manu_u_k", "hh_igg_u_k", "hh_iga_manu_u_k",
                                             "hh_rf_u_k", "hh_svm_u_k",
                                             "w_constituency", "w_household",
                                             "w_hh_cal", "w_hh_cal_igg", "g", "g_igg", "w_hh_noshare"))])

summary(res.lm.igg.manu)

res.lm.iga.manu <- lm(hh_iga_manu_u_k ~ ., weights = data_house$w_hh_cal_igg,
                 data = data_house[setdiff(colnames(data_house), 
                                           c("hh_id", "hh_roche_u_k", "hh_igg_u_k",
                                             "hh_roche_manu_u_k", "hh_igg_manu_u_k", "hh_iga_u_k",
                                             "hh_rf_u_k", "hh_svm_u_k",
                                             "w_constituency", "w_household",
                                             "w_hh_cal", "w_hh_cal_igg", "g", "g_igg", "w_hh_noshare"))])

summary(res.lm.iga.manu)

res.lm.rf <- lm(hh_rf_u_k ~ ., weights = data_house$w_hh_cal_igg,
                      data = data_house[setdiff(colnames(data_house), 
                                                c("hh_id", "hh_roche_u_k", "hh_igg_u_k", "hh_iga_u_k",
                                                  "hh_roche_manu_u_k", "hh_igg_manu_u_k", "hh_iga_manu_u_k",
                                                  "hh_svm_u_k",
                                                  "w_constituency", "w_household",
                                                  "w_hh_cal", "w_hh_cal_igg", "g", "g_igg", "w_hh_noshare"))])

summary(res.lm.rf)

res.lm.svm <- lm(hh_svm_u_k ~ ., weights = data_house$w_hh_cal_igg,
                data = data_house[setdiff(colnames(data_house), 
                                          c("hh_id", "hh_roche_u_k", "hh_igg_u_k", "hh_iga_u_k",
                                            "hh_roche_manu_u_k", "hh_igg_manu_u_k", "hh_iga_manu_u_k",
                                            "hh_rf_u_k",
                                            "w_constituency", "w_household",
                                            "w_hh_cal", "w_hh_cal_igg", "g", "g_igg", "w_hh_noshare"))])

summary(res.lm.svm)


###
# Variance estimation
###

# For the variance estimation, we will not directly use the residuals e_k but the
# product of e_k and g_k, plus a term to account for the weight sharing
data_res <- cbind(data_house, e_roche = res.lm.roche$residuals,
                  e_igg = res.lm.igg$residuals, e_iga = res.lm.iga$residuals,
                  e_roche_manu = res.lm.roche.manu$residuals,
                  e_igg_manu = res.lm.igg.manu$residuals, e_iga_manu = res.lm.iga.manu$residuals,
                  e_rf = res.lm.rf$residuals, e_svm = res.lm.svm$residuals)
data_res$ge_roche <- data_res$g * data_res$e_roche * data_res$w_household / data_res$w_hh_noshare
data_res$ge_igg <- data_res$g_igg * data_res$e_igg * data_res$w_household / data_res$w_hh_noshare
data_res$ge_iga <- data_res$g_igg * data_res$e_iga * data_res$w_household / data_res$w_hh_noshare
data_res$ge_roche_manu <- data_res$g * data_res$e_roche_manu * data_res$w_household / data_res$w_hh_noshare
data_res$ge_igg_manu <- data_res$g_igg * data_res$e_igg_manu * data_res$w_household / data_res$w_hh_noshare
data_res$ge_iga_manu <- data_res$g_igg * data_res$e_iga_manu * data_res$w_household / data_res$w_hh_noshare
data_res$ge_rf <- data_res$g_igg * data_res$e_rf * data_res$w_household / data_res$w_hh_noshare
data_res$ge_svm <- data_res$g_igg * data_res$e_svm * data_res$w_household / data_res$w_hh_noshare


# Keep useful variables
data_res <- data_res[, c("hh_id", "ge_roche", "ge_igg", "ge_iga",
                         "ge_roche_manu", "ge_igg_manu", "ge_iga_manu",
                         "ge_rf", "ge_svm", "w_hh_noshare", "w_household")]


# Add the constituency ID
data_res <- merge(data_res, Const, by.x = "hh_id", by.y = "hht_ID")


### First term of the variance estimation

# Total number of constituencies
N_const <- length(unique(Munich_hh$const))

# number of consituencies surveyed
n_const <- length(unique(Const$const_start))


# Calculate the estimated totals of ge at the constituency level
split_const <- split(data_res, data_res$const_start)

tot_e_const <- sapply(split_const, function(z){
  t_e_roche <- sum(z[, "ge_roche"] * z[, "w_household"])
  t_e_igg <- sum(z[, "ge_igg"] * z[, "w_household"])
  t_e_iga <- sum(z[, "ge_iga"] * z[, "w_household"])
  t_e_roche_manu <- sum(z[, "ge_roche_manu"] * z[, "w_household"])
  t_e_igg_manu <- sum(z[, "ge_igg_manu"] * z[, "w_household"])
  t_e_iga_manu <- sum(z[, "ge_iga_manu"] * z[, "w_household"])
  t_e_rf <- sum(z[, "ge_rf"] * z[, "w_household"])
  t_e_svm <- sum(z[, "ge_svm"] * z[, "w_household"])
  return(c(t_e_roche = t_e_roche, t_e_igg = t_e_igg, t_e_iga = t_e_iga,
           t_e_roche_manu = t_e_roche_manu, t_e_igg_manu = t_e_igg_manu, t_e_iga_manu = t_e_iga_manu,
           t_e_rf = t_e_rf, t_e_svm = t_e_svm))
})

# Calculate the dispersion of ge
s_e_roche <- var(tot_e_const[1, ])
s_e_igg <- var(tot_e_const[2, ])
s_e_iga <- var(tot_e_const[3, ])
s_e_roche_manu <- var(tot_e_const[4, ])
s_e_igg_manu <- var(tot_e_const[5, ])
s_e_iga_manu <- var(tot_e_const[6, ])
s_e_rf <- var(tot_e_const[7, ])
s_e_svm <- var(tot_e_const[8, ])

# First term of the variance estimation
V1_roche <- N_const^2 * (1/n_const - 1/N_const) * s_e_roche
V1_igg <- N_const^2 * (1/n_const - 1/N_const) * s_e_igg
V1_iga <- N_const^2 * (1/n_const - 1/N_const) * s_e_iga
V1_roche_manu <- N_const^2 * (1/n_const - 1/N_const) * s_e_roche_manu
V1_igg_manu <- N_const^2 * (1/n_const - 1/N_const) * s_e_igg_manu
V1_iga_manu <- N_const^2 * (1/n_const - 1/N_const) * s_e_iga_manu
V1_rf <- N_const^2 * (1/n_const - 1/N_const) * s_e_rf
V1_svm <- N_const^2 * (1/n_const - 1/N_const) * s_e_svm

### Second term of the variance estimation

# In each constituency, variance of the error terms ge
s_e_const_roche <- tapply(data_res$ge_roche, data_res$const_start, var)
s_e_const_igg <- tapply(data_res$ge_igg, data_res$const_start, var)
s_e_const_iga <- tapply(data_res$ge_iga, data_res$const_start, var)
s_e_const_roche_manu <- tapply(data_res$ge_roche_manu, data_res$const_start, var)
s_e_const_igg_manu <- tapply(data_res$ge_igg_manu, data_res$const_start, var)
s_e_const_iga_manu <- tapply(data_res$ge_iga_manu, data_res$const_start, var)
s_e_const_rf <- tapply(data_res$ge_rf, data_res$const_start, var)
s_e_const_svm <- tapply(data_res$ge_svm, data_res$const_start, var)

# Add the number of households surveyed and in the population for each constituency
s_e_const_roche <- data.frame(hh_id = names(s_e_const_roche), s_e_roche = s_e_const_roche)
s_e_const_igg <- data.frame(hh_id = names(s_e_const_igg), s_e_igg = s_e_const_igg)
s_e_const_iga <- data.frame(hh_id = names(s_e_const_iga), s_e_iga = s_e_const_iga)
s_e_const_roche_manu <- data.frame(hh_id = names(s_e_const_roche_manu), s_e_roche_manu = s_e_const_roche_manu)
s_e_const_igg_manu <- data.frame(hh_id = names(s_e_const_igg_manu), s_e_igg_manu = s_e_const_igg_manu)
s_e_const_iga_manu <- data.frame(hh_id = names(s_e_const_iga_manu), s_e_iga_manu = s_e_const_iga_manu)
s_e_const_rf <- data.frame(hh_id = names(s_e_const_rf), s_e_rf = s_e_const_rf)
s_e_const_svm <- data.frame(hh_id = names(s_e_const_svm), s_e_svm = s_e_const_svm)

# For constituencies with freq==1, tapply(, , var) is giving NA, therefore we replace NA by 0
s_e_const_roche[is.na(s_e_const_roche$s_e_roche)==T, "s_e_roche"] <- 0
s_e_const_igg[is.na(s_e_const_igg$s_e_igg)==T, "s_e_igg"]<- 0
s_e_const_iga[is.na(s_e_const_iga$s_e_iga)==T, "s_e_iga"]<- 0
s_e_const_roche_manu[is.na(s_e_const_roche_manu$s_e_roche_manu)==T, "s_e_roche_manu"] <- 0
s_e_const_igg_manu[is.na(s_e_const_igg_manu$s_e_igg_manu)==T, "s_e_igg_manu"] <- 0
s_e_const_iga_manu[is.na(s_e_const_iga_manu$s_e_iga_manu)==T, "s_e_iga_manu"] <- 0
s_e_const_rf[is.na(s_e_const_rf$s_e_rf)==T, "s_e_rf"] <- 0
s_e_const_svm[is.na(s_e_const_svm$s_e_svm)==T, "s_e_svm"] <- 0

# Nb hh surveyed
nb_hh_s <- as.data.frame(table(Const$const_start))

# Merge all the information
s_e_const <- merge(s_e_const_roche, s_e_const_igg, by = "hh_id")
s_e_const <- merge(s_e_const, s_e_const_iga, by = "hh_id")
s_e_const <- merge(s_e_const, s_e_const_roche_manu, by = "hh_id")
s_e_const <- merge(s_e_const, s_e_const_igg_manu, by = "hh_id")
s_e_const <- merge(s_e_const, s_e_const_iga_manu, by = "hh_id")
s_e_const <- merge(s_e_const, s_e_const_rf, by = "hh_id")
s_e_const <- merge(s_e_const, s_e_const_svm, by = "hh_id")
s_e_const <- merge(s_e_const, nb_hh_s, by.x = "hh_id", by.y = "Var1")
s_e_const <- merge(s_e_const, Munich_hh, by.x = "hh_id", by.y = "const", all.x = TRUE)

# Second term of the variance estimation
V2_roche <- N_const / n_const * sum(s_e_const$Nb_hh^2 * (1/s_e_const$Freq - 1/s_e_const$Nb_hh) * s_e_const$s_e_roche)
V2_igg <- N_const / n_const * sum(s_e_const$Nb_hh^2 * (1/s_e_const$Freq - 1/s_e_const$Nb_hh) * s_e_const$s_e_igg)
V2_iga <- N_const / n_const * sum(s_e_const$Nb_hh^2 * (1/s_e_const$Freq - 1/s_e_const$Nb_hh) * s_e_const$s_e_iga)
V2_roche_manu <- N_const / n_const * sum(s_e_const$Nb_hh^2 * (1/s_e_const$Freq - 1/s_e_const$Nb_hh) * s_e_const$s_e_roche_manu)
V2_igg_manu <- N_const / n_const * sum(s_e_const$Nb_hh^2 * (1/s_e_const$Freq - 1/s_e_const$Nb_hh) * s_e_const$s_e_igg_manu)
V2_iga_manu <- N_const / n_const * sum(s_e_const$Nb_hh^2 * (1/s_e_const$Freq - 1/s_e_const$Nb_hh) * s_e_const$s_e_iga_manu)
V2_rf <- N_const / n_const * sum(s_e_const$Nb_hh^2 * (1/s_e_const$Freq - 1/s_e_const$Nb_hh) * s_e_const$s_e_rf)
V2_svm <- N_const / n_const * sum(s_e_const$Nb_hh^2 * (1/s_e_const$Freq - 1/s_e_const$Nb_hh) * s_e_const$s_e_svm)


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


### IgG
w_p_igg <- freq(x = KoCo_BLab$IgG_result, w = KoCo_BLab$w_ind_cal_igg, plot=F)[2, 2]

V_igg <- V1_igg + V2_igg

# Upper bound CI
w_u_ci_igg <- w_p_igg + qnorm(0.975) * sqrt(V_igg) * 100

# Lower bound CI
w_l_ci_igg <- w_p_igg - qnorm(0.975) * sqrt(V_igg) * 100


### IgA
w_p_iga <- freq(x = KoCo_BLab$IgA_result, w = KoCo_BLab$w_ind_cal_igg, plot=F)[2, 2]

V_iga <- V1_iga + V2_iga

# Upper bound CI
w_u_ci_iga <- w_p_iga + qnorm(0.975) * sqrt(V_iga) * 100

# Lower bound CI
w_l_ci_iga <- w_p_iga - qnorm(0.975) * sqrt(V_iga) * 100


### Roche manufacturer

w_p_roche_manu <- freq(x = KoCo_BLab$R_Result_manu_cutoff, w = KoCo_BLab$w_ind_cal, plot=F)[2, 2]

V_roche_manu <- V1_roche_manu + V2_roche_manu

# Upper bound CI
w_u_ci_roche_manu <- w_p_roche_manu + qnorm(0.975) * sqrt(V_roche_manu) * 100

# Lower bound CI
w_l_ci_roche_manu <- w_p_roche_manu - qnorm(0.975) * sqrt(V_roche_manu) * 100


### IgG manufacturer
w_p_igg_manu <- freq(x = KoCo_BLab$IgG_result_manu_cutoff, w = KoCo_BLab$w_ind_cal_igg, plot=F)[2, 2]

V_igg_manu <- V1_igg_manu + V2_igg_manu

# Upper bound CI
w_u_ci_igg_manu <- w_p_igg_manu + qnorm(0.975) * sqrt(V_igg_manu) * 100

# Lower bound CI
w_l_ci_igg_manu <- w_p_igg_manu - qnorm(0.975) * sqrt(V_igg_manu) * 100


### IgA manufacturer
w_p_iga_manu <- freq(x = KoCo_BLab$IgA_result_manu_cutoff, w = KoCo_BLab$w_ind_cal_igg, plot=F)[2, 2]

V_iga_manu <- V1_iga_manu + V2_iga_manu

# Upper bound CI
w_u_ci_iga_manu <- w_p_iga_manu + qnorm(0.975) * sqrt(V_iga_manu) * 100

# Lower bound CI
w_l_ci_iga_manu <- w_p_iga_manu - qnorm(0.975) * sqrt(V_iga_manu) * 100


### RF
w_p_rf <- freq(x = KoCo_BLab$rf_result, w = KoCo_BLab$w_ind_cal_igg, plot=F)[2, 2]

V_rf <- V1_rf + V2_rf

# Upper bound CI
w_u_ci_rf <- w_p_rf + qnorm(0.975) * sqrt(V_rf) * 100

# Lower bound CI
w_l_ci_rf <- w_p_rf - qnorm(0.975) * sqrt(V_rf) * 100


### SVM
w_p_svm <- freq(x = KoCo_BLab$svm_result, w = KoCo_BLab$w_ind_cal_igg, plot=F)[2, 2]

V_svm <- V1_svm + V2_svm

# Upper bound CI
w_u_ci_svm <- w_p_svm + qnorm(0.975) * sqrt(V_svm) * 100

# Lower bound CI
w_l_ci_svm <- w_p_svm - qnorm(0.975) * sqrt(V_svm) * 100


###
# Confidence intervals for the unweighted version
###

### Roche
p_roche <- freq(x = KoCo_BLab$R_Result, plot=F)[2 , 2]

# Upper bound CI
u_ci_roche <- p_roche + qnorm(0.975)* sqrt(p_roche/100*(1-p_roche/100)/length(unique(KoCo_BLab$hh_id))
)*100

# Lower bound CI
l_ci_roche <- p_roche - qnorm(0.975) * sqrt(p_roche/100*(1-p_roche/100)/length(unique(KoCo_BLab$hh_id))
)*100


### IgG
p_igg <- freq(x = KoCo_BLab$IgG_result, plot=F)[2 , 2]

# Upper bound CI
u_ci_igg <- p_igg + qnorm(0.975) * sqrt(p_igg/100*(1-p_igg/100)/length(unique(KoCo_BLab$hh_id)))*100

# Lower bound CI
l_ci_igg <- p_igg - qnorm(0.975) * sqrt(p_igg/100*(1-p_igg/100)/length(unique(KoCo_BLab$hh_id)))*100


### IgA
p_iga <- freq(x = KoCo_BLab$IgA_result, plot=F)[2 , 2]

# Upper bound CI
u_ci_iga <- p_iga + qnorm(0.975) * sqrt(p_iga/100*(1-p_iga/100)/length(unique(KoCo_BLab$hh_id)))*100

# Lower bound CI
l_ci_iga <- p_iga - qnorm(0.975) * sqrt(p_iga/100*(1-p_iga/100)/length(unique(KoCo_BLab$hh_id)))*100


### Roche manufacturer
p_roche_manu <- freq(x = KoCo_BLab$R_Result_manu_cutoff, plot=F)[2 , 2]

# Upper bound CI
u_ci_roche_manu <- p_roche_manu + qnorm(0.975)* sqrt(p_roche_manu/100*(1-p_roche_manu/100)/length(unique(KoCo_BLab$hh_id))
)*100

# Lower bound CI
l_ci_roche_manu <- p_roche_manu - qnorm(0.975) * sqrt(p_roche_manu/100*(1-p_roche_manu/100)/length(unique(KoCo_BLab$hh_id))
)*100


### IgG manufacturer
p_igg_manu <- freq(x = KoCo_BLab$IgG_result_manu_cutoff, plot=F)[2 , 2]

# Upper bound CI
u_ci_igg_manu <- p_igg_manu + qnorm(0.975) * sqrt(p_igg_manu/100*(1-p_igg_manu/100)/length(unique(KoCo_BLab$hh_id)))*100

# Lower bound CI
l_ci_igg_manu <- p_igg_manu - qnorm(0.975) * sqrt(p_igg_manu/100*(1-p_igg_manu/100)/length(unique(KoCo_BLab$hh_id)))*100


### IgA manufacturer
p_iga_manu <- freq(x = KoCo_BLab$IgA_result_manu_cutoff, plot=F)[2 , 2]

# Upper bound CI
u_ci_iga_manu <- p_iga_manu + qnorm(0.975) * sqrt(p_iga_manu/100*(1-p_iga_manu/100)/length(unique(KoCo_BLab$hh_id)))*100

# Lower bound CI
l_ci_iga_manu <- p_iga_manu - qnorm(0.975) * sqrt(p_iga_manu/100*(1-p_iga_manu/100)/length(unique(KoCo_BLab$hh_id)))*100


### RF
p_rf <- freq(x = KoCo_BLab$rf_result, plot=F)[2 , 2]

# Upper bound CI
u_ci_rf <- p_rf + qnorm(0.975)* sqrt(p_rf/100*(1-p_rf/100)/length(unique(KoCo_BLab$hh_id))
)*100

# Lower bound CI
l_ci_rf <- p_rf - qnorm(0.975) * sqrt(p_rf/100*(1-p_rf/100)/length(unique(KoCo_BLab$hh_id))
)*100


### SVM
p_svm <- freq(x = KoCo_BLab$svm_result, plot=F)[2 , 2]

# Upper bound CI
u_ci_svm <- p_svm + qnorm(0.975)* sqrt(p_svm/100*(1-p_svm/100)/length(unique(KoCo_BLab$hh_id))
)*100

# Lower bound CI
l_ci_svm <- p_svm - qnorm(0.975) * sqrt(p_svm/100*(1-p_svm/100)/length(unique(KoCo_BLab$hh_id))
)*100


#############################
# Final table with the results
#############################

results <- data.frame(calculation = rep(c("weighted", "unweighted"), each = 8),
                       test = rep(c("Roche", "IgG", "IgA", "Roche_manu", "IgG_manu", "IgA_manu",
                                    "RF", "SVM"), times = 2),
                       estimate = c(w_p_roche, w_p_igg, w_p_iga, 
                                    w_p_roche_manu, w_p_igg_manu, w_p_iga_manu,
                                    w_p_rf, w_p_svm,
                                    p_roche, p_igg, p_iga, 
                                    p_roche_manu, p_igg_manu, p_iga_manu,
                                    p_rf, p_svm),
                       lower_ci = c(w_l_ci_roche, w_l_ci_igg, w_l_ci_iga,
                                    w_l_ci_roche_manu, w_l_ci_igg_manu, w_l_ci_iga_manu,
                                    w_l_ci_rf, w_l_ci_svm,
                                    l_ci_roche, l_ci_igg, l_ci_iga,
                                    l_ci_roche_manu, l_ci_igg_manu, l_ci_iga_manu,
                                    l_ci_rf, l_ci_svm),
                       upper_ci = c(w_u_ci_roche, w_u_ci_igg, w_u_ci_iga,
                                    w_u_ci_roche_manu, w_u_ci_igg_manu, w_u_ci_iga_manu,
                                    w_u_ci_rf, w_u_ci_svm,
                                    u_ci_roche, u_ci_igg, u_ci_iga,
                                    u_ci_roche_manu, u_ci_igg_manu, u_ci_iga_manu,
                                    u_ci_rf, u_ci_svm))

### Export results
write.csv(results, here_weights("Estimates.csv"), row.names = FALSE)

#############################
# Adjust for sensitivity and specificity
#############################

spec_sens_classifier <- as.data.frame(readRDS(here_algo_results("specificity_sensitivity_classifier.RData")))

# Reorder the rows
spec_sens_classifier <- spec_sens_classifier[c("Roche N pan-Ig optimized cut-off",
                                               "Euroimmun S1 IgG optimized cut-off",
                                               "Euroimmun S1 IgA optimized cut-off",
                                               "Roche N pan-Ig manufacturer's cut-off",
                                               "Euroimmun S1 IgG manufacturer's cut-off",
                                               "Euroimmun S1 IgA manufacturer's cut-off",
                                               "Random Forest",
                                               "SVM"), ]

### Manufacturers specificity and sensitivity: Check folder Sensit_Spec

# Change specificity and sensitivity (7-13 days for Roche, 11-20 days for IgG and IgA) for manufacturers
spec_sens_classifier["Roche N pan-Ig manufacturer's cut-off", ] <- c(0.998, 0.853)
spec_sens_classifier["Euroimmun S1 IgG manufacturer's cut-off", ] <- c(0.993, 0.875)
spec_sens_classifier["Euroimmun S1 IgA manufacturer's cut-off", ] <- c(0.924, 0.917)


### Adjust for specificity and sensitivity (7-13 days for Roche, 11-20 days for IgG and IgA)
results_adj <- results

results_adj[, c("estimate", "lower_ci", "upper_ci")] <- (results_adj[, c("estimate", "lower_ci", "upper_ci")]/100 +
  spec_sens_classifier$Specificity - 1)/(spec_sens_classifier$Sensitivity + spec_sens_classifier$Specificity - 1)*100


### Adjust for specificity and sensitivity (>= 14 days for Roche, >=21 days for IgG and IgA)

# Change specificity and sensitivity (>=14 days for Roche, >=21 days for IgG and IgA) for manufacturers
spec_sens_classifier["Roche N pan-Ig manufacturer's cut-off", ] <- c(0.998, 0.995)
spec_sens_classifier["Euroimmun S1 IgG manufacturer's cut-off", ] <- c(0.993, 1)
spec_sens_classifier["Euroimmun S1 IgA manufacturer's cut-off", ] <- c(0.924, 1)


results_adj_2 <- results

results_adj_2[, c("estimate", "lower_ci", "upper_ci")] <- (results_adj_2[, c("estimate", "lower_ci", "upper_ci")]/100 +
                                                           spec_sens_classifier$Specificity - 1)/(spec_sens_classifier$Sensitivity + spec_sens_classifier$Specificity - 1)*100


### Export results

# Merge the results for the different sensitivities

results_adj$test[results_adj$test == "Roche_manu"] <- "Roche_manu_7_13"
results_adj$test[results_adj$test == "IgG_manu"] <- "IgG_manu_11_20"
results_adj$test[results_adj$test == "IgA_manu"] <- "IgA_manu_11_20"

results_adj_2$test[results_adj_2$test == "Roche_manu"] <- "Roche_manu_14_plus"
results_adj_2$test[results_adj_2$test == "IgG_manu"] <- "IgG_manu_21_plus"
results_adj_2$test[results_adj_2$test == "IgA_manu"] <- "IgA_manu_21_plus"

results_adj <- rbind(results_adj[1:6,], results_adj_2[4:6,], results_adj[7:14,], results_adj_2[12:16,])

write.csv(results_adj, here_weights("Estimates_adjusted.csv"), row.names = FALSE)
