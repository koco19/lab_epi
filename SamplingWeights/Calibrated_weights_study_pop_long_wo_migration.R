#' This script calibrates the sampling weights (from the script Sampling_weights.R)
#' and calculates the variance associated with the estimation of the proportion of ppl.
#' infected y the virus(positive tested).

here_koco_data = function (...) here::here("KoCo19_Datasets", ...)
here_algo_results = function (...) here::here("AlgorithmResults", ...)
here_weights = function(...) here::here("SamplingWeights", ...)

library(descr)
library(reshape2)
library(sampling)

#############################
# Load data sets
#############################

if (!exists("KoCo_BLab_split_study_pop")) {                 
  source(here_weights("Sampling_weights_study_pop_long.R"))
}else{"Needed file already loaded"}

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
totals <- c(totals, n_single_house = n_single_house, n_multi_house = n_multi_house,
            n_house_0_child = n_house_0_child, n_house_1plus_child = n_house_1plus_child)

# Removing temporary data
rm(margins_age_sex, n_single_house, n_multi_house, n_house_0_child, n_house_1plus_child)

#############################
# Create in the sample the variables needed for the calibration
#############################

data_house_split <- lapply(KoCo_BLab_split_study_pop, function(subdata){
  
  ###
  # Age / Sex structure 
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
 
    # Setting empty cells to 0
  data_house[is.na(data_house)] <- 0
  
  # Renaming columns and rows 
  colnames(data_house) <- gsub(x = colnames(data_house), pattern = "freq.", replacement = "")
  rownames(data_house) <- NULL
  
  # Reordering the columns
  data_house <- data_house[, c("hh_id", "Male_14-19", "Male_20-34", "Male_35-49", "Male_50-64", "Male_65-79", "Male_>=80",
                               "Female_14-19", "Female_20-34", "Female_35-49", "Female_50-64", "Female_65-79", "Female_>=80")]
  
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
                   bounds = c(0.5, 3), max_iter = 1000)
w_hh_cal_logit_1 <- g_logit_1 * d_house[[1]]

# Checking if calibrated weights sum up to auxiliary data
sum(t(w_hh_cal_logit_1) %*% as.matrix(data_house_split[[1]]) - totals[])
summary(w_hh_cal_logit_1)
plot(density(g_logit_1))

### Time point 2
g_logit_2 <- calib(data_house_split[[2]], d = d_house[[2]], totals, method = "logit",
                   bounds = c(0.2, 5), max_iter = 1000)
w_hh_cal_logit_2 <- g_logit_2 * d_house[[2]]

# Checking if calibrated weights sum up to auxiliary data
sum(t(w_hh_cal_logit_2) %*% as.matrix(data_house_split[[2]]) - totals[])
summary(w_hh_cal_logit_2)
plot(density(g_logit_2))
sd(w_hh_cal_logit_2)

d_house[[2]][which(w_hh_cal_logit_2 == max(w_hh_cal_logit_2))]

### Time point 3
g_logit_3 <- calib(data_house_split[[3]], d = d_house[[3]], totals, method = "logit",
                   bounds = c(0.5, 3), max_iter = 1000)
w_hh_cal_logit_3 <- g_logit_3 * d_house[[3]]

# Checking if calibrated weights sum up to auxiliary data
sum(t(w_hh_cal_logit_3) %*% as.matrix(data_house_split[[3]]) - totals[])
summary(w_hh_cal_logit_3)
sd(w_hh_cal_logit_3)

### Time point 4
g_logit_4 <- calib(data_house_split[[4]], d = d_house[[4]], totals, method = "logit",
                   bounds = c(0.09, 12), max_iter = 1000)
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
  write.csv(results, here_koco_data(paste("Estimates_study_pop_t", paste(i, "_wo_mig.csv", sep=""), sep="")), row.names = FALSE)
  
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
  write.csv(results_adj, here_koco_data(paste("Estimates_study_pop_adjusted_t", paste(i, "_wo_mig.csv", sep=""), sep="")), row.names = FALSE) 
}