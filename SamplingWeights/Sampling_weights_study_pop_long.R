#' This script calculates the sampling weights for confounding effects based 
#' on the sampling design (selection of the constituencies, the households,
#' and the individuals). The weights of the households are shared to account
#' for the random routes crossing the boundaries of the constituencies.
#' Finally it splits the data in sub datasets based on the weeks of visit.

here_koco_data = function (...) here::here("KoCo19_Datasets", ...)
here_algo_results = function (...) here::here("AlgorithmResults", ...)

library(descr)
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