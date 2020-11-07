## author: Peter Pütz, peter.puetz@uni-bielefeld.de

# This script calculates performance criteria such as specificity for
# different classifiers:
# - For single tests based on the manufacturers' cutoffs
# - for random forest and support vector machine (svm)

here_algo_results = function (...)
  here::here("AlgorithmResults", ...)

# Specify the list of required packages to be installed and load
Required_Packages <- c("tidyverse",
                       "rpart",
                       "magrittr",
                       "naniar",
                       "randomForest",
                       "pROC",
                       "e1071")

# install (if necessary) and load packages
Install_And_Load(Required_Packages)

# set seed for replicability
set.seed(1)

# set number of replications for tuning hyperparameters of random forest
# and support vector machine, computing time increases massively is this
# number is increased
tuning_repit = 1000

# set number of replications for estimating already tuned random forest
# and support vector machine
repit = 2000

# function to calculate area under roc curve
auc_roc <- function (outcome, pred) {
  # calculate ROC
  roc <- pROC:::roc(outcome, pred)
  # area under ROC
  auc_roc <- roc$auc
  return(auc_roc)
}

# select people with "known" outcome variable / serological status
all_data <- data %>% dplyr::filter(!is.na(model_outcome))

# overview of outcome variable
all_data %>% dplyr::select(model_outcome) %>%
  table()

# combine true negatives
all_data %<>% mutate(outcome = as.ordered(
  ifelse(
    model_outcome == "true_negatives_Blood_donors" |
      model_outcome == "true_negatives_KoCo19",
    "negative",
    "positive"
  )
))

# number of negatives and positives
all_data %>%
  dplyr::select(outcome) %>%
  table()

# for better comparability exclude few observations without test results
all_cont <- all_data %>%
  dplyr::filter(!is.na(iga), !is.na(igg), !is.na(roche)) %>%
  mutate(outcome = as.ordered(outcome))


# peformance provided cutoffs ---------------------------------------------
# cutoffs provided by manufacturer (now single for EI IgA and IGG)
cutoffs <- unlist(cutoff$old)

# get test names
tests <- names(cutoff$old) %>%
  str_replace("Roche", "roche") %>%
  str_replace("Eur_IgA", "iga") %>%
  str_replace("Eur_IgG", "igg")#

# results matrix
#  results[1, ] are the provided
#  results[2, ] are the test specificities
#  results[3, ] are the test sensitivities
#  results[4, ] are the accuracies
#  results[5, ] are the areas under the ROC curves
results <- data.frame(matrix(NA, 5, length(tests)))
colnames(results) <- tests

# replace categorical value indicating an interval
# by an arbitrary value within the interval
all_cont[all_cont == "<10"] <- "5"
all_cont[all_cont == ">80"] <- "90"
all_cont[all_cont == "not_reactive"] <- "0.5"

# start loop over cutoffs
for (i in 1:length(tests)) {
  var <- tests[i]
  # store cut-off
  results[1, i] <- cutoffs[i]
  
  # remove NAs for current variable
  help_data <- all_cont %>%
    dplyr::filter(!is.na(.data[[var]]))
  
  # if there is a manufacturer's cut-off
  if (!is.na(cutoffs[i])) {
    # predict serological status
    pred <-
      ifelse(help_data[, var]  %>% pull() %>% as.numeric() >= cutoffs[i], 1, 0)
    
    # confusion matrix
    pred_table <- table(pred, help_data$outcome)
    
    # estimated probability for positive outcome
    prob_pos <-
      ifelse(
        help_data$outcome == "positive" & pred == 1,
        pred_table[2, "positive"] /
          sum(pred_table[2, ]),
        pred_table[1, "positive"] /
          sum(pred_table[1, ])
      )
    # specificity (1 - FPR)
    results[2, i] <-
      1 - pred_table[2, "negative"] / sum(pred_table[, "negative"])
    # sensitivity (1 - FNR)
    results[3, i] <-
      1 - pred_table[1, "positive"] / sum(pred_table[, "positive"])
    # accuracy
    results[4, i] <- sum(diag(pred_table)) / sum(pred_table)
    # make outcome variable numeric
    outcome <- ifelse(help_data$outcome == "positive", 1, 0)
    # predicted probabilities for a positive outcome
    pred <- as.numeric(prob_pos)
    # area under ROC curve
    auc <- as.numeric(auc_roc(outcome, pred))
    results[5, i] <- auc
  }
}


# Random Forest -----------------------------------------------------------
# oob = "out-of-bag"
oob.err = numeric()
oob.help = numeric()
formula = outcome ~ roche + igg + iga
# mtry is no of variables randomly chosen at each split, nodesize
# the minimum number of observations at each split (tree becomes smaller
# if this number is decreased). choose optimal value based on grid search and
# by minimizing out-of-bag forecast error
for (minnodesize in 1:10) {
  for (mtry in 1:3) {
    rf <- randomForest(
      formula = formula,
      data = all_cont,
      mtry = mtry,
      nodesize = minnodesize,
      ntree = tuning_repit
    )
    # store average oob error when all trees fitted for different number of mtry
    oob.help[mtry] = rf$err.rate[tuning_repit, "OOB"]
  }
  # append these oob errors to previous ones for other minimal nodesizes
  oob.err <- c(oob.err, oob.help)
}

# display oob error(s)
1 - oob.err[which(oob.err == min(oob.err))]
plot(1:30, oob.err)

# save optimal parameters
mtry_opt <- (which(oob.err == min(oob.err)) %% 3)[1]
mns_opt <- (round(which(oob.err == min(oob.err)) / 3) + 1)[1]

# estimate model using optimal parameters to estimate performance criteria,
# model will also be saved for later predictions
rf <- randomForest(
  formula = formula,
  data    = all_cont,
  nodesize = mns_opt,
  mtry = mtry_opt,
  ntree = repit,
  importance = TRUE
)
# plot importance of variables
varImpPlot(rf)

# getTree(rf, 4, labelVar=TRUE) exemplary tree
# specificity
spec_rf <- round(1 - rf$confusion[1, 3], 4)
# sensitivity
sens_rf <- round(1 - rf$confusion[2, 3], 4)
# overall accuracy
ovacc_rf <- round(sum(diag(rf$confusion[, -3])) /
                    sum((rf$confusion[, -3])), 4)
paste("Specificty:", spec_rf)
paste("Sensitivity:", sens_rf)
paste("Overall accuracy:", ovacc_rf)

# predicted probabilities
predictions <- predict(rf, type = "prob")
# probabilities for positive outcome
pred <- as.numeric(predictions[, 2])
# make outcome variable numeric
outcome <- ifelse(all_cont$outcome == "positive", 1, 0)
# area under roc
auc <- as.numeric(auc_roc(outcome, pred))
results_rf <- c(NA, spec_rf, sens_rf, ovacc_rf, auc)

# Support vector machine --------------------------------------------------
# select test variables
dat <- all_cont %>%
  dplyr::select(outcome, roche, igg, iga)
# tune svm with all three predictors based on grid search
set.seed(1)
obj <- tune.svm(
  outcome ~ igg + roche + iga,
  data = dat,
  gamma = 10 ^ (-2:1),
  cost = 2 ^ (6:12),
  tunecontrol = tune.control(sampling = "boot", nboot = tuning_repit)
)

# placeholder
predictions <- matrix(NA, nrow(dat), repit)

# start loop, repit specified at beginning of script
for (i in 1:repit) {
  # generate bootstrap sample
  ind <- sample(1:nrow(dat), nrow(dat), replace = TRUE)
  boot_sample <- dat[ind, ]
  # oob sample
  oob_obs <- dat[-ind, ]
  # run svm on boostrap sample with optimized parameters
  svmfit = svm(
    outcome ~ igg + roche + iga,
    data = boot_sample,
    #    kernel = "linear",
    probability = TRUE,
    gamma = obj$best.parameters[, "gamma"],
    cost = obj$best.parameters[, "cost"]
  )
  # predictions for oob sample
  pred <-
    attr(predict(svmfit, newdata = oob_obs, probability = TRUE),
         "probabilities")
  pos_pred <- ifelse(pred[, 1] < pred[, 2], 1, 0)
  predictions[-ind, i] <- pos_pred
}

# majority vote for predictions
votes <- apply(predictions, 1, function (x)
  mean(x, na.rm = TRUE))
pred <- ifelse(votes >= 0.5, "positive", "negative")

# confusion matrix
pred_table <- table(pred, dat$outcome)

# specificity
spec_svm <-
  round(1 - pred_table[2, "negative"] / sum(pred_table[, "negative"]), 4)

# sensitivity
sens_svm <-
  round(1 - pred_table[1, "positive"] / sum(pred_table[, "positive"]), 4)

# overall accuracy
ovacc_svm <- round(sum(diag(pred_table)) / sum(pred_table), 4)

# specificity
paste("Specificty:", spec_svm)
paste("Sensitivity:", sens_svm)
paste("Overall accuracy:", ovacc_svm)

# make outcome variable numeric
outcome <- ifelse(all_cont$outcome == "positive", 1, 0)
# predicted probablities for positive outcome
pred <- as.numeric(votes)
# are under roc curve
auc <- as.numeric(auc_roc(outcome, pred))
# store results
results_svm <-
  c(NA, spec_svm , sens_svm , ovacc_svm , auc)

# svmfit for export
svmfit = svm(
  outcome ~ roche + igg + iga,
  data = dat,
  #    kernel = "linear",
  probability = TRUE,
  gamma = obj$best.parameters[, "gamma"],
  cost = obj$best.parameters[, "cost"]
)

####################### save results ########################

# save results ------------------------------------------------------------
# save performance criteria
saveRDS(results,
        here_algo_results("results_provided_cutoffs.RData"))
saveRDS(results_rf, here_algo_results("results_rf.RData"))
saveRDS(results_svm, here_algo_results("results_svm.RData"))
# save the "optimal" random forest and svm, will be used for predictions for
# cohort data from Munich
saveRDS(rf, here_algo_results("rf_optimal.RData"))
saveRDS(svmfit, here_algo_results("svm_optimal.RData"))
