## author: Peter Pütz, peter.puetz@uni-bielefeld.de
# This script calculates optimised cut-offs
# and related performance criteria such as specificity 
# for single tests

# Specify the list of required packages to be installed and load
Required_Packages <-
  c("tidyverse", "rpart", "magrittr", "rtf", "pROC")

# install (if necessary) and load packages
Install_And_Load(Required_Packages)

here_algo_results = function (...)
  here::here("AlgorithmResults", ...)

# set seed for reproducibility
set.seed(1)
# number of bootstrap samples
repit = 10000

# function to calculate area under roc curve
auc_roc <- function (outcome, pred) {
  # calculate ROC
  roc <- pROC:::roc(outcome, pred)
  # area under ROC
  auc_roc <- roc$auc
  return(auc_roc)
}

# select people with "known" outcome variable and drop NAs of roche test to obtain
# comparable results (no missings for igg and iga)
all_data <- data %>%
  dplyr::filter(!is.na(model_outcome), !is.na(roche))

# overview of outcome variable
all_data %>% dplyr::select(model_outcome) %>%
  table()

# combine true negatives
all_data %<>% mutate(outcome = as.ordered(
  ifelse(
    model_outcome == "true_negatives_Blood_donors" |
      model_outcome == "true_negatives_KoCo19" |
      model_outcome == "true_negatives_KoCo19",
    "negative",
    "positive"
  )
))

# select relevant variables, i.e. outcome and all test variables
all_cont <- all_data %>%
  dplyr::select(
    outcome,
    roche,
    igg,
    iga,
    NT,
    cPass,
    starts_with("VC"),
    -contains("result"),
    starts_with("Line")
  )

# order outcome variable
all_cont %<>%
  mutate(outcome = as.ordered(outcome))

# check which variables are character variables (these are basically categorical)
all_cont %>% summarise_all( ~ is.character(.)) %>%
  t()

# main tests
tests_main <- c("roche", "igg", "iga")

# placeholders for...
# ...sample size
sample_size <- numeric()
# ...number of true positives and negatives
positives <- negatives <- numeric()
# ...cutoffs averaged over oob estimates
cutoffs_oob <- numeric(repit)
## ...all cutoffs
cutoffs_all <- matrix(0, repit, ncol(all_cont) - 1)

# for loop, consider only tests
all_cont_tests <- all_cont %>% dplyr::select(-outcome)

# placeholder for results for averaged cutoffs and performances for different criteria
results_opt_cutoffs <-
  data.frame(matrix(0, 5, length(all_cont_tests)))
colnames(results_opt_cutoffs) <- colnames(all_cont_tests)

# change factor variables to character variables to avoid problems in recoding
all_cont_tests %<>% mutate_if(is.factor, as.character)

# replace categorical value indicating an interval
# by an arbitrary value within the interval
all_cont_tests[all_cont_tests == "<10"] <- "5"
all_cont_tests[all_cont_tests == ">80"] <- "90"
all_cont_tests[all_cont_tests == "not_reactive"] <- "0.5"
all_cont_tests %<>% mutate_if(is.character, as.numeric)

# append outcome variable
all_cont <-
  all_cont_tests %>% bind_cols(all_cont %>% dplyr::select(outcome))

# start loop
for (j in 1:ncol(all_cont_tests)) {
  print(j)
  # store current test name
  var <- colnames(all_cont_tests)[j]
  # remove NAs for variable
  all_cont_random <-
    all_cont %>%
    dplyr::filter(!is.na(.data[[var]]))
  # "decision tree" formula
  formula = as.formula(paste("outcome ~ ", var, ""))
  # store cutoffs for single trees
  cutoffs_var <- numeric()
  # ...votes for all observations in every bootstrap sample
  predictions <- matrix(NA, nrow(all_cont_random), repit)
  # start bootstrapping
  for (i in 1:repit) {
    # if variable is not among the main tests
    if (!(var %in% tests_main)) {
      # placeholder for cutoff
      split = NULL
      # as in rare cases the tree cannot be estimated, we repeat the bootstrap
      # until the tree renders an optimal cutoff
      while (is.null(split))
      {
        # generate bootstrap sample with replacement
        ind <-
          sample(1:nrow(all_cont_random),
                 nrow(all_cont_random),
                 replace = TRUE)
        boot_sample <- all_cont_random[ind, ]
        # and calculate decision "tree" with one node
        dtree <-
          rpart(
            formula,
            data = boot_sample,
            method = "class",
            maxdepth = 1
          )
        # store optimal cutoff
        split <- dtree$splits[4]
        # the optimal cutoff is rarely unique and thus computed as the mean
        # of all possible optimal cutoffs
        # the index of the sorted variable that gives the lowest possible value
        # that still represents an optimal cutoff
        ind_low <- dtree$frame$n[2]
        # store the lowest possible cutoff, it is dependent on the numerical values
        # we assign to the categories (e.g. 5 for <10, see above).
        lowest_possible_cutoff <-
          sort(boot_sample[, var] %>% pull())[ind_low]
      }
      # To make the final
      # optimal cutoff yielded by R independent of the assignment of the numerical values
      # for the categories (see above), we have to check for
      # lowest possible cutoffs that are below 10 (1 for Lineblot test,
      # respectively)  whether 0 or 10 (1) is a better cutoff
      # for tests with categorical values from 0 to 10
      if (between(lowest_possible_cutoff, 0, 9.9999999999) &
          !grepl("Blot", var))
      {
        # prediction accuracy if 10 is the cutoff
        boot_sample %<>% mutate(pred = ifelse(.data[[var]] > 10, 1, 0))
        acc_10 <-
          boot_sample %>% dplyr::select(pred, outcome) %>% table() %>% diag() %>% sum()
        # prediction accuracy if 0 is the cutoff
        acc_0 <-
          boot_sample %>% filter(outcome == "positive") %>% nrow()
        # choose cutoff with higher prediction accuracy
        split <- ifelse(acc_10 > acc_0, 10, 0)
        
        # for variables with categorical values from 0 to 1 (Lineblot)
      } else  if (between(lowest_possible_cutoff, 0, 0.9999999999) &
                  grepl("Blot", var)) {
        # prediction accuracy if 1 is the cutoff
        boot_sample %<>% mutate(pred = ifelse(.data[[var]] > 1, 1, 0))
        acc_1 <-
          boot_sample %>% dplyr::select(pred, outcome) %>% table() %>% diag() %>% sum()
        # prediction accuracy if 0 is the cutoff
        acc_0 <-
          boot_sample %>% filter(outcome == "positive") %>% nrow()
        # choose cutoff with higher prediction accuracy
        split <- ifelse(acc_1 > acc_0, 1, 0)
      }
      
      # store optimal cutoff
      cutoffs_oob[i] <- split
      # calculate prediction accuracy
      # out of bag sample
      oob_obs <- all_cont_random[-ind, ]
      # predictions for oob sample
      pred <- predict(dtree, newdata = oob_obs)
      # if prediction probability >0.5, consider it a positive outcome
      pos_pred <- ifelse(pred[, 1] < pred[, 2], 1, 0)
      # store predictions for observations
      predictions[-ind, i] <- pos_pred
    } else {
      # (var %in% tests_main)
      # for the main variables, cutoffs are calculated as median cutoffs over
      # bootstrap samples
      
      # again repeat until optimal cutoff is estimated (works for vast
      # majority of bootstrap samples)
      split = NULL
      while (is.null(split)) {
        # generate bootstrap sample with replacement
        ind <-
          sample(1:nrow(all_cont_random),
                 nrow(all_cont_random),
                 replace = TRUE)
        boot_sample <- all_cont_random[ind, ]
        # and calculate decision "tree" with one node
        dtree <-
          rpart(
            formula,
            data = boot_sample,
            method = "class",
            maxdepth = 1
          )
        # store cutoff if available
        split <- dtree$splits[4]
      }
      cutoffs_oob[i] <- split
      # calculate prediction accuracy
      # out of bag sample
      oob_obs <- all_cont_random[-ind, ]
      # predictions for oob sample
      pred <- predict(dtree, newdata = oob_obs)
      # if prediction probability >0.5, consider it a positive outcome
      pos_pred <- ifelse(pred[, 1] < pred[, 2], 1, 0)
      # store predictions for observations
      predictions[-ind, i] <- pos_pred
    }
  }
  
  # store mean predictions for all observations
  votes <- apply(predictions, 1, function (x)
    mean(x, na.rm = TRUE))
  # majority votes for all observations
  pred <- ifelse(votes >= 0.5, "positive", "negative")
  ## calculate performance for different criteria by comparing majority votes
  ## with true outcomes
  # confusion matrix
  pred_table <- table(pred, all_cont_random$outcome)
  # specificity
  spec_oob <-
    1 - pred_table[2, "negative"] / sum(pred_table[, "negative"])
  # sensitivity
  sens_oob <-
    1 - pred_table[1, "positive"] / sum(pred_table[, "positive"])
  # overall accuracy
  ovacc_oob <- sum(diag(pred_table)) / sum(pred_table)
  # compute area under ROC
  outcome <- ifelse(all_cont_random$outcome == "positive", 1, 0)
  pred <- as.numeric(votes)
  auc <- as.numeric(auc_roc(outcome, pred))
  
  # store performance measures
  results_opt_cutoffs[, j] <-
    c(median(cutoffs_oob, na.rm = TRUE),
      spec_oob,
      sens_oob,
      ovacc_oob,
      auc)
  
  # store cutoffs
  cutoffs_all[, j] <- cutoffs_oob
  # store sample sizes, number of positive and negative outcomes
  tab_positives <-
    all_cont %>% dplyr::select(outcome, var) %>% na.omit() %>%
    dplyr::select(outcome) %>% table()
  sample_size[j] <- sum(tab_positives)
  positives[j] <- tab_positives[2]
  negatives[j] <- tab_positives[1]
}

# make results nicer
cutoffs_all <- as.data.frame(cutoffs_all)
colnames(cutoffs_all) <- colnames(all_cont_tests)

# how often 0 as cut-off for NT
sum(cutoffs_all$NT == 0)

## store means, medians, 95% intervals, variable type, sample size, no. of positives and negatives
stats <- cutoffs_all %>%
  summarise_all(~ list(
    min(., na.rm = TRUE),
    mean(., na.rm = TRUE),
    median (., na.rm = TRUE),
    max(., na.rm = TRUE),
    quantile (., probs = 0.025),
    quantile (., probs = 0.975)
  )) %>%
  mutate_all(~ unlist(.)) %>%
  t()
colnames(stats) <-
  c("Min", "Mean", "Median", "Max", "CI_lb", "CI_ub")

# combine with sample size and no. of positives and negatives
bootstrap_cutoffs <-
  as.data.frame(cbind(stats, sample_size,
                      positives,
                      negatives))

# split results for performance measures according to primary and confirmatory tests
perf_measures_primary <-
  results_opt_cutoffs[, colnames(results_opt_cutoffs) %in% tests_main]
perf_measures_confirmatory <-
  results_opt_cutoffs[, !(colnames(results_opt_cutoffs) %in% tests_main)]

# save results ------------------------------------------------------------
saveRDS(bootstrap_cutoffs, here_algo_results("cutoffs.RData"))
write.csv(bootstrap_cutoffs, here_algo_results("cutoffs.csv"))
saveRDS(perf_measures_primary,
        here_algo_results("perf_measures_primary.RData"))
saveRDS(
  perf_measures_confirmatory,
  here_algo_results("perf_measures_confirmatory.RData")
)
