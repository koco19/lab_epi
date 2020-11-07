#' Global setup, variables and settings

here_statistics = function (...) here::here("Statistics", ...)
here_r = function (...) here_statistics("R", ...)
here_lab_data = function (...) here::here("Lab_paper2_data", ...)
here_koco_data = function (...) here::here("KoCo19_Datasets", ...)
here_algo_results = function (...) here::here("AlgorithmResults", ...)

###############################################################################
# Plotting settings

ggplot2::theme_set(ggplot2::theme_classic())

###############################################################################
# Style

style = new.env()

#  4 categorical colors
# orange-red, green, orange, blue
style$pal4 = c("#D55E00", "#009E73", "#E69F00", "#56B4E9")
#  3 colors Red, green, grey
style$col_grey = "#999999"
style$col_trueneg = "#56B4E9"
style$col_truepos = "#D55E00"
style$pal3 <- c(style$col_grey, style$col_trueneg, style$col_truepos)

# Output formats
style$output_formats = c("pdf", "png")
style$dpi = 400

###############################################################################
# Columns

# Data frame column labels
columns = new.env()
# Corresponding output names
columns_pretty = new.env()

columns$eur_iga = "Eur_IgA"
columns_pretty$eur_iga = "EI-S1-IgA"

columns$eur_igg = "Eur_IgG"
columns_pretty$eur_igg = "EI-S1-IgG"

columns$roche = "Roche"
columns_pretty$roche = "Ro-N-Ig"

columns$main = c(columns$eur_iga, columns$eur_igg, columns$roche)
columns_pretty$main = c(columns_pretty$eur_iga, columns_pretty$eur_igg,
                        columns_pretty$roche)

columns$nt = "NT"
columns_pretty$nt = "NT"

columns$cpass = "cPass"
columns_pretty$cpass = "GS-cPass"

columns$vc = c("VC_N_IgA", "VC_N_IgM", "VC_N_IgG",
               "VC_S1_IgA", "VC_S1_IgM", "VC_S1_IgG",
               "VC_S2_IgA", "VC_S2_IgM", "VC_S2_IgG")
columns_pretty$vc = stringr::str_replace_all(columns$vc, c("_"="-"))

columns$lineblot = c("LineBlot_NP_SARS_2", "LineBlot_RBD_SARS_2", "LineBlot_S1_SARS_2")
columns_pretty$lineblot = c("MG-NP", "MG-RBD", "MG-S1")

columns$cold = c("Schnupfen_NP_229E", "Schnupfen_NP_NL63", "Schnupfen_NP_OC43", "Schnupfen_NP_HKU1")
columns_pretty$cold = stringr::str_sub(columns$cold, -4, -1)

columns$all = c(
  columns$main, columns$nt, columns$cpass, columns$vc, columns$lineblot)
columns_pretty$all = c(
  columns_pretty$main, columns_pretty$nt, columns_pretty$cpass,
  columns_pretty$vc, columns_pretty$lineblot)

columns$confirmatory = columns$all[4:length(columns$all)]
columns_pretty$confirmatory = columns_pretty$all[4:length(columns$all)]

###############################################################################
# Thresholds
# Linetype
style$thr_lt_old = 3 # dotted
style$thr_lt_new = 2 # dashed
# Line color
style$thr_lc_old = "grey30"
style$thr_lc_new = "black"
# Label
style$thr_lb_old = "Manufacturer"
style$thr_lb_new = "Optimised"

###############################################################################
# Cutoffs

cutoff = new.env()

cutoff$old = list(
  # main
  Eur_IgA = 1.1,
  Eur_IgG = 1.1,
  Roche = 1.0,
  # nt and cpass
  NT = NA,
  cPass = 20,
  # array
  VC_S1_IgA = 100,
  VC_S2_IgA = 100,
  VC_N_IgA = 100,
  VC_S1_IgM = 100,
  VC_S2_IgM = 100,
  VC_N_IgM = 100,
  VC_S1_IgG = 100,
  VC_S2_IgG = 100,
  VC_N_IgG = 100,
  # line blot
  LineBlot_NP_SARS_2 = 1,
  LineBlot_RBD_SARS_2 = 1,
  LineBlot_S1_SARS_2 = 1
)

cutoff_new = readRDS(here_algo_results("cutoffs.RData"))
rownames(cutoff_new)[rownames(cutoff_new) == "iga"] = "Eur_IgA"
rownames(cutoff_new)[rownames(cutoff_new) == "igg"] = "Eur_IgG"
rownames(cutoff_new)[rownames(cutoff_new) == "roche"] = "Roche"

cutoff$new = list()
for (col in names(cutoff$old)) {
  cutoff$new[col] = cutoff_new[col, "Median"]
}

rm(cutoff_new, col)
