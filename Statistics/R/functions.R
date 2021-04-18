#' Extract for Euroimmune and Roche the last values.
#' Creates columns Eur_IgA, Eur_IgG, Roche, and with appending _result
#' containing the values of the latest available measurement.
#' Note: This function assumes that wrong zeros etc. have already been
#' removed.
#' 
#' @param df The single_data data frame
extract_last_values = function(df) {
  # Assuming that the dates of replicates 1,2,3(,4) are increasing

  # Eur IgA
  # Preferring replicate 3,2,1
  df$Eur_IgA = ifelse(!is.na(df$eur_quotient_3_IgA), df$eur_quotient_3_IgA,
                      ifelse(!is.na(df$eur_quotient_2_IgA), df$eur_quotient_2_IgA,
                             df$eur_quotient_1_IgA))
  df$Eur_IgA_result = ifelse(!is.na(df$eur_quotient_3_IgA), df$eur_der_test_result_3_IgA,
                             ifelse(!is.na(df$eur_quotient_2_IgA), df$eur_der_test_result_2_IgA,
                                    df$eur_der_test_result_1_IgA))
  
  # IgG
  # Preferring replicate 3,2,1
  df$Eur_IgG = ifelse(!is.na(df$eur_quotient_3_IgG), df$eur_quotient_3_IgG,
                      ifelse(!is.na(df$eur_quotient_2_IgG), df$eur_quotient_2_IgG,
                             df$eur_quotient_1_IgG))
  df$Eur_IgG_result = ifelse(!is.na(df$eur_quotient_3_IgG), df$eur_der_test_result_3_IgG,
                             ifelse(!is.na(df$eur_quotient_2_IgG), df$eur_der_test_result_2_IgG,
                                    df$eur_der_test_result_1_IgG))
  
  # Roche
  # Preferring machine 2 over machine 1, and then replicates 4,3,2,1
  df$Roche = ifelse(!is.na(df$roche_COI_4_2), df$roche_COI_4_2,
                    ifelse(!is.na(df$roche_COI_3_2), df$roche_COI_3_2,
                           ifelse(!is.na(df$roche_COI_2_2), df$roche_COI_2_2,
                                  ifelse(!is.na(df$roche_COI_1_2), df$roche_COI_1_2,
                                         ifelse(!is.na(df$roche_COI_4_1), df$roche_COI_4_1,
                                                ifelse(!is.na(df$roche_COI_3_1), df$roche_COI_3_1,
                                                       ifelse(!is.na(df$roche_COI_2_1), df$roche_COI_2_1,
                                                              df$roche_COI_1_1)))))))
  df$Roche_result = ifelse(!is.na(df$roche_COI_4_2), df$roche_Interpretation_4_2,
                           ifelse(!is.na(df$roche_COI_3_2), df$roche_Interpretation_3_2,
                                  ifelse(!is.na(df$roche_COI_2_2), df$roche_Interpretation_2_2,
                                         ifelse(!is.na(df$roche_COI_1_2), df$roche_Interpretation_1_2,
                                                ifelse(!is.na(df$roche_COI_4_1), df$roche_Interpretation_4_1,
                                                       ifelse(!is.na(df$roche_COI_3_1), df$roche_Interpretation_3_1,
                                                              ifelse(!is.na(df$roche_COI_2_1), df$roche_Interpretation_2_1,
                                                                     df$roche_Interpretation_1_1)))))))
  df
}

#' Convert all measurement data to numeric values
#' Uses conventions for categorical data, e.g. 5 for "<10"
#' 
#' @param df The single_data data frame
to_numeric = function(df) {
  # NT to numeric
  df[,"NT"] = as.character(df[,"NT"])
  df[df[,"NT"]=="<5" & !is.na(df[,"NT"]),"NT"] = 4
  df[df[,"NT"]==">80" & !is.na(df[,"NT"]),"NT"] = 81
  df[,"NT"] = as.numeric(df[,"NT"])
  
  # VC to numeric
  for (col in columns$vc) {
    df[df[,col]=="<10" & !is.na(df[,col]),col] = 5
    df[,col] = as.numeric(df[,col])
  }
  
  # Line Blot to numeric
  for (col in columns$lineblot) {
    df[df[,col]=="not_reactive" & !is.na(df[,col]),col] = 0.5
    df[,col] = as.numeric(df[,col])
  }
  
  # Common Cold to numeric
  for (col in columns$cold) {
    df[df[,col]=="not_reactive" & !is.na(df[,col]),col] = 0.5
    df[,col] = as.numeric(df[,col])
  }
  
  df
}

#' Get positivity/negativity tables.
#' Returns data frame with columns pp, pn, np, nn with the numbers of how many
#' data points are positive-positive, negative-positive etc. according to the
#' thresholds.
#' 
#' @param data_val The value data frame
#' @param col1,col2 The columns 1 and 2
#' @param cutoff1,cutoff2 The cutoffs to apply to columns 1 and 2
#' @param label A label to add to the data row, optional
qualitative_2x2_table = function(data_val, col1, col2,
                                 cutoff1, cutoff2, label="") {
  pp = sum(data_val[,col1]>=cutoff1 & data_val[,col2]>=cutoff2, na.rm=T)
  pn = sum(data_val[,col1]>=cutoff1 & data_val[,col2]<cutoff2, na.rm=T)
  np = sum(data_val[,col1]<cutoff1 & data_val[,col2]>=cutoff2, na.rm=T)
  nn = sum(data_val[,col1]<cutoff1 & data_val[,col2]<cutoff2, na.rm=T)
  data.frame(label=label, pp=pp, pn=pn, np=np, nn=nn)
}

#' Customized scatter plot with various decorations
#' 
#' @param data_val The value data frame
#' @param col1,col2 The columns of interest
#' @param cutoff1,cutoff2 The cutoffs to apply
#' @param stat_x,stat_y Where to put the correlation annotation
#' @param ann_x,ann_y Where to put the quadrant anotations
#'        (4-vectors, nn, pn, np, pp)
#' @param title Plot title
#' @param name1,name2 Axis labels
scatter = function(data_val, col1, col2, cutoff1, cutoff2,
                   stat_x, stat_y, ann_x, ann_y,
                   title=NULL,
                   name1=NA, name2=NA) {
  # Labels to column names if none passed
  if (is.na(name1)) { name1 = col1 }
  if (is.na(name2)) { name2 = col2 }
  # Qualitative values
  q2x2 = qualitative_2x2_table(data_val, col1, col2, cutoff1, cutoff2)
  # Plot it
  ggplot(data_val, aes(x=data_val[,col1], y=data_val[,col2])) + 
    # Points
    geom_point(alpha=0.5) +
    # Axis scaling and labeling
    scale_x_log10() + scale_y_log10() +
    labs(title=title, x=name1, y=name2) +
    # Cutoff lines (decoration assuming old cutoffs)
    geom_vline(xintercept = cutoff1, linetype = style$thr_lt_old,
               color = style$thr_lc_old) +
    geom_hline(yintercept = cutoff2, linetype = style$thr_lt_old,
               color = style$thr_lc_old) +
    # Regression line
    geom_smooth(formula=y~x, method="lm") +
    # Correlation number R and p
    ggpubr::stat_cor(label.x=stat_x, label.y=stat_y, r.digits = 3) +
    # Quadrant numbers
    annotate("text", x=ann_x, y=ann_y,
             label=c(paste("n =", q2x2$nn), paste("n =", q2x2$pn),
                     paste("n =", q2x2$np), paste("n =", q2x2$pp))) +
    # Diagonal line
    geom_abline(linetype="dashed")
}

#' Create bar plot of tests vs ground truth
#' 
#' @param data The single_data data frame
#' @param cols The columns of interest
#' @param cols_pretty Corresponding display names
#' @param ncol_output How many columns for the facet output
#' @param cutoff Cutoff environment with "old" and "new" cutoffs
test_gt_bars = function(data, cols, cols_pretty, ncol_output, cutoff) {
  # Convert all data to numeric
  data = to_numeric(extract_last_values(data))

  # Get recovered old
  rec_old = get_recovery_rates(data, cols, cutoff$old)
  print(rec_old)
  # Get recovered new
  rec_new = get_recovery_rates(data, cols, cutoff$new)
  print(rec_new)
  # Get recovered for coloring
  cutoff_color = cutoff$new
  #  Use old cutoff value if new value is NA
  for (col in cols) {
    if (is.na(cutoff_color[col])) {
      cutoff_color[col] = cutoff$old[col]
    }
  }
  rec_color = get_recovery_rates(data, cols, cutoff_color)

  # Normalize by true numbers
  truepos_cols = c("p_truepos", "n_truepos")
  trueneg_cols = c("p_trueneg", "n_trueneg")
  rec_new[,truepos_cols] = rec_new[,truepos_cols] / rec_new[,"truepos"]
  rec_new[,trueneg_cols] = rec_new[,trueneg_cols] / rec_new[,"trueneg"]
  #  Also add values for old
  rec_old[,truepos_cols] = rec_old[,truepos_cols] / rec_old[,"truepos"]
  rec_old[,trueneg_cols] = rec_old[,trueneg_cols] / rec_old[,"trueneg"]
  #  And for color
  rec_color[,truepos_cols] = rec_color[,truepos_cols] / rec_color[,"truepos"]
  rec_color[,trueneg_cols] = rec_color[,trueneg_cols] / rec_color[,"trueneg"]

  # Fill NAs into relevant columns if the cutoff is NA
  for (col in cols) {
    if (is.na(cutoff$old[col])) {
      rec_old[rec_old$Method==col, c(truepos_cols, trueneg_cols)] = NA
    }
    if (is.na(cutoff$new[col])) {
      rec_new[rec_new$Method==col, c(truepos_cols, trueneg_cols)] = NA
    }
    # Likely no NAs left, but just to be sure
    if (is.na(cutoff_color[col])) {
      rec_color[rec_color$Method==col, c(truepos_cols, trueneg_cols)] = NA
    }
  }

  # To long
  rec_long = reshape2::melt(
    rec_new[,c("Method", "truepos", "trueneg", truepos_cols, trueneg_cols)],
    id.vars=c("Method", "truepos", "trueneg"), value.name="Percentage new")
  colnames(rec_long)[colnames(rec_long) == 'variable'] = "Outcome"

  rec_long_old = reshape2::melt(
    rec_old[,c("Method", "truepos", "trueneg", truepos_cols, trueneg_cols)],
    id.vars=c("Method", "truepos", "trueneg"), value.name="Percentage old")
  # Fill in as "Percentage old"
  rec_long$`Percentage old` = rec_long_old$`Percentage old`
  
  rec_long_color = reshape2::melt(
    rec_color[,c("Method", "truepos", "trueneg", truepos_cols, trueneg_cols)],
    id.vars=c("Method", "truepos", "trueneg"), value.name="Percentage color")
  # Fill in as "Percentage color"
  rec_long$`Percentage color` = rec_long_color$`Percentage color`
  
  # We need to subdivide into ground truth and test outcome, thus creating
  #  two separate columns

  #  Label true associations of rows
  rec_long[grep("pos", rec_long$Outcome), "Ground truth"] = "True-positive"
  rec_long[grep("neg", rec_long$Outcome), "Ground truth"] = "True-negative"
  # Total is the number of true positives or negatives (number drawn in plot)
  rec_long$total = ifelse(rec_long["Ground truth"]=="True-positive",
                          rec_long$truepos, rec_long$trueneg)

  #  Only label as positive or negative
  rec_long$Outcome = as.character(rec_long$Outcome)
  rec_long[rec_long=="p_truepos" | rec_long=="p_trueneg"] = "Positive"
  rec_long[rec_long=="n_truepos" | rec_long=="n_trueneg"] = "Negative"
  rec_long$`Test outcome` = factor(
    rec_long$Outcome, levels = c("Positive", "Negative"))

  # Prettify
  rec_long$Test = as.character(rec_long$Method)
  for (i in 1:length(cols)) {
    rec_long[rec_long==cols[i]] = cols_pretty[i]
  }
  # For correct order
  rec_long$Test = factor(rec_long$Test, levels=cols_pretty)
  
  # To draw the cutoffs, we need the ground truth as numeric
  rec_long[rec_long$`Ground truth`=="True-negative","Ground truth i"] = 1
  rec_long[rec_long$`Ground truth`=="True-positive","Ground truth i"] = 2
  
  ggplot(rec_long, aes(fill=`Test outcome`, color=`Test outcome`,
                       x=`Ground truth i`, y=`Percentage color`)) + 
    # Facet by test (e.g. NT, cPass)
    facet_wrap(~Test, ncol=ncol_output) +
    # Create a bar of the percentages, colored by outcome,
    #  x divided by ground truth
    geom_bar(stat="identity", alpha=0.5) +
    # Draw total of true positives or negatives above
    geom_text(data=rec_long[!duplicated(rec_long[,c("Test","Ground truth")]),],
              aes(y=1.05, label=total), color="black") +
    # Horizontal lines indicating the fraction
    #  Old cutoff
    geom_segment(data=rec_long[rec_long$`Test outcome`=="Negative",],
                 aes(y=`Percentage old`, yend=`Percentage old`,
                     x=as.numeric(`Ground truth i`)-0.45,
                     xend=as.numeric(`Ground truth i`)+0.45,
                     linetype=style$thr_lb_old),
                 color=style$thr_lc_old) +
    #  New cutoff
    geom_segment(data=rec_long[rec_long$`Test outcome`=="Negative",],
                 aes(y=`Percentage new`, yend=`Percentage new`,
                     x=as.numeric(`Ground truth i`)-0.45,
                     xend=as.numeric(`Ground truth i`)+0.45,
                     linetype=style$thr_lb_new),
                 color=style$thr_lc_new) +
    #  Use linetype aesthetic to create a separate legend
    scale_linetype_manual(
      name="Cut-off", values=c(style$thr_lt_old,style$thr_lt_new)) +
    # Axes labels
    scale_y_continuous(labels=scales::percent) +
    scale_x_continuous(breaks=c(1,2),
                       labels=c("True\nnegative", "True\npositive")) +
    # Colors
    scale_fill_manual(values=c(style$col_truepos, style$col_trueneg)) +
    scale_color_manual(values=c(style$col_truepos, style$col_trueneg)) +
    labs(x="", y="Percentage")
}

#' Create bar plot of tests vs ground truth
#' Note: This function is not used and potentially not up to date.
gt_test_bars = function(data, cols, cols_pretty, ncol_output, cutoffs, title) {
  data = to_numeric(extract_last_values(data))
  
  # Get recovered
  rec = get_recovery_rates(data, cols, cutoffs)

  # Normalize by true numbers
  p_cols = c("p_truepos", "p_trueneg")
  n_cols = c("n_truepos", "n_trueneg")
  rec[,p_cols] = rec[,p_cols] / rec[,"p"]
  rec[,n_cols] = rec[,n_cols] / rec[,"n"]
  
  # To long
  rec_long = reshape2::melt(rec[,c("Method", "p", "n", p_cols, n_cols)],
                            id.vars=c("Method", "p", "n"),
                            value.name="Percentage")
  colnames(rec_long)[colnames(rec_long) == 'variable'] = "Ground truth"
  
  # Label true associations of rows
  rec_long[grep("p_", rec_long$`Ground truth`), "Test outcome"] = "Positive"
  rec_long[grep("n_", rec_long$`Ground truth`), "Test outcome"] = "Negative"
  rec_long$total =
    ifelse(rec_long["Test outcome"]=="Positive", rec_long$p, rec_long$n)
  
  # Only label as true positive or negative
  rec_long$`Ground truth` = as.character(rec_long$`Ground truth`)
  rec_long[rec_long=="p_truepos" | rec_long=="n_truepos"] = "True-positive"
  rec_long[rec_long=="p_trueneg" | rec_long=="n_trueneg"] = "True-negative"
  rec_long$`Ground truth` = factor(
    rec_long$`Ground truth`,levels = c("True-positive", "True-negative"))
  
  # Prettify
  rec_long$Test = as.character(rec_long$Method)
  for (i in 1:length(cols)) {
    rec_long[rec_long==cols[i]] = cols_pretty[i]
  }
  # For correct order
  rec_long$Test = factor(rec_long$Test, levels=cols_pretty)
  
  ggplot(rec_long, aes(fill=`Ground truth`, color=`Ground truth`,
                       x=`Test outcome`, y=Percentage)) + 
    facet_wrap(~Test, ncol=ncol_output) +
    geom_bar(stat="identity", alpha=0.5) +
    theme(axis.text.x = element_text(angle=0, vjust=0.5)) +
    geom_text(data=rec_long[!duplicated(rec_long[,c("Test","Test outcome")]),],
              aes(y=1.05, label=total), color="black") +
    scale_y_continuous(labels=scales::percent) +
    scale_fill_manual(values=c(style$col_truepos, style$col_trueneg)) +
    scale_color_manual(values=c(style$col_truepos, style$col_trueneg)) +
    labs(x="")
}

#' Create a plot with the `title` string above the `plt` plot row.
#' 
#' @param title The title string
#' @param plt The actual plot
#' @param plot.margin Margins of the title
#' @param rel_heights Relative heights of title and plot
title_plot = function(title, plt, plot.margin=margin(0,0,0,7),
                      rel_heights=c(0.1,1), fontface='plain') {
  plot_grid(
    ggdraw() + draw_label(title, fontface=fontface, x=0, hjust=0) +
      theme(plot.margin = plot.margin),
    plt, ncol=1, rel_heights=rel_heights
  )
}

#' Plot the districution of the main tests as a stacked histogram of true
#' positives, negatives and unknowns, with cumulatives and annotations.
#' 
#' @param data The data
#' @param col The column of interest
#' @param rec_old,rec_new Recovered old and new
#' @param col_pretty The pretty column name for display
#' @param x_old_pos,x_old_neg,x_new_pos,x_new_neg Offsets for old/new
#'        positive/negative labels
#' @param y_txt Height of the new annotations (old is *0.9)
#' @param y_pct Height of the cumulative distribution axis in data units
#' @param sec_axis_name Name of the second axis (Cumulative distribution)
stacked_distr = function(data, col, rec_old, rec_new, col_pretty,
                         x_old_pos, x_old_neg, x_new_pos, x_new_neg,
                         y_txt, y_pct, sec_axis_name="") {
  ggplot(data, aes_string(x=col)) + 
    geom_histogram(aes(fill=`Ground truth`, color=`Ground truth`),
                       alpha=0.5, position="stack") +
    scale_x_log10() + 
    # Thresholds
    geom_vline(aes(xintercept=cutoff$old[[col]], linetype=style$thr_lb_old),
               color=style$thr_lc_old) + 
    geom_vline(aes(xintercept=cutoff$new[[col]], linetype=style$thr_lb_new),
               color=style$thr_lc_new) +
    #  Use linetype aesthetic to create a separate legend
    scale_linetype_manual(
      name="Cut-off", values=c(style$thr_lt_old,style$thr_lt_new)) +
    # Annotate percentages
    annotate("text", size=3, color=style$col_truepos, hjust="center",
             x=cutoff$old[[col]]*x_old_pos, y=y_txt*0.9,
             label=sprintf("(%.0f%%)", rec_old[col, "frac_pos_rec"] * 100)) +
    annotate("text", size=3, color=style$col_trueneg, hjust="center",
             x=cutoff$old[[col]]*x_old_neg, y=y_txt*0.9,
             label=sprintf("(%.0f%%)", rec_old[col, "frac_neg_rec"] * 100)) +
    annotate("text", size=3, color=style$col_truepos, hjust="center",
             x=cutoff$new[[col]]*x_new_pos, y=y_txt,
             label=sprintf("%.0f%%", rec_new[col, "frac_pos_rec"] * 100)) +
    annotate("text", size=3, color=style$col_trueneg, hjust="center",
             x=cutoff$new[[col]]*x_new_neg, y=y_txt,
             label=sprintf("%.0f%%", rec_new[col, "frac_neg_rec"] * 100)) +
    # Total number
    labs(x=paste0(col_pretty, " (n=", sum(!is.na(data[col])), ")"), y="Count") +
    scale_fill_manual(values=style$pal3) + scale_color_manual(values=style$pal3) +
    stat_ecdf(data=data[data$`Ground truth`=="True-positive",],
              aes(y=..y..*y_pct, color=`Ground truth`),
              show.legend = F) +
    stat_ecdf(data=data[data$`Ground truth`=="True-negative",],
              aes(y=y_pct-..y..*y_pct, color=`Ground truth`),
              show.legend = F) +
    scale_y_continuous(sec.axis=sec_axis(~./y_pct, name=sec_axis_name))
}

#' Parallel coordinate (informally spaghetti) plots.
#' 
#' @param data The data frame
#' @param cols Columns of interest
#' @param cols_pretty Output columns labels
#' @param discrete_value How the discrete value is called
#' @param discrete_replacement What value to plot for the discrete values
#' @param cutoffs Environment with old and new cutoffs
#' @param y_breaks,y_labels Breaks and labels for the y axis
spaghetti_with_discrete = function(
    data, cols, cols_pretty, discrete_value, discrete_replacement,
    cutoffs, y_breaks, y_labels) {
  # To long and numeric
  data_long = data %>% tidyr::gather(Array, Concentration, cols)
  data_long[data_long$Concentration==discrete_value &
              !is.na(data_long$Concentration), "Concentration"] =
    discrete_replacement
  data_long$Concentration = as.numeric(data_long$Concentration)
  
  # For plotting, we need the array IDs as numbers
  for (i in 1:length(cols)) {
    data_long[data_long$Array==cols[i],"Array_i"] = as.character(i)
  }
  # Cutoffs
  for (col in cols) {
    data_long[data_long$Array==col,"Old_cutoff"] = cutoffs$old[col]
    data_long[data_long$Array==col,"New_cutoff"] = cutoffs$new[col]
  }
  
  ggplot(data=data_long,
         aes(y=Concentration, x=Array_i, group=tln_ID,
             color=`Ground truth`)) +
    # Draw points and connecting lines for a given participant
    geom_point(alpha = 0.5) + geom_line(alpha = 0.1) +
    # Horizontal lines for decision thresholds
    geom_segment(data=data_long[!duplicated(data_long["Array_i"]),],
                 color=style$thr_lc_old,
                 aes(x=as.numeric(Array_i)-0.5, y=as.numeric(Old_cutoff),
                     xend=as.numeric(Array_i)+0.5, yend=as.numeric(Old_cutoff),
                     linetype=style$thr_lb_old)) +
    geom_segment(data=data_long[!duplicated(data_long["Array_i"]),],
                 color=style$thr_lc_new,
                 aes(x=as.numeric(Array_i)-0.5, y=as.numeric(New_cutoff),
                     xend=as.numeric(Array_i)+0.5, yend=as.numeric(New_cutoff),
                     linetype=style$thr_lb_new)) +
    scale_linetype_manual(
      name="Cut-off", values=c(style$thr_lt_old,style$thr_lt_new)) +
    # Decoration
    scale_x_discrete(labels=cols_pretty) +
    scale_y_log10(breaks=y_breaks, labels=y_labels) +
    scale_fill_manual(values=style$pal3) +
    scale_color_manual(values=style$pal3) +
    labs(x="", y="Measurement value")
}

#' Create the long data formats needeed to the jitter-violin-bar plots
#' 
#' @param data The single_data data frame
#' @param cols Columns of interest
#' @param cols_pretty Corresponding nice labels
#' @param discrete_val Value for the discrete part of the data
#' @param cutoffs Environment with `old` and `new` cutoffs
data_long_for_viola = function(data, cols, cols_pretty,
                               discrete_val, cutoffs) {
  # To long format
  data_long = data %>% tidyr::gather(Array, Concentration, cols)
  # Count number of data points for each method
  for (col in cols) {
    data_long[data_long$Array==col,"Count"] =
      paste0("n=", sum(!is.na(data[,col])))
  }
  # For plotting, we need the array IDs as numbers
  for (i in 1:length(cols)) {
    data_long[data_long$Array==cols[i],"Array_i"] = as.character(i)
  }
  data_long$Array_i = factor(data_long$Array_i,
                             levels = as.character(1:length(cols)))
  # Cutoffs
  for (col in cols) {
    data_long[data_long$Array==col,"New_cutoff"] = cutoffs$new[col]
    data_long[data_long$Array==col,"Old_cutoff"] = cutoffs$old[col]
  }
  # Remember discrete part
  data_long_inv = data_long[data_long$Concentration==discrete_val &
                            !is.na(data_long$Concentration),]

  # Remove discrete part
  data_long = data_long[data_long$Concentration!=discrete_val &
                        !is.na(data_long$Concentration),]
  # To numeric
  data_long[,"Concentration"] = as.numeric(data_long$Concentration)

  # Percent above and below threshold
  data = to_numeric(data)
  rec_old = get_recovery_rates(
    data, cols, cutoffs=cutoffs$old, cutoff_label="Old")
  rec_new = get_recovery_rates(
    data, cols, cutoffs=cutoffs$new, cutoff_label="New")
  rec_old_unknown = get_unknown_recovery_rates(
    data, cols, cutoffs=cutoffs$old, cutoff_label="Old")
  rec_new_unknown = get_unknown_recovery_rates(
    data, cols, cutoffs=cutoffs$new, cutoff_label="New")

  # Comput fraction of recovered pos/neg/unknown
  for (col in cols) {
    data_long[data_long$Array==col,"Fraction true positive old"] =
      sprintf("%.0f%%", rec_old[rec_old$Method==col, "p_truepos"] /
              rec_old[rec_old$Method==col, "truepos"] * 100)
    data_long[data_long$Array==col,"Fraction true negative old"] =
      sprintf("%.0f%%", rec_old[rec_old$Method==col, "n_trueneg"] /
              rec_old[rec_old$Method==col, "trueneg"] * 100)
    data_long[data_long$Array==col,"Fraction unknown positive old"] =
      sprintf("%.0f%%", rec_old_unknown[rec_old_unknown$Method==col,
                                        "p_unknown"] /
              rec_old_unknown[rec_old_unknown$Method==col, "unknown"] * 100)

    data_long[data_long$Array==col,"Fraction true positive new"] =
      sprintf("%.0f%%", rec_new[rec_new$Method==col, "p_truepos"] /
              rec_new[rec_new$Method==col, "truepos"] * 100)
    data_long[data_long$Array==col,"Fraction true negative new"] =
      sprintf("%.0f%%", rec_new[rec_new$Method==col, "n_trueneg"] /
              rec_new[rec_new$Method==col, "trueneg"] * 100)
    data_long[data_long$Array==col,"Fraction unknown positive new"] =
      sprintf("%.0f%%", rec_new_unknown[rec_new_unknown$Method==col,
                                        "p_unknown"] /
              rec_new_unknown[rec_new_unknown$Method==col, "unknown"] * 100)
  }

  # Factorize for correct order
  data_long$`Ground truth` = factor(
    data_long$`Ground truth`,
    levels=c("Unknown", "True-negative", "True-positive"))

  list(data_long, data_long_inv)
}

#' Plot with jitter points on the left and half violins on the right, and
#' various annotations.
#' 
#' @param data_long Long form as created by data_long_for_viola
#' @param cols_pretty Nice column names
#' @param ylimits,ybreaks,ylabels y axis limits, breaks, and labels
#' @param ymeas y location for n numbers
#' @param thr_shift How much to shift the percentages in data y units
#' @param violin_width Width of the violins, in (0,1]
#' @param show_new_line,show_old_line Whether to show the new and old cutoffs
#' @param show_new_percent,show_old_percent Whether to shoe the new and old
#'        percentage numbers
#' @param unknown_text_offset Vertical offset of the "unknown" percentages
jitter_violin_plot = function(data_long, cols_pretty,
                              ylimits, ybreaks, ylabels,
                              ymeas, thr_shift=0.1, violin_width=0.7,
                              show_new_line=T, show_new_percent=T,
                              show_old_line=T, show_old_percent=T,
                              unknown_text_offset=2.5) {
  # Size of in-figure texts
  text_size=3
  # Base plot
  plt = ggplot(data_long, aes(x=Array_i, y=Concentration)) +
    # Jittered points to the left
    #  We plot these first as violins are not plotted with <=2 points,
    #  messing up color assignment
    geom_point(aes(x=as.numeric(Array_i)-0.22, y=Concentration,
                   col=`Ground truth`, fill=`Ground truth`), size=1,
               position=position_jitter(width=.15), alpha=0.5, show.legend=F) +
    # 3 Violins to the right for the ground truth values, and unknowns
    geom_flat_violin(aes(fill=`Ground truth`, col=`Ground truth`),
                     position = position_nudge(x = .0, y = 0),
                     alpha=0.2, trim=F, scale="area", width=violin_width) +
    # Labels, scales, colors
    scale_y_log10(limits=ylimits, breaks=ybreaks, labels=ylabels) +
    scale_x_discrete(labels=paste(cols_pretty)) +
    scale_fill_manual(values=style$pal3) + scale_color_manual(values=style$pal3) + 
    # Add measurement count
    geom_text(data=data_long[!duplicated(data_long["Array_i"]),], size=3,
              aes(x=Array_i, y=ymeas, label=Count)) + 
    labs(x="", y="Measurement value")
  
  linetype_manuals = c()

  # New annotations
  if (show_new_line) {
    #  Horizontal line
    plt = plt +
      geom_segment(data=data_long[!duplicated(data_long["Array_i"]),],
                   color=style$thr_lc_new,
                   aes(x=as.numeric(Array_i)-0.5, y=as.numeric(New_cutoff),
                       xend=as.numeric(Array_i)+0.5,
                       yend=as.numeric(New_cutoff), linetype=style$thr_lb_new))
    linetype_manuals = c(linetype_manuals, style$thr_lt_new)
  }
  if (show_new_percent) {
    #  Percentages
    plt = plt +
      geom_text(
        data=data_long[!duplicated(data_long["Array_i"]),],
        size=text_size, color=style$col_grey, hjust="right",
        aes(x=as.numeric(Array_i) + 0.5,
            y=as.numeric(New_cutoff)*10**(thr_shift*unknown_text_offset),
            label=`Fraction unknown positive new`)) +
      geom_text(
        data=data_long[!duplicated(data_long["Array_i"]),],
        size=text_size, color=style$col_truepos, hjust="right",
        aes(x=as.numeric(Array_i) + 0.5,
            y=as.numeric(New_cutoff)*10**thr_shift,
            label=`Fraction true positive new`)) + 
      geom_text(
        data=data_long[!duplicated(data_long["Array_i"]),],
        size=text_size, color=style$col_trueneg, hjust="right",
        aes(x=as.numeric(Array_i) + 0.5,
            y=as.numeric(New_cutoff)*10**-thr_shift,
            label=`Fraction true negative new`))
  }

  # Old annotations
  if (show_old_line) {
    # Horizontal line
    plt = plt +
      geom_segment(data=data_long[!duplicated(data_long["Array_i"]),],
                   color=style$thr_lc_old,
                   aes(x=as.numeric(Array_i)-0.5, y=as.numeric(Old_cutoff),
                       xend=as.numeric(Array_i)+0.5,
                       yend=as.numeric(Old_cutoff), linetype=style$thr_lb_old))
    linetype_manuals = c(style$thr_lt_old, linetype_manuals)
  }
  if (show_old_percent) {
    #  Percentages
    plt = plt + 
      geom_text(
        data=data_long[!duplicated(data_long["Array_i"]),],
        size=text_size, color=style$col_grey, hjust="right",
        aes(x=as.numeric(Array_i) + 0.5,
            y=as.numeric(Old_cutoff)*10**(thr_shift*unknown_text_offset),
            label=`Fraction unknown positive old`)) +
      geom_text(
        data=data_long[!duplicated(data_long["Array_i"]),],
        size=text_size, color=style$col_truepos, hjust="right",
        aes(x=as.numeric(Array_i) + 0.5,
            y=as.numeric(Old_cutoff)*10**thr_shift,
            label=`Fraction true positive old`)) + 
      geom_text(
        data=data_long[!duplicated(data_long["Array_i"]),],
        size=text_size, color=style$col_trueneg, hjust="right",
        aes(x=as.numeric(Array_i) + 0.5,
            y=as.numeric(Old_cutoff)*10**-thr_shift,
            label=`Fraction true negative old`))
  }

  # Linetype annotation
  plt = plt + scale_linetype_manual(name="Cut-off", values=linetype_manuals)

  plt
}

#' Bar plot of single-value discrete measurements, to be included in the
#' jitter-violin plot.
#' 
#' @param data_long_inv The discrete part, as return by data_long_for_viola
#' @param ylimits y axis limits
discrete_bar_plot = function(data_long_inv, ylimits) {
  ggplot(data_long_inv,
         aes(x=Array_i, fill=`Ground truth`, color=`Ground truth`)) +
    geom_bar(stat="count", position=position_dodge(), alpha=0.5) +
    scale_fill_manual(values=style$pal3) + 
    scale_color_manual(values=style$pal3) +
    # Adjust limits to accommodate the labels
    scale_y_continuous(limits=ylimits) +
    # Add measurement counts for each bar
    geom_text(stat='count', position=position_dodge(0.9), vjust=-0.4,
              aes(label=..count..), size=3, show.legend = F) +
    # Remove most decorations, as this will get inserted into the other plot
    theme_classic() + theme(
      axis.text = element_blank(), axis.line = element_blank(),
      axis.ticks = element_blank(), axis.title = element_blank())
}

#' Function to load all required packages and install them if necessary.
#' 
#' @param Required_Packages List of required packages
Install_And_Load <- function(Required_Packages)
{
  Remaining_Packages <-
    Required_Packages[!(Required_Packages %in% installed.packages()[, "Package"])]

  if (length(Remaining_Packages)) {
    install.packages(Remaining_Packages)
  }
  for (package_name in Required_Packages) {
    library(package_name,
            character.only = TRUE,
            quietly = TRUE)
  }
}

#' Create a contingency matrix for all requested combinations of columns.
#' If classifier is set, only columns containing a classifier string are
#' compared with each other.
#' TODO This function is kind of replicating the qualitative_2x2_table.
#' 
#' @param data The categorical data set with "Positive", "Negative" values
#' @param classifiers Columns are conditioned on these substrings
#'        (default none)
#' @param prnt Whether to print results on screen
create_contingency_matrix = function(data, classifiers=c(""), prnt=F) {
  # All column names
  cols = colnames(data)
  # Contingency matrix
  contis = data.frame(Methods=character(), pp=double(), pn=double(), 
                      np=double(), nn=double(), total=integer())
  # Iterate over classifiers
  for (cls in classifiers) {
    # Iterate over all columns for classifier
    for (col1 in grep(cls, cols, value=T)) {
      for (col2 in grep(cls, cols, value=T)) {
        if (which(cols == col2) > which(cols == col1)) {
          t = table(data[,col1], data[,col2])
          # Add missing columns and rows
          if (!"Positive" %in% colnames(t)) {t = cbind(t, "Positive"=0)}
          if (!"Positive" %in% rownames(t)) {t = rbind(t, "Positive"=0)}
          if (!"Negative" %in% colnames(t)) {t = cbind(t, "Negative"=0)}
          if (!"Negative" %in% rownames(t)) {t = rbind(t, "Negative"=0)}
          # To proportions
          p = prop.table(t)
          contis = contis %>% tibble::add_row(
            Methods=paste(col1, col2, sep=" & "),
            pp=p['Positive','Positive'], pn=p['Positive','Negative'],
            np=p['Negative','Positive'], nn=p['Negative','Negative'],
            total=sum(t))
          if (prnt) {
            # Print the matrices
            cat(col1, col2)
            print(addmargins(t))
            print(addmargins(p))
          }
        }
      }
    }
  }
  # tidy up labels
  contis = dplyr::rename(
    contis, 'positive-positive'='pp', 'positive-negative'='pn',
    'negative-positive'='np', 'negative-negative'='nn')
  contis
}

#' Get recovery rates of positive and negative true values by thresholds,
#' one for each column. Column and cutoff names must match, as always.
#' 
#' @param data The value data frame
#' @param cols The columns of interest
#' @param cutoffs The cutoffs to apply for qualitative assessment, old or new
#' @param cutoff_label Just a label for the cutoff type applied
#' 
#' @return Data frame with a row per col in cols, with:
#'  * p, n indicating positive and negative
#'    values by the test cutoff when a ground truth is available;
#'  * truepos, trueneg for true positives and negatives when a test
#'    was performed;
#'  * p_truepos, n_truepos, p_trueneg, n_trueneg for the intersection numbers
#'    of the above; 
#'  * frac_pos_rec and frac_neg_rec for recovered true positives and negatives,
#'    i.e. p_truepos/truepos and n_trueneg/true_neg.
get_recovery_rates = function (data, cols, cutoffs, cutoff_label="") {
  result = data.frame(Method=character(), Cutoff=character(),
                      p=integer(), n=integer(),
                      truepos=integer(), trueneg=integer(),
                      p_truepos=integer(), n_truepos=integer(),
                      p_trueneg=integer(), n_trueneg=integer(),
                      frac_pos_rec=double(), frac_neg_rec=double())
  for (col in cols) {
    p = sum(data[col]>=cutoffs[col] &
            data[,"Ground truth"]!="Unknown", na.rm=T)
    n = sum(data[col]<cutoffs[col] &
            data[,"Ground truth"]!="Unknown", na.rm=T)

    truepos = sum(!is.na(data[col]) &
                  data["Ground truth"]=="True-positive", na.rm=T)
    trueneg = sum(!is.na(data[col]) &
                  data["Ground truth"]=="True-negative", na.rm=T)

    p_truepos = sum(data[col]>=cutoffs[col] &
                    data["Ground truth"]=="True-positive", na.rm=T)
    n_truepos = sum(data[col]<cutoffs[col] &
                    data["Ground truth"]=="True-positive", na.rm=T)

    p_trueneg = sum(data[col]>=cutoffs[col] &
                    data["Ground truth"]=="True-negative", na.rm=T)
    n_trueneg = sum(data[col]<cutoffs[col] &
                    data["Ground truth"]=="True-negative", na.rm=T)

    result = result %>% tibble::add_row(
      Method=col, Cutoff=cutoff_label, p=p, n=n,
      truepos=truepos, trueneg=trueneg,
      p_truepos=p_truepos, n_truepos=n_truepos,
      p_trueneg=p_trueneg, n_trueneg=n_trueneg,
      frac_pos_rec=p_truepos/truepos, frac_neg_rec=n_trueneg/trueneg)
  }
  result
}

#' Get the numbers of positive and negative tests out of unknown groun truth.
#' Parameters and output like for get_recovery_rates.
get_unknown_recovery_rates = function (data, cols, cutoffs, cutoff_label="") {
  result = data.frame(Method=character(), Cutoff=character(),
                      unknown=integer(),
                      p_unknown=integer(), n_unknown=integer())
  for (col in cols) {
    unknown = sum(!is.na(data[col]) & data["Ground truth"]=="Unknown", na.rm=T)

    p_unknown = sum(data[col]>=cutoffs[col] &
                    data["Ground truth"]=="Unknown", na.rm=T)
    n_unknown = sum(data[col]<cutoffs[col] &
                    data["Ground truth"]=="Unknown", na.rm=T)

    result = result %>% tibble::add_row(
      Method=col, Cutoff=cutoff_label, unknown=unknown,
      p_unknown=p_unknown, n_unknown=n_unknown)
  }
  result
}
