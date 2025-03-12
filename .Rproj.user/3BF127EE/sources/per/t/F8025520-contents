# Warning takes 30 minutes to complete
library(flextable)
library(officer)
library(tibble)
library(stringr)
source("preprocessing_functions.R")
library(mice)
library(pROC)
library(psfmi)


###########################################################################
# FUNCTIONS FOR CREATING PERFORMANCE TABLE
create_roc_list <- function(y, preds) {
  # Create list of roc objects
  roc <- lapply(seq_along(y), function(i) roc(as.numeric(y[[i]]), 
                                        as.numeric(preds[[i]]),
                                        direction="<",
                                        levels=c(0,1)))
  return(roc)
}

calculate_auc_df <- function(roc_list) {
  # Creates a dataframe with nimp rows of auc mean and standard errors  
  auc_df <- bind_rows(lapply(seq_along(roc_list), function(i) {
    auc_df <- data.frame(
      auc_mean = ci(roc_list[[i]])[2],
      auc_se = (ci(roc_list[[i]])[3]-ci(roc_list[[i]])[1])/(2*1.96)
    )
    return(auc_df)
  }))
  return(auc_df)
}

calculate_sens_spec_df <- function(roc_list) {
  # Creates a dataframe with nimp rows of sensitivity and specificity
  #   means and standard errors
  sens_spec_df <- bind_rows(lapply(seq_along(roc_list), function(i) {
    boot.n <- 1000
    coord <- coords(roc_list[[i]], "best", 
                    ret=c("sensitivity", "specificity"), 
                    best.method="closest.topleft")
    sens <- ci.se(roc_list[[i]], specificities=coord$specificity,
                  boot.n=boot.n, progress="none")
    spec <- ci.sp(roc_list[[i]], sensitivities=coord$sensitivity,
                  boot.n=boot.n, progress="none")
    sens_spec_df <- data.frame(
      sens_mean = sens[2],
      sens_se = (sens[3]-sens[1])/(2*1.96),
      spec_mean = spec[2],
      spec_se = (spec[3]-spec[1])/(2*1.96)
    )
    return(sens_spec_df)
  }))
}

###########################################################################
# GENERATE TABLE
generate_performances_table <- function(dataset="train", 
                                        operating_point="min") {
  # Define outcomes to be generating across
  outcomes <- c("hosp_1_year", 
                "hosp_3_year",
                "hosp_5_year",
                "mort_1_year",
                "mort_3_year",
                "mort_5_year")
  nimp <- 100 #Predefined number of imputations

  # Define empty lists for AUC, Sensitivity, and Specificity for male and female
  auc_pooled.female <- as.numeric(list())
  sens_pooled.female <- as.numeric(list())
  spec_pooled.female <- as.numeric(list())
  
  auc_pooled.male <- as.numeric(list())
  sens_pooled.male <- as.numeric(list())
  spec_pooled.male <- as.numeric(list())
  
  # For p.values
  median_p <- as.numeric(list())
  
  # load coefficients
  if (operating_point=="min") {
    load("results/coefs_min.Rda")
  } else if (operating_point=="1se") {
    load("results/coefs_1se.Rda")
  }
  
  # Loop through every outcome
  for (outcome in outcomes) {
    # Print the time to keep track of how long the file is going
    print(outcome)
    start.time <- Sys.time()
    print(start.time)
    # load imputed database
    if (dataset == "train") {
      load(paste("data", outcome, "train_imp_100.Rda", sep="/"))
      dfs <- lapply(1:nimp, function(i) {
        prepare_categ_vars(complete(train_imp, action=i), outcome)
      })  
    } else if (dataset == "test") {
      load(paste("data", outcome, "test_imp_100.Rda", sep="/"))
      dfs <- lapply(1:nimp, function(i) {
        prepare_categ_vars(complete(test_imp, action=i), outcome)
      })
    }
    preds <- rownames(coef.df)[-1]

    # Female lists
    x.female <- list()
    y.female <- list()
    preds.female <- list()
    coef.female <- coef.df %>%
      select(paste(outcome, "female", sep=".")) %>%
      as.matrix()
    roc.female <- list()
    
    # Male lists
    x.male <- list()
    y.male <- list()
    preds.male <- list()
    coef.male <- coef.df %>% 
      select(paste(outcome, "male", sep=".")) %>%
      as.matrix()
    roc.male <- list()
    
    # For each imputation...
    for (i in 1:nimp) {
      # ...generate design matrices
      x.female[[i]] <-
        as.matrix(dfs[[i]][dfs[[i]][,"sex"]==0, preds])
      x.male[[i]] <-
        as.matrix(dfs[[i]][dfs[[i]][,"sex"]==1, preds])
      y.female[[i]] <- dfs[[i]][dfs[[i]][,"sex"]==0, "outcome"]
      y.male[[i]] <- dfs[[i]][dfs[[i]][,"sex"]==1, "outcome"]
      
      # ...calculate predictions
      preds.female[[i]] <- cbind(1, x.female[[i]]) %*% coef.female
      preds.male[[i]] <- cbind(1, x.male[[i]]) %*% coef.male
    }
    
    # Create ROC objects
    roc_list.female <- create_roc_list(y.female, preds.female)
    roc_list.male <- create_roc_list(y.male, preds.male)
    
    # Compare ROC curves (print p values)
    tests <- lapply(seq_along(roc_list.female), function(i) {
      roc.test(roc_list.female[[i]], roc_list.male[[i]])$p.value})
    median_p <- rbind(median_p, median(as.numeric(tests)))
    print(median_p)
    
    # Calculate AUC, Sens, and Spec mean and standard errors for each imp
    auc_df.female <- calculate_auc_df(roc_list.female)
    auc_df.male <- calculate_auc_df(roc_list.male)
    sens_spec_df.female <- calculate_sens_spec_df(roc_list.female)
    sens_spec_df.male <- calculate_sens_spec_df(roc_list.male)
    
    
    # Calculate AUC, Sens, Spec and pool using Rubin's rules
    ## Female
    auc_pooled.female <- rbind(auc_pooled.female,
                               pool_auc(auc_df.female$auc_mean, 
                                        auc_df.female$auc_se, 
                                        nimp=nimp))
    sens_pooled.female <- rbind(sens_pooled.female,
                                pool_auc(sens_spec_df.female$sens_mean,
                                         sens_spec_df.female$sens_se,
                                         nimp=nimp))
    spec_pooled.female <- rbind(spec_pooled.female,
                                pool_auc(sens_spec_df.female$spec_mean,
                                         sens_spec_df.female$spec_se,
                                         nimp=nimp))
    ## Male
    auc_pooled.male <- rbind(auc_pooled.male, 
                             pool_auc(auc_df.male$auc_mean, 
                                      auc_df.male$auc_se, 
                                      nimp=nimp))
    sens_pooled.male <- rbind(sens_pooled.male,
                              pool_auc(sens_spec_df.male$sens_mean,
                                       sens_spec_df.male$sens_se,
                                       nimp=nimp))
    spec_pooled.male <- rbind(spec_pooled.male,
                              pool_auc(sens_spec_df.male$spec_mean,
                                       sens_spec_df.male$spec_se,
                                       nimp=nimp))

    end.time <- Sys.time()
    print(end.time-start.time)
  }

  # Rename and combine the dataframes
  colnames <-c('low', 'mean', 'up')
  rownames <- c(
    "Hosp. 1 year",
    "Hosp. 3 years",
    "Hosp. 5 years",
    "Mort. 1 year",
    "Mort. 3 years",
    "Mort. 5 years"
  )
  auc_pooled.female <- as.data.frame(auc_pooled.female)
  rownames(auc_pooled.female) <- rownames
  colnames(auc_pooled.female) <- paste('auc', 'female', colnames, sep=".")
  
  sens_pooled.female <- as.data.frame(sens_pooled.female)
  rownames(sens_pooled.female) <- rownames
  colnames(sens_pooled.female) <- paste('sens','female',colnames, sep=".")
  
  spec_pooled.female <- as.data.frame(spec_pooled.female)
  rownames(spec_pooled.female) <- rownames
  colnames(spec_pooled.female) <- paste('spec', 'female', colnames, sep=".")
  
  auc_pooled.male <- as.data.frame(auc_pooled.male)
  rownames(auc_pooled.male) <- rownames
  colnames(auc_pooled.male) <- paste('auc','male',colnames, sep=".")
  
  sens_pooled.male <- as.data.frame(sens_pooled.male)
  rownames(sens_pooled.male) <- rownames
  colnames(sens_pooled.male) <- paste('sens','male',colnames, sep=".")
  
  spec_pooled.male <- as.data.frame(spec_pooled.male)
  rownames(spec_pooled.male) <- rownames
  colnames(spec_pooled.male) <- paste('spec','male',colnames, sep=".")
  
  median_p <- as.data.frame(median_p)
  rownames(median_p) <- rownames
  colnames(median_p) <- c("p-value")
  
  performances.all <- cbind(auc_pooled.female,
                            sens_pooled.female, 
                            spec_pooled.female,
                            auc_pooled.male, 
                            sens_pooled.male, 
                            spec_pooled.male,
                            median_p)
  
  # Save raw data table
  save(performances.all,
       file=paste("results/", operating_point,
                  "_", dataset, "_performances_all.Rda", sep=""))
  
  ## FORMAT FOR WORD DOC
  set_flextable_defaults(digits=2)
  ft <- flextable(performances.all %>% rownames_to_column("Outcome"),
                  col_keys = c("Outcome",
                               "auc.female", 
                               "sens.female",
                               "spec.female",
                               "auc.male",
                               "sens.male",
                               "spec.male",
                               "p-value")) %>%
    colformat_num(decimal.mark = ".", na_str="NaN") %>%
    # Format each column by combining the mean and lower+upper CIs
    mk_par(j="auc.female", 
           value = as_paragraph(auc.female.mean,
                                " (", auc.female.low,
                                ", ", auc.female.up,
                                ")")) %>%
    mk_par(j="sens.female",
           value = as_paragraph(sens.female.mean,
                                " (", sens.female.low,
                                ", ", sens.female.up,
                                ")")) %>%
    mk_par(j="spec.female",
           value = as_paragraph(spec.female.mean,
                                " (", spec.female.low,
                                ", ", spec.female.up,
                                ")")) %>%
    mk_par(j="auc.male",
           value = as_paragraph(auc.male.mean,
                                " (", auc.male.low,
                                ", ", auc.male.up,
                                ")")) %>%
    mk_par(j="sens.male",
           value = as_paragraph(sens.male.mean,
                                " (", sens.male.low,
                                ", ", sens.male.up,
                                ")")) %>%
    mk_par(j="spec.male",
           value = as_paragraph(spec.male.mean,
                                " (", spec.male.low,
                                ", ", spec.male.up,
                                ")")) %>%
    add_header_row(values=c("", "AUC", "Sens", "Spec", "AUC", "Sens", "Spec", "p-value"),
                   colwidths=c(1,1,1,1,1,1,1,1)) %>%
    add_header_row(values=c("", "Female", "Male", ""),
                   colwidths = c(1,3,3,1)) %>%
    theme_vanilla() %>%
    align(align = "center", part="header") %>%
    autofit()
  save_as_docx(ft,
               path=paste("results/", operating_point,
                          "_", dataset, "_performances_all.docx", sep=""))
}


# generate_performances_table(dataset="train", operating_point="min")
generate_performances_table(dataset="train", operating_point="1se")
# generate_performances_table(dataset="test", operating_point="min")
generate_performances_table(dataset="test", operating_point="1se")
