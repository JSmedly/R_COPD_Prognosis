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


nimp <- 5

# load coefficients
if (operating_point=="min") {
  load("results/coefs_min.Rda")
} else if (operating_point=="1se") {
  load("results/coefs_1se.Rda")
}

if (dataset == "train") {
  load("data/survival/train_surv_data_imp.Rda")
  dfs <- lapply(1:nimp, function(i) {
    prepare_categ_vars(complete(train_surv_data_imp, action=i), outcome)
  })  
} else if (dataset == "test") {
  load("data/survival/test_surv_data_imp.Rda")
  dfs <- lapply(1:nimp, function(i) {
    prepare_categ_vars(complete(test_surv_data_imp, action=i), outcome)
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