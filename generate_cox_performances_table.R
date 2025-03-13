# Warning takes 30 minutes to complete
library(flextable)
library(officer)
library(tibble)
library(stringr)
source("preprocessing_functions.R")
library(mice)
library(pROC)
library(psfmi)
library(glmnet)
library(survival)

###########################################################################
# FUNCTIONS FOR CREATING PERFORMANCE TABLE
create_roc_list <- function(y, preds) {
  # Create list of roc objects
  roc <- lapply(seq_along(preds), function(i) c.index(preds[[i]],
                                                      y[[i]]))
  return(roc)
}

c.index <- function (pred, y, weights = rep(1, nrow(y))) 
{
  if (!is.Surv(y)) 
    y = Surv(y[, "time"], y[, "status"])
  f = -pred
  if (missing(weights)) 
    concordance(y ~ f)
  else concordance(y ~ f, weights = weights)
}


calculate_auc_df <- function(roc_list) {
  # Creates a dataframe with nimp rows of auc mean and standard errors  
  auc_df <- bind_rows(lapply(seq_along(roc_list), function(i) {
    auc_df <- data.frame(
      auc_mean = roc_list[[i]]$concordance,
      auc_se = sqrt(roc_list[[i]]$var)
    )
    return(auc_df)
  }))
  return(auc_df)
}

outcomes_surv <- c("surv_time", "status")

nimp <- 100
operating_point <- "1se"
dataset <- "test"

# Load models
load("models/survival_new/fit_female.Rda")
load("models/survival_new/fit_male.Rda")

# load coefficients
if (operating_point=="min") {
  load("results/survival/coefs_min.Rda")
} else if (operating_point=="1se") {
  load("results/survival/coefs_1se.Rda")
}

if (dataset == "train") {
  load("data/survival/train_surv_data_imp.Rda")
  dfs <- lapply(1:nimp, function(i) {
    prepare_categ_vars(complete(train_surv_data_imp, action=i))
  })  
} else if (dataset == "test") {
  load("data/survival/test_surv_data_imp.Rda")
  dfs <- lapply(1:nimp, function(i) {
    prepare_categ_vars(complete(test_surv_data_imp, action=i))
  })
}
preds <- rownames(coef.df)

# Female lists
x.female <- list()
y.female <- list()
preds.female <- list()
coef.df
coef.female <- coef.df %>%
  select("Female") %>%
  as.matrix()
roc.female <- list()

# Male lists
x.male <- list()
y.male <- list()
preds.male <- list()
coef.male <- coef.df %>% 
  select("Male") %>%
  as.matrix()
roc.male <- list()

i <- 1
for (i in 1:nimp) {
  # ...generate design matrices
  x.female[[i]] <-
    as.matrix(dfs[[i]][dfs[[i]][,"sex"]==0, preds])
  x.male[[i]] <-
    as.matrix(dfs[[i]][dfs[[i]][,"sex"]==1, preds])
  y.female[[i]] <- dfs[[i]][dfs[[i]][,"sex"]==0, outcomes_surv]
  colnames(y.female[[i]]) <- c("time", "status")
  y.male[[i]] <- dfs[[i]][dfs[[i]][,"sex"]==1, outcomes_surv]
  colnames(y.male[[i]]) <- c("time", "status")
  # ...calculate predictions
  x.female[[i]]
  
  preds.female[[i]] <- predict(fit.female, newx=x.female[[i]], s="lambda.1se")
  preds.male[[i]] <- predict(fit.male, newx=x.male[[i]], s="lambda.1se")
  rownames(preds.female[[i]]) <- 1:length(preds.female[[i]])
  rownames(preds.male[[i]]) <- 1:length(preds.male[[i]])
}

# Create ROC objects
y.female[[i]][, "status"]
preds.female[[i]]
y.female[[i]]

conc <- c.index(preds.female[[i]], y.female[[i]])
conc$se

roc_list.female <- create_roc_list(y.female, preds.female)
roc_list.male <- create_roc_list(y.male, preds.male)

auc_df.female <- calculate_auc_df(roc_list.female)
auc_df.male <- calculate_auc_df(roc_list.male)

# Calculate AUC, Sens, Spec and pool using Rubin's rules
## Female
auc_pooled.female <- pool_auc(auc_df.female$auc_mean, 
                              auc_df.female$auc_se, 
                              nimp=nimp)
## Male
auc_pooled.male <- pool_auc(auc_df.male$auc_mean, 
                            auc_df.male$auc_se, 
                            nimp=nimp)

auc_pooled.all <- rbind(auc_pooled.female, auc_pooled.male)
rownames(auc_pooled.all) <- c("Female", "Male")
auc_pooled.all
save_as_docx(auc_pooled.all, path="results/survival/auc_all.docx")
