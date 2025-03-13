library(mice)
library(survival)
library(dplyr)

# Load data
load("data/survival/train_surv_data.Rda")
load("data/survival/test_surv_data.Rda")

preds_cts = c(
  "age", # Months
  "age_2",
  "height", # m
  "weight", # kg
  "bmi", # kg/m^2
  "fev1", # L
  "fvc", # L
  "fivc", # L 
  "fev1_fvc", # %
  "fev1_fev6", # %
  "pef", # L/s
  "mmef", # L/s
  "dlco",
  "spo2"
)
preds_bin = c("hosp_past_year", "sex") # Binary predictors
preds_categ = c("smoking_status", "mmrc") # Categorical
preds <- c(preds_cts, preds_bin, preds_categ)
outcomes_surv <- c("surv_time", "status")

# Calculate hazard
# Note Nelson-Aalen has NaNs in it which get imputed. 
# To the best of my knowledge, the Nelson-Aalen function assists with imputation
# but does not need to be used to create new models.
train_surv_data <- train_surv_data %>%
  select(all_of(c(outcomes_surv, preds))) %>%
  mutate(hazard = nelsonaalen(train_surv_data, surv_time, status)) 
test_surv_data <- test_surv_data %>%
  select(all_of(c(outcomes_surv, preds))) %>%
  mutate(hazard = nelsonaalen(test_surv_data, surv_time, status)) 

# Setup hyperparams for imputation
nimp = 100
maxiter = 15
n_core = 5

pred_mat <- make.predictorMatrix(train_surv_data)
pred_mat[c("weight", "height"), c("bmi")] <- 0 # Remove feedback dependency
pred_mat[,c("surv_time", "status")] <- 0
pred_mat
meth <- make.method(train_surv_data)

# Run imputation
print("Imputing training data...")
start.time <- Sys.time()
print(start.time)
train_surv_data_imp <- futuremice(train_surv_data, n.core=n_core,
                      predictorMatrix = pred_mat, method = meth,
                      m = nimp, maxit = maxiter, print = FALSE
)
print("DURATION:")
print(Sys.time()-start.time)
save(train_surv_data_imp, file="data/survival/train_surv_data_imp.Rda")

print("Imputing testing data...")
start.time <- Sys.time()
print(start.time)
test_surv_data_imp <- futuremice(test_surv_data, n.core=n_core,
                                  predictorMatrix = pred_mat, method = meth,
                                  m = nimp, maxit = maxiter, print = FALSE
)
print("DURATION:")
print(Sys.time()-start.time)
save(test_surv_data_imp, file="data/survival/test_surv_data_imp.Rda")