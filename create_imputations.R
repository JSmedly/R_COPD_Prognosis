library(mice)
# !!!Run this code in the background

outcomes <- c(
  "hosp_1_year",
  "hosp_3_year",
  "hosp_5_year",
  "mort_1_year",
  "mort_3_year",
  "mort_5_year"
)
preds_cts = c(
  "decades_from_start", # This is for imputing and including time (hospital effects)
  "age",
  "age_2", 
  "height",
  "weight",
  "bmi", 
  "fev1",
  "fvc", 
  "fivc",
  "fev1_fvc",
  "fev1_fev6",
  "pef", 
  "mmef",
  "dlco",
  "spo2"
)
preds_bin = c("hosp_past_year", "sex") # Binary predictors
preds_categ = c("smoking_status", "mmrc") # Categorical

# Imputation parameters for mice
nimp = 100
maxiter = 15
n_core = 5

for(outcome in outcomes) {
  start.time <- Sys.time()
  print(outcome)
  print(paste("Starting time:", start.time))
  
  print("Loading train and test data...")
  load(paste("data", outcome, "train_data.Rda", sep="/"))
  load(paste("data", outcome, "test_data.Rda", sep="/"))
  train_data <- train_data[c(preds_bin, preds_cts, preds_categ, outcome)]
  test_data <- test_data[c(preds_bin, preds_cts, preds_categ, outcome)]
  pred_mat <- make.predictorMatrix(train_data)
  # Note: Cannot condition BMI on w or h because of normalisation.
  pred_mat[c("weight", "height"), "bmi"] <- 0 # Remove feedback dependency
  meth <- make.method(train_data)

  print("Imputing training data...")
  print(paste("Number of rows to impute:", nrow(train_data)))
  # Training imputations
  train_imp <- futuremice(train_data, n.core=n_core,
                    predictorMatrix = pred_mat, method = meth,
                    m = nimp, maxit = maxiter, print = FALSE
  )
  save(train_imp, file=paste("data", outcome, 
                       paste("train_imp_", nimp, ".Rda", sep=""), 
                       sep="/"))
  mid.time <- Sys.time()
  print(paste("Time to impute training data:", mid.time-start.time))
  
  print("Imputing testing data...")
  print(paste("Number of rows to impute:", nrow(test_data)))
  # Testing imputations
  test_imp <- futuremice(test_data, n.core=n_core,
                          predictorMatrix = pred_mat, method = meth,
                          m = nimp, maxit = maxiter, print = FALSE
  )
  save(test_imp, file=paste("data", outcome, 
                             paste("test_imp_", nimp, ".Rda", sep=""), 
                             sep="/"))
  end.time <- Sys.time()
  print(paste("Time to impute testing data:", end.time-mid.time))
  
}