source("preprocessing_functions.R")
library(dplyr)
library(readr)


# Load data
path <- "/mnt/Data/deidentified_data/full_cohort/preprocessed_data_long.csv"
data <- read_csv(file=path, col_names=TRUE, col_select=-1, na=c("", "NA"))

data <- calculate_survtime_and_status(data)

ids <- c("RandID", "visit_number",
         "date")
preds_cts = c(
  "age", # Months
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
# Select variables we want
data <- data %>%
  select(all_of(c(outcomes_surv, ids, preds)))

# Preprocess variables
train_frac <- 0.8
outputs <- preprocess_survival(data, train_frac, preds_cts)
train_surv_data <- outputs[[1]]
test_surv_data <- outputs[[2]]
means <- outputs[[3]]
sds <- outputs [[4]]

sum(train_surv_data[["status"]])
sum(test_surv_data[["status"]])

# Save data
save_dir <- "data/survival"
create_folder_if_not_exists(save_dir)

save(train_surv_data, file=paste(save_dir, "train_surv_data.Rda", sep="/"))
save(test_surv_data, file=paste(save_dir, "test_surv_data.Rda", sep="/"))
save(means, file=paste(save_dir, "train_means.Rda", sep="/"))
save(sds, file=paste(save_dir, "train_sds.Rda", sep="/"))