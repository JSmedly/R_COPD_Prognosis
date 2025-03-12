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
outputs <- preprocess_survival(data, preds_cts)
surv.data <- outputs[[1]]
means <- outputs[[2]]
sds <- outputs [[3]]

# Save data
save_dir <- "data/survival"
save(surv.data, file=paste(save_dir, "surv_data.Rda", sep="/"))
save(means, file=paste(save_dir, "means.Rda", sep="/"))
save(sds, file=paste(save_dir, "sds.Rda", sep="/"))