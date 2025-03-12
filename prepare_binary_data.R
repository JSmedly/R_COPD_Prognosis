source("preprocessing_functions.R")
library(dplyr)
library(readr)

create_folder_if_not_exists <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
    message("Folder created: ", path)
  } else {
    message("Folder already exists: ", path)
  }
}


# Load data
path <- "/mnt/Data/deidentified_data/full_cohort/preprocessed_data_long.csv"
data <- read_csv(file=path, col_names=TRUE, col_select=-1, na=c("", "NA"))

# Get only required columns
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
outcomes_bin <- c(
  "hosp_1_year", "hosp_3_year", "hosp_5_year", 
  "mort_1_year", "mort_3_year", "mort_5_year"
)

preds <- c( preds_bin, preds_cts, preds_categ)
outcomes <- c(outcomes_bin)
data <- data %>% select(all_of(c(ids, preds, outcomes)))
head(data)

n_years_list <- c(1, 3, 5)
test_span <- 5
for(n_years in n_years_list){
  outcomes_nyear <- paste(c("hosp", "mort"), n_years, "year", sep="_")
  outputs <- preprocess_single_visit(data, n_years, test_span, preds_cts)
  for(outcome in outcomes_nyear) {
    train_data <- outputs[[1]] %>% select(all_of(c(ids, 
                                                   preds,
                                                   "age_2",
                                                   "decades_from_start",
                                                   outcome)))
    test_data <- outputs[[2]] %>% select(all_of(c(ids, 
                                                  preds,
                                                  "age_2",
                                                  "decades_from_start",
                                                  outcome)))
    means <- outputs[[3]]
    sds <- outputs[[4]]
    
    # Define where to save data
    folder_path <- paste("data", outcome, sep="/")
    create_folder_if_not_exists(folder_path)
    # Save all data
    save(train_data, file=paste(folder_path, "train_data.Rda", sep="/"))
    save(test_data, file=paste(folder_path, "test_data.Rda", sep="/"))
    save(means, file=paste(folder_path, "train_means.Rda", sep="/"))
    save(sds, file=paste(folder_path, "train_sds.Rda", sep="/"))
  }
}

  