library(glmnet)
library(survival)
library(mice)
source("preprocessing_functions.R")

# Load data
load("data/survival/surv_data.Rda")
load("data/survival/surv_data_imp.Rda")

preds_cts = c(
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
preds_bin = c("hosp_past_year") # Binary predictors
preds_categ = c("smoking_status.X1", "smoking_status.X2", 
                "mmrc.X1", "mmrc.X2", "mmrc.X3", "mmrc.X4") 
preds <- c(preds_cts, preds_bin, preds_categ)
npreds <- length(preds)
outcomes_surv <- c("surv_time", "status")

nimp = 100
# Create dfs
dfs <- lapply(1:nimp,
              function(i) prepare_categ_vars(
                complete(cox.imp, action=i), 
              )
)

# Stack dfs
stacked_data <- bind_rows(dfs)

# Generate design matrices
x.female <- stacked_data %>% 
  filter(sex==0) %>%
  select(all_of(preds)) %>%
  as.matrix()
x.male <- stacked_data %>%
  filter(sex==1) %>%
  select(all_of(preds)) %>%
  as.matrix()

y.female <- stacked_data %>%
  filter(sex==0) %>%
  select(outcomes_surv) %>%
  as.matrix()
y.male <- stacked_data %>%
  filter(sex==1) %>%
  select(outcomes_surv) %>%
  as.matrix()

# Need to rename the y's
colnames(y.female) <- c("time", "status")
colnames(y.male) <- c("time", "status")

# Calculate weightings
weights.female <- (1 - rowMeans(is.na(surv.data%>%filter(sex==0))))/nimp
weights.female <- rep(weights.female, nimp)
weights.male <- (1 - rowMeans(is.na(surv.data%>%filter(sex==1))))/nimp
weights.male <- rep(weights.male, nimp)

# Generate foldids so that the same row in each imputation is in the same fold
nfolds = 5
nrow.female <- nrow(surv.data%>%filter(sex==0))
nrow.male <- nrow(surv.data%>%filter(sex==1))
foldids.female <- rep(sample(rep(1:nfolds, length.out=nrow.female)), nimp)
foldids.male <- rep(sample(rep(1:nfolds, length.out=nrow.male)), nimp)


# Fit model
start.time <- Sys.time()
print(start.time)
fit.female <- cv.glmnet(x.female, y.female, 
                        alpha=0.5,
                        family="cox", type.measure="C",
                        weights=weights.female, foldid = foldids.female)
print(paste("Fit basic female model in:", Sys.time()-start.time))

start.time <- Sys.time()
print(start.time)
fit.male <- cv.glmnet(x.male, y.male, 
                      alpha = 0.5,
                      family="cox", type.measure="C",
                      weights=weights.male, foldid = foldids.male)
print(paste("Fit basic male model in:", Sys.time()-start.time))

model_dir <- "models/survival"
save(fit.female, file=paste(model_dir, "fit_female.Rda", sep="/"))
save(fit.male, file=paste(model_dir, "fit_male.Rda", sep="/"))
