library(miselect)
library(mice)
library(dplyr)
source("preprocessing_functions.R")

overwrite = T
outcomes <- c(
  # "hosp_1_year",
  # "hosp_3_year",
  # "hosp_5_year",
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
  # "dlco",
  "spo2"
)
preds_bin = c("hosp_past_year") #, "sex") # Binary predictors
# Categorical split difference encoding
preds_categ = c("smoking_status.X1", "smoking_status.X2", 
                "mmrc.X1", "mmrc.X2", "mmrc.X3", "mmrc.X4") 
preds <- c(preds_cts, preds_bin, preds_categ)

for(outcome in outcomes) {
  data_dir <- paste("data", outcome, sep="/")
  models_dir = paste("models", outcome, sep="/")

  # Load previously imputed data
  load(paste(data_dir, "train_imp_100.Rda", sep="/"))
  load(paste(data_dir, "train_data.Rda", sep="/"))
  # load(paste(data_dir, "test_imp_100.Rda", sep="/"))
  
  # We know we have 100 imputations. 
  nimp = 10
  preds <- c(preds_cts, preds_bin, preds_categ)
  npreds = length(preds)
  # Generate list of completed dataframes
  dfs <- lapply(1:nimp, 
                function(i) prepare_categ_vars(
                  complete(train_imp, action=i), 
                  outcome)
                )
  
  # Generate list of design matrices and imputed responses
  # x.female <- list()
  # x.male <- list()
  # y.female <- list()
  # y.male <- list()
  x.female <- lapply(dfs, function(df) as.matrix(df %>% 
                                                   filter(sex==0) %>% 
                                                   select(all_of(preds))))
  x.male <- lapply(dfs, function(df) as.matrix(df %>% 
                                                 filter(sex==1) %>% 
                                                 select(all_of(preds))))
  y.female <- lapply(dfs, function(df) df %>%
                       filter(sex==0) %>%
                       pull(outcome) %>%
                       as.numeric())
  y.male <- lapply(dfs, function(df) df %>% 
                     filter(sex==1) %>%
                     pull(outcome) %>%
                     as.numeric())

  # num_cores <- 2
  pf <- rep(1,npreds)
  adWeight <- rep(1,npreds)
  weights.female <- 1 - rowMeans(is.na(train_data%>%filter(sex==0)))
  weights.male <- 1 - rowMeans(is.na(train_data%>%filter(sex==1)))
  alpha <- c(1,0.5) #c(1.0, 0.5)
  nfolds=5
  
  ## Maybe this should have been using the futures library. I am not actually 
  ## sure how that works
  # cv.saenet.parallel <- function(x, y, pf, adWeight, weights, alpha, nfolds) {
  #   # This gets either the male or female paired X-Ys.
  #   # X is a list of imputations
  #   # Y is a list of impuatations
  #   # print(ym)
  #   # print(yf)
  #   # fit.female <- cv.saenet(x.female, y.female, pf, adWeight, weights, family="binomial",
  #   #                         alpha=alpha, nfolds=3)
  #   # fit.male <- cv.saenet(x.male, y.male, pf, adWeight, weights, family="binomial",
  #   #                       alpha=alpha, nfolds=3)
  #   fit <- cv.saenet(x, y, pf, adWeight, weights, family="binomial",
  #                    alpha=alpha, nfolds=nfolds)
  #   # return(fit)
  #   # print(x)
  #   # print(y)
  #   return(fit)
  # }
  # 
  # # combined_inputs <- lapply(1:nimp, function(i) list(x.female[[i]],
  # #                                                    x.male[[i]],
  # #                                                    y.female[[i]],
  # #                                                    y.male[[i]]))
  # combined_inputs <- list(list(x.female,y.female, pf, adWeight, weights.female, alpha, nfolds), 
  #                         list(x.male, y.male, pf, adWeight, weights.male, alpha, nfolds))
  # start.time <- Sys.time()
  # print(start.time)
  # # print(x.female)
  # results <- mclapply(FUN=function(inputs) cv.saenet.parallel(inputs[[1]],
  #                                                        inputs[[2]],
  #                                                        inputs[[3]],
  #                                                        inputs[[4]],
  #                                                        inputs[[5]],
  #                                                        inputs[[6]],
  #                                                        inputs[[7]]),
  #                     X=combined_inputs,
  #                     mc.cores=2)
  # # str(results)
  # end.time <- Sys.time()
  # print(end.time)
  # time_diff <- end.time-start.time
  # print(paste("Fitting male and female in parallel:", time_diff))
  #   
  
 
  # Fit basic female model with Ridge regularisation
  start.time <- Sys.time()
  print(start.time)
  fit.female.ridge <- cv.saenet(x.female, y.female, 
                                pf, adWeight, weights.female, family="binomial",
                                alpha=0, lambda.min.ratio=1e-4,
                                nfolds=nfolds) 
  save(fit.female.ridge, file=paste(models_dir, "fit_female_adW1.Rda", sep="/"))
  print(paste("Fit basic female model in:", Sys.time()-start.time))
  
  # Fit basic male model with Ridge regularisation
  start.time <- Sys.time()
  print(start.time)
  fit.male.ridge <- cv.saenet(x.male, y.male, 
                              pf, adWeight, weights.male, family="binomial",
                              alpha=0, lambda.min.ratio=1e-4,
                              nfolds=nfolds)
  save(fit.male.ridge, file=paste(models_dir, "fit_male_adW1.Rda", sep="/"))
  print(paste("Fit basic male model in:", Sys.time()-start.time))
  # if (!overwrite) {
  #   load(paste(models_dir, "fit_female_adW1.Rda", sep="/"))
  #   load(paste(models_dir, "fit_male_adW1.Rda", sep="/"))
  # }

  # Use coefficients from default fits for adaptive weights
  print("Using coefficients as calculated adaptive weights")
  adWeight.female <- 1 / (abs(coef(fit.female.ridge)[-1]) + 1 / nrow(x.female[[1]]))
  adWeight.male <- 1 / (abs(coef(fit.male.ridge)[-1]) + 1 / nrow(x.male[[1]]))
  start.time <- Sys.time()
  print(start.time)
  fit.female <- cv.saenet(x.female, y.female, 
                          pf, adWeight.female, weights.female, family="binomial",
                          alpha=alpha, nfolds=nfolds)
  save(fit.female, file=paste(models_dir, "fit_female_adWcoef.Rda", sep="/"))
  print(paste("Fit adaptive weight female model in:", Sys.time()-start.time))
  
  start.time <- Sys.time()
  print(start.time)
  fit.male <- cv.saenet(x.male, y.male, 
                        pf, adWeight.male, weights.male, family="binomial",
                        alpha=alpha, nfolds=nfolds)
  save(fit.male, file=paste(models_dir, "fit_male_adWcoef.Rda", sep="/"))
  print(paste("Fit adaptive weight male model in:", Sys.time()-start.time))
}