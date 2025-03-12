
library(tibble)
library(glmnet)
library(survival)
library(mice)
library(flextable)
library(officer)
source("preprocessing_functions.R")

# Load data
load("data/survival/surv_data.Rda")
load("data/survival/surv_data_imp.Rda")

create_coef_table <- function(operating_point) {
  load("models/survival/fit_female.Rda")
  load("models/survival/fit_male.Rda")
  
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
  
  if(operating_point == "min") {
    coef.female <- coef(fit.female, s=fit.female$lambda.min)
    coef.male <- coef(fit.male, s=fit.male$lambda.min)
  } else if(operating_point == "1se") {
    coef.female <- coef(fit.female, s=fit.female$lambda.1se)
    coef.male <- coef(fit.male, s=fit.male$lambda.1se)
  }
  coef.df <- as.data.frame(as.matrix(cbind(coef.female, coef.male)))
  
  colnames(coef.df) <- c("Female", "Male")
  # Save coefficient data
  save(coef.df, 
       file=paste("results/survival/coefs_", 
                  operating_point, ".Rda", sep=""))
  
  # Prepare coefs for being in a word document:
  coef.df <- coef.df %>% 
    # Transpose
    t() %>% 
    as.data.frame() %>%
    # group mmrc and smoking status
    group_mmrc() %>% 
    group_smoking_status() %>% 
    t() %>% 
    as.data.frame() 
  # Define rownames
  rownames(coef.df) <- c(
    "Age",
    "Age^{2}",
    "Height",
    "Weight",
    "BMI",
    "FEV_{1}",
    "FVC",
    "FIVC",
    "FEV_{1}/FVC",
    "FEV_{1}/FEV_{6}",
    "PEF",
    "MMEF",
    "DLCO",
    "SpO^{2}",
    "Hospitalisation (Past Year)",
    "mMRC=1",
    "mMRC=2",
    "mMRC=3",
    "mMRC=4",
    "Smoking Status=Ex",
    "Smoking Status=Current"
  )
  coef.df <- coef.df %>% 
    rownames_to_column("Variable")
  coef.df
  ft <- flextable(coef.df)
  for (i in 1:nrow(coef.df)) {
    ft <- ft %>%
      compose(i = i, j = "Variable",
              value = format_sub_super_script(coef.df[i,"Variable"])
      )
  }
  
  # Automatically make points bold
  bold_points <- which(coef.df[,2:ncol(coef.df)] != 0, arr.ind=TRUE)
  bold_points[,"col"] <- bold_points[,"col"] +1
  for (row in 1:nrow(bold_points)) {
    ft <- ft %>%
      bold(i=bold_points[row,"row"],
           j=bold_points[row,"col"])
  }
  
  ft <- ft %>%
    colformat_double(digits=3) %>%
    # theme_vanilla() %>%
    bold(part="header") %>%
    set_table_properties(width=0.5) %>%
    # align(align = "center", part="all") %>%
    autofit()
  save_as_docx(ft, 
               path=paste("results/survival/coefs_", 
                          operating_point, ".docx", sep=""))
}

create_coef_table(operating_point="min")
create_coef_table(operating_point="1se")