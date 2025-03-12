library(flextable)
library(officer)
library(tibble)
library(stringr)
library(miselect)
source("preprocessing_functions.R")


create_coef_table <- function(operating_point="min") {
  outcomes <- c("hosp_1_year", 
                "hosp_3_year",
                "hosp_5_year",
                "mort_1_year",
                "mort_3_year",
                "mort_5_year")
  
  female.coef_list <- list()
  male.coef_list <- list()
  for (outcome in outcomes) {
    # Load fit.female and fit.male
    load(paste("models", outcome, "fit_female_adWcoef.Rda", sep="/"))
    load(paste("models", outcome, "fit_male_adWcoef.Rda", sep="/"))
    
    # Get coefs from operating point
    if (operating_point=="min") {
      female.coefs <- coef(fit.female)
      male.coefs <- coef(fit.male)
    } else if (operating_point=="1se") {
      female.coefs <- coef(fit.female, 
                           lambda=fit.female$lambda.1se,
                           alpha=fit.female$alpha.1se)
      male.coefs <- coef(fit.male, 
                         lambda=fit.male$lambda.1se,
                         alpha=fit.male$alpha.1se)
    }
    # Compile into lists
    female.coef_list[paste(outcome, "female", sep=".")] <-list(female.coefs)
    male.coef_list[paste(outcome, "male", sep=".")] <- list(male.coefs)
  }
  # Combine female and male lists into one dataframe
  coef.df <- cbind(data.frame(female.coef_list), data.frame(male.coef_list))
  save(coef.df, file=paste("results/coefs_",operating_point,".Rda", sep=""))

  # Prepare coefs df for being in a word doc.
  coef.df <- coef.df %>% 
    # Transpose
    t() %>% 
    as.data.frame() %>%
    # group mmrc and smoking status
    group_mmrc() %>% 
    group_smoking_status() %>% 
    t() %>% 
    as.data.frame() 
  # Save coef.df for future use
  
  
  # Define rownames
  rownames(coef.df) <- c(
    "(Intercept)",
    "Time",
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
  # Make rownames a column
  coef.df <- coef.df %>% 
    rownames_to_column("Variable")
  # Prepare flextable
  options(digits=2)
  ft <- flextable(coef.df)
  # Apply sub and super scripting to the rownames.
  # This approach is a hack, but it works
  for (i in 1:nrow(coef.df)) {
    ft <- ft %>%
      compose(i = i, j = "Variable",
              value = format_sub_super_script(coef.df[i,"Variable"])
      )
  }
  
  # Bold numerics not equal to zero
  bold_points <- which(coef.df[,2:ncol(coef.df)] != 0, arr.ind=TRUE)
  bold_points[,"col"] <- bold_points[,"col"] +1
  for (row in 1:nrow(bold_points)) {
    ft <- ft %>%
      bold(i=bold_points[row,"row"],
           j=bold_points[row,"col"])
  }
  
  # Update headers
  ft <- ft %>% 
    set_header_labels(
      hosp_1_year.female="Hosp. 1 years", 
      hosp_3_year.female="Hosp. 3 years",
      hosp_5_year.female="Hosp. 5 years",
      mort_1_year.female="Mort. 1 years",
      mort_3_year.female="Mort. 3 years",
      mort_5_year.female="Mort. 5 years",
      hosp_1_year.male="Hosp. 1 years", 
      hosp_3_year.male="Hosp. 3 years",
      hosp_5_year.male="Hosp. 5 years",
      mort_1_year.male="Mort. 1 years",
      mort_3_year.male="Mort. 3 years",
      mort_5_year.male="Mort. 5 years"
    ) %>%
    add_header_row(values=c("", "Female", "Male"),
                   colwidths=c(1,6,6)) %>%
    merge_v(part="header") %>%
    set_table_properties(width=0.5) %>%
    align(align = "center", part="all") %>%
    autofit()
  ft  
  save_as_docx(ft, 
               path=paste("results/coefs_", operating_point, ".docx", sep=""))  
  options(digits=7)
}



############################################################
# RUN CODE

create_coef_table(operating_point="min")
create_coef_table(operating_point="1se")