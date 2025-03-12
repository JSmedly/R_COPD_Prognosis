library(miselect)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(gridExtra)
source("preprocessing_functions.R")


###########################################################
## FUNCTIONS
outcome_map <- c("hosp_1_year" = "Hosp. 1 year", 
                 "hosp_3_year" = "Hosp. 3 years",
                 "hosp_5_year" = "Hosp. 5 years",
                 "mort_1_year" = "Mort. 1 year",
                 "mort_3_year" = "Mort. 3 years",
                 "mort_5_year" = "Mort. 5 years")

plot_cv_lambda <- function(fit) {
  # Plot change in cvm vs lambda
  plot.data <- data.frame(lambda=fit$lambda,
                          cvm=fit$cvm)
  # Gather on alpha value
  plot.data <- plot.data %>% gather("alpha", "cvm", 2:ncol(plot.data))
  # Note if the models are run with more values of alpha, 
  #  then will need a different way of plotting the lambdas. Use gather.
  plot.data
  plt <- ggplot(data=plot.data) +
    geom_line(aes(x=log(lambda), y=cvm, 
                  group=alpha, color=alpha)) +
    scale_color_discrete(
      name = expression(alpha),
      labels = sprintf(r'(%.1f)', fit$alpha)
        #lapply(sprintf(r'($alpha = %.2f$)', fit$alpha), TeX)
      # unname(TeX(c("$\\alpha =$ 1", "$\\alpha$ = 0.5")))
      # labels = unname(TeX(c(paste("$\\alpha = 1$",fit$alpha, "$",sep=""))))
    ) +
    # geom_line(aes(x=log(lambda), y=cvm.2), color='red') + 
    geom_vline(aes(xintercept=log(fit$lambda.min)), linetype="dotted") + 
    geom_vline(aes(xintercept=log(fit$lambda.1se)), linetype="dashed") +
    labs(x=expression(log(lambda)),
         y="Cross-Validated Mean (deviance)",
         title=paste("Cross-Validated Performance (",
                     outcome_map[outcome],
                     ")", sep="")
    ) +
    theme_bw()
  return(plt)
}

plot_coef_paths <- function(fit, alpha=1) {
  # Plot the paths of the continuous variables vs -log(lambda)
  lambdas <- fit$lambda
  coef_list <- list()
  for(i in 1:length(lambdas)) {
    lambda= lambdas[i]
    coef_list[[i]] <- coef(fit, lambda=lambda, alpha=alpha)
  }
  coefs.df <- data.frame(do.call(rbind, coef_list))
  coefs.df["lambda"] <- lambdas # Append lambdas column for plotting
  coefs.df <- coefs.df %>% 
    # Remove categorical preds
    select(-X.Intercept., 
           -mmrc.X1, -mmrc.X2, -mmrc.X3, -mmrc.X4, 
           -smoking_status.X1, -smoking_status.X2) 
  
  # Parse the colnames with scales
  colnames(coefs.df) <- c(
    "Time",
    "Age",
    "Age^2",
    "Height",
    "Weight",
    "BMI",
    "FEV[1]",
    "FVC",
    "FIVC",
    "FEV[1]/FVC",
    "FEV[1]/FEV_6",
    "PEF",
    "MMEF",
    "DLCO",
    "SpO[2]",
    "Hospitalisation~Past~Year", # Tilde is parsed as space
    "lambda"
  )
  coefs.df <- coefs.df %>%
    gather("variable", "coef", 1:(ncol(coefs.df)-1)) # Gather so we can group across variables, don't gather lambda.
  
  # Define linetype formatting string to better differentiate
  available_linetypes <- c("solid", "dashed", "dotted",
                           "dotdash", "longdash", "twodash")
  num_variables <- length(unique(coefs.df$variable))
  linetypes <- rep(available_linetypes, length.out = num_variables)
  
  # Plot the coefs, gathered and colored by variable.
  plt <- ggplot(coefs.df) +
    geom_line(aes(x=-log(lambda), y=coef, group=variable, color=variable,
                  linetype=variable)) +
    scale_color_discrete(name="Variable",
                         labels=label_parse()) + 
    scale_linetype_manual(name="Variable",
                          labels=label_parse(),
                          values=linetypes) + 
    theme_bw() +
    labs(x=expression(-log(lambda)),
         y="Coefficient",
         title="Coefficient Paths (non-categorical)")
  return(plt)
}


############## 
# Functions for categorical variable coef plotting
format_var <- function(str_var) {
  if(str_var=="smoking_status") {
    title <- "Coefficient Paths (Smoking Status)"
    legend.name <- "Smoking Status"
    legend.breaks <- c("smoking_status.Ex", "smoking_status.Current")
    legend.labels <- c("Smoking Status=Ex", "Smoking Status=Current")
  }
  else if(str_var=="mmrc") {
    title <- "Coefficient Paths (mMRC)"
    legend.name = "mMRC"
    legend.breaks <- c("mmrc.1", "mmrc.2", "mmrc.3", "mmrc.4")
    legend.labels <- c("mMRC=1", "mMRC=2", "mMRC=3", "mMRC=4")
  }
  return(list(title=title, 
              legend.breaks=legend.breaks, 
              legend.labels=legend.labels))
}

plot_categ_coef_paths <- function(fit, alpha=1) {
  lambdas <- fit$lambda
  coef_list <- list()
  for(i in 1:length(lambdas)) {
    lambda= lambdas[i]
    coef_list[[i]] <- coef(fit, lambda=lambda, alpha=alpha)
  }
  coefs.df <- data.frame(do.call(rbind, coef_list))
  coefs.df["lambda"] <- lambdas # Append lambdas column for plotting
  categ_coefs <- coefs.df %>% 
    group_smoking_status() %>%
    group_mmrc() %>%
    select(
      lambda, 
      mmrc.1, mmrc.2, mmrc.3, mmrc.4, 
      smoking_status.Ex, smoking_status.Current
    ) %>%
    gather("level", "coef", 2:7)  %>% # Gather so we can group across variables
    mutate(variable = str_remove(level, "\\..*$"))
  
  
  # Plot the coefs, gathered and colored by variable.
  out <- by(
    data = categ_coefs, 
    INDICES = categ_coefs$variable,
    FUN = function(m) {
      m <- droplevels(m)
      formats <- format_var(m[1,'variable'])
      m <- ggplot(m) +
        geom_line(aes(x=-log(lambda), y=coef, group=level, color=level)) +
        # facet_wrap(vars(variable), scales="free_y") + 
        theme_minimal() +
        labs(x="-log(lambda)",
             y="Coefficient",
             title=formats$title) +
        scale_colour_discrete(name=formats$legend.name,
                              breaks=formats$legend.breaks,
                              labels=formats$legend.labels)+
        theme(legend.position = "right")
    }
  )
  plt <- do.call(grid.arrange, out)
  return(plt)
}




###########################################################

outcomes <- c("hosp_1_year", 
              "hosp_3_year",
              "hosp_5_year",
              "mort_1_year",
              "mort_3_year",
              "mort_5_year")
for (outcome in outcomes) {
  # Specify directories
  model_dir <- paste("models", outcome, sep="/")
  results_dir <- paste("results", outcome, sep="/")
  
  load(paste(model_dir, "fit_female_adWcoef.Rda", sep="/"))
  load(paste(model_dir, "fit_male_adWcoef.Rda", sep="/"))
  
  # Fit vs Lambda
  plt.female <- plot_cv_lambda(fit.female)
  plt.male <- plot_cv_lambda(fit.male)
  ggsave(paste(results_dir, "cvm_lambda_female.svg", sep="/"), plot=plt.female,
         dpi=300, height=5, width=5, units="in")
  ggsave(paste(results_dir, "cvm_lambda_male.svg", sep="/"), plot=plt.male,
         dpi=300, height=5, width=5, units="in")
  
  # Coef vs Lambda
  plt.female <- plot_coef_paths(fit.female, fit.female$alpha.min)
  plt.male <- plot_coef_paths(fit.male, fit.male$alpha.min)
  ggsave(paste(results_dir, "coef_lambda_female.svg", sep="/"), plot=plt.female,
         dpi=300, height=6, width=5, units="in")
  ggsave(paste(results_dir, "coef_lambda_male.svg", sep="/"), plot=plt.male,
         dpi=300, height=6, width=5, units="in")
  
  # Categ coef vs Lambda
  plt.female <- plot_categ_coef_paths(fit.female, fit.female$alpha.min)
  ggsave(paste(results_dir, "categ_coef_lambda_female.svg", sep="/"),
         plot=plt.female, 
         dpi=300, height=5, width=5, units="in")
  plt.male <- plot_categ_coef_paths(fit.male, fit.male$alpha.min)
  ggsave(paste(results_dir, "categ_coef_lambda_male.svg", sep="/"),
         plot=plt.male, 
         dpi=300, height=5, width=5, units="in")
  
}
