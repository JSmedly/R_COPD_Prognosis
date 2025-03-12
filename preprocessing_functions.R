library(dplyr)

calculate_survtime_and_status <- function(data) {
  data <- data %>%
    mutate(surv_time = pmax(mort_next_time, 
                            hosp_last_time, 
                            time_diff, 
                            na.rm=T)) %>%
    group_by(RandID) %>%
    mutate(surv_time = max(surv_time)) %>% # Get survtime in months
    mutate(status = if_else(!is.na(mort_next_time), 1, 0)) %>%  
    filter(surv_time!=0) %>%
    ungroup()
  return(data)
}

truncate_for_followup <- function(data, years, end_date) {
  # Remove patients without followup.
  data <- data %>% filter(end_date - date > years*365.25)
  return(data)
}

# Get first visits. Apply first.
get_first_visit <- function(data) {
  return(
    data %>% 
      group_by(RandID) %>%
      slice_min(order_by=date) %>%
      ungroup()
  )
}

# Apply inclusion/exclusion criteria. Apply after first visits have been selected. 
exclude_patients <- function(data) {
  return(
    data %>%
      filter(age >= 45*12) %>%
      filter(age <= 90*12)
  )
}

# Scale predictors and factorise categories.
basic_preprocessing <- function(data) {
  return(
    data %>% 
      mutate(
        # Categorical variables
        sex = factor(sex,
                     labels=c(0,1),
                     levels=c("female", "male")),
        smoking_status = factor(smoking_status, 
                                labels=c(0,1,2),
                                levels=c("never", "ex", "current")),
        mmrc = factor(mmrc),
        # Continuous variables
        # Convert date to decade after 2006-01-01
        decades_from_start = as.numeric(
          difftime(date, as.Date("2006-01-01"), units="days")
          )/(365.25*10),
        fev1_fvc = fev1_fvc*100, # Convert to percentage
        fev1_fev6 = fev1_fev6*100,
        age = age/(12*10), # Convert to Decades
        spo2 = log(101 - spo2), # Log transform
      ) 
  )
}

# Split into train and test groups, removing dates without sufficient followup. Do after all data is properly formatted but before standardisation.
prepare_train_test_splits <- function(data, n_years, test_span) {
  # Final date for test is 'n_years' prior to the end-date
  # First date for test is 'n_years+1' prior to the end-date
  # Final date for train is 'n_years+1' prior to the end-date
  # First date for train is '2006-01-01'.
  end_date = as.Date("2021-12-31")
  data <- data %>% 
    mutate(time_to_end = end_date - date) %>%
    mutate(test_split = case_when(
      time_to_end < n_years * 365.25 ~ NaN,
      time_to_end <= (n_years+test_span)*365.25 ~ TRUE,
      .default = FALSE)
    )
  test_data <- data %>% filter(test_split == TRUE)
  train_data <- data %>% filter(test_split == FALSE)
  dropped_data <- data %>% filter(is.na(test_split))
  
  return(list(train_data, test_data, dropped_data))
}

# Apply on each of the train-test splits.
standardise_variables <- function(data, preds_cts) {
  print("preds_cts:")
  print(preds_cts)
  # Get means for continuous variables
  data_means <- data %>% 
    summarise(across(all_of(preds_cts), ~mean(.x, na.rm=TRUE)))
  
  # Get standard deviations for continuous variables
  data_stddevs <- data %>%
    summarise(across(all_of(preds_cts), ~sd(.x, na.rm=TRUE)))
  # colnames(data_stddevs) <- paste(colnames(data_stddevs), "_stddev", sep="")
  
  # Mean centre 
  data <- data %>%
    mutate(across(all_of(preds_cts), ~.x-mean(.x, na.rm=T)))
  # Add age^2
  data <- data %>% 
    mutate(age_2 = age^2, .after="age") 
  # Scale variables
  data <- data %>%
    mutate(across(all_of(preds_cts), ~.x/(2*sd(.x, na.rm=T))),
           age_2 = age_2/(2*sd(age_2))) # Not included in preds_cts
  return(list(data, data_means, data_stddevs))
}



format_sub_super_script <- function(text) {
  # Formats strings to use flextable super-sub scripting
  # See this thread for the regex magic. 
  # https://stackoverflow.com/questions/15575221/why-does-strsplit-use-positive-lookahead-and-lookbehind-assertion-matches-differ 
  
  # Split text at '^' or '_', keeping the character in the right group.
  elements <- strsplit(text, split = "(?<=.)(?=[\\^\\_])", perl = TRUE)[[1]]
  # Create empty holder for formatted elements
  formatted_elements <- list() #vector("list", length = length(elements))
  for (i in seq_along(elements)) {
    if (grepl("\\{.*\\}", elements[i], perl = TRUE)) {
      parts <- strsplit(elements[i], split="\\{|\\}", perl = TRUE)[[1]]
      if (length(parts) == 3) { # Pattern: "^{script} (extra string)"
        formatted_elements[[length(formatted_elements)+1]] <- 
          if (grepl("\\^", parts[1])) as_sup(parts[2]) else as_sub(parts[2])
        formatted_elements[[length(formatted_elements)+1]] <- parts[3]
      } else if (length(parts) == 2) { # Pattern: "^{script}"
        formatted_elements[[length(formatted_elements)+1]] <- 
          if (grepl("\\^", parts[1])) as_sup(parts[2]) else as_sub(parts[2])
      } else { # Should not get here
        print(paste("ERROR:", elements[i], "contains weird pattern")) 
        formatted_elements[[length(formatted_elements)+1]] <- elements[i]
      }
    } else { # Element does not contain subscript or superscript
      formatted_elements[[length(formatted_elements)+1]] <- elements[i]
    }
  }
  # return(as_paragraph(unlist(formatted_elements), sep=""))
  return(as_paragraph(list_values=formatted_elements))
}

###############################################################
# Main data preprocessing functions

preprocess_single_visit <- function(data, n_years, test_span, preds_cts) {
  splits <- data %>%
    get_first_visit() %>%
    exclude_patients() %>%
    basic_preprocessing() %>%
    prepare_train_test_splits(n_years, test_span)
  preds_cts <- c(preds_cts, "decades_from_start")
  train_split <- splits[[1]]
  test_split <- splits[[2]]
  
  # Prepare dates to be a predictive variable
  
  train_outputs <- standardise_variables(train_split, preds_cts)
  # test_outputs <- standardise_variables(test_split)
  train_split <- train_outputs[[1]]
  train_means <- train_outputs[[2]]
  train_sds <- train_outputs[[3]]
  
  # Scale test data
  
  print(head(train_split))
  print(head(test_split))
  print(preds_cts)
  print(train_means)
  test_split[preds_cts] <- sweep(test_split[preds_cts], 2,
                                 as.numeric(train_means), "-")
  test_split <- test_split %>% mutate(age_2 = age^2, .after="age")
  test_split[preds_cts] <- sweep(test_split[preds_cts]/2, 2,
                                 as.numeric(train_sds), "/")
  test_split <- test_split %>% mutate(age_2 = age_2/(2*sd(age_2)))
  
  #   # Convert logical to numeric now that standardisation has occured.
  # data <- data %>%
  #     mutate(across(where(is.logical), as.numeric)) 
  
  return(list(train_split, test_split, train_means, train_sds))
}

preprocess_survival <- function(data, preds_cts) {
  data <- data %>%
    get_first_visit() %>%
    exclude_patients() %>%
    basic_preprocessing() # No train-test splits
  preds_cts <- c(preds_cts, "decades_from_start")
  standardised_data <- standardise_variables(data, preds_cts)
  data <- standardised_data[[1]]
  data.means <- standardised_data[[2]]
  data.sds <- standardised_data[[3]]
  return(list(data, data.means, data.sds))
}


###############################################################

prepare_categ_vars <- function(data, outcome=NaN) {
  # Converts binary variables into numerics (0,1) and categorical variables
  # into split encodings.
  # academic.oup.com/aje/article/125/2/319/109714
  smoking_encoding_map <- data.frame(
    smoking_status = c("0", "1", "2"),
    smoking_status.X1 = c(0, 1, 1),
    smoking_status.X2 = c(0, 0, 1)
  )
  mmrc_encoding_map <- data.frame(
    mmrc = c("0", "1", "2", "3", "4"),
    mmrc.X1 = c(0, 1, 1, 1, 1),
    mmrc.X2 = c(0, 0, 1, 1, 1),
    mmrc.X3 = c(0, 0, 0, 1, 1),
    mmrc.X4 = c(0, 0, 0, 0, 1)
  )
  
  data <- data %>%
    mutate( # Convert binary variables 
      sex = as.numeric(as.character(sex)),
      hosp_past_year = as.numeric(hosp_past_year)
      ) %>%
    left_join(smoking_encoding_map, by="smoking_status") %>%
    left_join(mmrc_encoding_map, by="mmrc") %>%
    select(-smoking_status, -mmrc)
  
  if (!is.nan(outcome)) {
    data <- data %>%
      # Rename outcome to outcome.
      mutate(outcome = as.numeric(.data[[outcome]])) %>%
      select(-{{outcome}}) # {{var}} refers to the string stored in var.
  }
  return(data)
}



###################### Technically post-processing functions
group_smoking_status <- function(df) {
  df <- df %>% 
    mutate(smoking_status.Ex = smoking_status.X1,
           smoking_status.Current = smoking_status.X1 + smoking_status.X2) %>%
    select(-smoking_status.X1, -smoking_status.X2)
  return(df)
}

group_mmrc <- function(df) {
  df <- df %>%
    mutate(mmrc.1 = mmrc.X1,
           mmrc.2 = mmrc.X1 + mmrc.X2,
           mmrc.3 = mmrc.X1 + mmrc.X2 + mmrc.X3,
           mmrc.4 = mmrc.X1 + mmrc.X2 + mmrc.X3 + mmrc.X4) %>%
    select(-mmrc.X1, -mmrc.X2, -mmrc.X3, -mmrc.X4)
}