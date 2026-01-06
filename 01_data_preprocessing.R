#==============================================================================
# Step 1: Imputation and Time-Stratified Splitting
#==============================================================================

library(missForest)
library(tidyverse)
library(rsample)
#==============================================================================
# 1. Load Data & Filter
#==============================================================================
raw_data <- read.csv("Example_Data.csv", na.strings = c("NA", ""))

# Remove 'Grade' (>30% missing rate)
if("Grade" %in% names(raw_data)) raw_data$Grade <- NULL

#==============================================================================
# 2. Variable Type Conversion
#==============================================================================
cont_vars <- c("BMI", "Tumor_length", "Age", "Chemotherapy_cycle", "Immunotherapy_cycle", "rfstime")
cat_vars  <- setdiff(names(raw_data), cont_vars)
raw_data[cat_vars] <- lapply(raw_data[cat_vars], as.factor)


#==============================================================================
# 3. missForest Imputation
#==============================================================================
set.seed(123)
print("Running imputation...")
impute_res <- missForest(raw_data, ntree = 100, maxiter = 10, variablewise = TRUE, verbose = FALSE)
complete_data <- impute_res$ximp

#==============================================================================
# 4. Time-Stratified Splitting (7:3)
#==============================================================================
complete_data <- complete_data %>%
  mutate(strata = case_when(
    status == 0 ~ "Censored",
    status == 1 & rfstime <= quantile(rfstime[status==1], 0.33) ~ "Early",
    status == 1 & rfstime <= quantile(rfstime[status==1], 0.66) ~ "Mid",
    TRUE ~ "Late"
  ))

set.seed(69)
split_obj <- initial_split(complete_data, prop = 0.7, strata = strata)

train_data <- training(split_obj) %>% select(-strata)
test_data  <- testing(split_obj)  %>% select(-strata)

#==============================================================================
# 5. Save & Verify
#==============================================================================
save(train_data, test_data, file = "Processed_Data.RData")


