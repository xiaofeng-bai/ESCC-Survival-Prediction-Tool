#==============================================================================
# Step 2: Feature Selection (Boruta)
# Feature selection and hyperparameter tuning are only performed on the training set
# The external validation set  was completely independent and never participated in
# any development steps, including feature selection, model training, or hyperparameter tuning.
#==============================================================================

library(Boruta)
library(survival)
library(tidyverse)

#==============================================================================
# 1. Load Processed Data
#==============================================================================
load("Processed_Data.RData") 

#==============================================================================
# 2. Run Boruta Selection
#==============================================================================
print("Running Boruta algorithm on training set...")
set.seed(42)
boruta_res <- Boruta(
  x = train_data %>% select(-rfstime, -status), 
  y = Surv(train_data$rfstime, train_data$status), 
  doTrace = 0,        
  maxRuns = 100,
  getImp = getImpRfZ   
)

# Resolve tentative attributes
boruta_final <- TentativeRoughFix(boruta_res)

# Extract confirmed relevant features
selected_feats <- getSelectedAttributes(boruta_final)

#==============================================================================
# 3. Output Results
#==============================================================================
cat("\n--- Boruta Selection Results ---\n")
print(boruta_final)
cat("\nSelected Features:", length(selected_feats), "\n")
print(selected_feats)

#==============================================================================
# 4. Filter Datasets
#==============================================================================
# Retain only selected features + Outcome (rfstime, status)
final_cols <- c(selected_feats, "rfstime", "status")
train_data_sel <- train_data[, final_cols]



# 5. Save Final Datasets for Modeling
save(train_data_sel,file = "Selected_Data.RData")


