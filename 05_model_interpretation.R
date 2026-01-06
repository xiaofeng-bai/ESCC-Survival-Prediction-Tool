#==============================================================================
# Step 5: Model Interpretability (SHAP Analysis)
# Description: Generates Variable Importance plots and SHAP (SHapley Additive 
#              exPlanations) summaries to interpret the RSF model.
#==============================================================================
library(tidyverse)
library(survival)
library(mlr3verse)
library(survex) 
source("functions.R")

#==============================================================================
# 1. Load Trained Model and Data
#==============================================================================
if(file.exists("trained_rfsB_model.rds")) {
  learner_rfsB <- readRDS("trained_rfsB_model.rds")
  traindata <- readRDS("traindata.rds")
  itps <- readRDS("itps.rds")}


#==============================================================================
# 2. Prepare Data for Interpretation
#==============================================================================
task_train <- as_task_surv(traindata, time = "rfstime", event = "status", type = "right")
traindatax <- traindata[, task_train$feature_names]
catvars <- colnames(traindatax)[sapply(traindatax, is.factor)]
convars <- setdiff(colnames(traindatax), catvars)
traindatay <- survival::Surv(time = traindata$rfstime, 
                             event = traindata$status
                             )

#==============================================================================
# 3. Create Explainer
#==============================================================================
exper_rfsB <- survex::explain(
  learner_rfsB$model, 
  data = traindatax,
  y = traindatay,
  times = itps
  )

#==============================================================================
# 4. Global Interpretation (Variable Importance)
#==============================================================================
viplot(exper_rfsB, output_type = "survival")

#==============================================================================
# 5. Local Interpretation (Single Sample Prediction)
#==============================================================================
shap4one(exper_rfsB, traindatax[1,], output_type = "risk")

#==============================================================================
# 6. Summary SHAP Analysis 
#==============================================================================
sumshap_rfsB <- sumshap(
  explainer = exper_rfsB, 
  data      = traindatax, 
  catvars   = catvars, 
  convars   = convars, 
  sampleN   = 200
)
sumshap_rfsB$shapplotc

