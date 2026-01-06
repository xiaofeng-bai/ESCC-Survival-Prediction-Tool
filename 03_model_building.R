#==============================================================================
# Step 3: Model Establishment
# Description: Hyperparameter tuning and training of the Boruta-RSF model.
#==============================================================================

library(tidyverse)
library(readxl)
library(survival)
library(mlr3verse)
library(mlr3proba)
library(mlr3extralearners)
#==============================================================================
# 1. Load Data
#==============================================================================
load("Selected_Data.RData") 
traindata <- train_data_sel
traindata$status <- as.numeric(as.character(traindata$status))
traindata <- traindata %>% mutate_if(is.character, as.factor)

#==============================================================================
# 2. Define Task & Learner
#==============================================================================
task_train <- as_task_surv(traindata, time = "rfstime", event = "status", type = "right")
learner_rfsB <- lrn("surv.rfsrc",
                    ntree    = to_tune(500, 1000),
                    mtry     = to_tune(2, 5),
                    nodesize = to_tune(20, 50))
learner_rfsB$id <- "Boruta_RSF"


#==============================================================================
# 3. Hyperparameter Tuning
#==============================================================================
future::plan("multisession")
set.seed(48)
tune_rfsB <- tune(
  tuner = tnr("random_search", batch_size = 20),
  task = task_train,
  learner = learner_rfsB,
  resampling = rsmp("cv", folds = 5),
  measure = msr("surv.cindex"),
  terminator = trm("evals", n_evals = 40)
)

#==============================================================================
# 4. Train Final Model
#==============================================================================
learner_rfsB$param_set$values <- tune_rfsB$result_learner_param_vals
set.seed(43)
learner_rfsB$train(task_train)

#==============================================================================
# 5. Save Model
#==============================================================================
itps <- c(6, 12, 18) # Time points for evaluation
saveRDS(learner_rfsB, file = "trained_rfsB_model.rds")
saveRDS(traindata, file = "traindata.rds")
saveRDS(itps, file = "itps.rds")


