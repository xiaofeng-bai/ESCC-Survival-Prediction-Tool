#==============================================================================
# Step 4: Model Evaluation
# Description: Evaluates the trained RSF model performance using custom functions.
#              Metrics: C-index, Time-dependent AUC, Brier Score.
#              Plots: ROC, Calibration Curves, and Decision Curve Analysis (DCA).
#==============================================================================
library(tidyverse)
library(survival)
library(mlr3verse)
library(mlr3proba)
source("functions.R")


#==============================================================================
# 1. Load Trained Model and Data
#==============================================================================
if(file.exists("trained_rfsB_model.rds")) {
  learner_rfsB <- readRDS("trained_rfsB_model.rds")
  traindata <- readRDS("traindata.rds")
    itps <- readRDS("itps.rds")
}


# Define Performance Metric
measure_sa <- msrs("surv.cindex")

# Re-define Survival Task for Training Data
task_train <- as_task_surv(traindata, time = "rfstime", event = "status", type = "right")

#==============================================================================
# 2. Performance Evaluation (For example: training Set)
#==============================================================================
predtrain_rfsB <- learner_rfsB$predict(task_train)
predprobtrain_rfsB <- predprob(pred = predtrain_rfsB, 
                               preddata = traindata, 
                               etime = "rfstime",
                               estatus = "status",
                               model = "rfsB", 
                               dataset = "train", 
                               timepoints = itps)
# Calculate C-Index
cat("C-index:", predtrain_rfsB$score(measure_sa), "\n")
# Comprehensive Evaluation (AUC & Brier Score)
evaltrain_rfsB <- eval4sa(predprob = predprobtrain_rfsB,
                          preddata = traindata, 
                          etime = "rfstime",
                          estatus = "status",
                          model = "rfsB",
                          dataset = "train",
                          timepoints = sort(itps),
                          plotcalimethod = "quantile",
                          bw4nne = NULL,
                          q4quantile = 3,
                          cutoff = "median")

print(evaltrain_rfsB$auc)
print(evaltrain_rfsB$brierscore)

#==============================================================================
# 3. Visualization
#==============================================================================
evaltrain_rfsB$rocplot
evaltrain_rfsB$calibrationplot
sadca(predprob = predprobtrain_rfsB,
      preddata = traindata,
      etime = "rfstime",
      estatus = "status", 
      model = "rfsB",
      dataset = "train",
      timepoints = itps,
      timepoint = 6, #12/18
      xrange = 0:100 / 100)

