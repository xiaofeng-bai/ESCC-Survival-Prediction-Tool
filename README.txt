========================================================================
MACHINE LEARNING-BASED CLINICAL DECISION-SUPPORT TOOL FOR ESCC
========================================================================

This repository contains the R source code for the study: 
"Machine learning-based clinical decision-support tool for overall survival prediction and treatment plan selection of treatment-naïve advanced ESCC 
in the immunotherapy era: A multi-center study".

------------------------------------------------------------------------
SCOPE OF THE REPOSITORY
------------------------------------------------------------------------
This codebase focuses on the implementation of the optimal Boruta-RSF pipeline and its deployment preparation.

Rationale: 
  As detailed in the manuscript, the Boruta-RSF strategy demonstrated superior predictive performance and clinical utility compared to other 
  candidate algorithms (e.g., CoxPH, XGBoost, GBM). Code for other benchmarked models follows standard package implementations and is omitted for brevity.

Purpose: 
  To maintain clarity and usability, this repository provides the complete, end-to-end workflow for the final selected model, facilitating 
  reproduction of the methodology.

------------------------------------------------------------------------
DATA PRIVACY & REPRODUCIBILITY
------------------------------------------------------------------------
IMPORTANT NOTE FOR REVIEWERS AND READERS:
Due to ethical and privacy regulations regarding multi-center patient data, the original training and validation datasets cannot be made publicly 
available.

File: "Example_Data.csv"
We have provided a synthetic dataset containing samples with the same structure (variable names, levels, and data types) as the original data.
  
DISCLAIMER:This dataset contains randomly generated values. It does not reproduce the exact results (AUC, C-index) reported in the manuscript. 
It is intended solely for code demonstration and verifying the functionality of the pipeline.

------------------------------------------------------------------------
PIPELINE OVERVIEW
------------------------------------------------------------------------
Note: Please ensure your R working directory is set to the root of this repository.


00_setup.R
    Checks and installs all necessary R packages required for the analysis.

01_data_preprocessing.R
    Data imputation (MissForest) and splitting.

02_feature_selection.R
    Boruta feature selection to identify key prognostic variables.

03_model_building.R
    Performs hyperparameter tuning and trains the final Boruta-RSF model.

04_model_evaluation.R
    Evaluates model performance.

05_model_interpretation.R
    Visualizations for model interpretability.
    


------------------------------------------------------------------------
SOFTWARE REQUIREMENTS
------------------------------------------------------------------------
R Version: 4.4.1
R Studio: Recommended for running scripts.

