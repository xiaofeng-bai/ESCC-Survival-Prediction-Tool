#==============================================================================
# Step 0: Environment Setup
# Description: Installs and loads all necessary R packages for the analysis.
#              Run this script ONCE before starting Step 1.
#==============================================================================

# 1. Define required packages
required_packages <- c(
  # Data Manipulation & Plotting
  "tidyverse", "readxl", "survminer", "ggbeeswarm", "ggh4x", 
  
  # Survival Analysis & Modeling
  "survival", "missForest", "rsample", "Boruta", 
  
  # Machine Learning Framework (mlr3)
  "mlr3verse", "mlr3proba", "mlr3extralearners", 
  
  # Model Interpretation & Evaluation
  "survex", "dcurves", "riskRegression",
  
  # Utilities
  "future", "progress"
)

# 2. Function to check and install missing packages
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    message(paste("Installing package:", pkg))
    tryCatch({
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
      message(paste("Successfully installed:", pkg))
    }, error = function(e) {
      message(paste("Error installing:", pkg, "- Please install manually."))
    })
  } else {
    message(paste("Package already installed:", pkg))
  }
}

# 3. Execute installation
cat("--- Starting Environment Setup ---\n")
invisible(sapply(required_packages, install_if_missing))
cat("\n--- Setup Completed!")
