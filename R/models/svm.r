library(here)
library(e1071)
library(caret) 

# Load data
train_raw <- readRDS(here("data", "train.rds"))
test_raw  <- readRDS(here("data", "test.rds"))

# Utility function for data prep
prep_data <- function(df) {
  clean_df <- data.frame(
    N = df$AccidentLocation_CHLV95_N,
    E = df$AccidentLocation_CHLV95_E,
    Severity = as.factor(df$AccidentSeverityCategory_en)
  )
  return(na.omit(clean_df))
}

train_data <- prep_data(train_raw)
test_data  <- prep_data(test_raw)

# Calculate class weights (Imbalance Handling)
w <- table(train_data$Severity)
weights <- 1 / (w / sum(w))

# Setting up model training (including tuning and cross-validation)
cat("Start model training...\n")

tune_result <- tune.svm(
  Severity ~ N + E, 
  data = train_data, 
  kernel = "radial",
  class.weights = weights,
  # Test config
  cost = c(1), 
  gamma = c(0.1),
  # Real tune config
  # cost = c(1, 10, 50), 
  # gamma = c(0.1, 1, 5),
  tunecontrol = tune.control(sampling = "cross", cross = 2) # Use 5 for real training
)

cat("\nHyperparameter results:\n")
print(tune_result$best.parameters)

# Extract best model
best_svm <- tune_result$best.model

# Save model
if(!dir.exists(here("models"))) dir.create(here("models"))
saveRDS(best_svm, here("models", "svm_mvp_model.rds"))

cat("Model successfully saved to models/svm.rds!\n")