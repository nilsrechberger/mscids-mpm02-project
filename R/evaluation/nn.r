library(tidyverse)
library(caret)
library(here)

# Load data and model
val   <- readRDS(here("data", "val.rds"))
test  <- readRDS(here("data", "test.rds"))
nn <- readRDS(here("models", "nn.rds"))

# Clean data for NN
clean_columns <- function(df) {
  df %>%
    select(-any_of(c("accident_dt_dummy", "CantonCode")))
}

val  <- clean_columns(val)
test <- clean_columns(test)

# Best hyperparameters from cross-validation
message("Best tuning parameters:")
print(nn$bestTune)

# Evaluation on the validation set (val.rds)
message("\n--- Evaluation: Validation Set ---")

predictions_val <- predict(
    nn,
    newdata = val
)

conf_matrix_val <- confusionMatrix(
  data = predictions_val, 
  reference = val$AccidentSeverityCategory_en
)
print(conf_matrix_val)

# Evaluation on the final test set (test.rds)
message("\n--- Evaluation: Test Set ---")

predictions_test <- predict(
    nn,
    newdata = test
)

conf_matrix_test <- confusionMatrix(
  data = predictions_test, 
  reference = test$AccidentSeverityCategory_en
)
print(conf_matrix_test)