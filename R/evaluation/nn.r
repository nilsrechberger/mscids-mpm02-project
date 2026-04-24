library(tidyverse)
library(caret)
library(here)

# 1. Daten und Modell laden
val   <- readRDS(here("data", "val.rds"))
test  <- readRDS(here("data", "test.rds"))
nn <- readRDS(here("models", "nn.rds"))

# Clean data for NN
clean_columns <- function(df) {
  df %>%
    select(-any_of(c("accident_dt_dummy", "CantonCode")), -1)
}

val  <- clean_columns(val)
test <- clean_columns(test)

# Evaluation on the validation set (val.rds)
cat("\n--- Evaluation: Validation Set ---\n")

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
cat("\n--- Evaluation: Test Set ---\n")

predictions_test <- predict(
    nn,
    newdata = test
)

conf_matrix_test <- confusionMatrix(
  data = predictions_test, 
  reference = test$AccidentSeverityCategory_en
)
print(conf_matrix_test)