library(tidyverse)
library(nnet)
library(here)
library(caret)

# Ensuring reproducibility
set.seed(42)

# Load training data
train <- readRDS(here("data", "train.rds"))

# Clean data for NN
clean_columns <- function(df) {
  df %>%
    select(-any_of(c("accident_dt_dummy", "CantonCode")), -1)
}
train <- clean_columns(train)

# Calculate class weights (Imbalance Handling)
class_counts <- table(train$AccidentSeverityCategory_en)
class_weights <- 1 / class_counts
class_weights <- (class_weights / sum(class_weights)) * nrow(train)
model_weights <- class_weights[train$AccidentSeverityCategory_en]

# Setting up model training (including tuning and cross-validation)
train_ctrl <- trainControl(
  method = "cv",
  number = 2,
  verboseIter = TRUE
)

# Hyperparameter grid
tune_grid <- expand.grid(
  size = c(3),
  decay = c(0.01)
)

# # Hyperparameter grid
# tune_grid <- expand.grid(
#   size = c(3, 5, 7),
#   decay = c(0.01, 0.1, 0.5)
# )

cat("Start model training...\n")

nn <- train(
  AccidentSeverityCategory_en ~ .,
  data = train,
  method = "nnet",
  preProcess = c("center", "scale"),
  tuneGrid = tune_grid,
  trControl = train_ctrl,
  weights = model_weights,
  maxit = 500,
  MaxNWts = 5000,
  trace = FALSE
)

print(nn)

# Save model
# Erstelle einen Ordner "models", falls er nicht existiert
dir.create(here("models"), showWarnings = FALSE)
saveRDS(nn, here("models", "nn.rds"))

cat("Model successfully saved to models/nn.rds!\n")