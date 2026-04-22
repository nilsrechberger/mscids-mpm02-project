library(tidyverse)
library(nnet)
library(here)
library(caret)

# Set seed for reproducibility
set.seed(42)

# Load data
train <- readRDS(here("data", "train.rds"))
val   <- readRDS(here("data", "val.rds"))
test  <- readRDS(here("data", "test.rds"))

clean_data <- function(df) {
  # Cleans data specific for the NN
  df %>%
    select(-any_of(c("accident_dt_dummy", "CantonCode")), -1) %>% 
    mutate(
      AccidentLocation_CHLV95_E = as.numeric(scale(AccidentLocation_CHLV95_E)),
      AccidentLocation_CHLV95_N = as.numeric(scale(AccidentLocation_CHLV95_N))
    )
}

# Apply function on all sets
train <- clean_data(train)
val   <- clean_data(val)
test  <- clean_data(test)

# Calculate class weights to avoid imbalance 
class_counts <- table(train$AccidentSeverityCategory_en)
class_weights <- 1 / class_counts
class_weights <- class_weights / sum(class_weights) * nrow(train)

# Apply class weights
model_weights <- class_weights[train$AccidentSeverityCategory_en]

# Train model
nn_model <- nnet(
  AccidentSeverityCategory_en ~ .,
  data = train,
  size = 5,
  decay = 0.1,
  maxit = 500,
  MaxNWts = 5000,
  weights = model_weights, # Set model weights
  trace = TRUE
)

print(nn_model)

# Model validation
predictions_val <- predict(
  nn_model,
  newdata = val,
  type = "class"
)

# Re-factor if some classes are missing in the prediction
predictions_val <- factor(
  predictions_val,
  levels = levels(val$AccidentSeverityCategory_en)
)

# Conf. Matrix for evaluation
conf_matrix <- confusionMatrix(
  data = predictions_val, 
  reference = val$AccidentSeverityCategory_en
)

print(conf_matrix)