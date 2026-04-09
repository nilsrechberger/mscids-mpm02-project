library(tidyverse)
library(nnet)
library(here)

data <- readRDS(here("data", "train.rds"))

data %>%
  mutate(
    AccidentSeverityCategory_en = as.factor(AccidentSeverityCategory_en),
    across(where(is.character), as.factor),
    across(where(is.logical), as.numeric),
    AccidentLocation_CHLV95_E = as.numeric(scale(AccidentLocation_CHLV95_E)),
    AccidentLocation_CHLV95_N = as.numeric(scale(AccidentLocation_CHLV95_N))
  ) %>%
  select(-matches("accident_dt"), -matches("CantonCode"), -1) 

set.seed(42)
train_idx <- sample(1:nrow(poc_data), 0.8 * nrow(poc_data))
train_set <- poc_data[train_idx, ]
test_set  <- poc_data[-train_idx, ]

nn_model <- nnet(AccidentSeverityCategory_en ~ ., 
                 data = train_set, 
                 size = 5, 
                 decay = 0.1,
                 maxit = 500)

print(nn_model)
predictions <- predict(nn_model, test_set, type = "class")
print(table(Predicted = predictions, Actual = test_set$AccidentSeverityCategory_en))