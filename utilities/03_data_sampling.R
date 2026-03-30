library(here)
library(tidyverse)

set.seed(123) # Set seed for reproductivity

processed_data <- here("data", "processed", "processed_data.rds")
train_data <- here("data", "train.rds")
val_data <- here("data", "val.rds")
test_data <- here("data", "test.rds")

df <- readRDS(processed_data)

cat("Start data splitting...\n\n")

shuffled_df <- df[sample(nrow(df)), ]

# Project requirement: Limit Data set sizes
# DO NOT CHANGE THIS VALUES!
n_train <- 100000
n_val   <- 15000
n_test  <- 15000

# Split shuffled data
train_set <- shuffled_df[1:n_train, ]
val_set   <- shuffled_df[(n_train + 1):(n_train + n_val), ]
test_set  <- shuffled_df[(n_train + n_val + 1):(n_train + n_val + n_test), ]

saveRDS(train_set, file = train_data)
cat("Save train data\n")

saveRDS(val_set, file = val_data)
cat("Save validation data\n")

saveRDS(test_set, file = test_data)
cat("Save test data\n\n")

cat("Done data splitting\n\n")