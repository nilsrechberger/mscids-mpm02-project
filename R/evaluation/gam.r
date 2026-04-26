library(tidyverse)
library(here)
library(lubridate)
library(mgcv)
library(pROC)

set.seed(42)

# Use the already feature-engineered train if you have it.
# Otherwise recreate features:
train <- readRDS(here("data", "train.rds")) %>%
  mutate(
    AccidentInvolvingPedestrian = as.numeric(AccidentInvolvingPedestrian),
    hour_num = as.numeric(lubridate::hour(accident_dt_dummy)),
    month_fac = factor(lubridate::month(accident_dt_dummy), levels = 1:12),
    day_type = factor(
      ifelse(lubridate::wday(accident_dt_dummy) %in% c(1, 7),
             "Weekend", "Weekday")
    )
  )

k <- 5
n <- nrow(train)
folds <- sample(rep(seq_len(k), length.out = n))

cv_auc_gam <- sapply(seq_len(k), function(i) {
  cat("Running fold", i, "of", k, "\n")
  
  tr <- train[folds != i, ]
  te <- train[folds == i, ]
  
  model <- gam(
    AccidentInvolvingPedestrian ~
      s(hour_num) +
      s(AccidentLocation_CHLV95_E,
        AccidentLocation_CHLV95_N,
        k = 40) +
      day_type +
      AccidentSeverityCategory_en +
      month_fac,
    data = tr,
    family = binomial,
    method = "REML"
  )
  
  pred <- predict(model, te, type = "response")
  
  as.numeric(pROC::roc(
    te$AccidentInvolvingPedestrian,
    pred
  )$auc)
})

cat("5-fold CV AUC (GAM):", round(mean(cv_auc_gam), 3),
    "+/-", round(sd(cv_auc_gam), 3), "\n")