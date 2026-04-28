library(tidyverse)
library(here)
library(lubridate)
library(mgcv)
library(pROC)

test <- readRDS(here("data", "test.rds"))

prepare_gam_data <- function(df) {
  df %>%
    mutate(
      AccidentInvolvingPedestrian = as.integer(AccidentInvolvingPedestrian),
      hour_num = as.numeric(lubridate::hour(accident_dt_dummy)),
      month_fac = factor(lubridate::month(accident_dt_dummy), levels = 1:12),
      day_type = factor(
        ifelse(lubridate::wday(accident_dt_dummy) %in% c(1, 7),
               "Weekend", "Weekday")
      ),
      AccidentInvolvingBicycle = as.integer(AccidentInvolvingBicycle),
      AccidentInvolvingMotorcycle = as.integer(AccidentInvolvingMotorcycle)
    )
}

test_gam <- prepare_gam_data(test)

gam_model <- readRDS(here("models", "gam_binomial.rds"))

pred_test_gam <- predict(gam_model, newdata = test_gam, type = "response")

auc_test_gam <- roc(
  response = test_gam$AccidentInvolvingPedestrian,
  predictor = pred_test_gam
)$auc

cat("GAM Binomial Test AUC:", as.numeric(auc_test_gam), "\n")