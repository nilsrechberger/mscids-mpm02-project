library(tidyverse)
library(here)
library(lubridate)
library(mgcv)
library(pROC)

train <- readRDS(here("data", "train.rds"))
val   <- readRDS(here("data", "val.rds"))

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

train_gam <- prepare_gam_data(train)
val_gam   <- prepare_gam_data(val)

gam_model <- gam(
  AccidentInvolvingPedestrian ~
    s(hour_num, k = 10) +
    s(AccidentLocation_CHLV95_E, AccidentLocation_CHLV95_N, k = 40) +
    day_type +
    AccidentSeverityCategory_en +
    month_fac +
    AccidentInvolvingBicycle +
    AccidentInvolvingMotorcycle,
  data = train_gam,
  family = binomial,
  method = "REML"
)

summary(gam_model)

pred_val_gam <- predict(gam_model, newdata = val_gam, type = "response")

auc_val_gam <- pROC::roc(
  response = val_gam$AccidentInvolvingPedestrian,
  predictor = pred_val_gam
)$auc

cat("Validation AUC (GAM):", as.numeric(auc_val_gam), "\n")

dir.create(here("models"), showWarnings = FALSE)
saveRDS(gam_model, here("models", "gam_binomial.rds"))

cat("GAM model successfully saved to models/gam_binomial.rds\n")