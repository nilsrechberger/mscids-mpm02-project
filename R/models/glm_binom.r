library(tidyverse)
library(here)
library(lubridate)
library(pROC)

train <- readRDS(here("data", "train.rds"))
val   <- readRDS(here("data", "val.rds"))

train <- train %>%
  mutate(
    AccidentInvolvingPedestrian = as.numeric(AccidentInvolvingPedestrian),
    hour_num = as.numeric(lubridate::hour(accident_dt_dummy)),
    month_fac = factor(lubridate::month(accident_dt_dummy), levels = 1:12)
  )

val <- val %>%
  mutate(
    AccidentInvolvingPedestrian = as.numeric(AccidentInvolvingPedestrian),
    hour_num = as.numeric(lubridate::hour(accident_dt_dummy)),
    month_fac = factor(lubridate::month(accident_dt_dummy), levels = 1:12)
  )

glm_bin <- glm(
  AccidentInvolvingPedestrian ~
    AccidentSeverityCategory_en +
    hour_num +
    month_fac,
  data = train,
  family = binomial
)

summary(glm_bin)

pred_val <- predict(glm_bin, val, type = "response")

cat("Validation AUC:", as.numeric(roc(val$AccidentInvolvingPedestrian, pred_val)$auc), "\n")

dir.create(here("models"), showWarnings = FALSE)

saveRDS(glm_bin, here("models", "glm_bin.rds"))

cat("Model successfully saved to models/glm_bin.rds\n")