library(tidyverse)
library(here)
library(lubridate)
library(pROC)

test <- readRDS(here("data", "test.rds"))

prepare_glm_data <- function(df) {
  df %>%
    mutate(
      AccidentInvolvingPedestrian = as.integer(AccidentInvolvingPedestrian),
      hour_num = as.numeric(lubridate::hour(accident_dt_dummy)),
      month_fac = factor(lubridate::month(accident_dt_dummy), levels = 1:12),
      RoadType_simplified = case_when(
        RoadType_en %in% c("expressway", "motorway") ~ "high_speed",
        TRUE ~ "other"
      ),
      AccidentInvolvingBicycle = as.integer(AccidentInvolvingBicycle),
      AccidentInvolvingMotorcycle = as.integer(AccidentInvolvingMotorcycle)
    )
}

test_glm <- prepare_glm_data(test)

glm_bin <- readRDS(here("models", "glm_bin.rds"))

pred_test <- predict(glm_bin, newdata = test_glm, type = "response")

auc_test <- roc(
  response = test_glm$AccidentInvolvingPedestrian,
  predictor = pred_test
)$auc

cat("GLM Binomial Test AUC:", as.numeric(auc_test), "\n")