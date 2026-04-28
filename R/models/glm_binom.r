library(tidyverse)
library(here)
library(lubridate)
library(pROC)

train <- readRDS(here("data", "train.rds"))
val   <- readRDS(here("data", "val.rds"))

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

train_glm <- prepare_glm_data(train)
val_glm   <- prepare_glm_data(val)
test_glm  <- prepare_glm_data(test)

glm_bin <- glm(
  AccidentInvolvingPedestrian ~
    RoadType_simplified +
    AccidentSeverityCategory_en +
    hour_num +
    month_fac +
    AccidentInvolvingBicycle +
    AccidentInvolvingMotorcycle,
  data = train_glm,
  family = binomial
)

summary(glm_bin)

pred_val <- predict(glm_bin, val_glm, type = "response")
auc_val <- roc(val_glm$AccidentInvolvingPedestrian, pred_val)$auc

cat("Validation AUC:", as.numeric(auc_val), "\n")

dir.create(here("models"), showWarnings = FALSE)
saveRDS(glm_bin, here("models", "glm_bin.rds"))

cat("Model successfully saved to models/glm_bin.rds\n") 