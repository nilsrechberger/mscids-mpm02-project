library(tidyverse)
library(here)
library(lubridate)
library(mgcv)
library(pROC)
library(gratia)
library(ggplot2)

# Load data
train <- readRDS(here("data", "train.rds"))
val   <- readRDS(here("data", "val.rds"))

# Feature engineering
train <- train %>%
  mutate(
    AccidentInvolvingPedestrian = as.numeric(AccidentInvolvingPedestrian),
    hour_num = as.numeric(lubridate::hour(accident_dt_dummy)),
    month_fac = factor(lubridate::month(accident_dt_dummy), levels = 1:12),
    day_type = factor(
      ifelse(lubridate::wday(accident_dt_dummy) %in% c(1, 7),
             "Weekend", "Weekday")
    )
  )

val <- val %>%
  mutate(
    AccidentInvolvingPedestrian = as.numeric(AccidentInvolvingPedestrian),
    hour_num = as.numeric(lubridate::hour(accident_dt_dummy)),
    month_fac = factor(lubridate::month(accident_dt_dummy), levels = 1:12),
    day_type = factor(
      ifelse(lubridate::wday(accident_dt_dummy) %in% c(1, 7),
             "Weekend", "Weekday")
    )
  )

# GAM model
gam_model <- gam(
  AccidentInvolvingPedestrian ~
    s(hour_num) +
    s(AccidentLocation_CHLV95_E,
      AccidentLocation_CHLV95_N,
      k = 40) +
    day_type +
    AccidentSeverityCategory_en +
    month_fac,
  data = train,
  family = binomial,
  method = "REML"
)

# Model summary
summary(gam_model)

# Validation prediction
pred_val_gam <- predict(gam_model, val, type = "response")

# Validation AUC
auc_gam <- pROC::roc(
  val$AccidentInvolvingPedestrian,
  pred_val_gam
)$auc

cat("Validation AUC (GAM):", as.numeric(auc_gam), "\n")

# Plot

dir.create(here("img"), showWarnings = FALSE)

set.seed(42)
train_sample <- train %>% sample_n(5000)

p_spatial <- draw(
  gam_model,
  select = "s(AccidentLocation_CHLV95_E,AccidentLocation_CHLV95_N)",
  residuals = FALSE,
  rug = FALSE
)

p_spatial_sample <- p_spatial +
  geom_point(
    data = train_sample,
    aes(
      x = AccidentLocation_CHLV95_E,
      y = AccidentLocation_CHLV95_N
    ),
    alpha = 0.15,
    size = 0.7,
    color = "black"
  )

ggsave(
  filename = here("img", "GAM_spatial_smooth_sample.png"),
  plot = p_spatial_sample,
  width = 8,
  height = 6
)

p_spatial_sample

dir.create(here("models"), showWarnings = FALSE)

saveRDS(gam_model, here("models", "gam_binomial.rds"))

cat("GAM model successfully saved to models/gam_binomial.rds\n")