# =============================================================================
# Linear Model: Predict north coordinate from accident characteristics
# Lead: [assigned by group]
# =============================================================================

library(tidyverse)
library(here)

set.seed(42)

train <- readRDS(here("data", "train.rds"))
val   <- readRDS(here("data", "val.rds"))

add_features <- function(df) {
  df %>% mutate(
    hour  = as.numeric(hour(accident_dt_dummy)),
    month = factor(month(accident_dt_dummy), levels = 1:12)
  )
}

train <- add_features(train)
val   <- add_features(val)

rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))

# Baseline: main effects only
lm1 <- lm(AccidentLocation_CHLV95_N ~ AccidentType_en + RoadType_en,
           data = train)

# Complex: interactions + quadratic hour (non-linear time-of-day effect)
# CantonCode excluded: it would trivially encode geography, not accident features
lm2 <- lm(
  AccidentLocation_CHLV95_N ~
    AccidentType_en * RoadType_en +
    AccidentSeverityCategory_en +
    poly(hour, 2) +
    month,
  data = train
)

print(summary(lm1))
print(summary(lm2))

cat("\nAIC  — lm1:", round(AIC(lm1)), "| lm2:", round(AIC(lm2)), "\n")
cat("RMSE (val) — lm1:", round(rmse(val$AccidentLocation_CHLV95_N, predict(lm1, val))),
    "| lm2:", round(rmse(val$AccidentLocation_CHLV95_N, predict(lm2, val))), "metres\n")

dir.create(here("models"), showWarnings = FALSE)
saveRDS(lm1, here("models", "lm1.rds"))
saveRDS(lm2, here("models", "lm2.rds"))
message("Saved lm1.rds and lm2.rds")
