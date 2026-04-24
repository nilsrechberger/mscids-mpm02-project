# =============================================================================
# LM Evaluation: RMSE, 5-fold CV, Residual Diagnostics, Coefficient Plot
# Depends on: R/models/lm.r having been run first (creates lm1.rds, lm2.rds)
# =============================================================================

library(tidyverse)
library(here)

set.seed(42)

train <- readRDS(here("data", "train.rds")) %>%
  mutate(hour  = as.numeric(hour(accident_dt_dummy)),
         month = factor(month(accident_dt_dummy), levels = 1:12))

val <- readRDS(here("data", "val.rds")) %>%
  mutate(hour  = as.numeric(hour(accident_dt_dummy)),
         month = factor(month(accident_dt_dummy), levels = 1:12))

lm1 <- readRDS(here("data", "lm1.rds"))
lm2 <- readRDS(here("data", "lm2.rds"))

rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))

# --- RMSE + AIC summary ---
cat("=== Model Comparison ===\n")
cat(sprintf("lm1 — val RMSE: %d m  | AIC: %d\n",
            round(rmse(val$AccidentLocation_CHLV95_N, predict(lm1, val))),
            round(AIC(lm1))))
cat(sprintf("lm2 — val RMSE: %d m  | AIC: %d\n",
            round(rmse(val$AccidentLocation_CHLV95_N, predict(lm2, val))),
            round(AIC(lm2))))

# --- 5-fold cross-validation on lm2 ---
k     <- 5
n     <- nrow(train)
folds <- sample(rep(seq_len(k), length.out = n))

cv_rmse <- sapply(seq_len(k), function(i) {
  tr  <- train[folds != i, ]
  te  <- train[folds == i, ]
  mod <- lm(AccidentLocation_CHLV95_N ~
              AccidentType_en * RoadType_en +
              AccidentSeverityCategory_en +
              poly(hour, 2) + month,
            data = tr)
  rmse(te$AccidentLocation_CHLV95_N, predict(mod, te))
})
cat(sprintf("\n5-fold CV RMSE (lm2): %d +/- %d metres\n",
            round(mean(cv_rmse)), round(sd(cv_rmse))))

# --- Residuals vs Fitted ---
res_df <- tibble(fitted = fitted(lm2), residuals = resid(lm2))

p_res <- ggplot(res_df, aes(fitted, residuals)) +
  geom_point(alpha = 0.05, size = 0.4) +
  geom_hline(yintercept = 0, colour = "red", linewidth = 0.8) +
  geom_smooth(se = FALSE, colour = "steelblue") +
  labs(title = "Residuals vs Fitted — lm2",
       x = "Fitted values (North, metres)", y = "Residuals") +
  theme_minimal()

ggsave(here("img", "LM_residuals.png"), p_res, width = 8, height = 5)

# --- Road type coefficient plot ---
coef_names  <- names(coef(lm2))
road_idx    <- grep("RoadType", coef_names)
road_coefs  <- coef(lm2)[road_idx]
road_se     <- sqrt(diag(vcov(lm2)))[road_idx]

coef_df <- tibble(
  term      = str_remove(names(road_coefs), "RoadType_en"),
  estimate  = road_coefs,
  conf_low  = road_coefs - 1.96 * road_se,
  conf_high = road_coefs + 1.96 * road_se
)

p_coef <- ggplot(coef_df, aes(x = estimate, y = reorder(term, estimate))) +
  geom_pointrange(aes(xmin = conf_low, xmax = conf_high)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  labs(title = "LM: Road Type Effect on North Coordinate (vs minor road)",
       x = "Estimated shift (metres north)", y = NULL) +
  theme_minimal()

ggsave(here("img", "LM_coef_roadtype.png"), p_coef, width = 8, height = 4)
message("Plots saved to img/")
