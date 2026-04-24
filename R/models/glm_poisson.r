# =============================================================================
# GLM Poisson: Accident frequency by road type x hour x month
# Lead: [assigned by group]
#
# Since every row = one accident, we aggregate to obtain count data.
# Grouping: RoadType x hour-of-day x month-of-year
# =============================================================================

library(tidyverse)
library(here)

train <- readRDS(here("data", "train.rds"))

# Aggregate individual accident rows into counts per cell
accident_counts <- train %>%
  mutate(
    hour  = hour(accident_dt_dummy),
    month = factor(month(accident_dt_dummy), levels = 1:12)
  ) %>%
  count(RoadType_en, hour, month, name = "n_accidents")

cat("Aggregated dimensions:", dim(accident_counts), "\n")
cat("Count range:", paste(range(accident_counts$n_accidents), collapse = "–"), "\n")
cat(sprintf("Mean: %.1f  |  Variance: %.1f\n",
            mean(accident_counts$n_accidents),
            var(accident_counts$n_accidents)))

# Model 1: road type only (baseline)
glm_p1 <- glm(n_accidents ~ RoadType_en,
               family = poisson, data = accident_counts)

# Model 2: road type + quadratic hour + month seasonality
glm_p2 <- glm(
  n_accidents ~ RoadType_en + poly(hour, 2) + month,
  family = poisson, data = accident_counts
)

# Model 3: road type x hour interaction (does commute peak differ by road type?)
glm_p3 <- glm(
  n_accidents ~ RoadType_en * poly(hour, 2) + month,
  family = poisson, data = accident_counts
)

cat("\n=== AIC comparison ===\n")
cat("glm_p1:", round(AIC(glm_p1), 1), "\n")
cat("glm_p2:", round(AIC(glm_p2), 1), "\n")
cat("glm_p3:", round(AIC(glm_p3), 1), "\n")

cat("\n=== Deviance ANOVA ===\n")
print(anova(glm_p1, glm_p2, glm_p3, test = "Chisq"))

# Overdispersion: ratio >> 1 indicates more variance than Poisson assumes
disp <- deviance(glm_p2) / df.residual(glm_p2)
cat(sprintf("\nOverdispersion ratio (glm_p2): %.2f\n", disp))

if (disp > 1.5) {
  cat("Overdispersion detected — fitting quasi-Poisson as robustness check.\n")
  glm_p2_qp <- glm(
    n_accidents ~ RoadType_en + poly(hour, 2) + month,
    family = quasipoisson, data = accident_counts
  )
  saveRDS(glm_p2_qp, here("data", "glm_p2_qp.rds"))
}

print(summary(glm_p2))

saveRDS(accident_counts, here("data", "accident_counts.rds"))
saveRDS(glm_p1, here("data", "glm_p1.rds"))
saveRDS(glm_p2, here("data", "glm_p2.rds"))
saveRDS(glm_p3, here("data", "glm_p3.rds"))
message("Models saved.")
