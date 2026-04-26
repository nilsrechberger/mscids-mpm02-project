# =============================================================================
# GLM Poisson Evaluation: AIC, Deviance ANOVA, Overdispersion, Interpretation
# Depends on: R/models/glm_poisson.r having been run first
# =============================================================================

library(tidyverse)
library(here)

accident_counts <- readRDS(here("data", "accident_counts.rds"))
glm_p1 <- readRDS(here("models", "glm_p1.rds"))
glm_p2 <- readRDS(here("models", "glm_p2.rds"))
glm_p3 <- readRDS(here("models", "glm_p3.rds"))

# --- AIC / dispersion table ---
cat("=== Model Comparison ===\n")
model_tbl <- tibble(
  Model = c("glm_p1 (road type only)",
            "glm_p2 (+ hour² + month)",
            "glm_p3 (+ road×hour)"),
  AIC          = round(c(AIC(glm_p1), AIC(glm_p2), AIC(glm_p3)), 1),
  Deviance     = round(c(deviance(glm_p1), deviance(glm_p2), deviance(glm_p3)), 1),
  `Disp.ratio` = round(c(deviance(glm_p1) / df.residual(glm_p1),
                          deviance(glm_p2) / df.residual(glm_p2),
                          deviance(glm_p3) / df.residual(glm_p3)), 2)
)
print(model_tbl)

# --- Deviance ANOVA ---
cat("\n=== Deviance ANOVA ===\n")
print(anova(glm_p1, glm_p2, glm_p3, test = "Chisq"))

# --- Multiplicative effects on the rate scale ---
cat("\n=== Rate Ratios exp(coef) for glm_p2 ===\n")
b   <- coef(glm_p2)
se  <- sqrt(diag(vcov(glm_p2)))
rr_tbl <- tibble(
  Term        = names(b),
  `Rate ratio`= round(exp(b), 3),
  `95% low`   = round(exp(b - 1.96 * se), 3),
  `95% high`  = round(exp(b + 1.96 * se), 3)
)
print(rr_tbl)

# --- Predicted vs observed scatter ---
preds <- predict(glm_p2, type = "response")
plot_df <- accident_counts %>% mutate(predicted = preds)

p1 <- ggplot(plot_df, aes(predicted, n_accidents, colour = RoadType_en)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "GLM Poisson — Predicted vs Observed Counts",
       x = "Predicted count", y = "Observed count", colour = "Road type") +
  theme_minimal()

ggsave(here("img", "Poisson_pred_vs_actual.png"), p1, width = 8, height = 5)

# --- Mean hourly profile: observed vs fitted ---
p2 <- plot_df %>%
  group_by(RoadType_en, hour) %>%
  summarise(obs = mean(n_accidents), pred = mean(predicted), .groups = "drop") %>%
  pivot_longer(c(obs, pred), names_to = "source", values_to = "count") %>%
  ggplot(aes(hour, count, colour = source, linetype = source)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ RoadType_en, scales = "free_y") +
  scale_colour_manual(values = c(obs = "grey30", pred = "steelblue"),
                      labels = c(obs = "Observed", pred = "Fitted")) +
  scale_linetype_manual(values = c(obs = "solid", pred = "dashed"),
                        labels = c(obs = "Observed", pred = "Fitted")) +
  labs(title = "Accident Counts by Hour: Observed vs Fitted (glm_p2)",
       x = "Hour of Day", y = "Mean count", colour = NULL, linetype = NULL) +
  theme_minimal()

ggsave(here("img", "Poisson_hourly_fit.png"), p2, width = 10, height = 5)
message("Plots saved to img/")
