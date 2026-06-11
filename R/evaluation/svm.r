library(here)
library(e1071)
library(caret)

# Load model and data
svm_model  <- readRDS(here("models", "svm_mvp_model.rds"))
val_raw    <- readRDS(here("data", "val.rds"))
test_raw   <- readRDS(here("data", "test.rds"))

prep_data <- function(df) {
  clean_df <- data.frame(
    N = df$AccidentLocation_CHLV95_N,
    E = df$AccidentLocation_CHLV95_E,
    Severity = as.factor(df$AccidentSeverityCategory_en)
  )
  na.omit(clean_df)
}

val_data  <- prep_data(val_raw)
test_data <- prep_data(test_raw)

# Align factor levels to model training levels
model_levels <- svm_model$levels
val_data$Severity  <- factor(val_data$Severity,  levels = model_levels)
test_data$Severity <- factor(test_data$Severity, levels = model_levels)

# --- Validation set ---
message("\n--- Evaluation: Validation Set ---")
pred_val <- predict(svm_model, newdata = val_data)
cm_val   <- confusionMatrix(pred_val, val_data$Severity)
print(cm_val)

# --- Test set ---
message("\n--- Evaluation: Test Set ---")
pred_test <- predict(svm_model, newdata = test_data)
cm_test   <- confusionMatrix(pred_test, test_data$Severity)
print(cm_test)

# --- Per-class summary table ---
message("\n--- Per-class metrics (Test Set) ---")
per_class <- data.frame(
  Class     = rownames(cm_test$byClass),
  Precision = round(cm_test$byClass[, "Precision"], 3),
  Recall    = round(cm_test$byClass[, "Recall"],    3),
  F1        = round(cm_test$byClass[, "F1"],        3)
)
print(per_class, row.names = FALSE)

# --- Overall accuracy summary ---
message("\n--- Overall Accuracy (Test Set) ---")
cat("Accuracy :", round(cm_test$overall["Accuracy"],  3), "\n")
cat("Kappa    :", round(cm_test$overall["Kappa"],     3), "\n")

# --- Decision boundary plots ---
message("\n--- SVM Decision Boundary Plots ---")
library(ggplot2)

set.seed(42)
plot_idx  <- sample(nrow(test_data), min(1000, nrow(test_data)))
plot_data <- test_data[plot_idx, ]

# Build prediction grid for decision boundary
n_seq  <- seq(min(plot_data$N), max(plot_data$N), length.out = 200)
e_seq  <- seq(min(plot_data$E), max(plot_data$E), length.out = 200)
grid   <- expand.grid(N = n_seq, E = e_seq)
grid$Severity <- predict(svm_model, newdata = grid)

# Shared point layer and theme
base_plot <- ggplot() +
  geom_point(
    data  = plot_data,
    aes(x = N, y = E, colour = Severity),
    alpha = 0.6, size = 0.9
  ) +
  labs(x = "N (CHLV95)", y = "E (CHLV95)", colour = "Severity") +
  theme_minimal()

if (!dir.exists(here("img"))) dir.create(here("img"))

# Without model
png(here("img", "SVM_accident_locations.png"), width = 800, height = 600)
print(base_plot + ggtitle("Accident Locations by Severity (test set sample)"))
dev.off()
message("Plot saved to img/SVM_accident_locations.png")

# With decision boundary
png(here("img", "SVM_decision_boundary.png"), width = 800, height = 600)
print(
  base_plot +
    geom_raster(
      data  = grid,
      aes(x = N, y = E, fill = Severity),
      alpha = 0.25
    ) +
    scale_fill_discrete(guide = "none") +
    ggtitle("SVM Decision Boundary (test set sample)")
)
dev.off()
message("Plot saved to img/SVM_decision_boundary.png")
