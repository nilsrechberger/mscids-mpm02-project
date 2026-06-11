library(here)
library(e1071)

# Load data
train_raw <- readRDS(here("data", "train.rds"))

prep_data <- function(df) {
  clean_df <- data.frame(
    N = df$AccidentLocation_CHLV95_N,
    E = df$AccidentLocation_CHLV95_E,
    Severity = as.factor(df$AccidentSeverityCategory_en)
  )
  na.omit(clean_df)
}

train_data <- prep_data(train_raw)

# PoC: subsample to keep training fast (~2000 obs per class cap)
set.seed(42)
POC_PER_CLASS <- 1000
train_data <- do.call(rbind, lapply(split(train_data, train_data$Severity), function(g) {
  g[sample(nrow(g), min(nrow(g), POC_PER_CLASS)), ]
}))

# Class weights to handle imbalance
w <- table(train_data$Severity)
weights <- 1 / (w / sum(w))

cat("Tuning SVM via 5-fold CV grid search (n =", nrow(train_data), ")...\n")

tune_result <- tune.svm(
  Severity ~ N + E,
  data          = train_data,
  kernel        = "radial",
  cost          = c(1, 10, 50),
  gamma         = c(0.01, 0.1, 1),
  class.weights = weights,
  tunecontrol   = tune.control(sampling = "cross", cross = 5)
)

cat("\nBest hyperparameters:\n")
print(tune_result$best.parameters)
cat("Best CV error:", tune_result$best.performance, "\n")

# Refit best model with probability support
best_params <- tune_result$best.parameters
svm_model <- svm(
  Severity ~ N + E,
  data          = train_data,
  kernel        = "radial",
  cost          = best_params$cost,
  gamma         = best_params$gamma,
  class.weights = weights,
  probability   = TRUE
)

if (!dir.exists(here("models"))) dir.create(here("models"))
saveRDS(svm_model, here("models", "svm_mvp_model.rds"))

cat("Model saved to models/svm_mvp_model.rds\n")