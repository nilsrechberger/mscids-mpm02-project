# 5-fold Cross Validation

set.seed(42)

k <- 5
n <- nrow(train)
folds <- sample(rep(seq_len(k), length.out = n))

cv_auc <- sapply(seq_len(k), function(i) {
  tr <- train[folds != i, ]
  te <- train[folds == i, ]
  
  model <- glm(
    AccidentInvolvingPedestrian ~
      AccidentSeverityCategory_en +
      hour_num +
      month_fac,
    data = tr,
    family = binomial
  )
  
  pred <- predict(model, te, type = "response")
  pROC::roc(te$AccidentInvolvingPedestrian, pred)$auc
})

cat("5-fold CV AUC:", round(mean(cv_auc), 3),
    "+/-", round(sd(cv_auc), 3), "\n")