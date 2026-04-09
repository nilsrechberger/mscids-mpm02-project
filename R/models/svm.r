library(here)
library(e1071)

data <- readRDS(here("data", "train.rds"))

set.seed(42)
indices <- sample(nrow(data), 5000)

dat_sub <- data.frame(
  N = data$AccidentLocation_CHLV95_N[indices],
  E = data$AccidentLocation_CHLV95_E[indices],
  Severity = as.factor(data$AccidentSeverityCategory_en[indices])
)

w <- table(dat_sub$Severity)
weights <- 1 / (w / sum(w))

svmfit = svm(Severity ~ N + E, 
             data = dat_sub, 
             kernel = "radial", 
             gamma = 1,      
             cost = 10,      
             class.weights = weights)

plot(svmfit, dat_sub)