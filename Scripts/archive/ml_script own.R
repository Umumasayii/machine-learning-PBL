rm(list = ls())

library(tidyverse)
library(caret)
library(readr)
path <- "Data//RW_LFS2025_Youth_Model.csv"

df_m <- read_csv(path)

df_m <- df_m %>% 
  mutate(across(where(is.character), as.factor)) %>% 
  mutate(across(where(is.numeric), as.numeric)) %>% 
  select(-c(province,household_id))

df_m <- df_m %>% 
  mutate(employment_status = factor(employment_status, levels = c("Unemployed","Employed")))
# View(df_m)
set.seed(123)
ids <- createDataPartition(df_m$employment_status,
                           p = 0.7,list = F)

train <- df_m[ids,]
test <- df_m[-ids,]

cat("Train Obs: ", nrow(train),"\n", "Test Obs: ", nrow(test), "\n")

ctrl <- trainControl(method = "cv", 
                     number = 5,
                     classProbs = T,
                     summaryFunction = twoClassSummary, 
                     sampling = "down")

formula = employment_status~.
models <- list(
  m_log = caret::train(formula, data = train, method = "glm", trControl = ctrl),
  m_dt  = caret::train(formula, data = train, method = "rpart", trControl = ctrl),
  
  # Regularized Discriminant Analysis (avoids within-class rank deficiency)
  m_rda = caret::train(
    formula, 
    data = train, 
    method = "rda",
    tuneGrid = expand.grid(gamma = c(0.1, 0.5, 1), lambda = c(0.1, 0.5, 1)),
    trControl = ctrl
  ),
  
  m_lda = caret::train(formula, data = train, method = "lda", trControl = ctrl),
  
  # glmnet with alpha = 1 is Lasso for classification
  m_las = caret::train(
    formula, 
    data = train, 
    method = "glmnet",
    tuneGrid = expand.grid(alpha = 1, lambda = 10^seq(-4, -1, length = 10)),
    trControl = ctrl
  ),
  
  # Scale numeric predictors for kNN
  m_knn = caret::train(
    formula, 
    data = train, 
    method = "knn", 
    preProcess = c("center", "scale"),
    trControl = ctrl, 
    tuneGrid = data.frame(k = 5)
  ),
  
  m_nb  = caret::train(formula, data = train, method = "naive_bayes", trControl = ctrl)
)

models$m_log

list_pred = list(
  pred_log = predict(models$m_log, newdata = test),
  pred_dt = predict(models$m_dt, newdata = test),
  pred_rda = predict(models$m_rda, newdata = test),
  pred_lda = predict(models$m_lda, newdata = test),
  pred_las = predict(models$m_las, newdata = test),
  pred_knn = predict(models$m_knn, newdata = test),
  pred_nb = predict(models$m_nb, newdata = test)
)

list_con <- list(
  con_log = confusionMatrix(list_pred$pred_log, test$employment_status),
  con_dt = confusionMatrix(list_pred$pred_dt, test$employment_status),
  con_rda = confusionMatrix(list_pred$pred_rda, test$employment_status),
  con_lda = confusionMatrix(list_pred$pred_lda, test$employment_status),
  con_las = confusionMatrix(list_pred$pred_las, test$employment_status),
  con_knn = confusionMatrix(list_pred$pred_knn, test$employment_status),
  con_nb = confusionMatrix(list_pred$pred_nb, test$employment_status)
)

list_acc <- list(
  acc_log = list_con$con_log$overall["Accuracy"],
  acc_dt = list_con$con_dt$overall["Accuracy"],
  acc_rda = list_con$con_rda$overall["Accuracy"],
  acc_lda = list_con$con_lda$overall["Accuracy"],
  acc_las = list_con$con_las$overall["Accuracy"],
  acc_knn = list_con$con_knn$overall["Accuracy"],
  acc_nb = list_con$con_nb$overall["Accuracy"]
)

acc_df <- data.frame(
  Logistic = list_acc$acc_log,
  Decision_t = list_acc$acc_dt,
  QDA = list_acc$acc_rda,
  LDA = list_acc$acc_lda,
  LAS = list_acc$acc_las,
  KNN = list_acc$acc_knn,
  Bayes = list_acc$acc_nb
)

knitr::kable(acc_df)

test_err <- data.frame(
  test_log = 1-acc_df$Logistic,
  test_dt = 1-acc_df$Decision_t,
  test_qda = 1-acc_df$QDA,
  test_lda = 1-acc_df$LDA,
  test_las = 1-acc_df$LAS,
  test_knn = 1-acc_df$KNN,
  test_bayes = 1-acc_df$Bayes
)

knitr::kable(test_err)


cat("The Best model is LOgistic Regression")
