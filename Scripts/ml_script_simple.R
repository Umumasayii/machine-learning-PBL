library(readr)
library(dplyr)
library(ggplot2)
library(caret)
library(pROC)

dir.create("Outputs/ML_simple/figures", recursive = TRUE, showWarnings = FALSE)

path <- "Data/RW_LFS2025_Youth_Model.csv"

df_m <- read_csv(path, trim_ws = TRUE, show_col_types = FALSE)

df_m <- df_m %>%
  mutate(across(where(is.character), as.factor)) %>%
  select(-c(province, household_id)) %>%   # province repeats district; id is not a predictor
  as.data.frame()                          # caret's varImp() needs a data.frame, not a tibble

# "Unemployed" must be the FIRST level, because caret treats it as the positive class
df_m$employment_status <- factor(df_m$employment_status, levels = c("Unemployed", "Employed"))

cat("Rows: ", nrow(df_m), "\n", "Predictors: ", ncol(df_m) - 1, "\n")
prop.table(table(df_m$employment_status))   # about 15% unemployed


# 2. Train / test split (70 / 30)
set.seed(123)
ids <- createDataPartition(df_m$employment_status, p = 0.7, list = FALSE)

train <- df_m[ids, ]
test  <- df_m[-ids, ]

cat("Train Obs: ", nrow(train), "\n", "Test Obs: ", nrow(test), "\n")


set.seed(123)
folds <- createFolds(train$employment_status, k = 5, returnTrain = TRUE)

ctrl <- trainControl(method = "cv",
                     index = folds,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     sampling = "down")

formula <- employment_status ~ .


m_log <- train(formula, data = train, method = "glm", family = "binomial",
               metric = "ROC", trControl = ctrl)

m_dt <- train(formula, data = train, method = "rpart",
              tuneLength = 10,
              metric = "ROC", trControl = ctrl)

# Regularized Discriminant Analysis (avoids within-class rank deficiency)
m_rda <- train(formula, data = train, method = "rda",
               tuneGrid = expand.grid(gamma = c(0.1, 0.5, 1), lambda = c(0.1, 0.5, 1)),
               metric = "ROC", trControl = ctrl)

m_lda <- train(formula, data = train, method = "lda",
               metric = "ROC", trControl = ctrl)

# glmnet with alpha = 1 is the Lasso
m_las <- train(formula, data = train, method = "glmnet",
               tuneGrid = expand.grid(alpha = 1, lambda = 10^seq(-4, -1, length = 10)),
               metric = "ROC", trControl = ctrl)

# kNN needs scaled predictors; we try k = 5, 11, 17, ..., 47
m_knn <- train(formula, data = train, method = "knn",
               preProcess = c("center", "scale"),
               tuneGrid = data.frame(k = seq(5, 47, by = 6)),
               metric = "ROC", trControl = ctrl)

m_nb <- train(x = train[, names(train) != "employment_status"],
              y = train$employment_status,
              method = "naive_bayes",
              tuneGrid = expand.grid(laplace = 1, usekernel = FALSE, adjust = 1),
              metric = "ROC", trControl = ctrl)

# Elastic net: a mix of Lasso (alpha = 1) and Ridge (alpha = 0); caret tries both
m_enet <- train(formula, data = train, method = "glmnet",
                tuneGrid = expand.grid(alpha = c(0, 0.25, 0.5, 0.75, 1),
                                       lambda = 10^seq(-4, -1, length = 10)),
                metric = "ROC", trControl = ctrl)

# Random forest (ranger): many decision trees, each on a random sample of rows
# and predictors; their votes are averaged.
#  - x / y form, so ranger uses the factors directly
#  - mtry = number of predictors tried at each split
m_rf <- train(x = train[, names(train) != "employment_status"],
              y = train$employment_status,
              method = "ranger",
              num.trees = 300,
              importance = "impurity",
              tuneGrid = expand.grid(mtry = c(2, 3, 4, 6),
                                     splitrule = "gini",
                                     min.node.size = c(10, 30, 60)),
              metric = "ROC", trControl = ctrl)

# Gradient boosting (gbm): small trees built one after another, each one
# correcting the mistakes of the trees before it.
m_gbm <- train(formula, data = train, method = "gbm",
               tuneGrid = expand.grid(n.trees = c(100, 200, 300),
                                      interaction.depth = c(1, 2, 3),
                                      shrinkage = 0.05,
                                      n.minobsinnode = 20),
               verbose = FALSE,
               metric = "ROC", trControl = ctrl)

# Best tuning values
m_dt$bestTune
m_rda$bestTune
m_las$bestTune
m_knn$bestTune
m_enet$bestTune
m_rf$bestTune
m_gbm$bestTune


# -----------------------------------------------------------------------------
# 5. Compare the models on cross-validation
# -----------------------------------------------------------------------------
models <- list(Logistic = m_log, Decision_tree = m_dt, RDA = m_rda, LDA = m_lda,
               Lasso = m_las, KNN = m_knn, Naive_Bayes = m_nb,
               Elastic_net = m_enet, Random_forest = m_rf, GBM = m_gbm)

cv_results <- resamples(models)
summary(cv_results)

png("Outputs/ML_simple/figures/01_cv_comparison.png", width = 1600, height = 1000, res = 200)
print(dotplot(cv_results, metric = "ROC", main = "Cross-validated ROC AUC (5 folds)"))
dev.off()   # print() is needed so the plot is saved when the script is sourced


test_x <- test[, names(test) != "employment_status"]

# Predicted class (Unemployed / Employed)
pred_log <- predict(m_log, newdata = test_x)
pred_dt  <- predict(m_dt,  newdata = test_x)
pred_rda <- predict(m_rda, newdata = test_x)
pred_lda <- predict(m_lda, newdata = test_x)
pred_las <- predict(m_las, newdata = test_x)
pred_knn <- predict(m_knn, newdata = test_x)
pred_nb  <- predict(m_nb,  newdata = test_x)
pred_enet <- predict(m_enet, newdata = test_x)
pred_rf   <- predict(m_rf,   newdata = test_x)
pred_gbm  <- predict(m_gbm,  newdata = test_x)

# Predicted probability of being unemployed (needed for the ROC curve)
prob_log <- predict(m_log, newdata = test_x, type = "prob")$Unemployed
prob_dt  <- predict(m_dt,  newdata = test_x, type = "prob")$Unemployed
prob_rda <- predict(m_rda, newdata = test_x, type = "prob")$Unemployed
prob_lda <- predict(m_lda, newdata = test_x, type = "prob")$Unemployed
prob_las <- predict(m_las, newdata = test_x, type = "prob")$Unemployed
prob_knn <- predict(m_knn, newdata = test_x, type = "prob")$Unemployed
prob_nb  <- predict(m_nb,  newdata = test_x, type = "prob")$Unemployed
prob_enet <- predict(m_enet, newdata = test_x, type = "prob")$Unemployed
prob_rf   <- predict(m_rf,   newdata = test_x, type = "prob")$Unemployed
prob_gbm  <- predict(m_gbm,  newdata = test_x, type = "prob")$Unemployed


# -----------------------------------------------------------------------------
# 7. Confusion matrices and ROC curves
# -----------------------------------------------------------------------------
actual <- test$employment_status

con_log <- confusionMatrix(pred_log, actual, positive = "Unemployed", mode = "everything")
con_dt  <- confusionMatrix(pred_dt,  actual, positive = "Unemployed", mode = "everything")
con_rda <- confusionMatrix(pred_rda, actual, positive = "Unemployed", mode = "everything")
con_lda <- confusionMatrix(pred_lda, actual, positive = "Unemployed", mode = "everything")
con_las <- confusionMatrix(pred_las, actual, positive = "Unemployed", mode = "everything")
con_knn <- confusionMatrix(pred_knn, actual, positive = "Unemployed", mode = "everything")
con_nb  <- confusionMatrix(pred_nb,  actual, positive = "Unemployed", mode = "everything")
con_enet <- confusionMatrix(pred_enet, actual, positive = "Unemployed", mode = "everything")
con_rf   <- confusionMatrix(pred_rf,   actual, positive = "Unemployed", mode = "everything")
con_gbm  <- confusionMatrix(pred_gbm,  actual, positive = "Unemployed", mode = "everything")

roc_log <- roc(actual, prob_log, levels = c("Employed", "Unemployed"), direction = "<")
roc_dt  <- roc(actual, prob_dt,  levels = c("Employed", "Unemployed"), direction = "<")
roc_rda <- roc(actual, prob_rda, levels = c("Employed", "Unemployed"), direction = "<")
roc_lda <- roc(actual, prob_lda, levels = c("Employed", "Unemployed"), direction = "<")
roc_las <- roc(actual, prob_las, levels = c("Employed", "Unemployed"), direction = "<")
roc_knn <- roc(actual, prob_knn, levels = c("Employed", "Unemployed"), direction = "<")
roc_nb  <- roc(actual, prob_nb,  levels = c("Employed", "Unemployed"), direction = "<")
roc_enet <- roc(actual, prob_enet, levels = c("Employed", "Unemployed"), direction = "<")
roc_rf   <- roc(actual, prob_rf,   levels = c("Employed", "Unemployed"), direction = "<")
roc_gbm  <- roc(actual, prob_gbm,  levels = c("Employed", "Unemployed"), direction = "<")


# -----------------------------------------------------------------------------
# 8. Results table
# -----------------------------------------------------------------------------
results <- data.frame(
  Model = c("Logistic", "Decision_tree", "RDA", "LDA", "Lasso", "KNN", "Naive_Bayes",
            "Elastic_net", "Random_forest", "GBM"),

  ROC_AUC = c(auc(roc_log), auc(roc_dt), auc(roc_rda), auc(roc_lda),
              auc(roc_las), auc(roc_knn), auc(roc_nb),
              auc(roc_enet), auc(roc_rf), auc(roc_gbm)),

  Accuracy = c(con_log$overall["Accuracy"], con_dt$overall["Accuracy"],
               con_rda$overall["Accuracy"], con_lda$overall["Accuracy"],
               con_las$overall["Accuracy"], con_knn$overall["Accuracy"],
               con_nb$overall["Accuracy"], con_enet$overall["Accuracy"],
               con_rf$overall["Accuracy"], con_gbm$overall["Accuracy"]),

  # Sensitivity = share of unemployed youth the model finds (recall)
  Sensitivity = c(con_log$byClass["Sensitivity"], con_dt$byClass["Sensitivity"],
                  con_rda$byClass["Sensitivity"], con_lda$byClass["Sensitivity"],
                  con_las$byClass["Sensitivity"], con_knn$byClass["Sensitivity"],
                  con_nb$byClass["Sensitivity"], con_enet$byClass["Sensitivity"],
                  con_rf$byClass["Sensitivity"], con_gbm$byClass["Sensitivity"]),

  Specificity = c(con_log$byClass["Specificity"], con_dt$byClass["Specificity"],
                  con_rda$byClass["Specificity"], con_lda$byClass["Specificity"],
                  con_las$byClass["Specificity"], con_knn$byClass["Specificity"],
                  con_nb$byClass["Specificity"], con_enet$byClass["Specificity"],
                  con_rf$byClass["Specificity"], con_gbm$byClass["Specificity"]),

  Precision = c(con_log$byClass["Precision"], con_dt$byClass["Precision"],
                con_rda$byClass["Precision"], con_lda$byClass["Precision"],
                con_las$byClass["Precision"], con_knn$byClass["Precision"],
                con_nb$byClass["Precision"], con_enet$byClass["Precision"],
                con_rf$byClass["Precision"], con_gbm$byClass["Precision"]),

  F1 = c(con_log$byClass["F1"], con_dt$byClass["F1"], con_rda$byClass["F1"],
         con_lda$byClass["F1"], con_las$byClass["F1"], con_knn$byClass["F1"],
         con_nb$byClass["F1"], con_enet$byClass["F1"], con_rf$byClass["F1"],
         con_gbm$byClass["F1"]),

  Kappa = c(con_log$overall["Kappa"], con_dt$overall["Kappa"], con_rda$overall["Kappa"],
            con_lda$overall["Kappa"], con_las$overall["Kappa"], con_knn$overall["Kappa"],
            con_nb$overall["Kappa"], con_enet$overall["Kappa"], con_rf$overall["Kappa"],
            con_gbm$overall["Kappa"])
)

results <- results %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(desc(ROC_AUC))

knitr::kable(results)
write_csv(results, "Outputs/ML_simple/test_results.csv")

# Reference: a "model" that always says Employed
cat("Accuracy if we always predict 'Employed': ", mean(actual == "Employed"), "\n")


# -----------------------------------------------------------------------------
# 9. Best model (highest ROC AUC on the test set)
# -----------------------------------------------------------------------------
best <- results$Model[1]
cat("The best model is", best, "with ROC AUC =", results$ROC_AUC[1], "\n")

best_model <- models[[best]]

# Confusion matrix of the best model
best_pred <- predict(best_model, newdata = test_x)
best_con  <- confusionMatrix(best_pred, actual, positive = "Unemployed", mode = "everything")
best_con
capture.output(best_con, file = "Outputs/ML_simple/confusion_matrix_best.txt")


roc_list <- list(Logistic = roc_log, Decision_tree = roc_dt, RDA = roc_rda, LDA = roc_lda,
                 Lasso = roc_las, KNN = roc_knn, Naive_Bayes = roc_nb,
                 Elastic_net = roc_enet, Random_forest = roc_rf, GBM = roc_gbm)

roc_plot <- ggroc(roc_list, linewidth = 0.9) +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed", colour = "grey50") +
  labs(title = "ROC curves on the test set", colour = "Model",
       x = "Specificity", y = "Sensitivity (recall of Unemployed)") +
  theme_minimal()

roc_plot
ggsave("Outputs/ML_simple/figures/02_roc_curves.png", roc_plot, width = 8, height = 6, dpi = 200)

importance <- varImp(best_model)
importance

png("Outputs/ML_simple/figures/03_feature_importance.png", width = 1600, height = 1200, res = 200)
print(plot(importance, top = 15, main = paste("Top 15 features -", best)))
dev.off()


# -----------------------------------------------------------------------------
# 11. Save what the Shiny dashboard needs (the app uses the best model)
# -----------------------------------------------------------------------------
# Predicted probability of "Unemployed" for every test person and every model
test_probs <- data.frame(
  actual        = actual,
  Logistic      = prob_log,
  Decision_tree = prob_dt,
  RDA           = prob_rda,
  LDA           = prob_lda,
  Lasso         = prob_las,
  KNN           = prob_knn,
  Naive_Bayes   = prob_nb,
  Elastic_net   = prob_enet,
  Random_forest = prob_rf,
  GBM           = prob_gbm
)

cv_roc <- summary(cv_results)$statistics$ROC

dashboard <- list(
  app_model  = best_model,                 # the model with the highest test ROC AUC
  app_name   = best,
  results    = results,
  cv_roc     = cv_roc,
  test_probs = test_probs,
  importance = varImp(best_model)$importance
)
# 
saveRDS(dashboard, "Outputs/ML_simple/dashboard_model.rds")
cat("Saved: Outputs/ML_simple/dashboard_model.rds\n")


# -----------------------------------------------------------------------------
# 12. Tuning results (which hyper-parameters were tried and which won)
# -----------------------------------------------------------------------------
# For every model, caret tried each value in the tuning grid on the 5 folds
# and kept the value with the highest cross-validated ROC AUC.

tuning <- data.frame(
  Model = c("Decision tree", "RDA", "Lasso", "Elastic net", "KNN",
            "Random forest", "GBM", "Naive Bayes"),

  Parameters_tuned = c("cp (tree complexity)",
                       "gamma, lambda",
                       "lambda (alpha fixed at 1)",
                       "alpha, lambda",
                       "k (number of neighbours)",
                       "mtry, min.node.size",
                       "n.trees, interaction.depth",
                       "laplace fixed at 1"),

  Combinations_tried = c(nrow(m_dt$results), nrow(m_rda$results), nrow(m_las$results),
                         nrow(m_enet$results), nrow(m_knn$results), nrow(m_rf$results),
                         nrow(m_gbm$results), nrow(m_nb$results)),

  Best_values = c(paste("cp =", signif(m_dt$bestTune$cp, 3)),
                  paste("gamma =", m_rda$bestTune$gamma, ", lambda =", m_rda$bestTune$lambda),
                  paste("lambda =", signif(m_las$bestTune$lambda, 3)),
                  paste("alpha =", m_enet$bestTune$alpha, ", lambda =", signif(m_enet$bestTune$lambda, 3)),
                  paste("k =", m_knn$bestTune$k),
                  paste("mtry =", m_rf$bestTune$mtry, ", min.node.size =", m_rf$bestTune$min.node.size),
                  paste("n.trees =", m_gbm$bestTune$n.trees, ", depth =", m_gbm$bestTune$interaction.depth),
                  "laplace = 1"),

  # the best cross-validated ROC AUC is the one of the chosen values
  Best_CV_ROC = c(max(m_dt$results$ROC), max(m_rda$results$ROC), max(m_las$results$ROC),
                  max(m_enet$results$ROC), max(m_knn$results$ROC), max(m_rf$results$ROC),
                  max(m_gbm$results$ROC), max(m_nb$results$ROC))
)

tuning$Best_CV_ROC <- round(tuning$Best_CV_ROC, 3)
knitr::kable(tuning)
write_csv(tuning, "Outputs/ML_simple/tuning_results.csv")

# Tuning plots: cross-validated ROC AUC for every value that was tried
png("Outputs/ML_simple/figures/04_tuning_random_forest.png", width = 1600, height = 1000, res = 200)
print(plot(m_rf, main = "Random forest: CV ROC AUC by mtry and min.node.size"))
dev.off()

png("Outputs/ML_simple/figures/05_tuning_gbm.png", width = 1600, height = 1000, res = 200)
print(plot(m_gbm, main = "GBM: CV ROC AUC by number of trees and tree depth"))
dev.off()

png("Outputs/ML_simple/figures/06_tuning_knn.png", width = 1600, height = 1000, res = 200)
print(plot(m_knn, main = "KNN: CV ROC AUC by number of neighbours (k)"))
dev.off()

png("Outputs/ML_simple/figures/07_tuning_decision_tree.png", width = 1600, height = 1000, res = 200)
print(plot(m_dt, main = "Decision tree: CV ROC AUC by complexity (cp)"))
dev.off()

png("Outputs/ML_simple/figures/08_tuning_elastic_net.png", width = 1600, height = 1000, res = 200)
print(plot(m_enet, main = "Elastic net: CV ROC AUC by alpha and lambda"))
dev.off()


# -----------------------------------------------------------------------------
# 13. Is the random forest really better? (DeLong test on the test set)
# -----------------------------------------------------------------------------
# The DeLong test compares two ROC AUCs on the same test set.
# p-value < 0.05 -> the difference is statistically significant.

test_rf_gbm <- roc.test(roc_rf, roc_gbm, method = "delong")
test_rf_lda <- roc.test(roc_rf, roc_lda, method = "delong")
test_rf_las <- roc.test(roc_rf, roc_las, method = "delong")
test_rf_log <- roc.test(roc_rf, roc_log, method = "delong")
test_rf_knn <- roc.test(roc_rf, roc_knn, method = "delong")

significance <- data.frame(
  Comparison = c("Random forest vs GBM", "Random forest vs LDA", "Random forest vs Lasso",
                 "Random forest vs Logistic", "Random forest vs KNN"),
  AUC_difference = c(auc(roc_rf) - auc(roc_gbm), auc(roc_rf) - auc(roc_lda),
                     auc(roc_rf) - auc(roc_las), auc(roc_rf) - auc(roc_log),
                     auc(roc_rf) - auc(roc_knn)),
  p_value = c(test_rf_gbm$p.value, test_rf_lda$p.value, test_rf_las$p.value,
              test_rf_log$p.value, test_rf_knn$p.value)
)
significance$AUC_difference <- round(significance$AUC_difference, 3)
significance$p_value <- round(significance$p_value, 3)
significance$Significant <- ifelse(significance$p_value < 0.05, "Yes", "No")

knitr::kable(significance)
write_csv(significance, "Outputs/ML_simple/significance_tests.csv")

# 95% confidence interval of the random forest ROC AUC
ci_rf <- ci.auc(roc_rf)
cat("Random forest ROC AUC:", round(ci_rf[2], 3),
    " 95% CI:", round(ci_rf[1], 3), "to", round(ci_rf[3], 3), "\n")


# -----------------------------------------------------------------------------
# 14. Does the random forest work equally well for every group?
# -----------------------------------------------------------------------------
female <- test$sex == "Female"
male   <- test$sex == "Male"
urban  <- test$residence == "Urban"
rural  <- test$residence == "Rural"

con_female <- confusionMatrix(pred_rf[female], actual[female], positive = "Unemployed")
con_male   <- confusionMatrix(pred_rf[male],   actual[male],   positive = "Unemployed")
con_urban  <- confusionMatrix(pred_rf[urban],  actual[urban],  positive = "Unemployed")
con_rural  <- confusionMatrix(pred_rf[rural],  actual[rural],  positive = "Unemployed")

roc_female <- roc(actual[female], prob_rf[female], levels = c("Employed", "Unemployed"), direction = "<")
roc_male   <- roc(actual[male],   prob_rf[male],   levels = c("Employed", "Unemployed"), direction = "<")
roc_urban  <- roc(actual[urban],  prob_rf[urban],  levels = c("Employed", "Unemployed"), direction = "<")
roc_rural  <- roc(actual[rural],  prob_rf[rural],  levels = c("Employed", "Unemployed"), direction = "<")

subgroups <- data.frame(
  Group = c("Female", "Male", "Urban", "Rural"),
  Youth_in_test = c(sum(female), sum(male), sum(urban), sum(rural)),
  Unemployment_rate = c(mean(actual[female] == "Unemployed"), mean(actual[male] == "Unemployed"),
                        mean(actual[urban] == "Unemployed"), mean(actual[rural] == "Unemployed")),
  ROC_AUC = c(auc(roc_female), auc(roc_male), auc(roc_urban), auc(roc_rural)),
  Recall = c(con_female$byClass["Sensitivity"], con_male$byClass["Sensitivity"],
             con_urban$byClass["Sensitivity"], con_rural$byClass["Sensitivity"]),
  Specificity = c(con_female$byClass["Specificity"], con_male$byClass["Specificity"],
                  con_urban$byClass["Specificity"], con_rural$byClass["Specificity"])
)
subgroups$Unemployment_rate <- round(subgroups$Unemployment_rate, 3)
subgroups$ROC_AUC     <- round(subgroups$ROC_AUC, 3)
subgroups$Recall      <- round(subgroups$Recall, 3)
subgroups$Specificity <- round(subgroups$Specificity, 3)

knitr::kable(subgroups)
write_csv(subgroups, "Outputs/ML_simple/subgroup_performance.csv")


# -----------------------------------------------------------------------------
# 15. Direction of the effects (does a variable RAISE or LOWER the risk?)
# -----------------------------------------------------------------------------
# Feature importance only says WHICH variables matter. To see HOW they matter
# we use two tools:
#   a) odds ratios from the logistic regression (easy to read: above 1 raises
#      the risk, below 1 lowers it). Training on down-sampled data changes only
#      the intercept, not the odds ratios.
#   b) partial dependence plots from the random forest: the average risk score
#      when we change one variable and keep everything else as it is.

# a) Odds ratios -------------------------------------------------------------
coefs <- summary(m_log$finalModel)$coefficients

# Careful: glm() models the SECOND level of the outcome, which is "Employed".
# Its coefficients are for the odds of being employed, so we change their sign
# to get the odds of being UNEMPLOYED (exp(-b) = 1 / exp(b)).
estimate <- -coefs[, "Estimate"]
std_err  <- coefs[, "Std. Error"]

odds_ratios <- data.frame(
  Term       = gsub("`", "", rownames(coefs)),
  Odds_ratio = exp(estimate),
  Lower_95   = exp(estimate - 1.96 * std_err),
  Upper_95   = exp(estimate + 1.96 * std_err),
  p_value    = coefs[, 4],
  row.names  = NULL
)
odds_ratios <- odds_ratios %>%
  filter(Term != "(Intercept)") %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(p_value)

knitr::kable(head(odds_ratios, 20))
write_csv(odds_ratios, "Outputs/ML_simple/odds_ratios.csv")

# Plot: significant odds ratios (p < 0.05), districts left out to keep it readable
or_plot_data <- odds_ratios %>% filter(p_value < 0.05, !grepl("^district", Term))

or_plot <- ggplot(or_plot_data, aes(x = Odds_ratio, y = reorder(Term, Odds_ratio))) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(xmin = Lower_95, xmax = Upper_95), width = 0.2, orientation = "y") +
  geom_point(aes(colour = Odds_ratio > 1), size = 3, show.legend = FALSE) +
  scale_colour_manual(values = c("TRUE" = "#E76F51", "FALSE" = "#3AA3A2")) +
  scale_x_log10() +
  labs(title = "Odds ratios of unemployment (logistic regression, p < 0.05)",
       subtitle = "Right of the dashed line = higher risk; left = lower risk",
       x = "Odds ratio (log scale) with 95% CI", y = NULL) +
  theme_minimal()

or_plot
ggsave("Outputs/ML_simple/figures/09_odds_ratios.png", or_plot, width = 9, height = 7, dpi = 200)

# b) Partial dependence (random forest) ---------------------------------------
library(pdp)

# A random sample of 2,000 training youth keeps the computation fast
set.seed(123)
pdp_rows <- sample(nrow(train), 2000)
pdp_data <- train[pdp_rows, names(train) != "employment_status"]

pd_school <- partial(m_rf, pred.var = "years_of_schooling", train = pdp_data,
                     prob = TRUE, which.class = "Unemployed")
pd_age    <- partial(m_rf, pred.var = "age", train = pdp_data,
                     prob = TRUE, which.class = "Unemployed")
pd_hhsize <- partial(m_rf, pred.var = "household_size", train = pdp_data,
                     prob = TRUE, which.class = "Unemployed")
pd_sex    <- partial(m_rf, pred.var = "sex", train = pdp_data,
                     prob = TRUE, which.class = "Unemployed")
pd_edu    <- partial(m_rf, pred.var = "education_level", train = pdp_data,
                     prob = TRUE, which.class = "Unemployed")
pd_rel    <- partial(m_rf, pred.var = "household_relationship", train = pdp_data,
                     prob = TRUE, which.class = "Unemployed")

# Put the education levels in their natural order (not alphabetical)
pd_edu$education_level <- factor(pd_edu$education_level,
                                 levels = c("None", "Primary", "Lower secondary",
                                            "Upper secondary", "University"))

plot_school <- ggplot(pd_school, aes(x = years_of_schooling, y = yhat)) +
  geom_line(colour = "#3AA3A2", linewidth = 1.2) + geom_point(colour = "#3AA3A2") +
  labs(title = "Years of schooling", x = "Years of schooling", y = "Average risk score") +
  theme_minimal()

plot_age <- ggplot(pd_age, aes(x = age, y = yhat)) +
  geom_line(colour = "#3AA3A2", linewidth = 1.2) + geom_point(colour = "#3AA3A2") +
  labs(title = "Age", x = "Age (years)", y = "Average risk score") +
  theme_minimal()

plot_hhsize <- ggplot(pd_hhsize, aes(x = household_size, y = yhat)) +
  geom_line(colour = "#3AA3A2", linewidth = 1.2) + geom_point(colour = "#3AA3A2") +
  labs(title = "Household size", x = "Household members", y = "Average risk score") +
  theme_minimal()

plot_sex <- ggplot(pd_sex, aes(x = sex, y = yhat)) +
  geom_col(fill = "#3AA3A2", width = 0.6) +
  labs(title = "Sex", x = NULL, y = "Average risk score") +
  theme_minimal()

plot_edu <- ggplot(pd_edu, aes(x = education_level, y = yhat)) +
  geom_col(fill = "#3AA3A2", width = 0.6) +
  labs(title = "Education level", x = NULL, y = "Average risk score") +
  theme_minimal()

plot_rel <- ggplot(pd_rel, aes(x = yhat, y = reorder(household_relationship, yhat))) +
  geom_col(fill = "#3AA3A2", width = 0.6) +
  labs(title = "Relationship to household head", x = "Average risk score", y = NULL) +
  theme_minimal()

ggsave("Outputs/ML_simple/figures/10_pdp_schooling.png",      plot_school, width = 6, height = 4, dpi = 200)
ggsave("Outputs/ML_simple/figures/11_pdp_age.png",            plot_age,    width = 6, height = 4, dpi = 200)
ggsave("Outputs/ML_simple/figures/12_pdp_household_size.png", plot_hhsize, width = 6, height = 4, dpi = 200)
ggsave("Outputs/ML_simple/figures/13_pdp_sex.png",            plot_sex,    width = 5, height = 4, dpi = 200)
ggsave("Outputs/ML_simple/figures/14_pdp_education.png",      plot_edu,    width = 7, height = 4, dpi = 200)
ggsave("Outputs/ML_simple/figures/15_pdp_relationship.png",   plot_rel,    width = 7, height = 4, dpi = 200)

cat("Done. All results are in Outputs/ML_simple/\n")
