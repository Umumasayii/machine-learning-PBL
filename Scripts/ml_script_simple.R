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

# -----------------------------------------------------------------------------
# 4. Train the models (the same list as PyCaret's compare_models(), plus a few)
# -----------------------------------------------------------------------------
# Every model uses the same 5 folds, so their results can be compared fairly.
set.seed(123)   # down-sampling is random: this makes the results repeatable

# Logistic regression
m_log <- train(formula, data = train, method = "glm", family = "binomial",
               metric = "ROC", trControl = ctrl)

# Naive Bayes (x / y form, so the factors are used directly)
m_nb <- train(x = train[, names(train) != "employment_status"],
              y = train$employment_status,
              method = "naive_bayes",
              tuneGrid = expand.grid(laplace = 1, usekernel = FALSE, adjust = 1),
              metric = "ROC", trControl = ctrl)

# Ridge: glmnet with alpha = 0 (all coefficients are shrunk, none set to zero)
m_ridge <- train(formula, data = train, method = "glmnet",
                 tuneGrid = expand.grid(alpha = 0, lambda = 10^seq(-3, 1, length = 10)),
                 metric = "ROC", trControl = ctrl)

# Dummy model: caret's "null" model always predicts the most common class.
# It is trained WITHOUT down-sampling, so the most common class is "Employed".
# It is the reference: a useful model must do better than this.
ctrl_dummy <- trainControl(method = "cv",
                           index = folds,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)

m_dummy <- train(formula, data = train, method = "null",
                 metric = "ROC", trControl = ctrl_dummy)

# AdaBoost: boosting with the exponential (AdaBoost) loss, fitted with gbm
m_ada <- train(formula, data = train, method = "gbm",
               distribution = "adaboost",
               tuneGrid = expand.grid(n.trees = c(100, 200, 300),
                                      interaction.depth = c(1, 2, 3),
                                      shrinkage = 0.05,
                                      n.minobsinnode = 20),
               verbose = FALSE,
               metric = "ROC", trControl = ctrl)

# Linear Discriminant Analysis
m_lda <- train(formula, data = train, method = "lda",
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

# Random forest (ranger): many decision trees, each on a random sample of rows
# and predictors; their votes are averaged.
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

# kNN needs scaled predictors; we try k = 5, 11, 17, ..., 47
m_knn <- train(formula, data = train, method = "knn",
               preProcess = c("center", "scale"),
               tuneGrid = data.frame(k = seq(5, 47, by = 6)),
               metric = "ROC", trControl = ctrl)

# Extra trees: like a random forest, but the split points are chosen at random
m_et <- train(x = train[, names(train) != "employment_status"],
              y = train$employment_status,
              method = "ranger",
              num.trees = 300,
              importance = "impurity",
              tuneGrid = expand.grid(mtry = c(2, 4, 6, 8),
                                     splitrule = "extratrees",
                                     min.node.size = c(10, 30)),
              metric = "ROC", trControl = ctrl)

# Support vector machine with a linear kernel (needs scaled predictors)
m_svm <- train(formula, data = train, method = "svmLinear",
               preProcess = c("center", "scale"),
               tuneGrid = data.frame(C = c(0.01, 0.1, 1)),
               metric = "ROC", trControl = ctrl)

# Decision tree
m_dt <- train(formula, data = train, method = "rpart",
              tuneLength = 10,
              metric = "ROC", trControl = ctrl)

# Quadratic Discriminant Analysis. Plain QDA (method = "qda") stops with
# "rank deficiency": some dummy variables (e.g. small districts) never change
# inside the unemployed group. RDA with lambda = 0 is QDA with a little
# shrinkage (gamma) of the covariance matrices, which solves this.
m_qda <- train(formula, data = train, method = "rda",
               tuneGrid = expand.grid(gamma = c(0.05, 0.1, 0.3, 0.5), lambda = 0),
               metric = "ROC", trControl = ctrl)

# Regularized Discriminant Analysis (a mix between LDA and QDA)
m_rda <- train(formula, data = train, method = "rda",
               tuneGrid = expand.grid(gamma = c(0.1, 0.5, 1), lambda = c(0.1, 0.5, 1)),
               metric = "ROC", trControl = ctrl)

# Lasso: glmnet with alpha = 1
m_las <- train(formula, data = train, method = "glmnet",
               tuneGrid = expand.grid(alpha = 1, lambda = 10^seq(-4, -1, length = 10)),
               metric = "ROC", trControl = ctrl)

# Elastic net: a mix of Lasso (alpha = 1) and Ridge (alpha = 0); caret tries both
m_enet <- train(formula, data = train, method = "glmnet",
                tuneGrid = expand.grid(alpha = c(0, 0.25, 0.5, 0.75, 1),
                                       lambda = 10^seq(-4, -1, length = 10)),
                metric = "ROC", trControl = ctrl)


# All models in one named list. The names are used in every table and chart.
models <- list(Logistic      = m_log,
               Naive_Bayes   = m_nb,
               Ridge         = m_ridge,
               Dummy         = m_dummy,
               AdaBoost      = m_ada,
               LDA           = m_lda,
               GBM           = m_gbm,
               Random_forest = m_rf,
               KNN           = m_knn,
               Extra_trees   = m_et,
               SVM_linear    = m_svm,
               Decision_tree = m_dt,
               QDA           = m_qda,
               RDA           = m_rda,
               Lasso         = m_las,
               Elastic_net   = m_enet)

baseline <- "Dummy"   # the reference model, left out when we pick the best model


# -----------------------------------------------------------------------------
# 5. Compare the models on cross-validation
# -----------------------------------------------------------------------------
cv_results <- resamples(models)
summary(cv_results)

png("Outputs/ML_simple/figures/01_cv_comparison.png", width = 1600, height = 1200, res = 200)
print(dotplot(cv_results, metric = "ROC", main = "Cross-validated ROC AUC (5 folds)"))
dev.off()   # print() is needed so the plot is saved when the script is sourced


# -----------------------------------------------------------------------------
# 6. Test-set results of every model
# -----------------------------------------------------------------------------
test_x <- test[, names(test) != "employment_status"]
actual <- test$employment_status

# Matthews correlation coefficient (MCC) from a 2 x 2 confusion table.
# It goes from -1 to 1; 0 means "no better than guessing".
mcc <- function(tab) {
  tp <- as.numeric(tab["Unemployed", "Unemployed"])
  tn <- as.numeric(tab["Employed",   "Employed"])
  fp <- as.numeric(tab["Unemployed", "Employed"])
  fn <- as.numeric(tab["Employed",   "Unemployed"])
  bottom <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  if (bottom == 0) return(0)
  (tp * tn - fp * fn) / bottom
}

results    <- data.frame()             # one row per model
roc_list   <- list()                   # ROC curve of every model
test_probs <- data.frame(actual = actual)   # risk score of every test person

for (name in names(models)) {
  model <- models[[name]]

  # Predicted class and predicted probability of being unemployed
  pred <- predict(model, newdata = test_x)
  prob <- predict(model, newdata = test_x, type = "prob")$Unemployed

  con     <- confusionMatrix(pred, actual, positive = "Unemployed", mode = "everything")
  roc_obj <- roc(actual, prob, levels = c("Employed", "Unemployed"), direction = "<", quiet = TRUE)

  roc_list[[name]]   <- roc_obj
  test_probs[[name]] <- prob

  results <- rbind(results, data.frame(
    Model       = name,
    Method      = model$method,                  # caret method name
    ROC_AUC     = as.numeric(auc(roc_obj)),
    Accuracy    = con$overall[["Accuracy"]],
    Sensitivity = con$byClass[["Sensitivity"]],  # recall: share of unemployed youth found
    Specificity = con$byClass[["Specificity"]],
    Precision   = con$byClass[["Precision"]],
    F1          = con$byClass[["F1"]],
    Kappa       = con$overall[["Kappa"]],
    MCC         = mcc(con$table),
    Train_time_sec = model$times$everything[["elapsed"]]   # tuning + final fit
  ))
}

results <- results %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(desc(ROC_AUC))

knitr::kable(results)
write_csv(results, "Outputs/ML_simple/test_results.csv")


# -----------------------------------------------------------------------------
# 7. Best model (highest ROC AUC on the test set, the dummy is not a candidate)
# -----------------------------------------------------------------------------
candidates <- results %>% filter(Model != baseline)
best <- candidates$Model[1]
cat("The best model is", best, "with ROC AUC =", candidates$ROC_AUC[1], "\n")

best_model <- models[[best]]
best_pred  <- predict(best_model, newdata = test_x)
best_prob  <- test_probs[[best]]

# Confusion matrix of the best model
best_con <- confusionMatrix(best_pred, actual, positive = "Unemployed", mode = "everything")
best_con
capture.output(best_con, file = "Outputs/ML_simple/confusion_matrix_best.txt")

# ROC curves (the dummy is the dashed diagonal line, so it is not drawn)
roc_plot <- ggroc(roc_list[names(roc_list) != baseline], linewidth = 0.8) +
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
# 8. Save what the Shiny dashboard needs (the app uses the best model)
# -----------------------------------------------------------------------------
cv_roc <- summary(cv_results)$statistics$ROC

dashboard <- list(
  app_model  = best_model,       # the model with the highest test ROC AUC
  app_name   = best,
  baseline   = baseline,         # the dummy model, shown as the reference row
  results    = results,
  cv_roc     = cv_roc,
  test_probs = test_probs,
  importance = varImp(best_model)$importance
)

saveRDS(dashboard, "Outputs/ML_simple/dashboard_model.rds")
cat("Saved: Outputs/ML_simple/dashboard_model.rds\n")


# -----------------------------------------------------------------------------
# 9. Tuning results (which hyper-parameters were tried and which won)
# -----------------------------------------------------------------------------
# For every model, caret tried each row of its tuning grid on the 5 folds
# and kept the row with the highest cross-validated ROC AUC (bestTune).

tuning <- data.frame()

for (name in names(models)) {
  model <- models[[name]]
  tuned <- model$bestTune

  tuning <- rbind(tuning, data.frame(
    Model              = name,
    Parameters         = paste(names(tuned), collapse = ", "),
    Combinations_tried = nrow(model$results),
    Best_values        = paste(names(tuned), "=", unlist(format(tuned, digits = 3)), collapse = ", "),   # numbers to 3 digits
    Best_CV_ROC        = round(max(model$results$ROC, na.rm = TRUE), 3)
  ))

  # Tuning plot for every model that tried more than one value
  if (nrow(model$results) > 1) {
    png(paste0("Outputs/ML_simple/figures/04_tuning_", tolower(name), ".png"),
        width = 1600, height = 1000, res = 200)
    print(plot(model, main = paste(gsub("_", " ", name), ": CV ROC AUC for each value tried")))
    dev.off()
  }
}

knitr::kable(tuning)
write_csv(tuning, "Outputs/ML_simple/tuning_results.csv")


# -----------------------------------------------------------------------------
# 10. Is the best model really better? (DeLong test on the test set)
# -----------------------------------------------------------------------------
# The DeLong test compares two ROC AUCs on the same test set.
# p-value < 0.05 -> the difference is statistically significant.

significance <- data.frame()

for (name in candidates$Model[-1]) {     # every model except the best one
  delong <- roc.test(roc_list[[best]], roc_list[[name]], method = "delong")

  significance <- rbind(significance, data.frame(
    Comparison     = paste(gsub("_", " ", best), "vs", gsub("_", " ", name)),
    AUC_difference = round(auc(roc_list[[best]]) - auc(roc_list[[name]]), 3),
    p_value        = round(delong$p.value, 3)
  ))
}
significance$Significant <- ifelse(significance$p_value < 0.05, "Yes", "No")

knitr::kable(significance)
write_csv(significance, "Outputs/ML_simple/significance_tests.csv")

# 95% confidence interval of the best model's ROC AUC
ci_best <- ci.auc(roc_list[[best]])
cat(best, "ROC AUC:", round(ci_best[2], 3),
    " 95% CI:", round(ci_best[1], 3), "to", round(ci_best[3], 3), "\n")


# -----------------------------------------------------------------------------
# 11. Does the best model work equally well for every group?
# -----------------------------------------------------------------------------
female <- test$sex == "Female"
male   <- test$sex == "Male"
urban  <- test$residence == "Urban"
rural  <- test$residence == "Rural"

con_female <- confusionMatrix(best_pred[female], actual[female], positive = "Unemployed")
con_male   <- confusionMatrix(best_pred[male],   actual[male],   positive = "Unemployed")
con_urban  <- confusionMatrix(best_pred[urban],  actual[urban],  positive = "Unemployed")
con_rural  <- confusionMatrix(best_pred[rural],  actual[rural],  positive = "Unemployed")

roc_female <- roc(actual[female], best_prob[female], levels = c("Employed", "Unemployed"), direction = "<")
roc_male   <- roc(actual[male],   best_prob[male],   levels = c("Employed", "Unemployed"), direction = "<")
roc_urban  <- roc(actual[urban],  best_prob[urban],  levels = c("Employed", "Unemployed"), direction = "<")
roc_rural  <- roc(actual[rural],  best_prob[rural],  levels = c("Employed", "Unemployed"), direction = "<")

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
subgroups <- subgroups %>% mutate(across(where(is.numeric) & !Youth_in_test, ~ round(.x, 3)))

knitr::kable(subgroups)
write_csv(subgroups, "Outputs/ML_simple/subgroup_performance.csv")


# -----------------------------------------------------------------------------
# 12. Direction of the effects (does a variable RAISE or LOWER the risk?)
# -----------------------------------------------------------------------------
# Feature importance only says WHICH variables matter. To see HOW they matter
# we use two tools:
#   a) odds ratios from the logistic regression (easy to read: above 1 raises
#      the risk, below 1 lowers it). Training on down-sampled data changes only
#      the intercept, not the odds ratios.
#   b) partial dependence plots from the best model: the average risk score
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

# b) Partial dependence (best model) ---------------------------------------
library(pdp)

# A random sample of 2,000 training youth keeps the computation fast
set.seed(123)
pdp_rows <- sample(nrow(train), 2000)
pdp_data <- train[pdp_rows, names(train) != "employment_status"]

pd_school <- partial(best_model, pred.var = "years_of_schooling", train = pdp_data,
                     prob = TRUE, which.class = "Unemployed")
pd_age    <- partial(best_model, pred.var = "age", train = pdp_data,
                     prob = TRUE, which.class = "Unemployed")
pd_hhsize <- partial(best_model, pred.var = "household_size", train = pdp_data,
                     prob = TRUE, which.class = "Unemployed")
pd_sex    <- partial(best_model, pred.var = "sex", train = pdp_data,
                     prob = TRUE, which.class = "Unemployed")
pd_edu    <- partial(best_model, pred.var = "education_level", train = pdp_data,
                     prob = TRUE, which.class = "Unemployed")
pd_rel    <- partial(best_model, pred.var = "household_relationship", train = pdp_data,
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
