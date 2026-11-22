# =============================================================================
#  Youth Employment in Rwanda (LFS 2025)
#  Script 3 - Machine learning with caret
# =============================================================================
#  Goal   : classify youth in the labour force as Unemployed or Employed
#  Input  : Data/RW_LFS2025_Youth_Model.rds   (made by script 2)
#  Output : Outputs/ML/       tables (.csv) and results (.rds)
#           Outputs/Figures/  charts (.png) for the report
#
#  Steps
#    1. Setup               6. Compare models (cross-validation)
#    2. Load data           7. Choose the decision threshold
#    3. Train/test split    8. Test-set evaluation
#    4. Cross-validation    9. Best model: importance, calibration, fairness
#    5. Train the models   10. Class-imbalance experiment and saving
#
#  Run from the project folder. Takes about 5-10 minutes.
# =============================================================================


# -----------------------------------------------------------------------------
# Step 1. Setup
# -----------------------------------------------------------------------------

library(caret)       # all modelling: split, CV, tuning, evaluation
library(dplyr)
library(tidyr)
library(ggplot2)
library(pROC)        # ROC curves and AUC
library(PRROC)       # precision-recall AUC
library(doParallel)  # run cross-validation on several CPU cores

set.seed(2025)

dir.create("Outputs/ML", recursive = TRUE, showWarnings = FALSE)
dir.create("Outputs/Figures", recursive = TRUE, showWarnings = FALSE)

# Colours used in every chart
col_unemp <- "#E76F51"; col_emp <- "#3AA3A2"; col_navy <- "#051650"
model_colours <- c("Logistic regression" = "#8A94A6", "Elastic net" = "#3AA3A2",
                   "Decision tree" = "#F4A261", "Naive Bayes" = "#8E7DBE",
                   "Random forest" = "#1A5251", "XGBoost" = "#E76F51",
                   "Baseline (majority)" = "#C9D3E6")
theme_set(theme_minimal(base_size = 12))

save_plot <- function(plot, name, width = 8, height = 5) {
  ggsave(file.path("Outputs/Figures", name), plot, width = width, height = height, dpi = 300)
}

# Use all CPU cores except two
cluster <- makePSOCKcluster(max(1, parallel::detectCores() - 2))
registerDoParallel(cluster)


# -----------------------------------------------------------------------------
# Step 2. Load the data
# -----------------------------------------------------------------------------
# caret treats the FIRST level of the outcome as the positive class.
# We want "Unemployed" to be positive, so it goes first.

model_data <- readRDS("Data/RW_LFS2025_Youth_Model.rds") %>%
  mutate(employment_status = factor(employment_status,
                                    levels = c("Unemployed", "Employed")))

cat("Rows:", nrow(model_data), "| Predictors:", ncol(model_data) - 2, "\n")
print(round(prop.table(table(model_data$employment_status)), 3))


# -----------------------------------------------------------------------------
# Step 3. Train/test split (70/30), by household
# -----------------------------------------------------------------------------
# Young people from the same household are similar. If siblings end up in
# both the training and the test set, the test score looks better than it
# should. So we split HOUSEHOLDS, not people. createDataPartition() keeps the
# share of households with an unemployed youth the same in both sets.

households <- model_data %>%
  group_by(household_id) %>%
  summarise(has_unemployed = factor(any(employment_status == "Unemployed")))

train_hh <- createDataPartition(households$has_unemployed, p = 0.70, list = FALSE)
train_ids <- households$household_id[train_hh]

train_set <- filter(model_data, household_id %in% train_ids)
test_set  <- filter(model_data, !household_id %in% train_ids)

split_table <- bind_rows(
  train_set %>% count(employment_status) %>% mutate(set = "Training"),
  test_set  %>% count(employment_status) %>% mutate(set = "Test")
) %>%
  group_by(set) %>% mutate(share = n / sum(n)) %>% ungroup()
print(split_table)
write.csv(split_table, "Outputs/ML/01_train_test_split.csv", row.names = FALSE)

# Quick predictor checks on the training set
nzv <- nearZeroVar(select(train_set, -household_id, -employment_status), saveMetrics = TRUE)
nzv$variable <- rownames(nzv)
write.csv(nzv, "Outputs/ML/02_near_zero_variance.csv", row.names = FALSE)
cat("\nNear-zero-variance predictors:", paste(nzv$variable[nzv$nzv], collapse = ", "), "\n")


# -----------------------------------------------------------------------------
# Step 4. Cross-validation set-up
# -----------------------------------------------------------------------------
# 5-fold cross-validation on the TRAINING set only. groupKFold() keeps each
# household inside a single fold. The test set is not touched until Step 8.

# Our own summary: AUC plus metrics for the UNEMPLOYED class
youth_summary <- function(data, lev = NULL, model = NULL) {
  pos   <- lev[1]
  truth <- as.integer(data$obs == pos)
  prob  <- data[[pos]]
  pred  <- as.integer(data$pred == pos)

  tp <- sum(pred == 1 & truth == 1); fp <- sum(pred == 1 & truth == 0)
  fn <- sum(pred == 0 & truth == 1); tn <- sum(pred == 0 & truth == 0)
  sens <- tp / (tp + fn); spec <- tn / (tn + fp)
  prec <- if ((tp + fp) > 0) tp / (tp + fp) else 0
  f1   <- if ((prec + sens) > 0) 2 * prec * sens / (prec + sens) else 0
  mcc_den <- sqrt(as.numeric(tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  mcc  <- if (mcc_den > 0) (tp * tn - fp * fn) / mcc_den else 0

  c(ROC    = as.numeric(pROC::auc(pROC::roc(truth, prob, levels = c(0, 1),
                                            direction = "<", quiet = TRUE))),
    PR_AUC = PRROC::pr.curve(scores.class0 = prob[truth == 1],
                             scores.class1 = prob[truth == 0])$auc.davis.goadrich,
    Sens = sens, Spec = spec, Precision = prec, F1 = f1,
    BalAcc = (sens + spec) / 2, MCC = mcc)
}

cv_folds <- groupKFold(train_set$household_id, k = 5)

cv_control <- trainControl(
  method          = "cv",
  index           = cv_folds,       # household-grouped folds
  classProbs      = TRUE,           # needed for ROC and thresholds
  summaryFunction = youth_summary,
  savePredictions = "final",        # keep out-of-fold predictions
  allowParallel   = TRUE
)

model_formula <- employment_status ~ . - household_id


# -----------------------------------------------------------------------------
# Step 5. Train the models
# -----------------------------------------------------------------------------
# Every model is tuned on cross-validated ROC AUC: how well it RANKS
# unemployed youth above employed youth, whatever cut-off is used later.
#
#  Model                Why it is included
#  Baseline (majority)  Reference: always predicts "Employed"
#  Logistic regression  Simple, transparent, gives odds ratios
#  Elastic net          Logistic regression with penalties against overfitting
#  Decision tree        Easy-to-explain rules
#  Naive Bayes          Fast probabilistic benchmark
#  Random forest        Many trees; captures interactions and non-linearity
#  XGBoost              Gradient boosting; often the strongest on tabular data

# --- XGBoost as a custom caret model ------------------------------------------
# caret's built-in "xgbTree" does not work with xgboost version 3 or newer.
# caret lets us register our own model: we start from the built-in definition
# and only replace how the model is fitted and how it predicts. Tuning,
# cross-validation and evaluation still all run through caret::train().
xgb_caret <- getModelInfo("xgbTree", regex = FALSE)[[1]]
xgb_caret$label <- "XGBoost (custom caret model)"
xgb_caret$loop  <- NULL
xgb_caret$fit <- function(x, y, wts, param, lev, last, classProbs, ...) {
  dtrain <- xgboost::xgb.DMatrix(as.matrix(x), label = as.numeric(y == lev[1]))
  booster <- xgboost::xgb.train(
    params = list(objective = "binary:logistic", eta = param$eta,
                  max_depth = param$max_depth, gamma = param$gamma,
                  colsample_bytree = param$colsample_bytree,
                  min_child_weight = param$min_child_weight,
                  subsample = param$subsample, nthread = 1),
    data = dtrain, nrounds = param$nrounds, verbose = 0)
  list(booster = booster, lev = lev, xNames = colnames(x))
}
xgb_caret$prob <- function(modelFit, newdata, submodels = NULL) {
  p <- predict(modelFit$booster, xgboost::xgb.DMatrix(as.matrix(newdata)))
  out <- data.frame(p, 1 - p)
  names(out) <- modelFit$lev
  out
}
xgb_caret$predict <- function(modelFit, newdata, submodels = NULL) {
  p <- predict(modelFit$booster, xgboost::xgb.DMatrix(as.matrix(newdata)))
  factor(ifelse(p >= 0.5, modelFit$lev[1], modelFit$lev[2]), levels = modelFit$lev)
}
xgb_caret$varImp <- function(object, ...) {
  imp <- xgboost::xgb.importance(model = object$booster)
  out <- data.frame(Overall = rep(0, length(object$xNames)), row.names = object$xNames)
  out[imp$Feature, "Overall"] <- imp$Gain
  out
}

# --- Train one model (saved to disk, so a re-run skips finished models) -------
# Delete the folder Outputs/ML/models to force all models to be retrained.
dir.create("Outputs/ML/models", showWarnings = FALSE)

# use_factors = TRUE gives the model the original categories instead of
# 0/1 dummy columns (needed for Naive Bayes, which models categories directly).
fit_model <- function(name, method, ..., use_factors = FALSE) {
  file <- file.path("Outputs/ML/models", paste0(gsub("[^A-Za-z]", "_", name), ".rds"))
  if (file.exists(file)) {
    cat("Loading saved model:", name, "\n")
    return(readRDS(file))
  }
  cat("Training", name, "...\n")
  set.seed(2025)
  start <- Sys.time()
  fit <- if (use_factors) {
    train(x = select(train_set, -household_id, -employment_status),
          y = train_set$employment_status, method = method,
          metric = "ROC", trControl = cv_control, ...)
  } else {
    train(model_formula, data = train_set, method = method,
          metric = "ROC", trControl = cv_control, ...)
  }
  cat("  done in", round(difftime(Sys.time(), start, units = "mins"), 1), "minutes\n")
  saveRDS(fit, file)
  fit
}

models <- list()

models[["Baseline (majority)"]] <- fit_model("Baseline (majority)", "null")

models[["Logistic regression"]] <- fit_model("Logistic regression", "glm", family = "binomial")

models[["Elastic net"]] <- fit_model(
  "Elastic net", "glmnet", preProcess = c("center", "scale"),
  tuneGrid = expand.grid(alpha  = c(0, 0.25, 0.5, 0.75, 1),
                         lambda = 10^seq(-4, -1, length.out = 10)))

models[["Decision tree"]] <- fit_model(
  "Decision tree", "rpart",
  tuneGrid = data.frame(cp = c(0.0005, 0.001, 0.002, 0.003, 0.005, 0.01)))

models[["Naive Bayes"]] <- fit_model(
  "Naive Bayes", "naive_bayes", use_factors = TRUE,
  tuneGrid = expand.grid(laplace = c(0, 1), usekernel = c(FALSE, TRUE), adjust = c(1, 2)))

models[["Random forest"]] <- fit_model(
  "Random forest", "ranger", num.trees = 300, num.threads = 1, importance = "impurity",
  tuneGrid = expand.grid(mtry = c(2, 4, 6, 8),
                         splitrule = c("gini", "extratrees"),
                         min.node.size = c(10, 30, 60)))

models[["XGBoost"]] <- fit_model(
  "XGBoost", xgb_caret,
  tuneGrid = expand.grid(nrounds = c(100, 200, 300), max_depth = c(2, 3, 4),
                         eta = c(0.05, 0.1), gamma = 0, colsample_bytree = 0.8,
                         min_child_weight = c(1, 5), subsample = 0.8))

saveRDS(models, "Outputs/ML/trained_models.rds")


# -----------------------------------------------------------------------------
# Step 6. Compare models with cross-validation results
# -----------------------------------------------------------------------------

# Best tuning values for each model
tuning_table <- bind_rows(lapply(names(models), function(m) {
  bt <- models[[m]]$bestTune
  values <- sapply(bt, function(v) if (is.numeric(v)) as.character(signif(v, 3)) else as.character(v))
  tibble(Model = m,
         `Best hyper-parameters` = paste(names(bt), values, sep = " = ", collapse = ", "))
}))
print(tuning_table)
write.csv(tuning_table, "Outputs/ML/03_best_hyperparameters.csv", row.names = FALSE)

# Full tuning results (every combination tried)
for (m in names(models)) {
  write.csv(models[[m]]$results,
            file.path("Outputs/ML", paste0("tuning_", gsub("[^A-Za-z]", "_", m), ".csv")),
            row.names = FALSE)
}

# Cross-validation scores (mean and SD over the 5 folds)
cv_results <- resamples(models[names(models) != "Baseline (majority)"])
cv_long <- cv_results$values %>%
  pivot_longer(-Resample, names_to = c("Model", "Metric"), names_sep = "~") %>%
  group_by(Model, Metric) %>%
  summarise(mean = mean(value), sd = sd(value), .groups = "drop")

cv_table <- cv_long %>%
  mutate(value = sprintf("%.3f (%.3f)", mean, sd)) %>%
  select(Model, Metric, value) %>%
  pivot_wider(names_from = Metric, values_from = value) %>%
  select(Model, ROC, PR_AUC, Sens, Spec, Precision, F1, BalAcc, MCC) %>%
  arrange(desc(ROC))
print(cv_table)
write.csv(cv_table, "Outputs/ML/04_cv_comparison.csv", row.names = FALSE)

# Are the differences in ROC between models statistically significant?
cv_diff <- summary(diff(cv_results, metric = "ROC"))
capture.output(cv_diff, file = "Outputs/ML/05_cv_roc_differences.txt")

p_cv <- cv_long %>%
  filter(Metric %in% c("ROC", "PR_AUC")) %>%
  mutate(Metric = recode(Metric, ROC = "ROC AUC", PR_AUC = "PR AUC"),
         Model = reorder(Model, mean)) %>%
  ggplot(aes(x = mean, y = Model, colour = Model)) +
  geom_errorbar(aes(xmin = mean - sd, xmax = mean + sd), width = 0.25, orientation = "y") +
  geom_point(size = 3) +
  facet_wrap(~ Metric, scales = "free_x") +
  scale_colour_manual(values = model_colours, guide = "none") +
  labs(title = "Cross-validated performance (mean +/- SD over 5 folds)",
       x = NULL, y = NULL)
save_plot(p_cv, "ml_01_cv_comparison.png")

# Best model = highest mean cross-validated ROC AUC
best_name <- cv_long %>% filter(Metric == "ROC") %>% slice_max(mean, n = 1) %>% pull(Model)
best_model <- models[[best_name]]
cat("\nBest model by CV ROC AUC:", best_name, "\n")


# -----------------------------------------------------------------------------
# Step 7. Choose the decision threshold (training data only)
# -----------------------------------------------------------------------------
# With only ~15% unemployed, the default cut-off of 0.5 labels almost
# everybody "Employed". thresholder() re-scores the OUT-OF-FOLD predictions
# at many cut-offs. We keep the cut-off with the highest Youden's J
# (sensitivity + specificity - 1), which balances both kinds of error.

thresholds <- seq(0.02, 0.80, by = 0.01)

threshold_results <- lapply(names(models)[-1], function(m) {
  thresholder(models[[m]], threshold = thresholds, final = TRUE,
              statistics = c("Sensitivity", "Specificity", "Precision",
                             "F1", "J", "Balanced Accuracy")) %>%
    mutate(Model = m)
}) %>% bind_rows()

best_thresholds <- threshold_results %>%
  group_by(Model) %>%
  slice_max(J, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Model, threshold = prob_threshold, Sensitivity, Specificity, F1, J)
print(best_thresholds)
write.csv(best_thresholds, "Outputs/ML/06_chosen_thresholds.csv", row.names = FALSE)

p_thr <- threshold_results %>%
  filter(Model == best_name) %>%
  select(prob_threshold, Sensitivity, Specificity, Precision, F1) %>%
  pivot_longer(-prob_threshold) %>%
  filter(!is.na(value)) %>%
  ggplot(aes(prob_threshold, value, colour = name)) +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = best_thresholds$threshold[best_thresholds$Model == best_name],
             linetype = "dashed") +
  scale_colour_manual(values = c(Sensitivity = col_unemp, Specificity = col_emp,
                                 Precision = col_navy, F1 = "#F4A261")) +
  labs(title = paste("Effect of the cut-off -", best_name),
       subtitle = "Out-of-fold predictions; dashed line = chosen cut-off (max Youden's J)",
       x = "Probability cut-off for 'Unemployed'", y = NULL, colour = NULL)
save_plot(p_thr, "ml_02_threshold_tuning.png")


# -----------------------------------------------------------------------------
# Step 8. Evaluate every model on the untouched test set
# -----------------------------------------------------------------------------

test_metrics <- function(prob, truth, cutoff) {
  pred <- factor(ifelse(prob >= cutoff, "Unemployed", "Employed"),
                 levels = c("Unemployed", "Employed"))
  cm <- confusionMatrix(pred, truth, positive = "Unemployed", mode = "everything")
  tp <- cm$table[1, 1]; fp <- cm$table[1, 2]; fn <- cm$table[2, 1]; tn <- cm$table[2, 2]
  mcc <- (tp * tn - fp * fn) / sqrt(as.numeric(tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  truth01 <- as.integer(truth == "Unemployed")
  tibble(
    Cutoff        = cutoff,
    `ROC AUC`     = as.numeric(auc(roc(truth01, prob, levels = c(0, 1), direction = "<", quiet = TRUE))),
    `PR AUC`      = pr.curve(scores.class0 = prob[truth01 == 1],
                             scores.class1 = prob[truth01 == 0])$auc.davis.goadrich,
    Accuracy      = unname(cm$overall["Accuracy"]),
    `Balanced accuracy` = unname(cm$byClass["Balanced Accuracy"]),
    Recall        = unname(cm$byClass["Sensitivity"]),
    Specificity   = unname(cm$byClass["Specificity"]),
    Precision     = unname(cm$byClass["Precision"]),
    F1            = unname(cm$byClass["F1"]),
    Kappa         = unname(cm$overall["Kappa"]),
    MCC           = mcc
  )
}

# Probability of "Unemployed" for new data. Models trained on factors
# (Naive Bayes) get only the predictor columns.
predict_unemployed <- function(model, data) {
  if (is.null(model$terms)) data <- select(data, -household_id, -employment_status)
  predict(model, newdata = data, type = "prob")$Unemployed
}

test_probs <- lapply(models, predict_unemployed, data = test_set)

test_table <- bind_rows(lapply(names(models), function(m) {
  cut <- if (m == "Baseline (majority)") 0.5 else
    best_thresholds$threshold[best_thresholds$Model == m]
  bind_rows(
    test_metrics(test_probs[[m]], test_set$employment_status, 0.5) %>% mutate(Rule = "Default 0.5"),
    test_metrics(test_probs[[m]], test_set$employment_status, cut) %>% mutate(Rule = "Tuned cut-off")
  ) %>% mutate(Model = m)
})) %>%
  select(Model, Rule, everything()) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
print(test_table, width = Inf)
write.csv(test_table, "Outputs/ML/07_test_metrics.csv", row.names = FALSE)

# ROC curves on the test set
roc_points <- bind_rows(lapply(names(models)[-1], function(m) {
  r <- roc(test_set$employment_status, test_probs[[m]],
           levels = c("Employed", "Unemployed"), direction = "<", quiet = TRUE)
  tibble(Model = sprintf("%s (AUC %.3f)", m, auc(r)), model = m,
         fpr = 1 - r$specificities, tpr = r$sensitivities)
}))
p_roc <- ggplot(roc_points, aes(fpr, tpr, colour = model)) +
  geom_abline(linetype = "dashed", colour = "grey60") +
  geom_path(linewidth = 1) +
  scale_colour_manual(values = model_colours,
                      labels = setNames(unique(roc_points$Model), unique(roc_points$model))) +
  coord_equal() +
  labs(title = "ROC curves on the test set", x = "False positive rate (1 - specificity)",
       y = "True positive rate (recall)", colour = NULL) +
  theme(legend.position = c(0.7, 0.25), legend.background = element_rect(fill = "white", colour = NA))
save_plot(p_roc, "ml_03_roc_curves.png", width = 7, height = 6.5)

# Precision-recall curves on the test set
pr_points <- bind_rows(lapply(names(models)[-1], function(m) {
  t01 <- as.integer(test_set$employment_status == "Unemployed")
  pr <- pr.curve(scores.class0 = test_probs[[m]][t01 == 1],
                 scores.class1 = test_probs[[m]][t01 == 0], curve = TRUE)
  tibble(model = m, recall = pr$curve[, 1], precision = pr$curve[, 2])
}))
prevalence <- mean(test_set$employment_status == "Unemployed")
p_pr <- ggplot(pr_points, aes(recall, precision, colour = model)) +
  geom_hline(yintercept = prevalence, linetype = "dashed", colour = "grey60") +
  geom_path(linewidth = 1) +
  scale_colour_manual(values = model_colours) +
  annotate("text", x = 0.85, y = prevalence + 0.02, label = "Random guess", colour = "grey40", size = 3.5) +
  labs(title = "Precision-recall curves on the test set", x = "Recall (unemployed found)",
       y = "Precision", colour = NULL)
save_plot(p_pr, "ml_04_pr_curves.png")


# -----------------------------------------------------------------------------
# Step 9. The best model in detail
# -----------------------------------------------------------------------------

best_cut  <- best_thresholds$threshold[best_thresholds$Model == best_name]
best_prob <- test_probs[[best_name]]
best_pred <- factor(ifelse(best_prob >= best_cut, "Unemployed", "Employed"),
                    levels = c("Unemployed", "Employed"))
best_cm   <- confusionMatrix(best_pred, test_set$employment_status,
                             positive = "Unemployed", mode = "everything")
print(best_cm)
capture.output(best_cm, file = "Outputs/ML/08_best_model_confusion_matrix.txt")

# Confusion matrix chart
cm_df <- as.data.frame(best_cm$table) %>%
  group_by(Reference) %>% mutate(share = Freq / sum(Freq)) %>% ungroup()
p_cm <- ggplot(cm_df, aes(Reference, Prediction, fill = share)) +
  geom_tile(colour = "white", linewidth = 1.5) +
  geom_text(aes(label = sprintf("%d\n(%.0f%%)", Freq, 100 * share)), size = 5, colour = "white") +
  scale_fill_gradient(low = col_emp, high = col_navy, guide = "none") +
  labs(title = paste("Confusion matrix -", best_name),
       subtitle = sprintf("Test set, cut-off = %.2f. Percentages are within each true class.", best_cut),
       x = "True status", y = "Predicted status")
save_plot(p_cm, "ml_05_confusion_matrix.png", width = 6, height = 5)

# Variable importance (caret::varImp, scaled 0-100)
imp <- varImp(best_model, scale = TRUE)$importance
imp <- tibble(feature = rownames(imp), importance = imp[[1]]) %>%
  arrange(desc(importance))
write.csv(imp, "Outputs/ML/09_variable_importance.csv", row.names = FALSE)
p_imp <- imp %>% slice_head(n = 15) %>%
  ggplot(aes(importance, reorder(feature, importance))) +
  geom_col(fill = col_emp, width = 0.7) +
  labs(title = paste("Top 15 most important features -", best_name),
       subtitle = "caret::varImp(), scaled from 0 to 100", x = "Importance", y = NULL)
save_plot(p_imp, "ml_06_variable_importance.png", height = 6)

# Calibration: do predicted probabilities match observed rates?
cal <- calibration(employment_status ~ best_prob,
                   data = data.frame(employment_status = test_set$employment_status, best_prob),
                   class = "Unemployed", cuts = 10)
p_cal <- ggplot(cal$data, aes(midpoint / 100, Percent / 100)) +
  geom_abline(linetype = "dashed", colour = "grey60") +
  geom_line(colour = col_unemp, linewidth = 1) +
  geom_point(colour = col_unemp, size = 2.5) +
  labs(title = paste("Calibration -", best_name),
       subtitle = "Dashed line = perfect calibration", x = "Predicted probability (bin midpoint)",
       y = "Observed share unemployed")
save_plot(p_cal, "ml_07_calibration.png", width = 6.5, height = 5.5)

# Lift: how many unemployed do we find by targeting the top-scored youth?
lift_obj <- lift(employment_status ~ best_prob,
                 data = data.frame(employment_status = test_set$employment_status, best_prob),
                 class = "Unemployed")
p_lift <- ggplot(lift_obj$data, aes(CumTestedPct, CumEventPct)) +
  geom_abline(linetype = "dashed", colour = "grey60") +
  geom_line(colour = col_navy, linewidth = 1.1) +
  labs(title = paste("Cumulative gains -", best_name),
       subtitle = "Share of all unemployed found when targeting the highest-risk youth first",
       x = "% of youth targeted (highest risk first)", y = "% of unemployed youth found")
save_plot(p_lift, "ml_08_cumulative_gains.png", width = 6.5, height = 5.5)

# Fairness check: does the model work equally well for different groups?
group_metrics <- function(var) {
  test_set %>%
    mutate(prob = best_prob, grp = .data[[var]]) %>%
    group_by(grp) %>%
    group_modify(~ test_metrics(.x$prob, .x$employment_status, best_cut)) %>%
    ungroup() %>%
    transmute(Variable = var, Group = grp, Recall, Specificity, Precision, F1, `ROC AUC`)
}
fairness <- bind_rows(group_metrics("sex"), group_metrics("residence"),
                      group_metrics("education_level")) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
print(fairness)
write.csv(fairness, "Outputs/ML/10_fairness_by_group.csv", row.names = FALSE)

# Logistic regression odds ratios (interpretable effects)
or_table <- summary(models[["Logistic regression"]]$finalModel)$coefficients %>%
  as.data.frame() %>%
  mutate(term = rownames(.), odds_ratio = exp(Estimate),
         lower = exp(Estimate - 1.96 * `Std. Error`), upper = exp(Estimate + 1.96 * `Std. Error`)) %>%
  filter(term != "(Intercept)") %>%
  transmute(term, odds_ratio = round(odds_ratio, 3), lower = round(lower, 3),
            upper = round(upper, 3), p_value = signif(`Pr(>|z|)`, 3)) %>%
  arrange(p_value)
write.csv(or_table, "Outputs/ML/11_logistic_odds_ratios.csv", row.names = FALSE)


# -----------------------------------------------------------------------------
# Step 10. Class-imbalance experiment, then save everything
# -----------------------------------------------------------------------------
# Alternative to moving the cut-off: re-balance the training folds.
# caret's `sampling` option does this INSIDE each fold, so the validation
# folds keep their real class mix. We refit the best model type with its
# best hyper-parameters using down-sampling and up-sampling.

imbalance <- list("None (tuned cut-off)" = best_model)
for (s in c("down", "up")) {
  ctrl <- cv_control; ctrl$sampling <- s
  set.seed(2025)
  extra <- switch(best_name,
                  "Random forest"       = list(num.trees = 300, num.threads = 1),
                  "Logistic regression" = list(family = "binomial"),
                  "Elastic net"         = list(preProcess = c("center", "scale")),
                  list())
  method_used <- if (best_name == "XGBoost") xgb_caret else best_model$method
  imbalance[[paste0(tools::toTitleCase(s), "-sampling")]] <- do.call(
    train, c(list(model_formula, data = train_set, method = method_used,
                  metric = "ROC", trControl = ctrl, tuneGrid = best_model$bestTune),
             extra))
}
imbalance_table <- bind_rows(lapply(names(imbalance), function(s) {
  cut <- if (s == "None (tuned cut-off)") best_cut else 0.5
  p <- predict_unemployed(imbalance[[s]], test_set)
  test_metrics(p, test_set$employment_status, cut) %>% mutate(Strategy = s)
})) %>%
  select(Strategy, everything()) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
print(imbalance_table, width = Inf)
write.csv(imbalance_table, "Outputs/ML/12_imbalance_strategies.csv", row.names = FALSE)

stopCluster(cluster)
registerDoSEQ()

# Everything the report and the dashboard need, in one file
ml_results <- list(
  best_name         = best_name,
  best_model        = best_model,
  best_cutoff       = best_cut,
  split             = split_table,
  tuning            = tuning_table,
  cv_long           = cv_long,
  cv_table          = cv_table,
  thresholds        = best_thresholds,
  threshold_curve   = filter(threshold_results, Model == best_name),
  test_table        = test_table,
  roc_points        = roc_points,
  pr_points         = pr_points,
  confusion         = cm_df,
  importance        = imp,
  calibration       = cal$data,
  lift              = lift_obj$data,
  fairness          = fairness,
  odds_ratios       = or_table,
  imbalance         = imbalance_table,
  test_predictions  = test_set %>% mutate(prob_unemployed = best_prob, predicted = best_pred)
)
saveRDS(ml_results, "Outputs/ML/ml_results.rds")

cat("\nDone. Best model:", best_name, "| cut-off:", best_cut, "\n")
cat("Tables in Outputs/ML, figures in Outputs/Figures\n")
