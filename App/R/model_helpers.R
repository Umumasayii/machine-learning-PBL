# =============================================================================
#  Helpers for the "Model & predictions" page
#  Results come from Outputs/ML_simple/dashboard_model.rds, made by
#  Scripts/ml_script_simple.R  (the app uses the model with the highest ROC AUC)
# =============================================================================

# One colour per model, made from the model names saved by the training script
# (no model names are typed here). The model used in the app is drawn in coral.
make_model_cols <- function(ml) {
  models <- clean_model(ml$results$Model[ml$results$Model != ml$baseline])
  cols <- setNames(grDevices::hcl.colors(length(models), "Dark 3"), models)
  cols[[clean_model(ml$app_name)]] <- pal$coral
  cols
}

clean_model <- function(x) gsub("_", " ", x)

# "household_relationshipDomestic worker" -> "Relationship to head: Domestic worker"
pretty_feature <- function(x) {
  vars <- names(predictor_labels)[order(-nchar(names(predictor_labels)))]
  out <- x
  for (i in seq_along(x)) {
    v <- vars[startsWith(x[i], vars)][1]
    if (is.na(v)) next
    level <- substring(x[i], nchar(v) + 1)
    out[i] <- if (level == "") predictor_labels[[v]] else paste0(predictor_labels[[v]], ": ", level)
  }
  out
}

# Readable names for the model's predictors
predictor_labels <- c(
  age = "Age", sex = "Sex", education_level = "Education level",
  years_of_schooling = "Years of schooling", current_student = "Currently a student",
  residence = "Residence", district = "District", marital_status = "Marital status",
  tvet_training = "TVET training", disability = "Disability",
  household_size = "Household size", household_relationship = "Relationship to head",
  field_of_education = "Field of education", education_track = "Education track",
  lived_outside_district = "Lived outside district", migration_status = "Migration status",
  lighting_source = "Lighting source"
)

f3 <- function(x) ifelse(is.na(x), "-", sprintf("%.3f", x))

# The model was trained on down-sampled data (50% unemployed), so its score is
# not the real chance. This turns the score back into a real-world chance,
# using the true share of unemployed youth (prior correction).
real_chance <- function(score, base_rate) {
  score * base_rate / (score * base_rate + (1 - score) * (1 - base_rate))
}


# =============================================================================
# 1. Metrics at a chosen cut-off
# =============================================================================

metrics_at <- function(prob, actual, cutoff) {
  pred <- ifelse(prob >= cutoff, "Unemployed", "Employed")
  tp <- sum(pred == "Unemployed" & actual == "Unemployed")
  fn <- sum(pred == "Employed"   & actual == "Unemployed")
  fp <- sum(pred == "Unemployed" & actual == "Employed")
  tn <- sum(pred == "Employed"   & actual == "Employed")
  recall <- tp / (tp + fn); spec <- tn / (tn + fp)
  precision <- if (tp + fp > 0) tp / (tp + fp) else NA
  f1 <- if (is.na(precision) || precision + recall == 0) NA else 2 * precision * recall / (precision + recall)
  list(tp = tp, fn = fn, fp = fp, tn = tn, recall = recall, specificity = spec,
       precision = precision, f1 = f1, accuracy = (tp + tn) / length(actual))
}

# ROC curve points for every model on the test set
roc_points <- function(test_probs, baseline) {
  models <- setdiff(names(test_probs), c("actual", baseline))   # the dummy is the dashed line
  bind_rows(lapply(models, function(m) {
    r <- pROC::roc(test_probs$actual, test_probs[[m]], levels = c("Employed", "Unemployed"),
                   direction = "<", quiet = TRUE)
    keep <- unique(round(seq(1, length(r$sensitivities), length.out = 400)))
    data.frame(model = clean_model(m), auc = as.numeric(pROC::auc(r)),
               fpr = 1 - r$specificities[keep], tpr = r$sensitivities[keep])
  }))
}


# =============================================================================
# 2. Tables
# =============================================================================

# Test-set results for all models; the app model is highlighted
model_metrics_table <- function(ml, app_name = clean_model(ml$app_name)) {
  model_cols <- make_model_cols(ml)
  d <- ml$results %>% mutate(Model = clean_model(Model))
  cv <- data.frame(Model = clean_model(rownames(ml$cv_roc)), CV_ROC = ml$cv_roc[, "Mean"])
  d <- left_join(d, cv, by = "Model")

  # Columns of the table: name in the data -> heading, and whether high or low is best
  cols <- data.frame(
    var  = c("CV_ROC", "ROC_AUC", "Accuracy", "Sensitivity", "Specificity",
             "Precision", "F1", "Kappa", "MCC", "Train_time_sec"),
    head = c("CV ROC", "Test ROC", "Accuracy", "Recall", "Specificity",
             "Precision", "F1", "Kappa", "MCC", "TT (sec)"),
    best = c(rep("high", 9), "low"))

  baseline <- clean_model(ml$baseline)
  models   <- d %>% filter(Model != baseline)   # the real models
  dummy    <- d %>% filter(Model == baseline)   # the reference row (footer)

  # Best value of each column among the real models (lowest for the time)
  best_value <- function(var, best) {
    if (best == "high") max(models[[var]], na.rm = TRUE) else min(models[[var]], na.rm = TRUE)
  }
  show_value <- function(var, x) if (var == "Train_time_sec") sprintf("%.1f", x) else f3(x)

  value_cells <- function(r, highlight = TRUE) {
    cells <- list()
    for (j in seq_len(nrow(cols))) {
      var <- cols$var[j]
      top <- highlight && !is.na(r[[var]]) && r[[var]] == best_value(var, cols$best[j])
      cells[[j]] <- tags$td(class = paste("num", if (top) "col-best"), show_value(var, r[[var]]))
    }
    cells
  }

  rows <- list()
  for (i in seq_len(nrow(models))) {
    r <- models[i, ]
    is_app <- r$Model == app_name
    name_cell <- tags$td(class = "model-cell",
      span(class = "dot", style = paste0("background:", model_cols[[r$Model]])),
      span(class = "model-name", r$Model),
      if (is_app) span(class = "app-mark", title = "Model used in the app", bs_icon("trophy-fill")))
    rows[[i]] <- tags$tr(class = if (is_app) "best-row",
                         name_cell, tags$td(class = "method", r$Method), value_cells(r))
  }

  footer <- tags$tr(tags$td(class = "model-cell", span(class = "model-name", paste(baseline, "(always 'Employed')"))),
                    tags$td(class = "method", dummy$Method), value_cells(dummy, highlight = FALSE))

  heads <- list(tags$th("Model"), tags$th("caret method"))
  for (h in cols$head) heads[[length(heads) + 1]] <- tags$th(class = "num", h)

  tagList(
    div(class = "table-wrap", tags$table(class = "sum-table model-table",
      tags$thead(tags$tr(heads)),
      tags$tbody(rows),
      tags$tfoot(footer))),
    div(class = "table-note",
        span(class = "app-mark", bs_icon("trophy-fill")), "= model used in the app.",
        span(class = "col-best legend-best", "Highlighted"), "= best value in each column (fastest for TT).",
        "Sorted by test ROC AUC. Test set = 30% of youth, never used for training. CV ROC = mean over 5 folds.",
        "Recall = share of unemployed youth the model finds. MCC = Matthews correlation (0 = guessing).",
        "TT = training time in seconds, including tuning. The footer row is the reference model.")
  )
}

# 2 x 2 confusion matrix as coloured tiles
confusion_tiles <- function(m) {
  tile <- function(n, label, total, cls) div(class = paste("cm-tile", cls),
    div(class = "cm-n", num(n)), div(class = "cm-label", label),
    div(class = "cm-share", pct(n / total), " of this row"))
  pos <- m$tp + m$fn; neg <- m$fp + m$tn
  div(class = "cm-grid",
      div(), div(class = "cm-head", "Predicted unemployed"), div(class = "cm-head", "Predicted employed"),
      div(class = "cm-side", "Actually unemployed"),
      tile(m$tp, "found", pos, "good"), tile(m$fn, "missed", pos, "bad"),
      div(class = "cm-side", "Actually employed"),
      tile(m$fp, "false alarms", neg, "warn"), tile(m$tn, "correctly cleared", neg, "good"))
}

metric_chips <- function(m) {
  chip <- function(label, value) div(class = "chip", span(class = "chip-v", value), span(class = "chip-l", label))
  div(class = "chips",
      chip("Accuracy", f3(m$accuracy)), chip("Recall", f3(m$recall)),
      chip("Specificity", f3(m$specificity)), chip("Precision", f3(m$precision)), chip("F1", f3(m$f1)))
}


# =============================================================================
# 3. Charts
# =============================================================================

# Grouped bars: test metrics of every model
model_compare_chart <- function(ml) {
  model_cols <- make_model_cols(ml)
  d <- ml$results %>% filter(Model != ml$baseline) %>% mutate(Model = clean_model(Model)) %>%
    select(Model, `ROC AUC` = ROC_AUC, Recall = Sensitivity, Specificity, F1) %>%
    tidyr::pivot_longer(-Model, names_to = "metric", values_to = "value") %>%
    mutate(metric = factor(metric, c("ROC AUC", "Recall", "Specificity", "F1")),
           Model = factor(Model, names(model_cols)))
  plot_ly(d, x = ~metric, y = ~value, color = ~Model, colors = model_cols, type = "bar",
          text = ~sprintf("%.2f", value), textposition = "outside", textangle = -90,
          textfont = list(size = 10), cliponaxis = FALSE,
          hovertemplate = "<b>%{fullData.name}</b><br>%{x}: %{y:.3f}<extra></extra>") %>%
    layout(barmode = "group", yaxis = list(range = c(0, 1.05))) %>%
    style_plot(y_title = "Score on the test set") %>%
    legend_right()
}

roc_chart <- function(points, ml) {
  model_cols <- make_model_cols(ml)
  d <- points %>% mutate(name = sprintf("%s (AUC %.3f)", model, auc),
                         model = factor(model, names(model_cols)))
  plot_ly(d, x = ~fpr, y = ~tpr, color = ~model, colors = model_cols, type = "scatter",
          mode = "lines", line = list(width = 2.5), text = ~name,
          hovertemplate = "<b>%{text}</b><br>False positive rate: %{x:.2f}<br>Recall: %{y:.2f}<extra></extra>") %>%
    layout(shapes = list(list(type = "line", x0 = 0, y0 = 0, x1 = 1, y1 = 1,
                              line = list(dash = "dash", color = pal$grey, width = 1)))) %>%
    style_plot(x_title = "False positive rate (1 - specificity)", y_title = "True positive rate (recall)") %>%
    legend_right()
}

# Cross-validated ROC: mean with min-max range over the 5 folds
cv_chart <- function(ml) {
  model_cols <- make_model_cols(ml)
  s <- ml$cv_roc[rownames(ml$cv_roc) != ml$baseline, ]
  d <- data.frame(model = clean_model(rownames(s)), mean = s[, "Mean"],
                  lo = s[, "Min."], hi = s[, "Max."]) %>%
    arrange(mean) %>% mutate(model = factor(model, levels = model))
  plot_ly(d, x = ~mean, y = ~model, type = "scatter", mode = "markers",
          marker = list(size = 13, color = unname(model_cols[as.character(d$model)])),
          error_x = list(type = "data", symmetric = FALSE, array = ~hi - mean,
                         arrayminus = ~mean - lo, color = pal$grey, thickness = 1.5, width = 6),
          hovertemplate = "<b>%{y}</b><br>Mean ROC AUC: %{x:.3f}<extra></extra>") %>%
    style_plot(x_title = "ROC AUC over 5 folds (dot = mean, line = min to max)", legend = FALSE)
}

# Distribution of the app model's risk score on the test set, by true status
risk_hist_chart <- function(test_probs, cutoff, model = "LDA") {
  test_probs$score <- test_probs[[model]]
  plot_ly(alpha = 0.65) %>%
    add_histogram(data = filter(test_probs, actual == "Employed"), x = ~score,
                  name = "Actually employed", marker = list(color = pal$teal),
                  histnorm = "probability", xbins = list(size = 0.025)) %>%
    add_histogram(data = filter(test_probs, actual == "Unemployed"), x = ~score,
                  name = "Actually unemployed", marker = list(color = pal$coral),
                  histnorm = "probability", xbins = list(size = 0.025)) %>%
    layout(barmode = "overlay", shapes = list(dashed_line(cutoff))) %>%
    style_plot(x_title = paste(model, "risk score"), y_title = "Share of group", y_pct = TRUE)
}

importance_chart <- function(ml, top = 15) {
  imp <- ml$importance
  d <- data.frame(variable = rownames(imp), importance = imp[, 1]) %>%
    slice_max(importance, n = top, with_ties = FALSE) %>%
    mutate(label = pretty_feature(variable)) %>%
    arrange(importance) %>% mutate(label = factor(label, levels = unique(label)))
  plot_ly(d, x = ~importance, y = ~label, type = "bar", orientation = "h",
          marker = list(color = pal$teal), text = ~sprintf("%.0f", importance), textposition = "outside",
          hovertemplate = "<b>%{y}</b><br>Importance: %{x:.1f}<extra></extra>") %>%
    layout(xaxis = list(range = c(0, 115))) %>%
    style_plot(x_title = "Importance (0-100, caret::varImp)", legend = FALSE)
}

# Gauge for the risk calculator (score from 0 to 100)
risk_gauge <- function(score, cutoff) {
  plot_ly(type = "indicator", mode = "gauge+number", value = 100 * score,
          number = list(suffix = "", valueformat = ".0f", font = list(size = 46)),
          gauge = list(
            axis = list(range = c(0, 100), tickcolor = "#7B8794"),
            bar = list(color = if (score >= cutoff) pal$coral else pal$teal, thickness = 0.3),
            bgcolor = "rgba(0,0,0,0)", borderwidth = 0,
            steps = list(list(range = c(0, 100 * cutoff), color = "rgba(31,78,121,.15)"),
                         list(range = c(100 * cutoff, 100), color = "rgba(176,58,46,.15)")),
            threshold = list(line = list(color = "#111827", width = 3), thickness = 0.8,
                             value = 100 * cutoff))) %>%
    layout(margin = list(l = 25, r = 25, t = 25, b = 0),
           font = list(family = "Inter, Segoe UI, Arial", color = "#7B8794"),
           paper_bgcolor = "rgba(0,0,0,0)") %>%
    config(displayModeBar = FALSE)   # the gauge does not need a toolbar
}

# Risk score for the same profile at every age from 16 to 30
age_curve_chart <- function(curve, current_age, cutoff) {
  plot_ly(curve, x = ~age, y = ~score, type = "scatter", mode = "lines+markers",
          line = list(color = pal$blue, width = 3), marker = list(size = 6, color = pal$blue),
          hovertemplate = "Age %{x}: score %{y:.2f}<extra></extra>") %>%
    add_markers(data = filter(curve, age == current_age), x = ~age, y = ~score,
                marker = list(size = 14, color = pal$coral), hoverinfo = "skip", inherit = FALSE) %>%
    layout(yaxis = list(range = c(0, 1)),
           shapes = list(list(type = "line", x0 = 16, x1 = 30, y0 = cutoff, y1 = cutoff,
                              line = list(dash = "dash", color = pal$grey, width = 1.5)))) %>%
    style_plot(x_title = "Age", y_title = "Risk score", legend = FALSE)
}

# With many models the legend is long: put it in one column on the right side
legend_right <- function(p) {
  layout(p, legend = list(orientation = "v", x = 1.02, xanchor = "left",
                          y = 1, yanchor = "top", font = list(size = 12)))
}
