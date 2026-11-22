# =============================================================================
#  Extra figures for the report (EDA)
#  1. Unemployment rate by education level and sex (survey-weighted)
#  2. Association matrix of the predictors (Cramer's V)
#  Run after 01_Data_Cleaning_EDA_Dashboard.R and 02_Data_Preparation_Model.R
# =============================================================================

library(dplyr)
library(ggplot2)

eda   <- readRDS("Data/RW_LFS2025_Youth_EDA.rds")
model <- readRDS("Data/RW_LFS2025_Youth_Model.rds")

dir.create("Outputs/Figures", showWarnings = FALSE)


# -----------------------------------------------------------------------------
# 1. Unemployment rate by education level and sex
# -----------------------------------------------------------------------------
# Weighted rate = sum of weights of unemployed / sum of weights of everybody
rates <- eda %>%
  group_by(education_level, sex) %>%
  summarise(rate = sum(survey_weight[employment_status == "Unemployed"]) / sum(survey_weight),
            .groups = "drop")

fig1 <- ggplot(rates, aes(x = education_level, y = rate, fill = sex)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_text(aes(label = paste0(round(100 * rate, 1), "%")),
            position = position_dodge(width = 0.8), vjust = -0.4, size = 3.6, fontface = "bold") +
  scale_fill_manual(values = c("Male" = "#1F4E79", "Female" = "#D9822B")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.32)) +
  labs(x = "Education level", y = "Unemployment rate (weighted)", fill = "Sex") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top", text = element_text(face = "bold"))

ggsave("Outputs/Figures/report_rate_education_sex.png", fig1, width = 8, height = 4.6, dpi = 200)


# -----------------------------------------------------------------------------
# 2. Association between predictors: Cramer's V
# -----------------------------------------------------------------------------
# Cramer's V goes from 0 (no association) to 1 (perfect association).
# It works for categories, so the three numeric variables are cut into 4 groups.
predictors <- model %>% select(-household_id, -employment_status)
predictors$age                <- cut(predictors$age, 4)
predictors$years_of_schooling <- cut(predictors$years_of_schooling, 4)
predictors$household_size     <- cut(predictors$household_size, 4)

cramer_v <- function(x, y) {
  tab  <- table(x, y)
  chi2 <- suppressWarnings(chisq.test(tab, correct = FALSE)$statistic)
  k    <- min(nrow(tab), ncol(tab)) - 1
  as.numeric(sqrt(chi2 / (sum(tab) * k)))
}

vars <- names(predictors)
n    <- length(vars)
v_matrix <- matrix(NA, n, n, dimnames = list(vars, vars))
for (i in 1:n) {
  for (j in 1:n) {
    v_matrix[i, j] <- cramer_v(predictors[[i]], predictors[[j]])
  }
}

# Long format for ggplot
v_long <- data.frame(
  var1  = rep(vars, times = n),
  var2  = rep(vars, each = n),
  value = as.vector(v_matrix)
)
v_long$var1 <- factor(v_long$var1, levels = vars)
v_long$var2 <- factor(v_long$var2, levels = rev(vars))

fig2 <- ggplot(v_long, aes(x = var1, y = var2, fill = value)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.2f", value)), size = 2.6) +
  scale_fill_gradient(low = "#FFFFFF", high = "#1F4E79", limits = c(0, 1)) +
  labs(x = NULL, y = NULL, fill = "Cramer's V") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(face = "bold"))

ggsave("Outputs/Figures/report_cramers_v.png", fig2, width = 9, height = 8, dpi = 200)

cat("Saved the two report figures in Outputs/Figures\n")
