library(dplyr)

eda <- readRDS("Data/RW_LFS2025_Youth_EDA.rds")

model_data <- eda %>%
  select(
    household_id, employment_status,
    # first level
    age, sex, education_level, years_of_schooling, current_student,
    residence, province, district, marital_status, tvet_training, disability,
    household_size, household_relationship,
    # second level (known for everybody)
    field_of_education, education_track, lived_outside_district,
    migration_status, lighting_source
  ) %>%
  droplevels()

stopifnot(sum(is.na(model_data)) == 0)

cat("\n========== Model data ==========\n")
cat("Rows       :", nrow(model_data), "\n")
cat("Predictors :", ncol(model_data) - 2, "\n")
cat("Missing    :", sum(is.na(model_data)), "\n\n")
print(round(100 * prop.table(table(model_data$employment_status)), 1))

saveRDS(model_data, "Data/RW_LFS2025_Youth_Model.rds")
write.csv(model_data, "Data/RW_LFS2025_Youth_Model.csv", row.names = FALSE)

cat("\nSaved: Data/RW_LFS2025_Youth_Model.rds\n")
