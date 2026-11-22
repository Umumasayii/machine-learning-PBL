library(haven)    
library(dplyr)
library(forcats)
library(readr)
library(stringr)


lfs_raw <- read_dta("Data/RW_LFS2025.dta")



youth_raw <- lfs_raw %>% filter(A04 >= 16, A04 <= 30)

n_duplicates <- sum(duplicated(youth_raw))
youth_raw    <- distinct(youth_raw)


youth <- youth_raw %>%
  select(
    # ---- CORE ---------------------------------------------------------------
    household_id            = HHID,
    cluster_id              = PSU_NO,
    survey_strata           = lfs_strata,
    employment_status       = status1,
    age                     = A04,
    sex                     = A01,
    education_level         = attained,
    residence               = Code_UR,
    province                = province,

    # ---- FIRST LEVEL ---------------------------------------------------------
    age_group               = age3_16_30,
    district                = code_dis,
    years_of_schooling      = indb1,
    current_student         = B01,
    marital_status          = A05,
    tvet_training           = B08,
    disability              = disable,
    household_size          = hhsize,
    household_relationship  = A02,

    # ---- SECOND LEVEL: education and skills ----------------------------------
    field_of_education      = indb05,
    education_track         = TVT2,
    attending_tvet_now      = B07,
    tvet_duration           = B09,
    tvet_completed          = B13,
    activity_after_tvet     = B14,
    time_to_job_after_tvet  = B15,

    # ---- SECOND LEVEL: migration ---------------------------------------------
    nationality             = A12,
    lived_outside_district  = A15,
    migration_status        = migrant,
    reason_for_moving       = A20,

    # ---- SECOND LEVEL: household living conditions and time use ---------------
    lighting_source         = EN01,
    own_use_farming         = H01,
    subsistence_producer    = sub,
    collects_firewood       = G01A,
    fetches_water           = G02A,
    household_chores        = G06A,
    care_work               = G07A,

    # ---- OPTIONAL: unemployed youth only --------------------------------------
    unemployment_duration   = UD,
    ever_worked_before      = F01A,
    main_income_source      = F05,

    # ---- OPTIONAL: employed youth only ----------------------------------------
    status_in_employment    = icse18A,
    occupation              = indd01,
    industry                = indd03,
    economic_sector         = main_sect,
    production_unit         = Prod_unit,
    formal_informal_work    = IEV2,
    time_underemployment    = TRU,
    usual_hours             = usualhrs,
    contract_type           = D06,
    social_security         = D09A,
    paid_leave              = D10A,
    how_job_found           = D18A,
    workplace_size          = D05A,
    commute_mode            = D23A,
    has_second_job          = C10,
    income_band             = intcash,
    income_cash             = cash,
    neet_status             = neetyoung
  )


numeric_vars <- c("household_id", "cluster_id", "age",
                  "years_of_schooling", "income_cash")

youth <- youth %>%
  mutate(
    across(all_of(numeric_vars), as.numeric),
    across(-all_of(c(numeric_vars, "disability", "neet_status", "household_size")),
           as_factor),
    survey_strata = as_factor(survey_strata),
    household_size = parse_number(as.character(as_factor(household_size))),
    disability  = factor(as.numeric(disability), levels = c(0, 1),
                         labels = c("No", "Yes")),
    neet_status = factor(as.numeric(neet_status), levels = c(0, 1),
                         labels = c("Not NEET", "NEET"))
  )

youth <- youth %>%
  filter(employment_status %in% c("Employed", "Unemployed")) %>%
  mutate(employment_status = factor(employment_status,
                                    levels = c("Employed", "Unemployed")))


tidy_label <- function(x) {
  x %>%
    str_replace_all("_", " ") %>%
    str_remove("^[0-9]+-(?=[A-Za-z])") %>%        # "4-Employees" -> "Employees"
    str_replace_all("\\s*(RWF|RFW|FRW)$", " RWF") %>%  # one spelling for francs
    str_replace_all(",(?=[A-Za-z])", ", ") %>%     # "Yes,with" -> "Yes, with"
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

youth <- youth %>%
  mutate(across(where(is.factor), ~ fct_relabel(.x, tidy_label))) %>%
  mutate(
    industry = fct_recode(industry,
      "Households as employers"   = "Activities of house13holds as employers",
      "Transport and storage"     = "Transportationa and storage",
      "Trade and vehicle repair"  = "Whole sale and retail trade; repair of motor vehicles and motorcycles",
      "Public administration"     = "Public administration and defense; compulsory social security",
      "Agriculture, forestry, fishing" = "Agriculture forestry and fishing"),
    field_of_education = fct_recode(field_of_education,
      "Engineering and construction" = "engineering, manufacturing and construction",
      "Social science and business"  = "Social Science business and art"),
    main_income_source = fct_recode(main_income_source,
      "Scholarship" = "Schorlaship"),
    commute_mode = fct_recode(commute_mode, "Public bicycle" = "Publc bicycle",
                              "Public motorcycle" = "Public motocycle")
  )


# -----------------------------------------------------------------------------
# Step 7. Group categories so every group has enough people
# -----------------------------------------------------------------------------

youth <- youth %>%
  mutate(
    age_group = fct_relevel(str_remove(age_group, " yrs"), "16-19", "20-24", "25-30"),

    marital_status = fct_collapse(marital_status,
      "Married"            = c("Married monogamously", "Married polygamous"),
      "Living together"    = "Living together",
      "Previously married" = c("Divorced", "Separated", "Widow/Widower"),
      "Single"             = "Single"),

    household_relationship = fct_lump_min(household_relationship, min = 100,
                                          other_level = "Other"),

    education_track = fct_recode(education_track,
      "No level completed"  = "No level completed",
      "General education"   = "Completed general",
      "TVET"                = "Completed TVET"),

    nationality = fct_other(nationality, keep = "Rwanda", other_level = "Other nationality"),

    # Lighting source is a good proxy for household living standards
    lighting_source = fct_collapse(lighting_source,
      "Grid electricity"       = "Electricity from National Grid (EUCL",
      "Solar or mini-grid"     = c("Private Solar Mini Grid", "Solar lantern",
                                   "Electricity from Mini Grid(Other distributors)",
                                   "Standalone solar system"),
      "Torch or phone light"   = c("Torch (rechargeable or not)/Phone flashlight",
                                   "Rechargeable lantern", "Rechargeable battery",
                                   "Batteries+ bulb"),
      other_level = "Traditional (candle, firewood, lamp)"),

    reason_for_moving = fct_lump_min(reason_for_moving, min = 100, other_level = "Other reason"),
    main_income_source = fct_lump_min(main_income_source, min = 50, other_level = "Other source"),
    how_job_found = fct_lump_min(how_job_found, min = 100, other_level = "Other method"),
    commute_mode  = fct_lump_min(commute_mode, min = 100, other_level = "Other"),

    unemployment_duration = fct_relevel(unemployment_duration,
      "Less than 3 months", "Less than 6 months", "Less than 12 months",
      "1 year to less than 2 years", "2 years and above"),

    usual_hours = fct_relevel(usual_hours, "1-24 hrs", "25-34 hrs", "35-40 hrs",
                              "41-48 hrs", "49-61 hrs", "62-79 hrs", "80+ hrs")
  )


# -----------------------------------------------------------------------------
# Step 8. Handle missing values
# -----------------------------------------------------------------------------
# Rule: first ask WHY a value is missing, then decide.
#
#  - "Not applicable" (question not asked to that person): replaced by a
#    clear label so charts and tables are easy to read.
#      * migration: people who never moved          -> "Non-migrant" / "Did not move"
#      * TVET details: people without TVET          -> "No TVET"
#  - Questions asked only to the employed or only to the unemployed are left
#    as NA. The dashboard filters to the right group before using them.
#  - Real non-response:
#      * tvet_training (3.5%)  -> "Not stated"
#      * time-use questions (6%) -> left as NA, excluded from percentages
#      * years_of_schooling (2 rows) -> rows dropped

youth <- youth %>%
  mutate(
    migration_status  = fct_na_value_to_level(migration_status,  "Non-migrant"),
    reason_for_moving = fct_na_value_to_level(reason_for_moving, "Did not move"),
    tvet_training     = fct_na_value_to_level(tvet_training,     "Not stated"),
    tvet_duration     = if_else(tvet_training == "Yes", tvet_duration, NA),
    tvet_completed    = if_else(tvet_training == "Yes", tvet_completed, NA)
  )

n_missing_school <- sum(is.na(youth$years_of_schooling))
youth <- youth %>% filter(!is.na(years_of_schooling)) %>% droplevels()


out_age    <- boxplot.stats(youth$age)$out
out_school <- boxplot.stats(youth$years_of_schooling)$out
out_hhsize <- boxplot.stats(youth$household_size)$out

cat("Outliers - age               :", length(out_age), "\n")
cat("Outliers - years of schooling:", length(out_school),
    " (values:", sort(unique(out_school)), ")\n")
cat("Outliers - household size    :", length(out_hhsize), "\n")

dir.create("Outputs/Figures", recursive = TRUE, showWarnings = FALSE)
png("Outputs/Figures/eda_outliers_boxplots.png", width = 1800, height = 700, res = 200)
par(mfrow = c(1, 3))
boxplot(youth$age, main = "Age", col = "#7FC9C8")
boxplot(youth$years_of_schooling, main = "Years of schooling", col = "#7FC9C8")
boxplot(youth$household_size, main = "Household size", col = "#7FC9C8")
dev.off()


stopifnot(
  all(youth$age >= 16 & youth$age <= 30),
  all(youth$household_size >= 1 & youth$household_size <= 10),
  !anyNA(youth$employment_status)
)

cat("\n========== EDA / dashboard data ==========\n")
cat("Raw records                 :", nrow(lfs_raw), "\n")
cat("Duplicate youth removed     :", n_duplicates, "\n")
cat("Missing schooling removed   :", n_missing_school, "\n")
cat("Youth in the labour force   :", nrow(youth), "\n")
cat("Variables                   :", ncol(youth), "\n\n")
print(table(youth$employment_status))

cat("\nMissing values (mostly questions asked only to some youth):\n")
missing <- colSums(is.na(youth))
print(round(100 * missing[missing > 0] / nrow(youth), 1))

# Saving

saveRDS(youth, "Data/RW_LFS2025_Youth_EDA.rds")
write.csv(youth, "Data/RW_LFS2025_Youth_EDA.csv", row.names = FALSE, na = "")

cat("\nSaved: Data/RW_LFS2025_Youth_EDA.rds\n")
