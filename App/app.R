# =============================================================================
#  Rwanda Youth Employment Dashboard (LFS 2025)
# =============================================================================
#  How to run: open this file in RStudio and click "Run App".
#
#  Files
#    app.R          - layout (UI) and logic (server)          <- this file
#    R/helpers.R    - charts, summary tables and insight text
#    www/styles.css - colours, cards, hover effects, dark mode
#
#  Data: Data/RW_LFS2025_Youth_EDA.rds, made by
#        Scripts/01_Data_Cleaning_EDA_Dashboard.R
#  Model: Outputs/ML_simple/dashboard_model.rds (best model), made by
#        Scripts/ml_script_simple.R
# =============================================================================

library(caret)        # needed to predict with the saved model
library(ranger)       # the saved model is a random forest made with ranger
library(shiny)
library(bslib)
library(bsicons)
library(shinyWidgets)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(forcats)


# =============================================================================
# 1. Data
# =============================================================================

data_file <- c("../Data/RW_LFS2025_Youth_EDA.rds", "Data/RW_LFS2025_Youth_EDA.rds")
data_file <- data_file[file.exists(data_file)][1]
if (is.na(data_file)) stop("Data not found. Run Scripts/01_Data_Cleaning_EDA_Dashboard.R first.")

lfs <- readRDS(data_file) %>%
  mutate(unemployed = as.numeric(employment_status == "Unemployed"))

# Machine-learning results (best model + test-set results of all models)
ml_file <- c("../Outputs/ML_simple/dashboard_model.rds", "Outputs/ML_simple/dashboard_model.rds")
ml_file <- ml_file[file.exists(ml_file)][1]
if (is.na(ml_file)) stop("Model not found. Run Scripts/ml_script_simple.R first.")
ml <- readRDS(ml_file)

app_name   <- ml$app_name                                  # model used in the app (highest ROC AUC)
app_label  <- clean_model(app_name)                         # e.g. "Random_forest" -> "Random forest"
model_cols[[app_label]] <- pal$coral                       # the app model is drawn in coral
app_cutoff <- 0.5                                          # default cut-off of the model
base_rate  <- mean(ml$test_probs$actual == "Unemployed")   # real share of unemployed youth
app_row    <- ml$results[ml$results$Model == app_name, ]
roc_data   <- roc_points(ml$test_probs)

# Variables people can explore, grouped by tier
explore_vars <- list(
  "Core" = c("Sex" = "sex", "Education level" = "education_level",
             "Residence" = "residence", "Province" = "province"),
  "First level" = c("Age group" = "age_group", "Marital status" = "marital_status",
                    "Relationship to head" = "household_relationship",
                    "Currently a student" = "current_student",
                    "TVET training" = "tvet_training", "Disability" = "disability"),
  "Second level" = c("Education track" = "education_track",
                     "Field of education" = "field_of_education",
                     "Lived outside district" = "lived_outside_district",
                     "Migration status" = "migration_status",
                     "Lighting source" = "lighting_source",
                     "Own-use farming" = "own_use_farming",
                     "Nationality" = "nationality")
)
flat_vars <- unlist(unname(explore_vars))
var_label <- function(v) names(flat_vars)[flat_vars == v][1]

# Columns shown in the data explorer, by tier
tiers <- list(
  "Core"         = c("employment_status", "age", "sex", "education_level", "residence", "province"),
  "First level"  = c("age_group", "district", "years_of_schooling", "current_student",
                     "marital_status", "tvet_training", "disability", "household_size",
                     "household_relationship"),
  "Second level" = c("field_of_education", "education_track", "attending_tvet_now",
                     "tvet_completed", "migration_status", "reason_for_moving",
                     "lighting_source", "own_use_farming", "fetches_water",
                     "collects_firewood", "household_chores", "care_work"),
  "Optional"     = c("unemployment_duration", "ever_worked_before", "main_income_source",
                     "status_in_employment", "occupation", "industry", "economic_sector",
                     "formal_informal_work", "time_underemployment", "usual_hours",
                     "contract_type", "social_security", "income_band", "how_job_found")
)

# Navigation menu: value = page id
pages <- c(
  "Executive summary"     = "exec",
  "Who is unemployed?"    = "who",
  "Education & skills"    = "skills",
  "Geography"             = "geo",
  "Unemployed youth"      = "unemp",
  "Employed youth"        = "jobs",
  "Household & time use"  = "house",
  "Model & predictions"   = "model",
  "Data explorer"         = "data",
  "Help & glossary"       = "help"
)
page_icons <- c("speedometer2", "people", "mortarboard", "geo-alt", "person-x",
                "briefcase", "house-door", "cpu", "table", "question-circle")


# =============================================================================
# 2. Theme
# =============================================================================

app_theme <- bs_theme(
  version = 5,
  primary = "#2C7A7B",
  secondary = "#051650",
  base_font = font_collection("Inter", "Segoe UI", "Roboto", "Arial", "sans-serif"),
  "border-radius" = "8px"
)


# =============================================================================
# 3. User interface
# =============================================================================

# ---- Sidebar: menu + filters -------------------------------------------------
picker <- function(id, label, var) {
  choices <- levels(droplevels(lfs[[var]]))
  pickerInput(id, label, choices = choices, selected = choices, multiple = TRUE,
              options = pickerOptions(actionsBox = TRUE, selectedTextFormat = "count > 2",
                                      countSelectedText = "{0} selected", noneSelectedText = "None"))
}

side_panel <- sidebar(
  width = 270, open = "desktop",
  div(class = "side-section", "Menu"),
  div(class = "nav-menu",
      radioGroupButtons(
        "page", NULL, direction = "vertical", justified = TRUE,
        choiceValues = unname(pages),
        choiceNames = lapply(seq_along(pages), function(i)
          tagList(bs_icon(page_icons[i]), names(pages)[i]))
      )),
  div(class = "side-section", bs_icon("funnel-fill"), " Dashboard filters"),
  div(class = "filter-box",
      picker("f_sex", "Sex", "sex"),
      picker("f_res", "Residence", "residence"),
      picker("f_prov", "Province", "province"),
      picker("f_edu", "Education level", "education_level"),
      sliderInput("f_age", "Age", min = 16, max = 30, value = c(16, 30), step = 1, ticks = FALSE),
      input_switch("weighted", "Use survey weights", value = TRUE),
      actionButton("reset", "Reset filters", icon = icon("rotate-left"), class = "btn-reset"),
      div(class = "filter-note", textOutput("filter_note"))
  )
)

# ---- Page contents ------------------------------------------------------------
page_exec <- tagList(
  page_banner("Executive Dashboard", "Youth employment in Rwanda at a glance - Labour Force Survey 2025"),
  page_intro(uiOutput("intro_exec")),
  div(class = "kpi-strip",
      layout_column_wrap(
        width = 1/4, fill = FALSE,
        kpi_hero("people-fill", "k_youth", "Youth in the labour force"),
        kpi_hero("person-x-fill", "k_unemp", "Unemployed youth"),
        kpi_hero("percent", "k_rate", "Youth unemployment rate"),
        kpi_hero("gender-female", "k_female", "Female unemployment rate")
      )),
  layout_column_wrap(
    width = 1/4, fill = FALSE, gap = "14px",
    kpi_card("Male unemployment", "k_male", "k_male_sub", "gender-male", "navy"),
    kpi_card("Urban unemployment", "k_urban", "k_urban_sub", "buildings", "teal"),
    kpi_card("Rural unemployment", "k_rural", "k_rural_sub", "tree", "mix"),
    kpi_card("University graduates", "k_uni", "k_uni_sub", "mortarboard", "coral"),
    kpi_card("Long-term unemployed", "k_long", "k_long_sub", "hourglass-split", "navy"),
    kpi_card("First-time jobseekers", "k_first", "k_first_sub", "person-plus", "teal"),
    kpi_card("Informal jobs", "k_informal", "k_informal_sub", "briefcase", "mix"),
    kpi_card("Underemployed workers", "k_under", "k_under_sub", "clock-history", "coral")
  ),
  br(),
  layout_columns(
    col_widths = c(6, 6),
    dash_box("Unemployment by education level", "mortarboard-fill",
             plotlyOutput("c_edu", height = chart_h), insight_id = "i_edu"),
    dash_box("Unemployment by age and sex", "graph-up", teal = TRUE,
             plotlyOutput("c_age", height = chart_h), insight_id = "i_age")
  ),
  layout_columns(
    col_widths = c(6, 6),
    dash_box("Unemployment by relationship to household head", "house-heart-fill", teal = TRUE,
             plotlyOutput("c_rel", height = chart_h), insight_id = "i_rel"),
    dash_box("Unemployment by province and residence", "geo-alt-fill",
             plotlyOutput("c_prov", height = chart_h), insight_id = "i_prov")
  )
)

page_who <- tagList(
  page_banner("Who is unemployed?", "Compare unemployment across any two characteristics"),
  page_intro(p("Pick a characteristic and, if you like, split it by a second one.",
               "Bars in", tags$b("red"), "are above the overall rate, bars in",
               tags$b("blue"), "are below it. The tables below give exact numbers with totals.")),
  # The two drop-downs sit inside the chart card, so the chart can use the full width
  dash_box(textOutput("who_title", inline = TRUE), "bar-chart-fill",
           layout_columns(
             col_widths = c(6, 6),
             selectInput("x_var", "Show unemployment by", explore_vars, "education_level", width = "100%"),
             selectInput("by_var", "Split by (clustered bars)", c(list("(none)" = "none"), explore_vars),
                         "sex", width = "100%")),
           plotlyOutput("c_who", height = chart_h), insight_id = "i_who"),
  layout_columns(
    col_widths = c(6, 6),
    dash_box("Summary table with margins", "table", uiOutput("t_who")),
    dash_box("Two-way table of unemployment rates", "grid-3x3-gap-fill", teal = TRUE,
             uiOutput("t_who_matrix"))
  )
)

page_skills <- tagList(
  page_banner("Education & Skills", "Schooling, field of study and technical training (TVET)"),
  page_intro(p("Rwanda shows an", tags$b("education paradox:"),
               "youth with secondary or university education are more often unemployed than",
               "youth with little schooling. This page looks at what kind of education and",
               "training is linked to finding work.")),
  layout_column_wrap(
    width = 1/4, fill = FALSE,
    kpi_card("Youth with TVET training", "k_tvet", "k_tvet_sub", "tools", "teal"),
    kpi_card("TVET with certificate", "k_cert", "k_cert_sub", "patch-check", "navy"),
    kpi_card("Average years of schooling", "k_years", "k_years_sub", "book", "mix"),
    kpi_card("Upper secondary unemployment", "k_upper", "k_upper_sub", "exclamation-triangle", "coral")
  ),
  br(),
  layout_columns(
    col_widths = c(6, 6),
    dash_box("Unemployment by education track", "signpost-split-fill",
             plotlyOutput("c_track", height = chart_h), insight_id = "i_track"),
    dash_box("Unemployment by field of education", "journal-bookmark-fill", teal = TRUE,
             plotlyOutput("c_field", height = chart_h), insight_id = "i_field")
  ),
  layout_columns(
    col_widths = c(4, 4, 4),
    dash_box("Did TVET trainees complete?", "patch-check-fill",
             plotlyOutput("c_tvet_done", height = chart_h)),
    dash_box("What happened after TVET?", "arrow-right-circle-fill", teal = TRUE,
             plotlyOutput("c_tvet_after", height = chart_h)),
    dash_box("Time to find a job after TVET", "stopwatch-fill",
             plotlyOutput("c_tvet_time", height = chart_h))
  ),
  dash_box("Education level: summary table with margins", "table", uiOutput("t_edu"))
)

page_geo <- tagList(
  page_banner("Geography", "Where youth unemployment is highest"),
  page_intro(p("District rates are based on smaller samples, so small differences",
               "between districts should not be over-interpreted.")),
  dash_box("Youth unemployment rate by district", "pin-map-fill",
           plotlyOutput("c_district", height = 680), insight_id = "i_district"),
  layout_columns(
    col_widths = c(6, 6),
    dash_box("Province summary with margins", "table", teal = TRUE, uiOutput("t_prov")),
    dash_box("Province x residence", "grid-3x3-gap-fill", uiOutput("t_prov_res"))
  )
)

page_unemp <- tagList(
  page_banner("Unemployed Youth", "How long they search, whether they worked before, how they live"),
  page_intro(p("These questions were asked only to unemployed youth, so this page",
               "describes the unemployed group only.")),
  layout_column_wrap(
    width = 1/4, fill = FALSE,
    kpi_card("Unemployed youth", "k_u_n", "k_u_n_sub", "person-x", "coral"),
    kpi_card("Searching < 3 months", "k_u_short", "k_u_short_sub", "hourglass-top", "teal"),
    kpi_card("Never worked before", "k_u_never", "k_u_never_sub", "person-plus", "navy"),
    kpi_card("Supported by parents", "k_u_parents", "k_u_parents_sub", "house-heart", "mix")
  ),
  br(),
  layout_columns(
    col_widths = c(6, 6),
    dash_box("How long have they been unemployed?", "hourglass-split",
             plotlyOutput("c_u_dur", height = chart_h), insight_id = "i_u_dur"),
    dash_box("Unemployment duration by sex", "gender-ambiguous", teal = TRUE,
             plotlyOutput("c_u_dur_sex", height = chart_h))
  ),
  layout_columns(
    col_widths = c(6, 6),
    dash_box("Ever had a paid job?", "clock-history", teal = TRUE,
             plotlyOutput("c_u_ever", height = chart_h), insight_id = "i_u_ever"),
    dash_box("Main source of income while unemployed", "cash-coin",
             plotlyOutput("c_u_income", height = chart_h), insight_id = "i_u_income")
  ),
  dash_box("Unemployment duration by education level", "mortarboard-fill",
           plotlyOutput("c_u_dur_edu", height = chart_h))
)

page_jobs <- tagList(
  page_banner("Employed Youth", "The quality of jobs young people hold"),
  page_intro(p("Having a job is not the end of the story. Most employed youth work",
               "informally, many work few hours, and few employees have social protection.",
               "Benefit and income questions were asked only to paid employees.")),
  layout_column_wrap(
    width = 1/4, fill = FALSE,
    kpi_card("Informal employment", "k_j_inf", "k_j_inf_sub", "briefcase", "coral"),
    kpi_card("Time-related underemployed", "k_j_under", "k_j_under_sub", "clock", "navy"),
    kpi_card("Employees with social security", "k_j_ss", "k_j_ss_sub", "shield-check", "teal"),
    kpi_card("Employees earning < 30,000 RWF", "k_j_low", "k_j_low_sub", "cash", "mix")
  ),
  br(),
  layout_columns(
    col_widths = c(6, 6),
    dash_box("Status in employment", "person-badge-fill",
             plotlyOutput("c_j_status", height = chart_h), insight_id = "i_j_status"),
    dash_box("Occupation", "tools", teal = TRUE,
             plotlyOutput("c_j_occ", height = chart_h), insight_id = "i_j_occ")
  ),
  layout_columns(
    col_widths = c(6, 6),
    dash_box("Top 10 industries", "building-fill", teal = TRUE,
             plotlyOutput("c_j_ind", height = chart_h), insight_id = "i_j_ind"),
    dash_box("Formal vs informal work by sex", "file-earmark-check-fill",
             plotlyOutput("c_j_formal", height = chart_h))
  ),
  layout_columns(
    col_widths = c(4, 4, 4),
    dash_box("Usual weekly working hours", "clock-fill",
             plotlyOutput("c_j_hours", height = chart_h)),
    dash_box("Employee benefits by sex", "shield-fill-check", teal = TRUE,
             plotlyOutput("c_j_benefits", height = chart_h)),
    dash_box("Monthly cash income (employees)", "cash-stack",
             plotlyOutput("c_j_income", height = chart_h))
  ),
  dash_box("How did employees find their job?", "search",
           plotlyOutput("c_j_found", height = chart_h), insight_id = "i_j_found")
)

page_house <- tagList(
  page_banner("Household & Time Use", "Living conditions, migration and unpaid work"),
  page_intro(p("The lighting source is used as a simple indicator of household living",
               "standards. Unpaid work covers fetching water and firewood, chores and care for",
               "children or older people during the past 7 days.")),
  layout_columns(
    col_widths = c(6, 6),
    dash_box("Unemployment by household lighting source", "lightbulb-fill",
             plotlyOutput("c_h_light", height = chart_h), insight_id = "i_h_light"),
    dash_box("Unemployment by migration status", "signpost-2-fill", teal = TRUE,
             plotlyOutput("c_h_mig", height = chart_h), insight_id = "i_h_mig")
  ),
  layout_columns(
    col_widths = c(6, 6),
    dash_box("Unpaid work in the past 7 days", "basket-fill", teal = TRUE,
             radioGroupButtons("care_by", NULL, size = "sm",
                               choices = c("By sex" = "sex", "By employment status" = "employment_status")),
             plotlyOutput("c_h_care", height = chart_h), insight_id = "i_h_care"),
    dash_box("Why did migrants move?", "arrow-left-right",
             plotlyOutput("c_h_reason", height = chart_h))
  )
)

# ---- Model & predictions ------------------------------------------------------
# Drop-down for the risk calculator, filled with the levels of a variable
calc_select <- function(id, label, var, selected) {
  selectInput(id, label, choices = levels(droplevels(lfs[[var]])), selected = selected)
}

page_model <- tagList(
  page_banner("Model & Predictions", "Who is at risk of unemployment? A machine-learning view (caret)"),
  page_intro(p(
    nrow(ml$results), "models were trained with", tags$b("caret"), "on 70% of the youth and tested on the other 30%.",
    "Each model was tuned with 5-fold cross-validation, chosen by ROC AUC, and trained on",
    tags$b("down-sampled"), "folds (as many employed as unemployed youth) because only 15% are unemployed.",
    "The dashboard uses", tags$b(model_full_name[[app_label]]), ", the model with the",
    tags$b("highest ROC AUC"), "on the test set. The sidebar filters do not apply to this page.")),
  div(class = "kpi-strip",
      layout_column_wrap(
        width = 1/4, fill = FALSE,
        kpi_hero("cpu-fill", "k_m_model", "Model used in the app"),
        kpi_hero("activity", "k_m_auc", "Test ROC AUC"),
        kpi_hero("person-check-fill", "k_m_recall", "Unemployed youth found (recall)"),
        kpi_hero("shield-check", "k_m_spec", "Employed correctly cleared")
      )),

  # Model comparison
  layout_columns(
    col_widths = c(6, 6),
    dash_box("Model comparison on the test set", "trophy-fill", uiOutput("t_models"),
             insight_id = "i_models"),
    dash_box("Main scores by model", "bar-chart-steps", teal = TRUE,
             plotlyOutput("c_models", height = chart_h))
  ),
  layout_columns(
    col_widths = c(6, 6),
    dash_box("ROC curves on the test set", "graph-up-arrow", teal = TRUE,
             plotlyOutput("c_roc", height = chart_h), insight_id = "i_roc"),
    dash_box("Cross-validated ROC AUC (5 folds)", "arrow-repeat",
             plotlyOutput("c_cv", height = chart_h), insight_id = "i_cv")
  ),

  # Cut-off explorer
  layout_columns(
    col_widths = c(6, 6),
    dash_box(paste("Confusion matrix of", app_label, "- try a cut-off"), "grid-fill",
             sliderInput("m_cut", "Cut-off: a score at or above this means 'unemployed'",
                         min = 0.2, max = 0.8, value = app_cutoff, step = 0.01, width = "100%"),
             uiOutput("m_confusion"), uiOutput("m_chips"), insight_id = "i_cut"),
    dash_box(paste(app_label, "risk scores on the test set"), "bar-chart-fill", teal = TRUE,
             plotlyOutput("c_hist", height = chart_h), insight_id = "i_hist")
  ),
  layout_columns(
    col_widths = c(6, 6),
    dash_box("Which variables matter most?", "list-ol", teal = TRUE,
             plotlyOutput("c_imp", height = chart_h), insight_id = "i_imp"),
    dash_box("How to read these results", "info-circle-fill",
             tags$dl(class = "glossary",
               tags$dt("ROC AUC"), tags$dd("Chance that the model gives a higher score to a random unemployed youth than to a random employed one. 0.5 = guessing, 1 = perfect."),
               tags$dt("Recall (sensitivity)"), tags$dd("Share of unemployed youth the model finds."),
               tags$dt("Specificity"), tags$dd("Share of employed youth the model correctly leaves out."),
               tags$dt("Precision"), tags$dd("Of the youth the model flags, the share who are really unemployed."),
               tags$dt("Kappa"), tags$dd("Agreement with the truth after removing agreement expected by chance."),
               tags$dt("Why is accuracy low?"), tags$dd("Saying 'employed' for everybody gives 85% accuracy but finds nobody. The models trade some accuracy to find most unemployed youth."),
               tags$dt("Risk score vs real chance"), tags$dd("The model learned on balanced data, so its score is not a real probability. The calculator converts it back using the true share of unemployed youth (15%).")))
  ),

  # Risk calculator
  card(class = "dash-box calc-box", full_screen = TRUE,
       card_header(bs_icon("calculator-fill"), paste0("Unemployment risk calculator (", app_label, ")")),
       card_body(
         layout_columns(
           col_widths = c(7, 5),
           div(
             p(class = "muted", "Describe a young person. The prediction updates as you change the profile."),
             layout_columns(
               col_widths = c(4, 4, 4),
               div(class = "calc-group", h6(bs_icon("person"), " Person"),
                   sliderInput("p_age", "Age", 16, 30, 24, step = 1, ticks = FALSE),
                   calc_select("p_sex", "Sex", "sex", "Female"),
                   calc_select("p_marital", "Marital status", "marital_status", "Single"),
                   calc_select("p_disability", "Disability", "disability", "No"),
                   calc_select("p_rel", "Relationship to household head", "household_relationship",
                               "Child (Son/daughter)"),
                   sliderInput("p_hhsize", "Household size", 1, 10, 4, step = 1, ticks = FALSE)),
               div(class = "calc-group", h6(bs_icon("mortarboard"), " Education"),
                   calc_select("p_edu", "Education level", "education_level", "Upper secondary"),
                   sliderInput("p_years", "Years of schooling", 0, 20, 12, step = 1, ticks = FALSE),
                   calc_select("p_track", "Education track", "education_track", "General education"),
                   calc_select("p_field", "Field of education", "field_of_education", "General education"),
                   calc_select("p_student", "Currently a student", "current_student", "No"),
                   calc_select("p_tvet", "TVET training", "tvet_training", "No")),
               div(class = "calc-group", h6(bs_icon("geo-alt"), " Place & household"),
                   calc_select("p_res", "Residence", "residence", "Urban"),
                   calc_select("p_prov", "Province (to filter districts)", "province", "City of Kigali"),
                   selectInput("p_district", "District", choices = NULL),
                   calc_select("p_light", "Lighting source", "lighting_source", "Grid electricity"),
                   calc_select("p_outside", "Lived outside district", "lived_outside_district", "No"),
                   calc_select("p_mig", "Migration status", "migration_status", "Non-migrant"))
             )),
           div(class = "calc-result",
               h6("Risk score (0-100)"),
               plotlyOutput("c_gauge", height = 210),
               uiOutput("calc_verdict"),
               h6(class = "mt-3", "Same profile at other ages"),
               plotlyOutput("c_age_curve", height = 200))
         )))
)

page_data <- tagList(
  page_banner("Data Explorer", "Browse and download the filtered records"),
  dash_box("Records", "table",
           layout_columns(
             col_widths = c(9, 3),
             checkboxGroupButtons("tiers", "Variable tiers to show", choices = names(tiers),
                                  selected = c("Core", "First level"), size = "sm",
                                  checkIcon = list(yes = icon("check"))),
             div(class = "text-end pt-4",
                 downloadButton("download", "Download CSV", class = "btn-primary btn-sm"))
           ),
           DTOutput("t_data"))
)

page_help <- tagList(
  page_banner("Help & Glossary", "Definitions, data source and variable tiers"),
  layout_columns(
    col_widths = c(6, 6),
    dash_box("Glossary", "book-half",
             tags$dl(class = "glossary",
               tags$dt("Labour force"), tags$dd("Youth who are employed or unemployed. Students and others not looking for work are outside it."),
               tags$dt("Unemployment rate"), tags$dd("Unemployed youth divided by all youth in the labour force."),
               tags$dt("Survey weights"), tags$dd("When on, numbers represent the whole youth population of Rwanda. When off, they describe the sample."),
               tags$dt("Informal employment"), tags$dd("Work without social security or other legal protection."),
               tags$dt("Time-related underemployment"), tags$dd("Working fewer hours than wanted and being available for more."),
               tags$dt("TVET"), tags$dd("Technical and Vocational Education and Training."),
               tags$dt("Margins"), tags$dd("Total rows and columns of a summary table."))),
    dash_box("Data and variable tiers", "layers-fill", teal = TRUE,
             p(tags$b("Source: "), "Rwanda Labour Force Survey 2025 (NISR). Youth aged 16-30 in the labour force."),
             tags$ul(
               tags$li(tags$b("Core:"), " outcome, age, sex, education, residence, province."),
               tags$li(tags$b("First level:"), " age group, district, schooling, student, marital status, TVET, disability, household."),
               tags$li(tags$b("Second level:"), " field of education, education track, migration, lighting source, own-use farming, unpaid work."),
               tags$li(tags$b("Optional:"), " questions asked only to the unemployed (duration, income source) or the employed (job quality).")
             ),
             p(class = "muted", "Machine-learning models use a separate, smaller dataset without",
               "variables that reveal the answer (data leakage)."))
  )
)

# ---- Assemble -------------------------------------------------------------------
ui <- page_sidebar(
  window_title = "Rwanda Youth Employment",
  theme = app_theme,
  fillable = FALSE,
  title = div(class = "app-title",
              div(class = "brand-icon", bs_icon("bar-chart-line-fill")),
              span(class = "brand-name", "Rwanda Youth Employment"),
              span(class = "brand-sub", "LFS 2025"),
              div(class = "spacer"),
              # Chart options: show/hide the numbers on the charts, sort the bars
              div(class = "top-switch", input_switch("labels", "Numbers on charts", value = TRUE)),
              div(class = "top-switch", input_switch("sort_bars", "Sort bars", value = FALSE)),
              span(class = "pill", textOutput("mode_pill", inline = TRUE)),
              input_dark_mode(id = "dark", mode = "light")),
  sidebar = side_panel,
  tags$head(
    tags$link(rel = "stylesheet", href = "styles.css"),
    # When "Numbers on charts" is off, the page gets the class "no-labels",
    # and styles.css hides the numbers on every chart (no need to redraw them).
    tags$script(HTML("
      $(document).on('change', '#labels', function() {
        document.body.classList.toggle('no-labels', !this.checked);
      });"))
  ),
  navset_hidden(
    id = "pages",
    nav_panel_hidden("exec", page_exec),
    nav_panel_hidden("who", page_who),
    nav_panel_hidden("skills", page_skills),
    nav_panel_hidden("geo", page_geo),
    nav_panel_hidden("unemp", page_unemp),
    nav_panel_hidden("jobs", page_jobs),
    nav_panel_hidden("house", page_house),
    nav_panel_hidden("model", page_model),
    nav_panel_hidden("data", page_data),
    nav_panel_hidden("help", page_help)
  )
)


# =============================================================================
# 4. Server
# =============================================================================

server <- function(input, output, session) {

  observeEvent(input$page, nav_select("pages", input$page))

  # ---- Filtered data ---------------------------------------------------------
  filtered <- reactive({
    d <- lfs %>%
      filter(sex %in% input$f_sex, residence %in% input$f_res,
             province %in% input$f_prov, education_level %in% input$f_edu,
             between(age, input$f_age[1], input$f_age[2]))
    d$.w <- if (isTRUE(input$weighted)) d$survey_weight else 1
    validate(need(nrow(d) >= 30, "Too few youth match these filters. Please widen the selection."))
    d
  })
  employed   <- reactive(filter(filtered(), employment_status == "Employed"))
  unemployed <- reactive(filter(filtered(), employment_status == "Unemployed"))
  employees  <- reactive(filter(employed(), !is.na(contract_type)))

  observeEvent(input$reset, {
    ids <- c(f_sex = "sex", f_res = "residence", f_prov = "province", f_edu = "education_level")
    for (id in names(ids)) updatePickerInput(session, id, selected = levels(droplevels(lfs[[ids[[id]]]])))
    updateSliderInput(session, "f_age", value = c(16, 30))
  })

  output$filter_note <- renderText({
    paste(num(nrow(filtered())), "of", num(nrow(lfs)), "youth in the sample selected")
  })
  output$mode_pill <- renderText(if (isTRUE(input$weighted)) "Weighted estimates" else "Sample counts")

  # Small helpers used by many outputs
  share_of <- function(df, var, level) {
    d <- filter(df, !is.na(.data[[var]]))
    if (nrow(d) == 0) return(NA)
    sum(d$.w[d[[var]] %in% level]) / sum(d$.w)
  }
  rate_in <- function(var, level) {
    d <- filter(filtered(), .data[[var]] == level)
    if (nrow(d) == 0) return(NA)
    overall_rate(d)
  }
  sample_text <- function(df) paste("Sample:", num(nrow(df)), "youth")

  # ===========================================================================
  # Executive summary
  # ===========================================================================
  output$intro_exec <- renderUI({
    d <- filtered(); r <- rate_by(d, "sex")
    f <- r$rate[r$category == "Female"]; m <- r$rate[r$category == "Male"]
    edu <- rate_by(d, "education_level") %>% filter(youth >= 30)
    top <- edu[which.max(edu$rate), ]
    p(HTML(sprintf(
      "In 2025, about <b>%s</b> young people aged 16-30 were in Rwanda's labour force%s, and <b>%s</b> of them were unemployed (<b>%s</b>). %s The highest rate is among youth with <b>%s</b> education (<b>%s</b>). Use the filters on the left to focus on any group.",
      num(sum(d$.w)), if (isTRUE(input$weighted)) "" else " (sample count)",
      num(sum(d$.w * d$unemployed)), pct(overall_rate(d)),
      if (length(f) && length(m)) sprintf("Young women are more affected (<b>%s</b>) than young men (<b>%s</b>).", pct(f), pct(m)) else "",
      tolower(top$category), pct(top$rate))))
  })

  output$k_youth  <- renderText(num(sum(filtered()$.w)))
  output$k_unemp  <- renderText(num(sum(filtered()$.w * filtered()$unemployed)))
  output$k_rate   <- renderText(pct(overall_rate(filtered())))
  output$k_female <- renderText(pct(rate_in("sex", "Female")))

  output$k_male     <- renderText(pct(rate_in("sex", "Male")))
  output$k_male_sub <- renderText("of young men in the labour force")
  output$k_urban     <- renderText(pct(rate_in("residence", "Urban")))
  output$k_urban_sub <- renderText("youth living in urban areas")
  output$k_rural     <- renderText(pct(rate_in("residence", "Rural")))
  output$k_rural_sub <- renderText("youth living in rural areas")
  output$k_uni       <- renderText(pct(rate_in("education_level", "University")))
  output$k_uni_sub   <- renderText("unemployment among graduates")
  output$k_long      <- renderText(pct(share_of(unemployed(), "unemployment_duration",
                                                c("1 year to less than 2 years", "2 years and above"))))
  output$k_long_sub  <- renderText("of unemployed, searching 1 year or more")
  output$k_first     <- renderText(pct(share_of(unemployed(), "ever_worked_before", "No")))
  output$k_first_sub <- renderText("of unemployed never had a paid job")
  output$k_informal     <- renderText(pct(share_of(employed(), "formal_informal_work", "Informal employment")))
  output$k_informal_sub <- renderText("of employed youth work informally")
  output$k_under     <- renderText(pct(share_of(employed(), "time_underemployment", "Time related underemployed")))
  output$k_under_sub <- renderText("of employed want more working hours")

  output$c_edu <- renderPlotly(rate_chart(filtered(), "education_level", sort = isTRUE(input$sort_bars)))
  output$i_edu <- renderUI(insight_rate(filtered(), "education_level", "education"))

  output$c_age <- renderPlotly({
    d <- filtered() %>% group_by(age, sex) %>%
      summarise(rate = weighted.mean(unemployed, .w), n = n(), .groups = "drop")
    plot_ly(d, x = ~age, y = ~rate, color = ~sex, colors = c(pal$blue, pal$gold),
            type = "scatter", mode = "lines+markers", line = list(width = 3),
            marker = list(size = 7), customdata = ~n,
            hovertemplate = "Age %{x}<br>%{fullData.name}: %{y:.1%}<br>Sample: %{customdata}<extra></extra>") %>%
      style_plot(x_title = "Age", y_title = "Unemployment rate", y_pct = TRUE)
  })
  output$i_age <- renderUI(insight_rate(filtered(), "age_group", "age"))

  output$c_rel <- renderPlotly(rate_chart(filtered(), "household_relationship", sort = isTRUE(input$sort_bars)))
  output$i_rel <- renderUI(insight_rate(filtered(), "household_relationship", "household-position"))

  output$c_prov <- renderPlotly(rate_split_chart(filtered(), "province", "residence"))
  output$i_prov <- renderUI(insight_rate(filtered(), "province", "provincial"))

  # ===========================================================================
  # Who is unemployed?
  # ===========================================================================
  split_on <- reactive(input$by_var != "none" && input$by_var != input$x_var)

  output$who_title <- renderText({
    if (split_on()) paste("Unemployment by", tolower(var_label(input$x_var)), "and", tolower(var_label(input$by_var)))
    else paste("Unemployment by", tolower(var_label(input$x_var)))
  })
  output$c_who <- renderPlotly({
    if (split_on()) rate_split_chart(filtered(), input$x_var, input$by_var)
    else rate_chart(filtered(), input$x_var, sort = isTRUE(input$sort_bars))
  })
  output$i_who <- renderUI(insight_rate(filtered(), input$x_var, tolower(var_label(input$x_var))))
  output$t_who <- renderUI(margin_table(filtered(), input$x_var, isTRUE(input$weighted)))
  output$t_who_matrix <- renderUI({
    by <- if (split_on()) input$by_var else "sex"
    if (by == input$x_var) by <- "residence"
    rate_matrix_table(filtered(), input$x_var, by)
  })

  # ===========================================================================
  # Education & skills
  # ===========================================================================
  output$k_tvet     <- renderText(pct(share_of(filtered(), "tvet_training", "Yes")))
  output$k_tvet_sub <- renderText("of youth learned a trade or TVET course")
  output$k_cert     <- renderText(pct(share_of(filter(filtered(), tvet_training == "Yes"), "tvet_completed",
                                               c("Yes, with certificate", "Yes with degree/Diploma"))))
  output$k_cert_sub <- renderText("of TVET trainees hold a certificate or diploma")
  output$k_years    <- renderText(sprintf("%.1f", weighted.mean(filtered()$years_of_schooling, filtered()$.w)))
  output$k_years_sub <- renderText("years, youth in the labour force")
  output$k_upper     <- renderText(pct(rate_in("education_level", "Upper secondary")))
  output$k_upper_sub <- renderText("the highest of all education levels")

  output$c_track <- renderPlotly(rate_chart(filtered(), "education_track", sort = isTRUE(input$sort_bars)))
  output$i_track <- renderUI(insight_rate(filtered(), "education_track", "education-track"))
  output$c_field <- renderPlotly(rate_chart(filtered(), "field_of_education", sort = TRUE))
  output$i_field <- renderUI(insight_rate(filtered(), "field_of_education", "field-of-study"))

  tvet <- reactive(filter(filtered(), tvet_training == "Yes"))
  output$c_tvet_done  <- renderPlotly(donut_chart(tvet(), "tvet_completed"))
  output$c_tvet_after <- renderPlotly(share_chart(tvet(), "activity_after_tvet", pal$teal))
  output$c_tvet_time  <- renderPlotly(share_chart(tvet(), "time_to_job_after_tvet", pal$blue, ordered = TRUE))
  output$t_edu <- renderUI(margin_table(filtered(), "education_level", isTRUE(input$weighted)))

  # ===========================================================================
  # Geography
  # ===========================================================================
  output$c_district <- renderPlotly(rate_chart(filtered(), "district", sort = TRUE))
  output$i_district <- renderUI(insight_rate(filtered(), "district", "district"))
  output$t_prov     <- renderUI(margin_table(filtered(), "province", isTRUE(input$weighted)))
  output$t_prov_res <- renderUI(rate_matrix_table(filtered(), "province", "residence"))

  # ===========================================================================
  # Unemployed youth
  # ===========================================================================
  output$k_u_n         <- renderText(num(sum(unemployed()$.w)))
  output$k_u_n_sub     <- renderText(sample_text(unemployed()))
  output$k_u_short     <- renderText(pct(share_of(unemployed(), "unemployment_duration", "Less than 3 months")))
  output$k_u_short_sub <- renderText("recently became unemployed")
  output$k_u_never     <- renderText(pct(share_of(unemployed(), "ever_worked_before", "No")))
  output$k_u_never_sub <- renderText("are first-time jobseekers")
  output$k_u_parents     <- renderText(pct(share_of(unemployed(), "main_income_source", "Parents")))
  output$k_u_parents_sub <- renderText("rely mainly on their parents")

  output$c_u_dur     <- renderPlotly(share_chart(unemployed(), "unemployment_duration", pal$coral, ordered = TRUE))
  output$i_u_dur     <- renderUI(insight_share(unemployed(), "unemployment_duration", "unemployed youth"))
  output$c_u_dur_sex <- renderPlotly(clustered_chart(unemployed(), "unemployment_duration", "sex"))
  output$c_u_ever    <- renderPlotly(donut_chart(unemployed(), "ever_worked_before"))
  output$i_u_ever    <- renderUI(insight_share(unemployed(), "ever_worked_before", "unemployed youth (answer to 'ever worked?')"))
  output$c_u_income  <- renderPlotly(share_chart(unemployed(), "main_income_source", pal$blue))
  output$i_u_income  <- renderUI(insight_share(unemployed(), "main_income_source", "unemployed youth"))
  output$c_u_dur_edu <- renderPlotly(clustered_chart(unemployed(), "unemployment_duration", "education_level"))

  # ===========================================================================
  # Employed youth
  # ===========================================================================
  output$k_j_inf       <- renderText(pct(share_of(employed(), "formal_informal_work", "Informal employment")))
  output$k_j_inf_sub   <- renderText("of employed youth")
  output$k_j_under     <- renderText(pct(share_of(employed(), "time_underemployment", "Time related underemployed")))
  output$k_j_under_sub <- renderText("work fewer hours than they want")
  output$k_j_ss        <- renderText(pct(share_of(employees(), "social_security", "Yes")))
  output$k_j_ss_sub    <- renderText("employer pays RSSB contributions")
  output$k_j_low       <- renderText(pct(share_of(filter(employees(), income_band != "Not stated"), "income_band", "Less than 30,000 RWF")))
  output$k_j_low_sub   <- renderText("of employees with a stated income")

  output$c_j_status <- renderPlotly(share_chart(employed(), "status_in_employment", pal$teal))
  output$i_j_status <- renderUI(insight_share(employed(), "status_in_employment", "employed youth"))
  output$c_j_occ    <- renderPlotly(share_chart(employed(), "occupation", pal$blue))
  output$i_j_occ    <- renderUI(insight_share(employed(), "occupation", "employed youth"))
  output$c_j_ind    <- renderPlotly(share_chart(employed(), "industry", pal$teal_dark, top = 10))
  output$i_j_ind    <- renderUI(insight_share(employed(), "industry", "employed youth"))
  output$c_j_formal <- renderPlotly(clustered_chart(employed(), "formal_informal_work", "sex"))
  output$c_j_hours  <- renderPlotly(share_chart(employed(), "usual_hours", pal$blue, ordered = TRUE))

  output$c_j_benefits <- renderPlotly({
    d <- employees() %>%
      group_by(sex) %>%
      summarise(`Written contract` = weighted.mean(contract_type == "Yes, Written contract", .w),
                `Social security`  = weighted.mean(social_security == "Yes", .w, na.rm = TRUE),
                `Paid leave`       = weighted.mean(paid_leave == "Yes", .w, na.rm = TRUE)) %>%
      pivot_longer(-sex, names_to = "benefit", values_to = "share")
    plot_ly(d, x = ~benefit, y = ~share, color = ~sex, colors = c(pal$blue, pal$gold), type = "bar",
            text = ~pct(share, 1), textposition = "outside", cliponaxis = FALSE,
            hovertemplate = "%{x}<br>%{fullData.name}: %{y:.1%}<extra></extra>") %>%
      layout(barmode = "group", yaxis = list(range = c(0, max(d$share) * 1.2))) %>%
      style_plot(y_title = "Share of employees", y_pct = TRUE)
  })
  output$c_j_income <- renderPlotly(share_chart(filter(employees(), income_band != "Not stated"),
                                                "income_band", pal$teal, ordered = TRUE))
  output$c_j_found  <- renderPlotly(share_chart(employees(), "how_job_found", pal$teal))
  output$i_j_found  <- renderUI(insight_share(employees(), "how_job_found", "employees"))

  # ===========================================================================
  # Household & time use
  # ===========================================================================
  output$c_h_light <- renderPlotly(rate_chart(filtered(), "lighting_source", sort = isTRUE(input$sort_bars)))
  output$i_h_light <- renderUI(insight_rate(filtered(), "lighting_source", "lighting-source"))
  output$c_h_mig   <- renderPlotly(rate_chart(filtered(), "migration_status", sort = isTRUE(input$sort_bars)))
  output$i_h_mig   <- renderUI(insight_rate(filtered(), "migration_status", "migration"))

  care_data <- reactive({
    filtered() %>%
      select(group = all_of(input$care_by), .w,
             `Fetching water` = fetches_water, `Collecting firewood` = collects_firewood,
             `Household chores` = household_chores, `Caring for others` = care_work) %>%
      pivot_longer(-c(group, .w), names_to = "activity", values_to = "answer") %>%
      filter(!is.na(answer)) %>%
      group_by(activity, group) %>%
      summarise(share = weighted.mean(answer == "Yes", .w), .groups = "drop")
  })
  output$c_h_care <- renderPlotly({
    d <- care_data()
    plot_ly(d, x = ~activity, y = ~share, color = ~group, colors = c(pal$blue, pal$gold),
            type = "bar", text = ~pct(share, 1), textposition = "outside", cliponaxis = FALSE,
            hovertemplate = "%{x}<br>%{fullData.name}: %{y:.1%}<extra></extra>") %>%
      layout(barmode = "group", yaxis = list(range = c(0, max(d$share) * 1.2))) %>%
      style_plot(y_title = "Did this in the past 7 days", y_pct = TRUE)
  })
  output$i_h_care <- renderUI({
    d <- care_data() %>% filter(activity == "Household chores")
    if (nrow(d) < 2) return(NULL)
    HTML(sprintf("Household chores: <b>%s</b> (%s) vs <b>%s</b> (%s).",
                 pct(d$share[1]), d$group[1], pct(d$share[2]), d$group[2]))
  })
  output$c_h_reason <- renderPlotly(
    share_chart(filter(filtered(), reason_for_moving != "Did not move"), "reason_for_moving", pal$blue))

  # ===========================================================================
  # Model & predictions (not affected by the sidebar filters)
  # ===========================================================================
  output$k_m_model  <- renderText(app_label)
  output$k_m_auc    <- renderText(sprintf("%.3f", app_row$ROC_AUC))
  output$k_m_recall <- renderText(pct(app_row$Sensitivity))
  output$k_m_spec   <- renderText(pct(app_row$Specificity))

  output$t_models <- renderUI(model_metrics_table(ml))
  output$i_models <- renderUI({
    second <- ml$results[2, ]   # results are sorted by ROC AUC
    HTML(sprintf(
      "<b>%s</b> has the highest test ROC AUC (<b>%.3f</b>) and finds <b>%s</b> of unemployed youth. %s is close behind (%.3f): the top models differ only slightly, and all beat guessing (0.5) by a modest margin.",
      app_label, app_row$ROC_AUC, pct(app_row$Sensitivity), clean_model(second$Model), second$ROC_AUC))
  })
  output$c_models <- renderPlotly(model_compare_chart(ml))

  output$c_roc <- renderPlotly(roc_chart(roc_data))
  output$i_roc <- renderUI({
    r <- ml$results
    HTML(sprintf("Test ROC AUC ranges from <b>%.3f</b> (%s) to <b>%.3f</b> (%s). Most curves lie close together, so the models rank youth in a similar way. The dashed line is random guessing.",
                 min(r$ROC_AUC), clean_model(r$Model[which.min(r$ROC_AUC)]),
                 max(r$ROC_AUC), clean_model(r$Model[which.max(r$ROC_AUC)])))
  })
  output$c_cv <- renderPlotly(cv_chart(ml))
  output$i_cv <- renderUI({
    s <- ml$cv_roc
    cv_best <- rownames(s)[which.max(s[, "Mean"])]
    first <- if (cv_best == app_name)
      sprintf("%s also has the best mean ROC AUC on cross-validation (<b>%.3f</b>), so the test result is not luck of one split.",
              app_label, s[app_name, "Mean"])
    else
      sprintf("On cross-validation the best mean ROC AUC is <b>%.3f</b> (%s) and %s scores <b>%.3f</b>.",
              max(s[, "Mean"]), clean_model(cv_best), app_label, s[app_name, "Mean"])
    HTML(paste(first, "The fold-to-fold spread is larger than the gaps between the top models, so their ranking can change from one sample to another."))
  })

  # Cut-off explorer
  cut_metrics <- reactive(metrics_at(ml$test_probs[[app_name]], ml$test_probs$actual, input$m_cut))
  output$m_confusion <- renderUI(confusion_tiles(cut_metrics()))
  output$m_chips     <- renderUI(metric_chips(cut_metrics()))
  output$i_cut <- renderUI({
    m <- cut_metrics()
    HTML(sprintf("At cut-off <b>%.2f</b> the model finds <b>%s</b> of %s unemployed youth (%s) and raises <b>%s</b> false alarms. A lower cut-off finds more unemployed youth but flags more employed ones.",
                 input$m_cut, num(m$tp), num(m$tp + m$fn), pct(m$recall), num(m$fp)))
  })
  output$c_hist <- renderPlotly(risk_hist_chart(ml$test_probs, input$m_cut, app_name))
  output$i_hist <- renderUI({
    med <- tapply(ml$test_probs[[app_name]], ml$test_probs$actual, median)
    HTML(sprintf("Unemployed youth get higher scores on average (median <b>%.2f</b> vs <b>%.2f</b>), but the two groups overlap a lot, which is why no cut-off separates them well.",
                 med[["Unemployed"]], med[["Employed"]]))
  })

  output$c_imp <- renderPlotly(importance_chart(ml))
  output$i_imp <- renderUI({
    imp <- ml$importance; top <- rownames(imp)[order(-imp[, 1])][1:3]
    how <- switch(app_name,
      Random_forest = "Random forest importance = how much each variable improves the splits of the trees (impurity). This measure favours variables with many values, such as district (30) and age.",
      GBM = "GBM importance = how much each variable improves the boosted trees.",
      Elastic_net = , Lasso = , Logistic = "Importance = size of the model coefficient (standardised).",
      "This model has no built-in importance, so caret ranks each variable by how well it alone separates the two groups (ROC).")
    HTML(sprintf("The strongest signals are <b>%s</b>. %s", paste(pretty_feature(top), collapse = ", "), how))
  })

  # Risk calculator
  observeEvent(input$p_prov, {
    districts <- sort(unique(as.character(lfs$district[lfs$province == input$p_prov])))
    updateSelectInput(session, "p_district", choices = districts, selected = districts[1])
  })

  # The profile as one row, with the same factor levels the model was trained on
  profile <- reactive({
    req(input$p_district)
    lv <- lapply(ml$app_model$trainingData, levels)   # levels of every factor the model saw
    data.frame(
      age = input$p_age,
      sex = factor(input$p_sex, levels = lv$sex),
      education_level = factor(input$p_edu, levels = lv$education_level),
      years_of_schooling = input$p_years,
      current_student = factor(input$p_student, levels = lv$current_student),
      residence = factor(input$p_res, levels = lv$residence),
      district = factor(input$p_district, levels = lv$district),
      marital_status = factor(input$p_marital, levels = lv$marital_status),
      tvet_training = factor(input$p_tvet, levels = lv$tvet_training),
      disability = factor(input$p_disability, levels = lv$disability),
      household_size = input$p_hhsize,
      household_relationship = factor(input$p_rel, levels = lv$household_relationship),
      field_of_education = factor(input$p_field, levels = lv$field_of_education),
      education_track = factor(input$p_track, levels = lv$education_track),
      lived_outside_district = factor(input$p_outside, levels = lv$lived_outside_district),
      migration_status = factor(input$p_mig, levels = lv$migration_status),
      lighting_source = factor(input$p_light, levels = lv$lighting_source)
    )
  })
  score <- reactive(predict(ml$app_model, newdata = profile(), type = "prob")$Unemployed)

  output$c_gauge <- renderPlotly(risk_gauge(score(), input$m_cut))
  output$calc_verdict <- renderUI({
    s <- score(); chance <- real_chance(s, base_rate)
    at_risk <- s >= input$m_cut
    div(class = paste("verdict", if (at_risk) "risk" else "safe"),
        div(class = "verdict-title", bs_icon(if (at_risk) "exclamation-triangle-fill" else "check-circle-fill"),
            if (at_risk) " Flagged as at risk of unemployment" else " Not flagged"),
        p(HTML(sprintf("Estimated real chance of being unemployed: <b>%s</b>, about <b>%.1f times</b> the average young person in the labour force (%s).",
                       pct(chance), chance / base_rate, pct(base_rate)))),
        p(class = "small muted", sprintf("Flag rule: score %s %.0f (the cut-off set above).",
                                         if (at_risk) ">=" else "<", 100 * input$m_cut)))
  })
  output$c_age_curve <- renderPlotly({
    rows <- profile()[rep(1, 15), ]
    rows$age <- 16:30
    curve <- data.frame(age = 16:30,
                        score = predict(ml$app_model, newdata = rows, type = "prob")$Unemployed)
    age_curve_chart(curve, input$p_age, input$m_cut)
  })

  # ===========================================================================
  # Data explorer
  # ===========================================================================
  table_data <- reactive({
    cols <- unname(unlist(tiers[input$tiers]))   # unname(): keep the real column names
    if (length(cols) == 0) cols <- tiers$Core
    filtered() %>% select(all_of(cols))
  })
  output$t_data <- renderDT({
    d <- table_data()
    # Readable headers: "employment_status" -> "Employment status"
    labels <- gsub("_", " ", names(d))
    labels <- paste0(toupper(substring(labels, 1, 1)), substring(labels, 2))
    # filter = "top": each text column gets a drop-down list of its categories,
    # each number column gets a range slider
    datatable(d, rownames = FALSE, colnames = labels, style = "bootstrap5",
              filter = list(position = "top", clear = TRUE, plain = FALSE),
              class = "table table-sm table-hover",
              options = list(pageLength = 12, scrollX = TRUE, dom = "tip", autoWidth = TRUE))
  })
  output$download <- downloadHandler(
    filename = function() paste0("rwanda_youth_lfs2025_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(table_data(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)
