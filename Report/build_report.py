"""Builds the PBL machine-learning report (PDF) with reportlab."""
import os
from reportlab.lib.pagesizes import A4
from reportlab.lib.units import cm
from reportlab.lib import colors
from reportlab.lib.styles import ParagraphStyle
from reportlab.lib.enums import TA_JUSTIFY, TA_CENTER, TA_LEFT
from reportlab.platypus import (BaseDocTemplate, PageTemplate, Frame, Paragraph, Spacer,
                                Table, TableStyle, Image, PageBreak, KeepTogether, NextPageTemplate)
from reportlab.lib.utils import ImageReader

ROOT = r"C:\Users\branc\OneDrive\Documenti\Academia\Year 4\Machine Learning\Assignments\PBL 1"
FIG_ML = os.path.join(ROOT, "Outputs", "ML_simple", "figures")
FIG_EDA = os.path.join(ROOT, "Outputs", "Figures")
FIG_REP = os.path.join(ROOT, "Report", "figures")
OUT = os.path.join(ROOT, "Report", "PBL_ML_Report_Draft.pdf")

NAVY = colors.HexColor("#1F3A5F")
LIGHT = colors.HexColor("#EEF2F7")

# ---------------------------------------------------------------- styles
body = ParagraphStyle("body", fontName="Times-Roman", fontSize=11, leading=15.2,
                      alignment=TA_JUSTIFY, spaceAfter=6)
body_ind = ParagraphStyle("body_ind", parent=body, firstLineIndent=0)
h1 = ParagraphStyle("h1", fontName="Times-Bold", fontSize=15, leading=19, spaceBefore=12,
                    spaceAfter=6, textColor=colors.black, keepWithNext=1)
h2 = ParagraphStyle("h2", fontName="Times-Bold", fontSize=12, leading=15, spaceBefore=8,
                    spaceAfter=4, keepWithNext=1)
cap = ParagraphStyle("cap", fontName="Times-Italic", fontSize=9.5, leading=12,
                     alignment=TA_CENTER, spaceBefore=3, spaceAfter=10)
cell = ParagraphStyle("cell", fontName="Times-Roman", fontSize=9.5, leading=11.5, alignment=TA_LEFT)
cell_b = ParagraphStyle("cell_b", parent=cell, fontName="Times-Bold", textColor=colors.white)
bullet = ParagraphStyle("bullet", parent=body, leftIndent=16, bulletIndent=4, spaceAfter=3)
ref = ParagraphStyle("ref", fontName="Times-Roman", fontSize=9.5, leading=12,
                     leftIndent=18, firstLineIndent=-18, spaceAfter=3)
center = ParagraphStyle("center", fontName="Times-Roman", fontSize=12, leading=16, alignment=TA_CENTER)

story = []
fig_no = [0]
tab_no = [0]


def P(text, style=body):
    story.append(Paragraph(text, style))


def bullets(items):
    for it in items:
        story.append(Paragraph(it, bullet, bulletText="\u2022"))


def numbered(items):
    for i, it in enumerate(items, 1):
        story.append(Paragraph(it, bullet, bulletText=f"{i}."))


def figure(path, width_cm, caption):
    img = ImageReader(path)
    w, h = img.getSize()
    width = width_cm * cm
    height = width * h / w
    fig_no[0] += 1
    story.append(KeepTogether([Image(path, width=width, height=height),
                               Paragraph(f"Figure {fig_no[0]}. {caption}", cap)]))


def table(rows, widths, caption, bold_rows=(), align_right_from=1):
    tab_no[0] += 1
    data = []
    for r, row in enumerate(rows):
        style = cell_b if r == 0 else cell
        data.append([Paragraph(str(c), style) for c in row])
    t = Table(data, colWidths=[w * cm for w in widths], repeatRows=1)
    ts = [
        ("BACKGROUND", (0, 0), (-1, 0), NAVY),
        ("LINEBELOW", (0, 0), (-1, 0), 1, colors.black, 0),
        ("LINEABOVE", (0, 0), (-1, 0), 1, colors.black, 0),
        ("LINEBELOW", (0, -1), (-1, -1), 1, colors.black, 0),
        ("VALIGN", (0, 0), (-1, -1), "MIDDLE"),
        ("TOPPADDING", (0, 0), (-1, -1), 3),
        ("BOTTOMPADDING", (0, 0), (-1, -1), 3),
    ]
    for r in range(1, len(rows)):
        if r % 2 == 0:
            ts.append(("BACKGROUND", (0, r), (-1, r), LIGHT))
    for r in bold_rows:
        ts.append(("BACKGROUND", (0, r), (-1, r), colors.HexColor("#F6E3DF")))
    t.setStyle(TableStyle(ts))
    story.append(KeepTogether([Paragraph(f"Table {tab_no[0]}. {caption}", ParagraphStyle(
        "tcap", parent=cap, fontName="Times-Bold", spaceBefore=6, spaceAfter=4)), t, Spacer(1, 8)]))


# ---------------------------------------------------------------- page decoration
def header_footer(canv, doc):
    canv.saveState()
    canv.setFont("Times-Roman", 9)
    canv.drawString(2.2 * cm, A4[1] - 1.4 * cm, "MACHINE LEARNING | PBL REPORT (DRAFT)")
    canv.drawRightString(A4[0] - 2.2 * cm, A4[1] - 1.4 * cm, "UNIVERSITY OF RWANDA")
    canv.setLineWidth(0.6)
    canv.line(2.2 * cm, A4[1] - 1.55 * cm, A4[0] - 2.2 * cm, A4[1] - 1.55 * cm)
    canv.drawCentredString(A4[0] / 2, 1.2 * cm, str(doc.page))
    canv.restoreState()


def cover_page(canv, doc):
    pass


doc = BaseDocTemplate(OUT, pagesize=A4, leftMargin=2.2 * cm, rightMargin=2.2 * cm,
                      topMargin=2.2 * cm, bottomMargin=2 * cm,
                      title="Predicting Youth Unemployment in Rwanda with Machine Learning",
                      author="Niyobyose Brancal")
frame = Frame(doc.leftMargin, doc.bottomMargin, doc.width, doc.height, id="f")
doc.addPageTemplates([PageTemplate(id="cover", frames=frame, onPage=cover_page),
                      PageTemplate(id="main", frames=frame, onPage=header_footer)])

# ================================================================= COVER
story.append(Spacer(1, 2.2 * cm))
story.append(Paragraph("<b>UNIVERSITY OF RWANDA</b>", ParagraphStyle("u", parent=center, fontSize=16, leading=22)))
for line in ["COLLEGE OF SCIENCE AND TECHNOLOGY", "SCHOOL OF SCIENCE", "DEPARTMENT OF MATHEMATICS"]:
    story.append(Paragraph(line, center))
story.append(Spacer(1, 1.6 * cm))
story.append(Table([[""]], colWidths=[13 * cm], style=[("LINEABOVE", (0, 0), (-1, -1), 1.2, colors.black)]))
story.append(Paragraph("MACHINE LEARNING: PROJECT-BASED LEARNING (PBL) REPORT", center))
story.append(Spacer(1, 0.5 * cm))
story.append(Paragraph(
    "<b>Predicting Unemployment among Rwandan Youth with Machine Learning: "
    "Evidence from the 2025 Labour Force Survey</b>",
    ParagraphStyle("t", parent=center, fontName="Times-Bold", fontSize=20, leading=26)))
story.append(Spacer(1, 0.4 * cm))
story.append(Table([[""]], colWidths=[13 * cm], style=[("LINEBELOW", (0, 0), (-1, -1), 1.2, colors.black)]))
story.append(Spacer(1, 1.6 * cm))
info = [["Student name", "Niyobyose Brancal"], ["Registration number", "223014431"],
        ["Department", "Mathematics"], ["Programme", "Statistics"],
        ["Course", "Machine Learning"], ["Lecturer", "Prof. Nzabanita Joseph"],
        ["Software", "R (caret)"]]
story.append(Table([[Paragraph(f"<b>{a}</b>", body), Paragraph(b, body)] for a, b in info],
                   colWidths=[5 * cm, 8 * cm]))
story.append(Spacer(1, 4.5 * cm))
story.append(Paragraph("Kigali, Rwanda", center))
story.append(Paragraph("November 2026", center))
story.append(NextPageTemplate("main"))
story.append(PageBreak())

# ================================================================= ABSTRACT
P("Abstract", h1)
P("Youth unemployment is one of Rwanda's main social and economic challenges. This project uses the "
  "2025 Rwanda Labour Force Survey (LFS) to build machine-learning models that classify young people "
  "aged 16 to 30 who are in the labour force as <i>employed</i> or <i>unemployed</i>. After cleaning, "
  "the analysis sample contains 15,958 youth, of whom 14.7% are unemployed, so the classes are strongly "
  "imbalanced. Seventeen demographic, educational, geographic and household predictors were used. Ten "
  "algorithms were trained in R with the <i>caret</i> package: logistic regression, Lasso, elastic net, "
  "linear and regularized discriminant analysis (LDA, RDA), naive Bayes, k-nearest neighbours (KNN), a "
  "decision tree, random forest and gradient boosting (GBM). The data were split 70/30 into training "
  "and test sets; hyper-parameters were tuned with 5-fold cross-validation and the training folds were "
  "down-sampled to balance the classes. Models were compared with ROC AUC, recall, specificity, "
  "precision, F1 and Kappa.")
P("The random forest achieved the highest test ROC AUC (0.676; 95% CI 0.656 to 0.696) and found 62% "
  "of unemployed youth at the default cut-off. However, its advantage over GBM, LDA and Lasso was not "
  "statistically significant (DeLong test). The most informative factors were district, position in "
  "the household, age, years of schooling and household size. The results confirm an <i>education "
  "paradox</i>: youth with upper secondary or university education have a higher risk of unemployment "
  "than youth with little schooling. Young women, youth who are not household heads, and teenagers and "
  "youth in their early twenties are also more at risk. Overall predictive power is modest, which shows that "
  "unemployment depends on many factors that the survey does not measure. A Shiny web application "
  "was developed to share the results and provide an interactive risk calculator.")
P("<b>Keywords:</b> youth unemployment; Rwanda; Labour Force Survey; classification; random forest; "
  "imbalanced data; ROC AUC.")

# ================================================================= 1. INTRODUCTION
P("1. Problem Identification and Translation", h1)
P("1.1 Background", h2)
P("Rwanda's policies on skills, job creation and economic transformation depend on knowing which young "
  "people have access to work. Youth form a large share of the working-age population, and every year "
  "many complete school or vocational training and look for their first job. According to the National "
  "Institute of Statistics of Rwanda (NISR), the unemployment rate among persons aged 16 to 30 was 15.7% "
  "in May 2026 and 18.8% in August 2026, compared with 11.9% and 13.1% for adults aged 31 and above "
  "[2, 3]. Youth are therefore clearly more affected than older workers.")
P("Having a job is also not the end of the story: in 2024 about 90.4% of employment in Rwanda was "
  "informal [4], and the World Bank reports that more than one million young Rwandans were neither in "
  "employment, education nor training in 2024 [5]. Earlier studies in Rwanda found that gender, age, "
  "education and location are associated with youth employment [10, 11]. Human capital theory "
  "suggests that more education should improve job chances [12, 13], while job-search and "
  "labour-market matching theories explain why educated youth may stay unemployed longer while they "
  "wait for suitable work. Knowing <i>who</i> is most at risk helps the government and training "
  "institutions target employment services, career guidance and work-based learning.")

P("1.2 Problem statement", h2)
P("Headline unemployment rates tell us how many young people are unemployed, but not which individual "
  "characteristics most strongly predict unemployment, nor how well these characteristics can identify "
  "an at-risk young person. This project translates the social problem into a <b>supervised binary "
  "classification problem</b>:")
P("<i>Given the demographic, educational, geographic and household characteristics of a young person "
  "aged 16 to 30 who is in the labour force, predict whether the person is Employed or Unemployed.</i>",
  ParagraphStyle("q", parent=body, leftIndent=20, rightIndent=20))
P("The target variable is the official NISR labour-force status. The positive class is "
  "<b>Unemployed</b>, because finding unemployed youth is the purpose of the model. Youth outside the "
  "labour force (mainly full-time students) are not part of this classification task.")

P("1.3 Research gap and objectives", h2)
P("The accompanying research-methodology proposal of the author studies the factors <i>associated</i> "
  "with labour-force status using survey-weighted statistics. Existing Rwandan studies [10, 11] used "
  "older survey designs and classical regression only. There is little evidence on how well modern "
  "machine-learning methods can <i>predict</i> individual unemployment risk with the redesigned LFS, "
  "and on which method works best. This project fills that gap with a predictive approach.")
P("<b>General objective:</b> to build, compare and interpret machine-learning models that predict "
  "unemployment among Rwandan youth (16 to 30) in the labour force, using the 2025 LFS.")
P("<b>Specific objectives:</b>")
numbered([
    "To prepare a clean, leakage-free modelling dataset from the 2025 LFS microdata and describe it.",
    "To train and tune at least eight classification algorithms with 5-fold cross-validation.",
    "To compare the models on an unseen test set (30% of the data) using ROC AUC as the main metric, "
    "together with recall, specificity, precision, F1 and Kappa, and to test whether the differences "
    "are statistically significant.",
    "To identify the factors that most strongly predict youth unemployment and the direction of their "
    "effects.",
    "To check how well the best model works for women, men, urban and rural youth.",
    "To share the findings through an interactive web application (bonus task).",
])

# ================================================================= 2. METHODOLOGY
P("2. Machine Learning Methodology", h1)
P("2.1 Data source and description", h2)
P("The data come from the <b>Rwanda Labour Force Survey 2025</b>, an official open-access survey "
  "conducted by NISR and distributed through its microdata catalogue [6]. The LFS is a nationally "
  "representative household survey. It uses a stratified two-stage sample: enumeration areas are "
  "selected first, with probability proportional to size, and then households are selected within "
  "each area. Trained enumerators interview household members face to face every quarter, and "
  "households are visited again in a rotating panel [8]. Labour-force status follows the "
  "international ILO definitions [9], and variable names follow the NISR data dictionary [7]. This "
  "is a secondary analysis of existing survey data [1]. The raw file contains <b>100,852 person records and 207 "
  "variables</b>. Table 1 summarises the steps from the raw file to the modelling dataset, and Table 2 "
  "describes the variables used.")
table([
    ["Step", "Records", "Comment"],
    ["Raw LFS 2025 file", "100,852", "All persons interviewed, 207 variables"],
    ["Youth aged 16 to 30", "26,290", "Age restriction (NISR youth definition)"],
    ["Exact duplicate records removed", "26,289", "1 identical record"],
    ["Keep youth in the labour force", "15,960", "10,329 youth outside the labour force excluded"],
    ["Remove missing years of schooling", "15,958", "2 records"],
    ["<b>Final modelling dataset</b>", "<b>15,958</b>", "<b>13,605 employed (85.3%), 2,353 unemployed (14.7%)</b>"],
], [6, 2.4, 8.2], "From the raw survey file to the modelling dataset")

table([
    ["Variable", "Type", "Values / levels"],
    ["Employment status (target)", "Binary", "Employed, Unemployed"],
    ["Age", "Numeric", "16 to 30 years"],
    ["Sex", "Categorical (2)", "Male, Female"],
    ["Education level", "Categorical (5)", "None, Primary, Lower secondary, Upper secondary, University"],
    ["Years of schooling", "Numeric", "0 to 21 years"],
    ["Currently a student", "Categorical (2)", "Yes, No"],
    ["Residence", "Categorical (2)", "Urban, Rural"],
    ["District", "Categorical (30)", "All 30 districts of Rwanda"],
    ["Marital status", "Categorical (4)", "Single, Married, Living together, Previously married"],
    ["TVET training", "Categorical (3)", "Yes, No, Not stated"],
    ["Disability", "Categorical (2)", "Yes, No"],
    ["Household size", "Numeric", "1 to 10 (10 means 10 or more)"],
    ["Relationship to household head", "Categorical (9)", "Head, spouse, child, grandchild, sibling, other relative, ..."],
    ["Field of education", "Categorical (10)", "General, science, engineering, services, ..."],
    ["Education track", "Categorical (3)", "General education, TVET, No level completed"],
    ["Lived outside district", "Categorical (2)", "Yes, No"],
    ["Migration status", "Categorical (3)", "Non-migrant, internal migrant, international migrant"],
    ["Lighting source", "Categorical (4)", "Grid electricity, solar, torch/phone, traditional"],
], [5.2, 3.2, 8.2], "Variables in the modelling dataset (1 target and 17 predictors)")

P("Table 3 gives the survey-weighted unemployment rate for the main groups. The overall youth "
  "unemployment rate is 14.7%, representing about 330,000 of the 2.25 million young people in the "
  "labour force. Women (17.4%) are more affected than men (12.3%), and youth with upper secondary "
  "education have the highest rate (22.5%).")
table([
    ["Group", "Sample size", "Unemployment rate (weighted)"],
    ["All youth in the labour force", "15,958", "14.7%"],
    ["Male / Female", "8,304 / 7,654", "12.3% / 17.4%"],
    ["Urban / Rural", "6,101 / 9,857", "14.1% / 15.1%"],
    ["Age 16-19 / 20-24 / 25-30", "3,587 / 5,970 / 6,401", "15.3% / 16.1% / 13.1%"],
    ["None / Primary education", "5,657 / 5,586", "12.8% / 12.7%"],
    ["Lower / Upper secondary", "1,831 / 2,322", "15.7% / 22.5%"],
    ["University", "562", "17.1%"],
], [6.2, 4.6, 5.8], "Youth unemployment rate by group (LFS 2025, survey weights)")

P("2.2 Choice of methods and evaluation metrics", h2)
P("Because the target has two classes, classification algorithms are appropriate. We chose a mix of "
  "simple, interpretable models and more flexible ones, so that we can see whether complex models "
  "really add value (Section 4.1) [14, 15]. All models were fitted with the <i>caret</i> package in "
  "R [16], which gives one common framework for tuning, cross-validation and evaluation.")
P("The choice of metric is very important because the data are imbalanced. A trivial rule that "
  "predicts \"Employed\" for every young person would have an <b>accuracy of 85.3%</b> (a test error "
  "of only 14.7%) and yet would find no unemployed person at all. Accuracy and test error therefore "
  "reward a useless model and were <b>not</b> used to choose the best model. Instead we used:")
bullets([
    "<b>ROC AUC</b> (main metric): the probability that the model gives a higher risk score to a random "
    "unemployed young person than to a random employed one. It does not depend on the cut-off, is not "
    "affected by class imbalance, and 0.5 means random guessing.",
    "<b>Recall (sensitivity)</b>: the share of unemployed youth the model finds; "
    "<b>specificity</b>: the share of employed youth correctly classified.",
    "<b>Precision</b>, <b>F1 score</b> and <b>Cohen's Kappa</b>, which measure how reliable a "
    "positive prediction is and how much better than chance the model agrees with the truth.",
])
P("To test whether the best model is really better than the others, ROC curves on the same test set "
  "were compared with the <b>DeLong test</b> [17] (R package <i>pROC</i> [20]).")

# ================================================================= 3. EDA
P("3. Exploratory Data Analysis and Preprocessing", h1)
P("3.1 Data cleaning", h2)
P("<b>Missing values.</b> For each variable we first asked <i>why</i> the value was missing. Many "
  "\"missing\" values were questions not asked to that person (for example, TVET details for youth "
  "who never attended TVET, or migration questions for people who never moved). These were replaced "
  "by clear labels such as \"No TVET\" or \"Non-migrant\". Real non-response was small: TVET training "
  "was not stated for 3.5% of youth, so a separate \"Not stated\" category was kept instead of "
  "deleting these records, and 2 records with missing years of schooling were removed. The final "
  "modelling dataset has <b>no missing values</b>.")
P("<b>Duplicates.</b> One record was an exact copy of another and was removed. Because of the "
  "rotating panel, some households appear in more than one quarter; these are real interviews in "
  "different periods and were not treated as duplicates.")
P("<b>Outliers.</b> The three numeric variables were checked with box plots (Figure 1) using the 1.5 "
  "&times; IQR rule. Age (limited to 16 to 30 by design) and household size (top-coded by NISR at "
  "10 or more) have no outliers. Years of schooling has 562 high values (16 to 21 years). These "
  "are university students and graduates, not errors, so all values were kept. Tree-based models "
  "are also not sensitive to extreme values.")
figure(os.path.join(FIG_EDA, "eda_outliers_boxplots.png"), 15,
       "Box plots of the numeric variables used to detect outliers.")
P("<b>Other preparation.</b> Category labels were cleaned (spelling, numbering), rare categories were "
  "grouped so that each group has enough people, and the target was coded with \"Unemployed\" as the "
  "positive class.")

P("3.2 Feature engineering and selection", h2)
P("Feature selection followed three rules. <b>(1) No data leakage:</b> variables that are only known "
  "because a person is employed or unemployed (occupation, industry, hours worked, income, "
  "unemployment duration, job-search method, NEET status, etc.) were removed, because they directly "
  "reveal the answer. <b>(2) No redundancy:</b> Figure 2 shows the association between predictors "
  "measured with Cramer's V. Province and district are perfectly associated (V = 1.00), because each "
  "district belongs to one province, so province was dropped and the more detailed district kept. "
  "Education level and years of schooling are also strongly related (V = 0.98), but both were kept "
  "because they carry slightly different information (level completed versus time spent in school) "
  "and penalized and tree models handle correlated inputs well. <b>(3) Identifiers removed:</b> the "
  "household identifier and survey design variables are not predictors.")
figure(os.path.join(FIG_EDA, "report_cramers_v.png"), 14.5,
       "Association between predictors (Cramer's V; 0 = none, 1 = perfect). Numeric variables were "
       "grouped into four classes.")
P("Figure 3 shows one of the main patterns found in the exploratory analysis: unemployment rises with "
  "education up to upper secondary level and is higher for women at every education level.")
figure(os.path.join(FIG_EDA, "report_rate_education_sex.png"), 14,
       "Youth unemployment rate by education level and sex (survey-weighted).")

P("3.3 Train/test split", h2)
P("The data were split with <b>stratified random sampling</b> into a <b>70% training set (11,172 "
  "youth)</b> and a <b>30% test set (4,786 youth)</b>. Stratification keeps the same share of "
  "unemployed youth (14.7%) in both sets. A random seed (123) makes the split reproducible. The test "
  "set was used only once, at the end, to evaluate the final models. Inside the training set, "
  "<b>5-fold cross-validation</b> was used for tuning, and all ten models used exactly the same "
  "folds so that their results are directly comparable.")
P("To deal with class imbalance, each training fold was <b>down-sampled</b>: the employed group was "
  "randomly reduced to the size of the unemployed group, so the models learn from balanced data. "
  "Down-sampling is applied only inside training folds; the test set keeps the real 14.7% rate.")

# ================================================================= 4. IMPLEMENTATION
P("4. Implementation of the ML Methodology", h1)
P("4.1 Algorithms", h2)
table([
    ["Algorithm", "Type", "Why it was chosen"],
    ["Logistic regression", "Linear, probabilistic", "Standard baseline for binary outcomes; gives odds ratios"],
    ["Lasso / Elastic net", "Penalized logistic", "Shrinks weak predictors; handles many dummy variables and correlated inputs"],
    ["LDA", "Linear discriminant", "Simple and stable linear classifier"],
    ["RDA", "Regularized discriminant", "Flexible between LDA and QDA; robust with many categories"],
    ["Naive Bayes", "Probabilistic", "Very fast; useful simple benchmark"],
    ["k-nearest neighbours", "Instance-based", "Non-parametric; no assumption about the shape of the boundary"],
    ["Decision tree (CART)", "Tree", "Easy to interpret; captures interactions"],
    ["Random forest [18]", "Ensemble of trees", "Strong, robust; handles non-linear effects and interactions"],
    ["Gradient boosting (GBM) [19]", "Boosted trees", "Often the most accurate method on tabular data"],
], [4.4, 3.8, 8.4], "The ten algorithms implemented and their justification")

P("4.2 Training and hyper-parameter tuning", h2)
P("Each model was tuned by grid search with 5-fold cross-validation, and the combination with the "
  "highest mean cross-validated ROC AUC was selected (Table 5). Figure 4 shows the tuning curve of the "
  "random forest: the best setting uses 4 randomly selected predictors at each split and a minimum of "
  "30 youth per leaf. Differences between settings are small (0.666 to 0.673), which shows that the "
  "random forest is stable.")
table([
    ["Model", "Parameters tuned", "Grid size", "Best values", "Best CV ROC AUC"],
    ["Random forest", "mtry, min. node size", "12", "mtry = 4, node size = 30", "0.673"],
    ["GBM", "number of trees, depth", "9", "100 trees, depth 3", "0.673"],
    ["RDA", "gamma, lambda", "9", "gamma = 0.1, lambda = 0.5", "0.668"],
    ["Lasso", "lambda", "10", "lambda = 0.01", "0.668"],
    ["Elastic net", "alpha, lambda", "50", "alpha = 0, lambda = 0.1", "0.667"],
    ["Naive Bayes", "Laplace smoothing (fixed)", "1", "laplace = 1", "0.649"],
    ["Decision tree", "complexity cp", "10", "cp = 0.00085", "0.637"],
    ["KNN", "number of neighbours k", "8", "k = 41", "0.625"],
    ["Logistic, LDA", "none", "1", "-", "0.658 / 0.661"],
], [3.0, 3.9, 2.2, 4.4, 3.1], "Hyper-parameter tuning results (5-fold cross-validation)")
figure(os.path.join(FIG_ML, "04_tuning_random_forest.png"), 13.5,
       "Random forest tuning: cross-validated ROC AUC by number of predictors tried at each split "
       "(mtry) and minimum node size.")

P("4.3 Results", h2)
P("Table 6 and Figure 5 show the performance of all models on the test set (cut-off 0.5 for the threshold-based metrics). The "
  "<b>random forest has the highest ROC AUC (0.676)</b>, followed closely by GBM (0.672) and LDA "
  "(0.664). All models are clearly better than random guessing (0.5), but no model is very strong. "
  "Models differ more in how they balance recall and specificity: Lasso finds the most unemployed "
  "youth (recall 0.78) but with more false alarms, while RDA and the random forest are more "
  "balanced. Precision is low for every model (about 0.2) because only 15% of youth are unemployed.")
table([
    ["Model", "ROC AUC", "Accuracy", "Recall", "Specificity", "Precision", "F1", "Kappa"],
    ["<b>Random forest</b>", "<b>0.676</b>", "0.623", "0.621", "0.624", "0.222", "0.327", "0.140"],
    ["GBM", "0.672", "0.565", "0.708", "0.540", "0.210", "0.324", "0.125"],
    ["LDA", "0.664", "0.577", "0.685", "0.558", "0.211", "0.323", "0.126"],
    ["RDA", "0.662", "0.644", "0.560", "0.658", "0.221", "0.317", "0.133"],
    ["Lasso", "0.660", "0.518", "0.782", "0.472", "0.204", "0.323", "0.117"],
    ["Logistic", "0.656", "0.570", "0.684", "0.550", "0.208", "0.319", "0.120"],
    ["Elastic net", "0.656", "0.575", "0.685", "0.556", "0.211", "0.322", "0.125"],
    ["Naive Bayes", "0.647", "0.621", "0.567", "0.630", "0.209", "0.306", "0.115"],
    ["Decision tree", "0.640", "0.576", "0.627", "0.567", "0.200", "0.303", "0.103"],
    ["KNN", "0.627", "0.547", "0.647", "0.529", "0.192", "0.296", "0.089"],
    ["Always \"Employed\"", "0.500", "0.853", "0.000", "1.000", "-", "-", "0.000"],
], [3.4, 2.0, 2.0, 1.8, 2.1, 2.0, 1.6, 1.7], "Performance of all models on the test set (4,786 youth, cut-off 0.5)",
    bold_rows=(1,))
figure(os.path.join(FIG_ML, "02_roc_curves.png"), 13,
       "ROC curves of the ten models on the test set. The dashed line is random guessing.")
P("<b>Confusion matrix of the best model.</b> Table 7 shows that, out of 705 unemployed youth in the "
  "test set, the random forest correctly found 438 (62%) and missed 267. It also wrongly flagged 1,536 "
  "employed youth. When the model says a person is employed, it is right 90.5% of the time "
  "(negative predictive value), so it is most useful for screening out low-risk youth and short-listing "
  "high-risk youth for further attention.")
table([
    ["", "Predicted Unemployed", "Predicted Employed", "Total"],
    ["<b>Actually Unemployed</b>", "438 (found)", "267 (missed)", "705"],
    ["<b>Actually Employed</b>", "1,536 (false alarm)", "2,545 (correct)", "4,081"],
    ["<b>Total</b>", "1,974", "2,812", "4,786"],
], [4.6, 4.4, 4.4, 3.2], "Confusion matrix of the random forest on the test set (cut-off 0.5)")

P("<b>Feature importance.</b> Figure 6 shows the variables the random forest relies on most: "
  "district, relationship to the household head, age, years of schooling and household size, followed "
  "by lighting source (a proxy for household living standard), education level and field of "
  "education. Note that this impurity-based importance tends to favour variables with many values, "
  "such as district (30 levels) and age, so it should be read together with the direction of effects "
  "below.")
figure(os.path.join(FIG_ML, "03_feature_importance.png"), 13,
       "Top 15 predictors in the random forest (importance scaled from 0 to 100).")

P("<b>Direction of effects.</b> Importance shows <i>which</i> variables matter, not <i>how</i>. Two "
  "tools were used. First, the logistic regression gives odds ratios of unemployment (Table 8, "
  "significant non-district terms). Second, partial dependence plots of the random forest show how "
  "the average risk score changes with one variable while the others are held at their observed "
  "values (Figures 7 to 9).")
table([
    ["Characteristic (reference group)", "Odds ratio", "95% CI", "p-value", "Interpretation"],
    ["Household head (vs brother/sister of the head)", "0.31", "0.17 to 0.54", "&lt;0.001", "About 70% lower odds"],
    ["Non-relative (vs brother/sister of the head)", "0.48", "0.25 to 0.93", "0.028", "About 52% lower odds"],
    ["Single (vs living together)", "0.56", "0.38 to 0.83", "0.004", "About 44% lower odds"],
    ["Male (vs female)", "0.77", "0.64 to 0.93", "0.006", "Men 23% lower odds than women"],
], [5.4, 1.8, 2.5, 1.6, 5.3], "Odds ratios of being unemployed (logistic regression, p &lt; 0.05, districts not shown)")
P("An odds ratio below 1 means lower odds of unemployment than the reference group, holding the other "
  "variables constant. Children of the head (odds ratio 1.13) and married youth (0.93) do not differ "
  "significantly from their reference groups. Seven districts (Nyabihu, Gakenke, Kayonza, Ngoma, "
  "Kirehe, Nyagatare and Nyarugenge) have significantly lower odds than the reference district "
  "(Bugesera). After controlling for the other variables, "
  "education level is not individually significant in the logistic model, but the flexible random "
  "forest captures a clear threshold effect of schooling (Figure 7).")
figure(os.path.join(FIG_ML, "10_pdp_schooling.png"), 11.5,
       "Partial dependence of the random-forest risk score on years of schooling.")
figure(os.path.join(FIG_ML, "11_pdp_age.png"), 11.5,
       "Partial dependence of the random-forest risk score on age.")
figure(os.path.join(FIG_ML, "14_pdp_education.png"), 11.5,
       "Partial dependence of the random-forest risk score on education level.")
P("The risk of unemployment is almost flat up to about 9 years of schooling and then <b>jumps sharply "
  "after 10 years</b> (end of lower secondary), staying high for upper secondary and university "
  "levels. By age, risk peaks at 17 to 18 years, falls to its lowest level around 25, and then rises "
  "slightly. These patterns agree with the descriptive rates in Table 3 and Figure 3.")

P("4.4 Testing, verification and validation", h2)
P("The final models were validated in three ways.")
P("<b>(a) Cross-validation versus test set.</b> Figure 10 shows the mean cross-validated ROC AUC with "
  "its 95% confidence interval. The random forest and GBM have the best CV score (0.673), and the "
  "random forest's test ROC AUC (0.676) is almost the same. The close agreement between CV and test "
  "results shows that the models are <b>not overfitting</b> and that the result is not due to a lucky "
  "split.")
figure(os.path.join(FIG_ML, "01_cv_comparison.png"), 13.5,
       "Cross-validated ROC AUC of the ten models (5 folds; dot = mean, bar = 95% confidence interval).")
P("<b>(b) Statistical significance.</b> Table 9 compares the random forest with other models using "
  "the DeLong test on the same test set. The random forest is significantly better than logistic "
  "regression and KNN, but <b>not significantly better than GBM, LDA or Lasso</b>. The 95% confidence "
  "interval of its ROC AUC is 0.656 to 0.696. In practice the top four models perform about equally "
  "well; the random forest was chosen for the web application because it has the highest score.")
table([
    ["Comparison", "AUC difference", "p-value", "Significant at 5%?"],
    ["Random forest vs GBM", "0.004", "0.587", "No"],
    ["Random forest vs LDA", "0.011", "0.198", "No"],
    ["Random forest vs Lasso", "0.016", "0.056", "No (borderline)"],
    ["Random forest vs Logistic", "0.019", "0.038", "Yes"],
    ["Random forest vs KNN", "0.049", "&lt;0.001", "Yes"],
], [6.0, 3.4, 3.0, 4.2], "DeLong tests comparing ROC curves on the test set")
P("<b>(c) Performance in subgroups.</b> Table 10 checks whether the model works equally well for "
  "different groups of youth. It separates employed and unemployed youth better in urban areas "
  "(ROC AUC 0.74) than in rural areas (0.64). For women it finds most unemployed youth (recall 0.73) "
  "but raises more false alarms (specificity 0.46), while for men it finds fewer than half (recall "
  "0.48). The model therefore behaves differently by sex, which must be considered before any "
  "practical use.")
table([
    ["Group", "Youth in test set", "Unemployment rate", "ROC AUC", "Recall", "Specificity"],
    ["Female", "2,265", "17.6%", "0.638", "0.732", "0.458"],
    ["Male", "2,521", "12.1%", "0.683", "0.477", "0.763"],
    ["Urban", "1,794", "13.5%", "0.737", "0.645", "0.675"],
    ["Rural", "2,992", "15.5%", "0.636", "0.609", "0.592"],
], [2.6, 3.0, 3.2, 2.6, 2.6, 2.6], "Performance of the random forest by subgroup (test set)")

# ================================================================= 5. FINDINGS
P("5. Communicating Findings", h1)
P("5.1 Key findings and interpretation", h2)
numbered([
    "<b>Youth unemployment is concentrated in specific groups.</b> The weighted rate is 14.7%, but it "
    "reaches 17.4% for young women and 22.5% for youth with upper secondary education.",
    "<b>The education paradox.</b> Both the descriptive rates and the random forest show that risk "
    "rises after about 10 years of schooling. Educated youth often wait for jobs that match their "
    "qualifications, while less educated youth enter agriculture or informal work quickly. Having "
    "a job does not mean having a good job, given that about 90% of employment is informal [4].",
    "<b>Position in the household matters.</b> Household heads have about 70% lower odds of "
    "unemployment than siblings of the head, and the random forest gives them the lowest risk. "
    "Sons, daughters, grandchildren and spouses, who can rely on family support while searching, "
    "have the highest unemployment rates.",
    "<b>Gender gap.</b> After controlling for education, age and location, young men still have 23% "
    "lower odds of unemployment than young women.",
    "<b>Location.</b> District is the strongest predictor in the random forest, and several districts "
    "differ significantly, reflecting local labour-market conditions.",
    "<b>Prediction is possible but modest.</b> The best ROC AUC is about 0.68. Simple linear models "
    "(LDA, Lasso) are nearly as good as the random forest and GBM, so the relationships are mostly "
    "simple and additive. Much of what decides whether a young person finds work (networks, job "
    "offers, motivation, local demand) is not measured in the survey.",
])
P("For policy, the model can be used as a <b>screening tool</b> to identify groups that should be "
  "prioritised for employment services, such as young women and secondary-school leavers in "
  "high-risk districts, rather than to make decisions about individuals.")

P("5.2 Limitations", h2)
bullets([
    "<b>Modest predictive power</b> (ROC AUC about 0.68) and low precision: many youth flagged as "
    "at risk are actually employed.",
    "<b>Associations, not causes.</b> The data are cross-sectional; the results show which "
    "characteristics predict unemployment, not what causes it.",
    "<b>Survey design not used in training.</b> The models were trained without survey weights, "
    "strata or clusters; descriptive rates are weighted, but model performance describes the sample.",
    "<b>Rotating panel.</b> The same household can appear in several quarters, so related records "
    "may fall in both training and test sets, which may make the test results slightly optimistic.",
    "<b>Importance measure.</b> Random-forest impurity importance favours variables with many "
    "levels (district, age).",
    "<b>Scope.</b> Only youth in the labour force were modelled; youth outside the labour force and "
    "the quality of employment were not studied. The analysis uses 2025 data, while the related "
    "research proposal refers to the 2024 survey.",
    "<b>Fairness.</b> Recall and specificity differ strongly between women and men.",
])
P("5.3 Potential improvements", h2)
bullets([
    "Split the data by household (grouped cross-validation) to avoid related records in training and test sets.",
    "Train with survey weights and report design-based uncertainty.",
    "Choose the cut-off according to the cost of missing an unemployed youth versus a false alarm, "
    "possibly with different cut-offs by group to reduce the gender gap.",
    "Add more informative variables (job-search behaviour, previous work experience, household income, "
    "local labour demand) and combine several survey years.",
    "Use permutation importance or SHAP values for fairer variable importance.",
    "Extend to a three-class model (employed, unemployed, outside the labour force), as in the "
    "research proposal.",
])

# ================================================================= 6. WEB APP
P("6. Bonus Task: Web Application", h1)
P("An interactive dashboard, <b>Rwanda Youth Employment (LFS 2025)</b>, was developed in R with "
  "<i>Shiny</i>, <i>bslib</i> and <i>plotly</i> (Figure 11). It runs locally from the project folder "
  "(App/app.R). Its main features are:")
bullets([
    "<b>Ten pages</b>: executive summary, who is unemployed, education and skills, geography, "
    "unemployed youth, employed youth, household and time use, model and predictions, data explorer, "
    "and help/glossary.",
    "<b>Filters</b> by sex, residence, province, education and age, with survey weights that can be "
    "switched on or off; all charts, KPI cards and summary tables update immediately.",
    "<b>Model page</b>: comparison table of the ten models, ROC and cross-validation charts, a cut-off "
    "slider that updates the confusion matrix and metrics, and the feature-importance chart.",
    "<b>Risk calculator</b>: the user describes a young person and the random forest returns a risk "
    "score, an estimated real chance of unemployment (corrected for down-sampling) and how the risk "
    "changes with age.",
    "<b>Usability</b>: dark mode, show/hide numbers on charts, sortable bars, PNG download of each "
    "chart and CSV download of the filtered data.",
])
figure(os.path.join(FIG_REP, "dashboard_exec.jpg"), 15.5,
       "Executive summary page of the Shiny dashboard (KPI cards and first charts).")

# ================================================================= 7. CONCLUSIONS
P("7. Conclusions", h1)
P("This project translated the problem of youth unemployment in Rwanda into a binary classification "
  "task and solved it with ten machine-learning algorithms using the 2025 Labour Force Survey. After "
  "careful cleaning, leakage-free feature selection, a stratified 70/30 split, 5-fold cross-validation "
  "and down-sampling for class imbalance, the <b>random forest</b> gave the best test ROC AUC (0.676), "
  "although it was not significantly better than GBM, LDA or Lasso. Accuracy and test error were not "
  "used to select models, because a useless model that predicts \"employed\" for everybody would "
  "have the highest accuracy.")
P("The models consistently identify the same risk factors: district, position in the household, "
  "age, schooling and sex. The clearest message is the <b>education paradox</b>: young people who "
  "complete more than lower secondary education face a higher risk of unemployment, and young women "
  "are more affected than young men at every education level. These findings support policies that "
  "link secondary and tertiary education more closely to the labour market (career guidance, "
  "internships, work-based learning) and that target young women. The modest predictive power also "
  "shows the limits of household-survey data for predicting individual outcomes, and points to the "
  "need for richer data on job search and local labour demand. The interactive web application makes "
  "these results accessible to policy makers, students and other stakeholders.")

# ================================================================= REFERENCES
P("References", h1)
refs = [
    "[1] Kothari, C. R. (2004). <i>Research Methodology: Methods and Techniques</i> (2nd ed.). New Age International.",
    "[2] National Institute of Statistics of Rwanda (NISR). (2026). <i>Labour Force Survey 2026 (Q2): May 2026</i>. Kigali: NISR.",
    "[3] NISR. (2026). <i>Labour Force Survey 2026 (Q3): August 2026</i>. Kigali: NISR.",
    "[4] NISR. (2025). <i>Labour Force Survey Annual Report 2024</i>. Kigali: NISR.",
    "[5] World Bank. (2026, February 23). <i>Rwanda's skills training delivers jobs and opportunity for youth</i>. World Bank Results Brief.",
    "[6] NISR. (2026). <i>Rwanda Labour Force Survey 2025: Microdata catalogue</i>, Study RWA-NISR-RLFS-2025-v0.1. https://microdatabk.statistics.gov.rw/catalog/125.",
    "[7] NISR. (2025). <i>Rwanda Labour Force Survey 2024: Microdata catalogue and data dictionary</i>. Kigali: NISR.",
    "[8] NISR. (2024). <i>Labour Force Survey: Methodology (2024 revision)</i>. Kigali: NISR.",
    "[9] International Labour Organization. (2013). <i>Resolution concerning statistics of work, employment and labour underutilization</i>. 19th ICLS, Geneva.",
    "[10] Ndagijimana, J., Nzasingizimana, T., &amp; Heshmati, A. (2018). An analysis of the determinants of youth employment in Rwanda. <i>UKH Journal of Social Sciences, 2</i>(2), 1-10.",
    "[11] African Development Bank. (2014). <i>Analysis of gender and youth employment in Rwanda</i>. Economic Brief.",
    "[12] Becker, G. S. (1993). <i>Human Capital: A Theoretical and Empirical Analysis</i> (3rd ed.). University of Chicago Press.",
    "[13] Mincer, J. A. (1974). <i>Schooling, Experience, and Earnings</i>. National Bureau of Economic Research.",
    "[14] James, G., Witten, D., Hastie, T., &amp; Tibshirani, R. (2021). <i>An Introduction to Statistical Learning with Applications in R</i> (2nd ed.). Springer.",
    "[15] Kuhn, M., &amp; Johnson, K. (2013). <i>Applied Predictive Modeling</i>. Springer.",
    "[16] Kuhn, M. (2008). Building predictive models in R using the caret package. <i>Journal of Statistical Software, 28</i>(5), 1-26.",
    "[17] DeLong, E. R., DeLong, D. M., &amp; Clarke-Pearson, D. L. (1988). Comparing the areas under two or more correlated receiver operating characteristic curves: a nonparametric approach. <i>Biometrics, 44</i>(3), 837-845.",
    "[18] Breiman, L. (2001). Random forests. <i>Machine Learning, 45</i>(1), 5-32.",
    "[19] Friedman, J. H. (2001). Greedy function approximation: a gradient boosting machine. <i>Annals of Statistics, 29</i>(5), 1189-1232.",
    "[20] Robin, X., et al. (2011). pROC: an open-source package for R and S+ to analyze and compare ROC curves. <i>BMC Bioinformatics, 12</i>, 77.",
]
for r in refs:
    story.append(Paragraph(r, ref))

doc.build(story)
print("written", OUT)
