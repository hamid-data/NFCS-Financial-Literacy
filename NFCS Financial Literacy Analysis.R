# ==============================================================================
# Financial Literacy and Emergency Fund Saving
# Analysis of the 2021 NFCS State-by-State Survey
# ==============================================================================
# Assumes the NFCS 2021 State-by-State microdata is already loaded in R as the
# data frame `df_raw` (one row per respondent).

library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(psych)
library(sandwich)
library(lmtest)
library(margins)
library(modelsummary)
library(gt)

set.seed(42)

# ==============================================================================
# SECTION 1: VARIABLE CONSTRUCTION
# ==============================================================================
# Responses coded 98 (Don't know) / 99 (Prefer not to say) are recoded to NA.

df <- df_raw

# ------------------------------------------------------------------------------
# 1.1 Financial literacy index (main independent variable)
# ------------------------------------------------------------------------------
# Items (correct answer = 1, incorrect or DK/PNS = 0 in the score):
#   M6  compound interest          (correct = 1)
#   M7  inflation                  (correct = 3)
#   M8  bond prices / rates        (correct = 2)
#   M9  mortgage term              (correct = 1)
#   M10 diversification            (correct = 2)
#   M31 debt doubling rule         (correct = 2)
#   M50 probability / numeracy     (correct = 1)
# Index = number of correct answers (0-7), then standardized (z-score).

df <- df %>%
  mutate(
    lit_M6  = ifelse(M6  == 1, 1, 0),
    lit_M7  = ifelse(M7  == 3, 1, 0),
    lit_M8  = ifelse(M8  == 2, 1, 0),
    lit_M9  = ifelse(M9  == 1, 1, 0),
    lit_M10 = ifelse(M10 == 2, 1, 0),
    lit_M31 = ifelse(M31 == 2, 1, 0),
    lit_M50 = ifelse(M50 == 1, 1, 0),

    # Set to NA if original was 98 or 99
    lit_M6  = ifelse(M6  %in% c(98, 99), NA, lit_M6),
    lit_M7  = ifelse(M7  %in% c(98, 99), NA, lit_M7),
    lit_M8  = ifelse(M8  %in% c(98, 99), NA, lit_M8),
    lit_M9  = ifelse(M9  %in% c(98, 99), NA, lit_M9),
    lit_M10 = ifelse(M10 %in% c(98, 99), NA, lit_M10),
    lit_M31 = ifelse(M31 %in% c(98, 99), NA, lit_M31),
    lit_M50 = ifelse(M50 %in% c(98, 99), NA, lit_M50)
  ) %>%
  mutate(
    fin_lit_score = rowSums(select(., lit_M6, lit_M7, lit_M8, lit_M9,
                                   lit_M10, lit_M31, lit_M50), na.rm = FALSE),
    fin_lit_z = as.numeric(scale(fin_lit_score))
  )

# Robustness: "Big Three" literacy index (interest, inflation, diversification)
df <- df %>%
  mutate(
    fin_lit_big3   = rowSums(select(., lit_M6, lit_M7, lit_M10), na.rm = FALSE),
    fin_lit_big3_z = as.numeric(scale(fin_lit_big3))
  )

# Subjective self-assessed financial knowledge (M4, scale 1-7)
df <- df %>%
  mutate(subj_literacy = ifelse(M4 %in% c(98, 99), NA, M4))

# Internal consistency of the 7-item scale
cat("Cronbach's Alpha for 7-item literacy scale:\n")
alpha_data <- df %>%
  select(lit_M6, lit_M7, lit_M8, lit_M9, lit_M10, lit_M31, lit_M50) %>%
  filter(complete.cases(.))
psych::alpha(alpha_data)$total$raw_alpha %>% round(3) %>% print()

# ------------------------------------------------------------------------------
# 1.2 Dependent variables
# ------------------------------------------------------------------------------
# Y1: emergency fund covering 3 months of expenses (J5: 1 = Yes, 2 = No)
# Y2: regular retirement account contribution (C5_2012: 1 = Yes, 2 = No) [robustness]
# Additional: spending less than income (J3: 1 = less than income)
df <- df %>%
  mutate(
    emerg_fund = case_when(J5 == 1 ~ 1L, J5 == 2 ~ 0L, TRUE ~ NA_integer_),
    retire_save = case_when(C5_2012 == 1 ~ 1L, C5_2012 == 2 ~ 0L, TRUE ~ NA_integer_),
    spend_less = case_when(J3 == 1 ~ 1L, J3 %in% c(2, 3) ~ 0L, TRUE ~ NA_integer_)
  )

# ------------------------------------------------------------------------------
# 1.3 Control variables
# ------------------------------------------------------------------------------
df <- df %>%
  mutate(
    # Income category (1 = <$15k ... 10 = $300k+)
    income = ifelse(A8_2021 %in% c(98, 99), NA, A8_2021),

    # Education (1 = < high school ... 7 = postgraduate)
    education   = ifelse(A5_2015 == 99, NA, A5_2015),
    college_deg = ifelse(education >= 6, 1L, 0L),

    # Age group (1 = 18-24 ... 6 = 65+) and approximate midpoint
    age_group = ifelse(A3Ar_w %in% c(98, 99), NA, A3Ar_w),
    age_midpoint = case_when(
      age_group == 1 ~ 21,
      age_group == 2 ~ 30,
      age_group == 3 ~ 40,
      age_group == 4 ~ 50,
      age_group == 5 ~ 60,
      age_group == 6 ~ 70,
      TRUE ~ NA_real_
    ),
    age_sq = age_midpoint^2,

    # Gender (A50A: 1 = Male, 2 = Female)
    female = ifelse(A50A == 2, 1L, ifelse(A50A == 1, 0L, NA_integer_)),

    # Employment (A9)
    employed = case_when(
      A9 %in% c(1, 2, 3)       ~ 1L,
      A9 %in% c(4, 5, 6, 7, 8) ~ 0L,
      TRUE ~ NA_integer_
    ),
    emp_fulltime   = ifelse(A9 == 2, 1L, 0L),
    emp_selfempl   = ifelse(A9 == 1, 1L, 0L),
    emp_retired    = ifelse(A9 == 8, 1L, 0L),
    emp_unemployed = ifelse(A9 == 7, 1L, 0L),

    # Number of financially dependent children (4 = 4 or more)
    n_dep_children = case_when(
      A11 == 1 ~ 1L,
      A11 == 2 ~ 2L,
      A11 == 3 ~ 3L,
      A11 == 4 ~ 4L,
      A11 %in% c(5, 6) ~ 0L,
      TRUE ~ NA_integer_
    ),

    # Marital status (A7A: 1 = Married)
    married = ifelse(A7A == 1, 1L, ifelse(A7A %in% c(2, 3), 0L, NA_integer_)),

    # Race/ethnicity (A4A_new_w: 1 = White non-Hispanic)
    white_nonhisp = ifelse(A4A_new_w == 1, 1L,
                           ifelse(A4A_new_w == 2, 0L, NA_integer_)),

    # Census Division (region fixed effects)
    census_div = factor(CENSUSDIV)
  )

# ==============================================================================
# SECTION 2: SAMPLE SELECTION (LISTWISE DELETION)
# ==============================================================================
cat("\n--- Sample Selection ---\n")
cat("Full sample N:", nrow(df), "\n")

vars_model1 <- c("emerg_fund", "fin_lit_z",
                 "income", "education", "age_midpoint",
                 "female", "employed", "n_dep_children", "married")

vars_model2 <- c(vars_model1, "white_nonhisp", "college_deg",
                 "emp_retired", "emp_unemployed", "emp_selfempl",
                 "age_sq", "subj_literacy")

df_analytic <- df %>%
  filter(complete.cases(select(., all_of(vars_model1))))

cat("Analytic sample (Model 1 complete cases) N:", nrow(df_analytic), "\n")
cat("Observations excluded:", nrow(df) - nrow(df_analytic), "\n")

df_analytic2 <- df %>%
  filter(complete.cases(select(., all_of(vars_model2))))

cat("Analytic sample (Model 2 complete cases) N:", nrow(df_analytic2), "\n")

# Missingness overview for key variables
key_vars <- c("J5", "C5_2012", "M6", "M7", "M8", "M9", "M10", "M31", "M50",
              "A8_2021", "A5_2015", "A3Ar_w", "A50A", "A9", "A7A", "A11")

missing_table <- data.frame(
  Variable    = key_vars,
  Total_N     = sapply(df_raw[key_vars], length),
  Missing_N   = sapply(df_raw[key_vars],
                       function(x) sum(is.na(x) | x %in% c(97, 98, 99))),
  Missing_Pct = sapply(df_raw[key_vars],
                       function(x) round(mean(is.na(x) | x %in% c(97, 98, 99)) * 100, 2))
)
missing_table

missing_table %>%
  gt() %>%
  tab_header(title = "Missingness Overview for Key NFCS Variables") %>%
  cols_label(Variable = "Variable", Total_N = "Total N",
             Missing_N = "Missing N", Missing_Pct = "Missing %")

# ==============================================================================
# SECTION 3: DESCRIPTIVE STATISTICS
# ==============================================================================
cat("\n--- Descriptive Statistics ---\n")

desc_vars <- c("emerg_fund", "retire_save", "spend_less",
               "fin_lit_score", "fin_lit_z", "fin_lit_big3",
               "subj_literacy",
               "income", "education", "age_midpoint",
               "female", "employed", "married", "white_nonhisp",
               "n_dep_children", "college_deg")

desc_table <- df_analytic %>%
  select(all_of(desc_vars)) %>%
  summarise(across(everything(),
                   list(
                     N    = ~sum(!is.na(.)),
                     Mean = ~round(mean(., na.rm = TRUE), 3),
                     SD   = ~round(sd(., na.rm = TRUE), 3),
                     Min  = ~min(., na.rm = TRUE),
                     Max  = ~max(., na.rm = TRUE)
                   ),
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(everything(),
               names_to = c("Variable", "Statistic"),
               names_sep = "_(?=[^_]+$)") %>%
  pivot_wider(names_from = Statistic, values_from = value)

print(desc_table, n = 50)

desc_table_gt <- desc_table %>%
  gt() %>%
  tab_header(title = "Descriptive Statistics Table") %>%
  cols_label(Variable = "Variable", N = "Sample Size (N)", Mean = "Mean",
             SD = "Standard Deviation (SD)", Min = "Min", Max = "Max") %>%
  tab_spanner(label = "Descriptive Stats",
              columns = c("Mean", "SD", "Min", "Max")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(columns = everything()))
desc_table_gt

cat("\nProportion with emergency fund:", mean(df_analytic$emerg_fund, na.rm = TRUE) %>% round(3), "\n")
cat("Proportion contributing to retirement:", mean(df$retire_save, na.rm = TRUE) %>% round(3), "\n")
cat("Mean financial literacy score (0-7):", mean(df_analytic$fin_lit_score, na.rm = TRUE) %>% round(3), "\n")

# ==============================================================================
# SECTION 4: MODEL ESTIMATION
# ==============================================================================
# Y1 (emerg_fund) is binary -> logit model, with HC3 robust standard errors
# and average marginal effects (AME).

# Helper: tidy a coeftest object into a data frame
tidy_coeftest <- function(ct) {
  data.frame(
    Variable  = rownames(ct),
    Estimate  = round(ct[, 1], 4),
    Std_Error = round(ct[, 2], 4),
    Z_value   = round(ct[, 3], 4),
    P_value   = round(ct[, 4], 4)
  )
}

# Helper: formatted table of model coefficients
coef_gt <- function(df_coef, title, spanner) {
  df_coef %>%
    gt() %>%
    tab_header(title = title) %>%
    cols_label(Variable = "Variable", Estimate = "Estimate",
               Std_Error = "Standard Error", Z_value = "Z-value",
               P_value = "P-value") %>%
    tab_spanner(label = spanner,
                columns = c("Estimate", "Std_Error", "Z_value", "P_value")) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_column_labels(columns = everything()))
}

# Helper: formatted table of average marginal effects
ame_gt <- function(ame_obj, title) {
  summary(ame_obj) %>%
    as.data.frame() %>%
    select(factor, AME, SE, z, p, lower, upper) %>%
    mutate(across(c(AME, SE, z, p, lower, upper), ~round(., 4))) %>%
    gt() %>%
    tab_header(title = title) %>%
    cols_label(factor = "Variable", AME = "Average Marginal Effect",
               SE = "Standard Error", z = "Z-value", p = "P-value",
               lower = "Lower CI", upper = "Upper CI") %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_column_labels(columns = everything()))
}

# ------------------------------------------------------------------------------
# 4.1 Model 1 - Baseline logit
# ------------------------------------------------------------------------------
m1 <- glm(emerg_fund ~ fin_lit_z + income + education + age_midpoint +
            female + employed + n_dep_children + married,
          data = df_analytic,
          family = binomial(link = "logit"))

cat("\n--- Model 1: Baseline Logit ---\n")
summary(m1)

m1_robust <- coeftest(m1, vcov = vcovHC(m1, type = "HC3"))
print(m1_robust)

m1_ame <- margins(m1, vcov = vcovHC(m1, type = "HC3"))
cat("\n--- Model 1 Average Marginal Effects ---\n")
summary(m1_ame)

coef_gt(tidy_coeftest(coef(summary(m1))),
        "Model 1: Baseline Logit Regression Results", "Logit Model Coefficients")
coef_gt(tidy_coeftest(m1_robust),
        "Model 1: Robust Logit Regression Results (HC3)", "Robust Logit Model Coefficients")
ame_gt(m1_ame, "Model 1: Average Marginal Effects (AME)")

# ------------------------------------------------------------------------------
# 4.2 Model 2 - Extended logit (additional controls + Census Division FE)
# ------------------------------------------------------------------------------
m2 <- glm(emerg_fund ~ fin_lit_z + income + education + age_midpoint + age_sq +
            female + employed + emp_retired + emp_unemployed + emp_selfempl +
            n_dep_children + married + white_nonhisp + college_deg +
            subj_literacy + census_div,
          data   = df_analytic2,
          family = binomial(link = "logit"))

cat("\n--- Model 2: Extended Logit ---\n")
m2_robust <- coeftest(m2, vcov = vcovHC(m2, type = "HC3"))
print(m2_robust)

m2_ame <- margins(m2, vcov = vcovHC(m2, type = "HC3"))
cat("\n--- Model 2 Average Marginal Effects ---\n")
summary(m2_ame)

coef_gt(tidy_coeftest(m2_robust),
        "Model 2: Extended Logit Regression Results (HC3)", "Extended Logit Model Coefficients")
ame_gt(m2_ame, "Model 2: Average Marginal Effects (AME)")

# ------------------------------------------------------------------------------
# 4.3 Model 3 - Heterogeneity (Literacy x Income interaction)
# ------------------------------------------------------------------------------
m3 <- glm(emerg_fund ~ fin_lit_z * income + education + age_midpoint +
            female + employed + n_dep_children + married,
          data   = df_analytic,
          family = binomial(link = "logit"))

cat("\n--- Model 3: Interaction (Literacy x Income) ---\n")
m3_robust <- coeftest(m3, vcov = vcovHC(m3, type = "HC3"))
print(m3_robust)

coef_gt(tidy_coeftest(m3_robust),
        "Model 3: Interaction Logit Regression Results (HC3)", "Interaction Model Coefficients")

# Marginal effect of literacy at low (1), median (5) and high (8) income
for (inc in c(1, 5, 8)) {
  cat("\nMarginal effects at income =", inc, "\n")
  print(margins(m3, at = list(income = inc), vcov = vcovHC(m3, type = "HC3")) %>% summary())
}

ame_gt(margins(m3, at = list(income = 1), vcov = vcovHC(m3, type = "HC3")),
       "Marginal Effects of Literacy at Low Income (Income = 1)")
ame_gt(margins(m3, at = list(income = 5), vcov = vcovHC(m3, type = "HC3")),
       "Marginal Effects of Literacy at Median Income (Income = 5)")
ame_gt(margins(m3, at = list(income = 8), vcov = vcovHC(m3, type = "HC3")),
       "Marginal Effects of Literacy at High Income (Income = 8)")

# ==============================================================================
# SECTION 5: ROBUSTNESS CHECKS
# ==============================================================================

# 5.1 Alternative literacy index (Big Three)
m_rob1 <- glm(emerg_fund ~ fin_lit_big3_z + income + education + age_midpoint +
                female + employed + n_dep_children + married,
              data   = df_analytic,
              family = binomial(link = "logit"))

cat("\n--- Robustness 1: Big-Three Literacy Index ---\n")
coeftest(m_rob1, vcov = vcovHC(m_rob1, type = "HC3"))

# 5.2 Alternative outcome (retirement saving)
df_rob2 <- df %>%
  filter(complete.cases(select(., retire_save, fin_lit_z,
                               income, education, age_midpoint,
                               female, employed, n_dep_children, married)))

m_rob2 <- glm(retire_save ~ fin_lit_z + income + education + age_midpoint +
                female + employed + n_dep_children + married,
              data   = df_rob2,
              family = binomial(link = "logit"))

cat("\n--- Robustness 2: Alternative Outcome (Retirement Saving) ---\n")
coeftest(m_rob2, vcov = vcovHC(m_rob2, type = "HC3"))

# 5.3 Linear probability model (comparison only) + heteroskedasticity tests
m_lpm <- lm(emerg_fund ~ fin_lit_z + income + education + age_midpoint +
              female + employed + n_dep_children + married,
            data = df_analytic)

cat("\n--- LPM for Comparison ---\n")
summary(m_lpm)

cat("\nBreusch-Pagan Test:\n")
bptest(m_lpm)

cat("\nWhite Test (using squared fitted values):\n")
bptest(m_lpm, ~ fitted(m_lpm) + I(fitted(m_lpm)^2))

cat("\nLPM with HC robust SEs:\n")
coeftest(m_lpm, vcov = vcovHC(m_lpm, type = "HC3"))

summary(m_lpm)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  mutate(across(c(Estimate, `Std. Error`, `t value`, `Pr(>|t|)`), ~round(., 4))) %>%
  gt() %>%
  tab_header(title = "Linear Probability Model (LPM) for Emergency Fund") %>%
  cols_label(`Std. Error` = "Standard Error", `t value` = "T-value",
             `Pr(>|t|)` = "P-value") %>%
  tab_spanner(label = "LPM Model Coefficients",
              columns = c("Estimate", "Std. Error", "t value", "Pr(>|t|)")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_column_labels(columns = everything()))

# 5.4 Subsample: working-age adults (18-64)
df_working <- df_analytic %>%
  filter(age_group %in% 1:5)

m_sub <- glm(emerg_fund ~ fin_lit_z + income + education + age_midpoint +
               female + employed + n_dep_children + married,
             data   = df_working,
             family = binomial(link = "logit"))

cat("\n--- Robustness 4: Working-age subsample (18-64) ---\n")
coeftest(m_sub, vcov = vcovHC(m_sub, type = "HC3"))
cat("N subsample:", nrow(df_working), "\n")

# ==============================================================================
# SECTION 6: MODEL FIT STATISTICS
# ==============================================================================
# McFadden's pseudo-R2
pseudo_r2 <- function(model) {
  ll_null <- logLik(glm(model$y ~ 1, family = binomial))
  1 - (as.numeric(logLik(model)) / as.numeric(ll_null))
}

cat("\n--- Model Fit ---\n")
cat("Model 1 - McFadden R2:", round(pseudo_r2(m1), 4),
    "| AIC:", round(AIC(m1), 1), "| N:", nobs(m1), "\n")
cat("Model 2 - McFadden R2:", round(pseudo_r2(m2), 4),
    "| AIC:", round(AIC(m2), 1), "| N:", nobs(m2), "\n")
cat("Model 3 - McFadden R2:", round(pseudo_r2(m3), 4),
    "| AIC:", round(AIC(m3), 1), "| N:", nobs(m3), "\n")

# ==============================================================================
# SECTION 7: SUMMARY TABLES
# ==============================================================================
robust_cov <- function(m) vcovHC(m, type = "HC3")

# Table 1: Descriptive statistics
desc_for_table <- df_analytic %>%
  select(
    `Emergency Fund (Y1)`      = emerg_fund,
    `Retire Contribution (Y2)` = retire_save,
    `Literacy Score (0-7)`     = fin_lit_score,
    `Literacy (Standardized)`  = fin_lit_z,
    `Subj. Literacy (1-7)`     = subj_literacy,
    `Income (1-10)`            = income,
    `Education (1-7)`          = education,
    `Age (midpoint)`           = age_midpoint,
    `Female`                   = female,
    `Employed`                 = employed,
    `Married`                  = married,
    `Dep. Children`            = n_dep_children,
    `White Non-Hispanic`       = white_nonhisp,
    `College Degree`           = college_deg
  )
cat("\n--- Table 1: Descriptive Statistics ---\n")
datasummary_skim(desc_for_table, output = "dataframe")

# Table 2: Logit models (robust SEs)
modelsummary(
  list("Model 1\n(Baseline)"    = m1,
       "Model 2\n(Extended)"    = m2,
       "Model 3\n(Interaction)" = m3),
  vcov  = list(robust_cov(m1), robust_cov(m2), robust_cov(m3)),
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "logLik", "AIC"),
  coef_rename = c(
    "fin_lit_z"        = "Fin. Literacy (std.)",
    "fin_lit_z:income" = "Literacy x Income",
    "income"           = "Income",
    "education"        = "Education",
    "age_midpoint"     = "Age",
    "female"           = "Female",
    "employed"         = "Employed",
    "n_dep_children"   = "Dep. Children",
    "married"          = "Married"
  ),
  title = "Table 2. Logit Models: Financial Literacy and Emergency Fund Saving",
  notes = "Notes: Dependent variable = Emergency fund (1=Yes). Robust (HC3) standard errors in parentheses. Data: 2021 NFCS State-by-State Survey. *** p<0.01, ** p<0.05, * p<0.10."
)

# Table 3: Average marginal effects (Models 1 and 2)
ame1_df <- summary(m1_ame) %>%
  as.data.frame() %>%
  select(factor, AME, SE, z, p) %>%
  mutate(across(c(AME, SE, z, p), ~round(., 4)))

ame2_df <- summary(m2_ame) %>%
  as.data.frame() %>%
  select(factor, AME, SE, z, p) %>%
  mutate(across(c(AME, SE, z, p), ~round(., 4)))

cat("\n--- Table 3: Average Marginal Effects ---\n")
cat("Model 1:\n"); print(ame1_df)
cat("\nModel 2:\n"); print(ame2_df)

# Table 4: Robustness checks
modelsummary(
  list("Rob 1\n(Big-3 Literacy)" = m_rob1,
       "Rob 2\n(Retire Saving)"  = m_rob2,
       "Rob 3\n(LPM)"            = m_lpm,
       "Rob 4\n(Age 18-64)"      = m_sub),
  vcov  = list(robust_cov(m_rob1), robust_cov(m_rob2),
               robust_cov(m_lpm),  robust_cov(m_sub)),
  stars = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
  gof_map = c("nobs", "AIC"),
  coef_rename = c(
    "fin_lit_z"      = "Fin. Literacy (std.)",
    "fin_lit_big3_z" = "Literacy Big-3 (std.)",
    "income"         = "Income",
    "education"      = "Education",
    "age_midpoint"   = "Age",
    "female"         = "Female",
    "employed"       = "Employed",
    "n_dep_children" = "Dep. Children",
    "married"        = "Married"
  ),
  title = "Table 4. Robustness Checks",
  notes = "Notes: Rob 1 uses Big-Three literacy index. Rob 2 uses retirement contribution as outcome. Rob 3 is Linear Probability Model (comparison only). Rob 4 restricts to working-age adults (18-64). Robust (HC3) SEs in parentheses. Data: 2021 NFCS. *** p<0.01, ** p<0.05, * p<0.10."
)

# ==============================================================================
# SECTION 8: FIGURES
# ==============================================================================

# Figure 1: Distribution of financial literacy score
p1 <- ggplot(df_analytic, aes(x = fin_lit_score)) +
  geom_histogram(binwidth = 1, fill = "#2c7fb8", color = "white", alpha = 0.85) +
  scale_x_continuous(breaks = 0:7) +
  labs(title = "Figure 1. Distribution of Financial Literacy Score",
       subtitle = "2021 NFCS State-by-State Survey",
       x = "Financial Literacy Score (0-7 correct)",
       y = "Frequency",
       caption = "Note: Score = sum of correct answers to 7 financial literacy questions.") +
  theme_minimal(base_size = 12)
print(p1)

# Figure 2: Emergency fund rate by literacy score
p2 <- df_analytic %>%
  group_by(fin_lit_score) %>%
  summarise(pct_fund = mean(emerg_fund, na.rm = TRUE),
            n = n()) %>%
  ggplot(aes(x = fin_lit_score, y = pct_fund)) +
  geom_col(fill = "#2ca25f", alpha = 0.85) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.4, size = 3) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_x_continuous(breaks = 0:7) +
  labs(title = "Figure 2. Emergency Fund Rate by Financial Literacy Score",
       subtitle = "2021 NFCS State-by-State Survey",
       x = "Financial Literacy Score (0-7)",
       y = "Share with Emergency Fund") +
  theme_minimal(base_size = 12)
print(p2)
