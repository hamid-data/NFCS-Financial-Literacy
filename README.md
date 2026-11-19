# Financial Literacy and Emergency Fund Saving

Does knowing more about money mean having a financial safety net? This project uses the 2021 National Financial Capability Study (NFCS) to look at how **financial literacy** relates to whether U.S. adults have an **emergency fund** covering three months of expenses.


## Overview

Households with a rainy-day fund cope much better with a job loss, illness or an unexpected bill. Financial education is often proposed as a way to encourage saving, so it matters whether literacy is actually linked to saving once income, education and other factors are taken into account.

The analysis builds a seven-question literacy score and estimates logistic regressions for having an emergency fund. It reports both log-odds and **average marginal effects** (changes in probability), and tests how stable the results are across alternative specifications.

## Key results

- **Income, education and being married** were strongly and positively associated with having an emergency fund in every model. Having more dependent children was negatively associated.
- **Financial literacy had a small association at baseline** (not significant in Model 1) and a modestly positive one once more controls were added (Model 2).
- The **Literacy × Income interaction was positive and significant**, which suggests the link between literacy and saving is stronger for higher-income households.
- Using the narrower "Big Three" literacy measure gave similar patterns, and literacy was positively associated with **retirement saving** as well.
- The linear probability model was used for comparison only. Breusch–Pagan and White tests showed heteroskedasticity, which supports using logit models.

| Model | What it adds |
|---|---|
| Model 1 (Baseline) | Literacy score plus income, education, age, gender, employment, dependent children, marital status |
| Model 2 (Extended) | Age², college degree, race/ethnicity, subjective financial knowledge, Census Division fixed effects |
| Model 3 (Interaction) | Literacy × Income interaction |


## Data

- **Source:** 2021 NFCS State-by-State Survey, from the FINRA Investor Education Foundation (public use data).
- **Size:** 27,118 respondents. After listwise deletion on the variables in the baseline model, the analytic sample is **7,600**.
- **Not included** in this repository, because redistribution is governed by the data provider. Download it from the FINRA Foundation's NFCS website and load it in R as `df_raw`.
- **Key survey items:** J5 (emergency fund), C5_2012 (retirement saving), M6–M10, M31 and M50 (financial literacy questions), M4 (self-rated knowledge), and demographics such as A8 (income), A5 (education), A3Ar_w (age), A50A (gender), A9 (employment), A7A (marital status), A11 (dependent children) and CENSUSDIV (region).

## Methods

1. **Literacy index:** one point for each correct answer to seven objective questions (compound interest, inflation, bonds, mortgages, diversification, debt doubling, probability). The score is standardized to a z-score. Cronbach's alpha is about 0.60.
2. **Outcome:** 1 if the respondent has an emergency fund, 0 if not. "Don't know" and "Prefer not to say" are treated as missing.
3. **Models:** logistic regression with heteroskedasticity-robust (HC3) standard errors, and average marginal effects.
4. **Robustness checks:**
   - Big Three literacy index
   - retirement saving as the outcome
   - a linear probability model
   - working-age adults only (18–64)
5. **Fit statistics:** McFadden's pseudo-R², AIC and log-likelihood.

## Repository structure

```
├── README.md
├── code/
│   └── NFCS Financial Literacy Analysis.R
├── report/
│   └── NFCS Empirical Report.docx

```

## How to run

1. Install R and, optionally, RStudio.
2. Install the packages:
   ```r
   install.packages(c("dplyr", "tidyr", "tibble", "ggplot2", "psych", "sandwich",
                      "lmtest", "margins", "modelsummary", "gt"))
   ```
3. Download the NFCS 2021 State-by-State data and load it into R as `df_raw`, for example with `readxl::read_excel()`.
4. Run `code/NFCS_Financial_Literacy_Analysis.R` from top to bottom. Tables appear in the RStudio viewer and figures in the plots pane.

## Limitations

- **Association, not causation.** The data are a single cross-section, so reverse causation and omitted factors (such as risk attitudes or family support) cannot be ruled out.
- **Large share of missing data.** Listwise deletion reduced the sample from 27,118 to 7,600 respondents, so results may not represent the full survey population.
- Regressions are **unweighted**, as is common in NFCS studies, so estimates do not represent the U.S. population directly.
- Self-reported survey measures may contain reporting error.

## Known issues and next steps

- Some table values in the report should be re-checked against a fresh run of the script (for example the Model 2 sample size and table labels).
- Possible extensions: survey-weighted estimates, multiple imputation for missing data, and pooling several NFCS waves.

## Tools

R, with `dplyr`, `tidyr`, `ggplot2`, `psych`, `sandwich`, `lmtest`, `margins`, `modelsummary` and `gt`.

## License

MIT License for the code and report. The NFCS data remain subject to the data provider's terms.

