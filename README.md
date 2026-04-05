# Snowflake Inc. ($SNOW) – Quantitative Equity Research
**Author:** Tinotenda Denver | MBA, Financial Management & Analytics

##  Project Overview
This project provides a deep-dive financial analysis of **Snowflake Inc.** from FY2019 to FY2026. Using R, I have modeled the divergence between Snowflake's GAAP accounting losses and its robust Free Cash Flow generation.

### Key Insights
* **The Rule of 40 Paradox:** Snowflake maintains an elite FCF-based Rule of 40 score (53), despite a GAAP-based score of 1, driven primarily by $1.6B in Stock-Based Compensation.
* **Operational Efficiency:** Collections efficiency has nearly doubled, with Days Sales Outstanding (DSO) dropping from 167 days to 87 days.
* **Valuation:** A 10,000-run Monte Carlo DCF simulation suggests a median intrinsic value of **$110/share**, highlighting a potential overvaluation at current market prices.

## Methodology
* **Environment:** R (tidyverse, ggplot2, patchwork)
* **Modeling:** Log-linear regression for margin expansion & Monte Carlo DCF.
* **Data Source:** Bloomberg Terminal.

##  Contents
* `analysis_script.R`: Full code for data wrangling, regression, and simulation.
* `Snowflake_R_Analysis.pdf`: The final 14-page visual report.
* `/data`: Standardized balance sheets and cash flow statements used in the model.
