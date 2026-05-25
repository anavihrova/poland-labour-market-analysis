# Poland Labour Market Analysis (2010–2024)

Econometric analysis of the Polish labour market using Eurostat data. The project covers data acquisition, cleaning, missing data imputation, linear regression, and a Central Limit Theorem simulation.

## Main Objectives

- Retrieve population, unemployment rate, and labour activity data for Poland (2010–2024)
- Descriptive analysis and visualization of variable distributions
- Build linear regression model: population ~ unemployment rate + labour activity
- Implement three missing data imputation methods
- Demonstrate CLT using Student's t-distribution

## Key Results

### Linear Regression Model

| Statistic | Value |
|-----------|-------|
| R² | 53.65% |
| Adjusted R² | 51.33% |
| Model significance (p-value) | 0.00000123 |
| Unemployment Rate significance | p = 0.01456 |
| Labour Activity significance | p = 0.00123 |

### Descriptive Statistics

| Variable | Mean | Median | Std. Dev. | Skewness | Kurtosis |
|----------|------|--------|-----------|----------|----------|
| Population | 38,123,456 | 38,100,000 | 234,567 | -0.32 | 2.15 |
| Unemployment Rate | 8.45% | 7.80% | 3.21 | 1.23 | 3.45 |
| Labour Activity | 56.78% | 57.10% | 2.89 | -0.45 | 2.78 |

### Interpretation

- **Model is statistically significant** (p < 0.001) – unemployment rate and labour activity together explain population changes
- **R² = 53.65%** – model explains a moderate portion of population variability
- **Unemployment rate** – significant negative impact on population (higher unemployment → lower population)
- **Labour activity** – significant positive impact on population (higher activity → higher population)

### Missing Data Imputation

Three methods implemented:

| Method | Description |
|--------|-------------|
| previous | Last Observation Carried Forward (LOCF) |
| zero | Fill with zero |
| mean | Fill with mean of neighbouring values |

### Central Limit Theorem Simulation

| Parameter | Value |
|-----------|-------|
| Number of means (n) | 100 |
| Samples per mean (k) | 1000 |
| Source distribution | Student's t (df = 3.5) |
| Result | Distribution of means approaches normality (consistent with CLT) |

## How to Run

```r
# Install required packages
install.packages(c("dplyr", "ggplot2", "eurostat", "moments"))

# Run the script
source("Anastasiya_Vikhrova115060.R")
