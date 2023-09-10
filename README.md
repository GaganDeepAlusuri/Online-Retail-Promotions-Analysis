# Online Retail Promotions Analysis

![Retail Promotions](https://www.cibirix.com/cdn/uploads//2018/12/christmas-sale-e-commerce-online-store.png)

## Overview
This repository contains the code and data for an analysis of online retail promotions. The analysis includes various aspects of promotions and their impact on customer behavior. The project is implemented in R and includes data preprocessing, exploratory data analysis, statistical tests, and model building.

## Files
- **Online Retail Promotions Report:** This file contains a detailed report of the analysis, including findings and recommendations.

- **Credit Card Online Retail Promotions.r:** The R script that contains the code for data preprocessing, exploratory data analysis, statistical tests, and model building.

- **Online Retail Promotions Dataset:** The dataset used for the analysis.

- **Models stargazer output:** An HTML file containing stargazer output for the models used in the analysis.

## Key Questions Addressed
1. **How did the promotion campaigns work relative to the control group?** The analysis evaluates the effectiveness of promotions compared to the control group and determines if men's promotions were more successful than women's promotions or vice versa.

2. **Should promotions target new customers or established customers?** The analysis assesses whether promotions should be aimed at new customers who joined in the last 12 months or established customers.

3. **Should promotions target customers with higher or lower spending history?** The analysis explores whether promotions are more effective for customers with a higher or lower spending history over the past year.

4. **Did promotions work better for the phone or web channel?** The analysis compares the effectiveness of promotions on the phone and web channels.

5. **Which promotions work better for specific merchandise purchases?** The analysis investigates whether the men's promotion is more effective when targeted at customers who bought men's merchandise in the last year and if the same applies to women's promotions.

## Results
Here are some key findings from the analysis:

- Customers who received men's promotions spent 3.5% less than the control group, while customers who received women's promotions spent 19% more.

- Promotions should be targeted more towards new customers, as they tend to spend 1.1% more than existing customers.

- Lower history customers tend to spend 14.5% more, suggesting that targeting them with promotions could be beneficial.

- Customers using the phone channel tend to spend 0.2% more compared to the web channel.

- Customers who bought female merchandise last year and received a female promotion spent 32.5% less than those who bought male merchandise last year and received a male promotion.

## Dependencies
- R libraries: `rio`, `dplyr`, `ggplot2`, `tidyr`, `corrplot`, `pscl`, `MASS`, `AER`, and others.

## Usage
1. Open the "Credit Card Online Retail Promotions.r" script in R or RStudio.

2. Run the code to perform the analysis.

3. Review the output and findings in the script.

4. Refer to the "Online Retail Promotions Report" for a detailed report of the analysis.

## Author
- Gagan Deep Alusuri
