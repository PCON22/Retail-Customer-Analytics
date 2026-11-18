# Retail Customer Analytics & Churn Prediction

## 📖 Project Overview
This project simulates the day-to-day work of a Data Analyst at a UK-based online retail company. Using the **Online Retail II dataset** (UCI Machine Learning Repository), I took over 1 million messy transactions from 2009–2011 and built a complete analytics pipeline.

**The Goal:** Answer 4 critical business questions:
1. **Who are our best customers, and how do they behave?** (RFM Analysis & Segmentation)
2. **Are we losing customers?** (Churn Risk Analysis)
3. **Can we predict who will leave before they do?** (Machine Learning)
4. **How much is a customer worth to us in the future?** (Customer Lifetime Value).

## Tools & Techniques
- **Language:** R
- **Data wrangling:** dplyr, tidyr, lubridate, janitor
- **Visualisation:** ggplot2, scales
- **Machine learning:** caret, randomForest, pROC (Logistic Regression, Random Forest, ROC-AUC evaluation)
- **CLV modelling:** BTYD (BG/NBD model)
- **Export:** writexl (Excel), base R (CSV)

## Project Structure
- Loyalty.R` — full analysis code
- `visualizations/` — 11 charts (PNG)
- `outputs/` — summary CSVs

## Visualizations & Key Insights

### 1. Data Cleaning Impact
[Data Cleaning Comparison](01_data_cleaning_comparison.png)
**What it shows:** A comparison of the raw data (with guest shoppers) versus the cleaned data. 
**Insight:** We removed ~X% of transactions (cancellations, negative quantities, zero-price errors) to ensure the analysis only reflects actual, completed sales. This establishes a trustworthy baseline for all subsequent analysis.

### 2. Top 3 Products by Monthly Revenue
[Monthly Revenue Top 3](02_monthly_revenue_top3_products.png)
**What it shows:** A line chart tracking the monthly revenue of the top 3 best-selling products over two years.
**Insight:** This highlights clear seasonality (typically peaking in Q4 leading up to Christmas). It helps the business plan inventory and staffing during peak months.

### 3. Customer Revenue Distribution
[Customer Revenue Distribution](03_customer_revenue_distribution.png)
**What it shows:** A histogram showing the spread of total revenue per customer.
**Insight:** The data is heavily right-skewed. Most customers spend a small amount, while a very small percentage of "whales" drive the majority of the revenue.

### 4. Customer Revenue Segments
[Customer Revenue Segments](04_revenue_segments_pie.png)
**What it shows:** A pie chart dividing customers into Top 10% (VIP), Regular, Low, and Very Low spenders.
**Insight:** This validates the Pareto principle (80/20 rule). The Top 10% of customers are responsible for the vast majority of the company's revenue.

### 5. RFM Customer Segments
[RFM Segments](05_rfm_segments.png)
**What it shows:** A bar chart classifying customers using **RFM Analysis** (Recency, Frequency, Monetary value).
**Insight:** Customers are grouped into actionable segments like "Champions", "Loyal", and "At Risk". Marketing can target "At Risk" big spenders with win-back campaigns, while rewarding "Champions" with VIP perks.

### 6. Customer Tenure Distribution
[Tenure Distribution](06_tenure_distribution.png)
**What it shows:** A histogram showing how long customers have been shopping with the company (in days).
**Insight:** Shows the typical customer lifecycle. If the bulk of customers have a short tenure, it indicates a high churn rate or a recent surge in new acquisitions that need nurturing.

### 7. Revenue vs. Tenure
[Revenue vs Tenure](07_revenue_vs_tenure.png)
**What it shows:** A scatter plot with a trend line comparing a customer's tenure to their total spend.
**Insight:** There is a clear positive correlation. The longer a customer stays with the company, the more they tend to spend over their lifetime, proving the value of retention efforts.

### 8. Churn Risk Analysis
[Churn Risk](08_churn_risk.png)
**What it shows:** A bar chart showing how many customers fall into "Active", "Low Risk", "Medium Risk", and "High Risk" churn categories based on days since last purchase.
**Insight:** Quantifies exactly how many customers are slipping away, giving the marketing team a specific audience size for re-engagement campaigns.

### 9. Random Forest Feature Importance
[Feature Importance](09_rf_feature_importance.png)
**What it shows:** A chart from the Machine Learning model ranking which customer features are most predictive of churn.
**Insight:** Features like "Recency" (days since last purchase) and "Frequency" (number of orders) are typically the strongest predictors of whether a customer will leave.

### 10. Customer Lifetime Value (CLV) Distribution
[CLV Distribution](10_clv_distribution.png)
**What it shows:** A histogram showing the predicted future value of every customer using the BG/NBD model.
**Insight:** Helps the business determine how much they can afford to spend on customer acquisition. If a customer's predicted CLV is £500, spending £100 to acquire them is highly profitable.

### 11. Historical vs. Predicted CLV
[Current Revenue vs CLV](11_current_revenue_vs_clv.png)
**What it shows:** A scatter plot comparing a customer's historical revenue against their predicted future CLV.
**Insight:** Points above the red line represent customers with high future potential. These are ideal targets for upselling and loyalty programs, as their future value exceeds their past behavior.

## Dataset
The dataset is not included because of its size. Download `online_retail_II.xlsx` from:
https://archive.ics.uci.edu/dataset/502/online+retail+ii

## Author
[Chukwuma NSionu] — [(https://www.linkedin.com/in/chukwuma-nsionu-a76148a6/)]
