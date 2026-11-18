
#### RETAIL CUSTOMER ANALYTICS & CHURN PREDICTION #####
# Dataset: Online Retail II

#### Section 1:SETUP and INSTALLATION ####
#Package Installation

install.packages(c(
  "readxl",      # Reads Excel files
  "dplyr",       # Data manipulation
  "tidyr",       # Data tidying
  "lubridate",   # Date handling
  "ggplot2",     # Visualization
  "data.table",  # Fast data handling
  "janitor",     # Data cleaning
  "scales",      # Number formatting
  "knitr",       # Report tables
  "writexl",     # Export to Excel

  #Clustering & segmentation
  "cluster",     # K-Means clustering
  "factoextra",  # Cluster visualization
  "fmsb",        # Radar charts

  #Time-to-event and CLV
  "survival",    # Survival analysis
  "BTYD",        # Customer Lifetime Value

  #Interactive tables
  "DT",          # Interactive tables

  #Machine learning (WITH ALL DEPENDENCIES)
  "caret",        # Machine learning toolbox
  "recipes",      # caret dependency
  "randomForest", # Random Forest algorithm
  "glmnet",       # Regularized regression
  "pROC"          # ROC curves
  ), dependencies = TRUE)


#Package Loading
#Open up all the packages installed so I can use them

library(readxl)      # Reads Excel files
library(dplyr)       # Helps sort, filter, and rearrange data
library(tidyr)       # Helps clean up messy data
library(lubridate)   # Works with dates and times easily
library(ggplot2)     # Makes pretty charts and graphs
library(data.table)  # Handles really big datasets quickly
library(janitor)     # Cleans up column names and bad data
library(scales)      # Formats numbers (1000000 becomes £1M)
library(knitr)       # Makes nice tables for reports
library(writexl)     # Exports data to Excel
library(cluster)     # For K-Means clustering
library(factoextra)  # Visualizes clustering results
library(fmsb)        # For radar charts
library(survival)    # For time-to-event analysis
library(BTYD)        # For Customer Lifetime Value
library(DT)          # Makes interactive tables
library(caret)       # For machine learning
library(randomForest)# Random Forest algorithm
library(glmnet)      # For logistic regression
library(pROC)        # For ROC curves



dir.create("outputs", showWarnings = FALSE)
dir.create("visualizations", showWarnings = FALSE)




################ SEction 2: DATA IMPORT CODE ###################
# Open the Excel file and read the first, second and retail sheets
# Read Excel files

getwd()

sheet1 <- read_excel("online_retail_II.xlsx", sheet = "Year 2009-2010")
sheet2 <- read_excel("online_retail_II.xlsx", sheet = "Year 2010-2011")

# Combine sheets and clean column names

retail <- bind_rows(sheet1, sheet2) %>% clean_names()

write.csv(retail, "outputs/retails_raw.csv", row.names = FALSE)

# Open a spreadsheet-like window showing my data
# View data


View(retail)
View(sheet1)
View(sheet2)


###############Section 3: DATA QUALITY and CLEANING  ###############

####Initial Data Exploration
#Shows me the structure of my data - how many rows, columns, and what type of data is in each column
# Check structure
str(retail)


# Count how many rows are missing a customer ID (guest shoppers)
# Count missing customer IDs
missing_customers <- sum(is.na(retail$customer_id))


# Calculate what percentage of transactions are from guest shoppers
# Calculate percentage missing

guest_percentage <- missing_customers / nrow(retail) * 100


# Count how many total transactions I have
# Count total rows
total_transactions <- nrow(retail)


##### Create Customer-Only Dataset

# Create a new dataset that only keeps rows where we know the customer ID (remove guest shoppers)
retail_with_customers <- retail %>%
  filter(!is.na(customer_id))




####### Full Data Cleaning

# Start with all data, then only keep customers we know
retail_clean <- retail %>%
  filter(!is.na(customer_id)) %>%

#Convert customer IDs from numbers to text (treat them as labels, not numbers)
  mutate(
    customer_id = as.character(customer_id),


    # Create a new column that says TRUE if the invoice starts with the letter C (means it was cancelled)
    is_cancellation = grepl("^C", invoice),

    #Convert the invoice date into a proper date format that R understands
    invoice_date = as_datetime(invoice_date)
  ) %>%

  #Remove all cancelled orders (only keep real sales)
  filter(
    !is_cancellation,

    # Remove rows with negative quantity (these are returns/refunds)
    quantity > 0,

    #Remove rows with zero or negative price (these are errors or adjustments)
    price > 0
  ) %>%


  #create a new column called revenue by multiplying quantity × price (total money for each transaction)
  mutate(revenue = quantity * price)


#### Check Cleaned Data

nrow(retail_clean) #Count how many rows are left after cleaning
str(retail_clean)  #Show me the structure of my cleaned data
head(retail_clean) #Show me the first 6 rows of my cleaned data
nrow(retail_with_customers) #Count how many customer-only rows I have
str(retail_with_customers)
head(retail_with_customers)


###### Find Removed Rows

# Find all rows that are in customer-only data but NOT in the clean data (these are the bad rows I removed)

removed_rows <- anti_join(retail_with_customers, retail_clean, by = "invoice")

# Count by category
#Count how many rows were removed

removed_summary <-removed_rows %>%
  summarise(
    total_removed = n(),

    cancellations = sum(grepl("^C", invoice)),#Count how many of the removed rows were cancellations
    negative_qty = sum(quantity < 0, na.rm = TRUE),# Count how many had negative quantity (returns)
    zero_price = sum(price <= 0, na.rm = TRUE), #Count how many had zero or negative price
    negative_price = sum(price < 0, na.rm = TRUE) #Count how many had negative price
  )

print(removed_summary)

# Percentage breakdown
removed_percentages  <- removed_rows %>%
  summarise(
    cancellations = sum(grepl("^C", invoice)) / n() * 100,
    negative_qty = sum(quantity < 0, na.rm = TRUE) / n() * 100,
    zero_price = sum(price <= 0, na.rm = TRUE) / n() * 100,
    negative_price = sum(price < 0, na.rm = TRUE) / n() * 100
  ) %>%
  round(2)


print(removed_percentages)

##### Compare Datasets
## See the difference

rows_removed <-nrow(retail_with_customers) - nrow(retail_clean)# How many rows did I remove
cat("\nRows removed during cleaning:", rows_removed, "\n")

# See percentage removed
(nrow(retail_with_customers) - nrow(retail_clean)) / nrow(retail_with_customers) * 100 #What percentage of customer transactions were removed as bad
cat("Percentage removed:", round(rows_removed / nrow(retail_with_customers) * 100, 2), "%\n")

# View removed rows
View(removed_rows)
head(removed_rows, 100)


#Create a small table comparing the two datasets
# Create summary comparison
##Create a small table comparing the two datasets
comparison <- data.frame(
  Dataset = c("retail_with_customers", "retail_clean"),
  Rows = c(nrow(retail_with_customers), nrow(retail_clean)),
  Columns = c(ncol(retail_with_customers), ncol(retail_clean))
)
comparison %>%
  mutate(
    Difference = Rows - lag(Rows),
    Removed_Percent = (Difference / Rows) * 100
  )

##### Visualize Data Cleaning Comparison


# Bar chart showing difference
### Create a table with the sizes of both datasets and the difference
library(ggplot2)

plot_data <- data.frame(
  Dataset = c("retail_with_customers", "retail_clean", "Removed"),
  Rows = c(
    nrow(retail_with_customers),
    nrow(retail_clean),
    nrow(retail_with_customers) - nrow(retail_clean)
  )
)

### Create a bar chart showing the three values
ggplot(plot_data, aes(x = Dataset, y = Rows, fill = Dataset)) +

  geom_bar(stat = "identity") + #Draw the bars

  #Put the numbers on top of each bar (with commas for thousands)
  geom_text(
    aes(label = scales::comma(Rows)),
    vjust = -0.5,#Position the numbers slightly above the bars
    size = 5, #Make the numbers bigger
    fontface = "bold" #Make the numbers bold
  ) +

  #add title, subtitle, and axis labels
  labs(
    title = "Comparison of Retail Datasets",
    subtitle = paste(scales::comma(nrow(retail_with_customers) - nrow(retail_clean)), "rows removed"),
    y = "Number of Rows",
    x = "Dataset"
  ) +

  #### Use a clean, simple style

  theme_minimal() + #Use a clean, simple style
  theme(

    ##Remove the legend (not needed)

    legend.position = "none", #Remove the legend (not needed)


    #Center the title and subtitle, make title bold
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )


################Section 4: PRODUCT ANALYSIS CODE Using Retails ##################

#Top Selling Products
### Group all transactions by product description (so all rows for the same product are together

top_products <- retail %>%
  group_by(description) %>%

  ### Add up the total quantity sold for each product

  summarise(total_sold = sum(quantity)) %>%

  ##Sort from highest quantity sold to lowest

  arrange(desc(total_sold))


print(head(top_products, 10))
View(top_products)

#Top product by revenue

top_products_by_revenue <- retail_clean %>%
  group_by(description) %>%
  summarise(
    total_revenue = sum(revenue),
    total_quantity = sum(quantity),
    n_orders = n_distinct(invoice),
    n_customers = n_distinct(customer_id),
    avg_price = mean(price),
    .groups = "drop"
  ) %>%
  arrange(desc(total_revenue)) %>%
  head(80)


print(top_products_by_revenue)
View(top_products_by_revenue)

##### Cancellation Rate
### Count invoices that start with C (cancellations), divide by total invoices, multiply by 100 to get a percentage
cancellation_summary <- retail %>%
  summarise(cancellation_rate = sum(grepl("^C", invoice)) / n() * 100)

View(cancellation_summary)
print(cancellation_summary)

#### Monthly Sales Trends

##### Group transactions by month (floor_date rounds each date to the first day of its month
Monthly_trends <- retail %>%
  group_by(month = floor_date(invoice_date, "month")) %>%

  summarise(total_revenue = sum(quantity * price), .groups = "drop") %>% #Add up the total revenue for each month

  arrange(desc(total_revenue)) # Sort from highest revenue month to lowest

cat("\nMONTHLY REVENUE TRENDS (Top 12 months):\n")
print(head(Monthly_trends, 12))
View(Monthly_trends)


#### Monthly Top Products (What sold best each month)
###This shows the best-selling products for each month
#Top products by month

# Group by both month AND product description
monthly_top_products <- retail %>%
  group_by(month = floor_date(invoice_date, "month"), description) %>%

  ## For each month-product combination, calculate total sold, revenue, and count unique customers
  summarise(
    total_sold = sum(quantity),
    total_revenue = sum(quantity * price),
    n_customers = n_distinct(customer_id),
    .groups = "drop_last"
  ) %>%

  arrange(month, desc(total_revenue)) %>% #Sort by month, then by highest revenue within each month

  #For each month, keep only the top 10 products
  group_by(month) %>%
  slice_head(n = 10)%>%  # Top 10 products per month
  ungroup()

View(monthly_top_products)


#######Monthly Trends with Product Breakdown
######This shows the monthly trend for EACH product



# Monthly trend per product

## Group by product and month
monthly_product_trends <- retail %>%
  group_by(description, month = floor_date(invoice_date, "month")) %>%

  ## Calculate total sold, revenue, and customer count for each product-month
  summarise(
    total_sold = sum(quantity),
    total_revenue = sum(quantity * price),
    n_customers = n_distinct(customer_id),
    .groups = "drop"
  ) %>%

  #ungroup() %>% #Remove the grouping (good practice)
  arrange(description, month) # Sort by product name, then by month

View(monthly_product_trends)


#top_3_products
top_3_products <- retail %>%
  group_by(description) %>%
  summarise(total_revenue = sum(quantity * price), .groups = "drop") %>%
  arrange(desc(total_revenue)) %>%
  slice_head(n = 3) %>%
  pull(description)

View(top_3_products)


####Monthly Revenue by Product
####This shows months as columns and products as rows


library(tidyr)

# Monthly Revenue Trends - Top 3 Products (wide format)
retail %>%
  mutate(month = floor_date(invoice_date, "month")) %>%
  filter(description %in% top_3_products) %>%
  group_by(month, description) %>%
  summarise(total_revenue = sum(quantity * price), .groups = "drop") %>%
  ggplot(aes(x = month, y = total_revenue, color = description)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Monthly Revenue Trends - Top 3 Products",
    x = "Month",
    y = "Total Revenue (£)",
    color = "Product"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")



##### SECTION 5:PRODUCT-CUSTOMER ANALYSIS Using Retails ############

####This shows which customers bought which products in each month


# Monthly product-customer summary

##Group by month, product, and customer
monthly_product_customer <- retail %>%
  mutate(month = floor_date(invoice_date, "month")) %>%
  group_by(month, description, customer_id) %>%

## Calculate quantity, spending, and orders for each combination
  summarise(
    quantity_bought = sum(quantity),
    total_spent = sum(quantity * price),
    orders = n_distinct(invoice)
  ) %>%

## Remove grouping, sort by month then highest spending
  ungroup() %>%
  arrange(month, desc(total_spent))

View(monthly_product_customer)


##### PRODUCT-CUSTOMER ANALYSIS Using Retail_clean ############

##### Monthly Trends with Customer Segmentation
# Monthly revenue by customer segment
##Run this code before your monthly segment trends code


top_products_with_customers <- retail_clean %>%
  group_by(description) %>%
  summarise(
    total_sold = sum(quantity),
    total_revenue = sum(revenue),
    n_customers = n_distinct(customer_id),
    .groups = "drop"
  ) %>%
  mutate(avg_per_customer = total_revenue / n_customers) %>%
  arrange(desc(total_sold))
View(top_products_with_customers)




product_customer <- retail_clean %>%
  group_by(description, customer_id) %>%
  summarise(
    quantity_bought = sum(quantity),
    total_spent = sum(revenue),
    .groups = "drop"
  ) %>%
  arrange(desc(quantity_bought))

View(product_customer)


top_customers_per_product <- retail_clean %>%
  group_by(description, customer_id) %>%
  summarise(
    quantity_bought = sum(quantity),
    total_spent = sum(revenue),
    .groups = "drop_last"
  ) %>%
  arrange(description, desc(total_spent)) %>%
  group_by(description) %>%
  slice_head(n = 5) %>%
  ungroup()

View(top_customers_per_product)


product_customer_summary <- retail_clean %>%
  group_by(description, customer_id) %>%
  summarise(
    quantity_bought = sum(quantity),
    total_spent = sum(revenue),
    orders = n_distinct(invoice),
    .groups = "drop"
  ) %>%
  mutate(avg_order_value = total_spent / orders) %>%
  arrange(desc(total_spent))

View(product_customer_summary)

########## SECTION 6:CUSTOMER ANALYSIS ######



top_customers <- retail_clean %>%
  group_by(customer_id) %>%
  summarise(total_spent = sum(revenue), .groups = "drop") %>%
  arrange(desc(total_spent))

View(top_customers)

avg_order_value <- retail_clean %>%
  group_by(customer_id) %>%
  summarise(avg_order = mean(revenue), .groups = "drop")

View(avg_order_value)


##Group all cleaned transactions by customer ID
customers <- retail_clean %>%
  group_by(customer_id) %>%
  summarise(
    first_purchase = min(invoice_date),#Find the date of their first purchase

    last_purchase  = max(invoice_date),#Find the date of their last purchase

    n_orders       = n_distinct(invoice), #Count how many unique orders they made

    n_items        = sum(quantity), #Count total items they bought

    total_revenue  = sum(revenue), #Calculate total money they spent

    avg_order_value = total_revenue / n_orders, #Divide total revenue by number of orders to get average order value

## Calculate how many days between first and last purchase (how long they've been a customer
    tenure_days    = as.numeric(difftime(max(invoice_date), min(invoice_date), units = "days")),

##Get their country (take the first one, all should be the same)
    country        = first(country),

.groups = "drop"
  ) %>%
  mutate(avg_order_value = total_revenue / n_orders)

View(customers)



# Customer Segmentation
# THEN create revenue segments

customers1 <- customers %>%
  mutate(
    revenue_segment = case_when(

    # If total revenue is in the top 10%, label as VIP
      total_revenue >= quantile(total_revenue, 0.9) ~ "Top 10% (VIP)",

    #If total revenue is in the top 50% (but not top 10%), label as Regular
      total_revenue >= quantile(total_revenue, 0.5) ~ "top 10% to 50% range (Regular)",

    # If total revenue is in the top 75% (but not top 50%), label as Low
      total_revenue >= quantile(total_revenue, 0.25) ~ "50% to 75% (Low)",

    #Everyone else (bottom 25%) label as Very Low
      TRUE ~ "Bottom 25% (Very Low)"
    ),


    frequency_segment = case_when(
      n_orders >= quantile(n_orders, 0.9) ~ "Very Frequent",
      n_orders >= quantile(n_orders, 0.5) ~ "Moderate",
      TRUE ~ "Infrequent"
    ),


    recency_days = as.numeric(difftime(max(retail_clean$invoice_date), last_purchase, units = "days")),
    recency_segment = case_when(
      recency_days <= 30  ~ "Active (0-30 days)",
      recency_days <= 90  ~ "Recent (31-90 days)",
      recency_days <= 180 ~ "Dormant (91-180 days)",
      TRUE ~ "Inactive (180+ days)"
    )
  )

View(customers1)



segment_summary <- customers1 %>%
  group_by(revenue_segment) %>%
  summarise(
    count = n(),
    avg_revenue = mean(total_revenue),
    avg_orders = mean(n_orders),
    .groups = "drop"
  )
print(segment_summary)

ggplot(customers1, aes(x = total_revenue)) +
  geom_histogram(fill = "#008000", bins = 50, alpha = 0.7) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Distribution of Customer Revenue",
    subtitle = "Most customers are in the lower revenue range",
    x = "Total Revenue (£)",
    y = "Number of Customers"
  ) +
  theme_minimal()

################
customers1 %>%
  group_by(revenue_segment) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ggplot(aes(x = "", y = count, fill = revenue_segment)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  labs(
    title = "Customer Revenue Segments",
    subtitle = "Top 10% of customers drive the most revenue"
  ) +
  theme_void() +
  theme(legend.position = "bottom")

cat("\n✅ Customer analysis complete!\n")


#############SECTION 7:RFM ANALYSIS #################



customers_rfm <- customers1 %>%
  mutate(
    recency_score   = ntile(-recency_days, 5),
    frequency_score = ntile(n_orders, 5),
    monetary_score  = ntile(total_revenue, 5),
    rfm_score = recency_score + frequency_score + monetary_score,
    rfm_segment = case_when(
      recency_score >= 4 & frequency_score >= 4 & monetary_score >= 4 ~ "Champions (Best Customers)",
      recency_score >= 4 & frequency_score >= 3 & monetary_score >= 3 ~ "Loyal Customers",
      recency_score >= 4 & frequency_score <= 2 & monetary_score <= 2 ~ "New Customers",
      recency_score <= 2 & frequency_score >= 4 & monetary_score >= 4 ~ "At Risk (Big Spenders)",
      recency_score <= 2 & frequency_score >= 3 & monetary_score >= 3 ~ "Need Attention",
      recency_score <= 2 & frequency_score <= 2 & monetary_score <= 2 ~ "Lost Customers",
      recency_score >= 3 & frequency_score <= 2 & monetary_score <= 2 ~ "Promising",
      TRUE ~ "Others"
    )
  )


View(customers_rfm)

############

rfm_summary <- customers_rfm %>%
  group_by(rfm_segment) %>%
  summarise(
    count = n(),
    avg_revenue = mean(total_revenue),
    avg_orders = mean(n_orders),
    avg_recency_days = mean(recency_days),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_revenue))


print(rfm_summary)

#########

customers_rfm %>%
  group_by(rfm_segment) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ggplot(aes(x = reorder(rfm_segment, count), y = count, fill = rfm_segment)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(
    title = "RFM Customer Segments",
    subtitle = "Champions = High recency + High frequency + High spending",
    x = "RFM Segment",
    y = "Number of Customers"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


########SECTION 8:RETENTION & CHURN RISK ANALYSIS #####################


cat("\n=== RETENTION & CHURN RISK ANALYSIS ===\n")

lifetime_stats <- customers1 %>%
  summarise(
    avg_tenure_days = mean(tenure_days),
    median_tenure_days = median(tenure_days),
    min_tenure = min(tenure_days),
    max_tenure = max(tenure_days)
  )

print(lifetime_stats)
View(lifetime_stats)


ggplot(customers, aes(x = tenure_days)) +
  geom_histogram(fill = "#F18F01", bins = 50, alpha = 0.7) +
  labs(
    title = "Customer Tenure Distribution",
    subtitle = "How long have customers been with us?",
    x = "Tenure (Days)",
    y = "Number of Customers"
  ) +
  theme_minimal()


tenure_summary <- customers1 %>%
  mutate(
    tenure_segment = case_when(
      tenure_days <= 30  ~ "New (0-30 days)",
      tenure_days <= 90  ~ "Recent (31-90 days)",
      tenure_days <= 180 ~ "Regular (91-180 days)",
      tenure_days <= 365 ~ "Loyal (181-365 days)",
      TRUE ~ "VIP (365+ days)"
    )
  ) %>%
  group_by(tenure_segment) %>%
  summarise(
    count = n(),
    avg_revenue = mean(total_revenue),
    avg_orders = mean(n_orders),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_revenue))


print(tenure_summary)


ggplot(customers1, aes(x = tenure_days, y = total_revenue)) +
  geom_point(alpha = 0.4, color = "#008000") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Revenue vs Customer Tenure",
    subtitle = "Longer-tenured customers tend to spend more",
    x = "Tenure (Days)",
    y = "Total Revenue (£)"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()


customers2 <- customers1 %>%
  mutate(
    days_since_last = as.numeric(difftime(max(retail_clean$invoice_date), last_purchase, units = "days")),
    churn_risk = case_when(
      days_since_last > 180 ~ "High Risk (Inactive > 6 months)",
      days_since_last > 90  ~ "Medium Risk (Inactive 3-6 months)",
      days_since_last > 30  ~ "Low Risk (Inactive 1-3 months)",
      TRUE ~ "Active (Purchased last 30 days)"
    )
  )

churn_summary <- customers2 %>%
  group_by(churn_risk) %>%
  summarise(
    count = n(),
    avg_revenue = mean(total_revenue),
    avg_orders = mean(n_orders),
    .groups = "drop"
  ) %>%
  arrange(desc(count))

print(churn_summary)

customers2 %>%
  group_by(churn_risk) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ggplot(aes(x = reorder(churn_risk, -count), y = count, fill = churn_risk)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            vjust = -0.5, size = 4) +
  labs(
    title = "Customer Churn Risk Analysis",
    subtitle = "Customers at high risk need re-engagement campaigns",
    x = "Churn Risk Category",
    y = "Number of Customers"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


######## SECTION 9: CUSTOMER CHURN PREDICTION #####



churn_data <- retail_clean %>%

  group_by(customer_id) %>%

  summarise(
    n_orders = n_distinct(invoice),

    total_revenue = sum(revenue),
    n_items = sum(quantity),
    avg_order_value = mean(revenue),

    recency_days = as.numeric(difftime(max(retail_clean$invoice_date), max(invoice_date), units = "days")),

    tenure_days = as.numeric(difftime(max(invoice_date), min(invoice_date), units = "days")),

    product_diversity = n_distinct(description),
    avg_price = mean(price),
    last_purchase_date = max(invoice_date),
    .groups = "drop"
  )

churn_data <- churn_data %>%
  mutate(
    churn = ifelse(last_purchase_date < as.Date("2011-10-01"), 1, 0)
  )

churn_rate <- mean(churn_data$churn) * 100
cat("\nOverall churn rate:", round(churn_rate, 2), "%\n")

features_for_model <- churn_data %>%
  select(
    n_orders, total_revenue, n_items, avg_order_value,
    recency_days, tenure_days, product_diversity, avg_price, churn
  ) %>%
  filter(complete.cases(.))



set.seed(123)
train_index <- createDataPartition(features_for_model$churn, p = 0.7, list = FALSE)

train_data <- features_for_model[train_index, ]
test_data  <- features_for_model[-train_index, ]

X_train <- train_data %>% select(-churn)
y_train <- train_data$churn
X_test  <- test_data %>% select(-churn)
y_test  <- test_data$churn


####################
# LOGISTIC REGRESSION (baseline)####

logistic_model <- glm(churn ~ ., data = train_data, family = binomial())

logistic_pred_prob  <- predict(logistic_model, newdata = X_test, type = "response")
logistic_pred_class <- ifelse(logistic_pred_prob > 0.5, 1, 0)

#Explicit factor levels avoid a crash if a class is missing from the test split
conf_matrix_log <- confusionMatrix(
  factor(logistic_pred_class, levels = c(0, 1)),
  factor(y_test, levels = c(0, 1))
)

roc_logistic <- roc(y_test, logistic_pred_prob)


##### Pareto Analysis (80/20 Rule)######

pareto <- customers %>%
  arrange(desc(total_revenue)) %>%
  mutate(
    cum_revenue = cumsum(total_revenue),
    cum_pct = cum_revenue / sum(total_revenue) * 100,
    cum_customers = row_number() / n() * 100
  )



######### RANDOM FOREST ########

set.seed(123)
rf_model <- randomForest(
  as.factor(churn) ~ .,
  data = train_data,
  ntree = 500,
  mtry = 3,
  importance = TRUE
)

importance_rf <- importance(rf_model)
print(importance_rf)
varImpPlot(rf_model, main = "Feature Importance for Churn Prediction")

rf_pred_prob  <- predict(rf_model, newdata = X_test, type = "prob")[, 2]
rf_pred_class <- predict(rf_model, newdata = X_test, type = "response")

conf_matrix_rf <- confusionMatrix(
  factor(rf_pred_class, levels = c(0, 1)),
  factor(y_test, levels = c(0, 1))
)
cat("\nRandom Forest Accuracy:", round(conf_matrix_rf$overall["Accuracy"] * 100, 2), "%\n")

roc_rf <- roc(y_test, rf_pred_prob)

###### COMPARE MODELS #######

model_comparison <- data.frame(
  Model = c("Logistic Regression", "Random Forest"),
  Accuracy = c(conf_matrix_log$overall["Accuracy"], conf_matrix_rf$overall["Accuracy"]),
  AUC = c(auc(roc_logistic), auc(roc_rf))
)

print(model_comparison)


###### HIGH-RISK CUSTOMERS #######

churn_predictions <- churn_data %>%
  mutate(
    churn_probability = predict(rf_model, newdata = churn_data, type = "prob")[, 2],
    predicted_churn = ifelse(churn_probability > 0.5, "High Risk", "Low Risk")
  ) %>%
  arrange(desc(churn_probability))

high_risk_customers <- churn_predictions %>%
  filter(churn_probability > 0.6) %>%
  select(customer_id, churn_probability, n_orders, total_revenue, last_purchase_date)


print(head(high_risk_customers, 10))
View(high_risk_customers)

write.csv(high_risk_customers, "outputs/high_risk_churn_customers.csv", row.names = FALSE)
write.csv(churn_predictions, "outputs/churn_predictions_all.csv", row.names = FALSE)


#########SECTION 10: CUSTOMER LIFETIME VALUE (CLV) ###########


clv_data <- retail_clean %>%
  group_by(customer_id) %>%
  summarise(
    frequency = n_distinct(invoice) - 1,
    recency = as.numeric(difftime(max(invoice_date), min(invoice_date), units = "days")),
    T_cal = as.numeric(difftime(max(retail_clean$invoice_date), min(invoice_date), units = "days")),
    avg_order_value = mean(revenue, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(frequency >= 0) %>%
  filter(T_cal > 0) %>%
  filter(!is.na(avg_order_value), avg_order_value > 0)

View(clv_data)

clv_matrix <- as.matrix(clv_data[, c("frequency", "recency", "T_cal")])
colnames(clv_matrix) <- c("x", "t.x", "T.cal")
str(clv_matrix)   # or head(clv_matrix)
colnames(clv_matrix)

clv_params <- tryCatch({
  params <- bgnbd.EstimateParameters(clv_matrix)
  names(params) <- c("r", "alpha", "s", "beta")
  cat("\nBG/NBD Model Parameters:\n")
  print(params)
  params
}, error = function(e) {

  fallback <- c(r = 0.5, alpha = 2, s = 0.5, beta = 3)
  print(fallback)
  fallback
})

future_period <- 365

clv_data <- clv_data %>%
  mutate(
    expected_purchases = bgnbd.ConditionalExpectedTransactions(
      clv_params,
      T.cal = T_cal,
      t.x = recency,
      x = frequency,
      T.star = future_period
    )
  ) %>%
  mutate(
    expected_purchases = ifelse(is.na(expected_purchases) | is.infinite(expected_purchases),
                                0, expected_purchases)
  )

profit_margin <- 0.4
discount_rate <- 0.10

clv_data <- clv_data %>%
  mutate(
    predicted_revenue_next_year = avg_order_value * expected_purchases,
    predicted_profit_next_year = predicted_revenue_next_year * profit_margin,
    clv = predicted_profit_next_year / (1 + discount_rate)
  ) %>%
  mutate(
    clv = ifelse(is.na(clv) | is.infinite(clv), 0, clv)
  )


View(clv_data)

clv_summary <- clv_data %>%
  summarise(
    total_customers = n(),
    avg_clv = mean(clv),
    median_clv = median(clv),
    total_clv = sum(clv),
    min_clv = min(clv),
    max_clv = max(clv)
  )

print(clv_summary)

top_clv_customers <- clv_data %>%
  arrange(desc(clv)) %>%
  head(10)


print(top_clv_customers)

ggplot(clv_data, aes(x = clv)) +
  geom_histogram(bins = 50, fill = "#2E86AB", alpha = 0.7) +
  scale_x_continuous(labels = comma) +
  labs(
    title = "Distribution of Customer Lifetime Value (CLV)",
    subtitle = paste("Mean CLV: £", round(mean(clv_data$clv), 2)),
    x = "Customer Lifetime Value (£)",
    y = "Number of Customers"
  ) +
  theme_minimal()

clv_with_segments <- clv_data %>%
  left_join(customers1 %>% select(customer_id, revenue_segment), by = "customer_id")

clv_by_segment <- clv_with_segments %>%
  group_by(revenue_segment) %>%
  summarise(
    count = n(),
    avg_clv = mean(clv, na.rm = TRUE),
    median_clv = median(clv, na.rm = TRUE),
    total_clv = sum(clv, na.rm = TRUE),
    min_clv = min(clv,na.rm = TRUE),
    max_clv = max(clvna.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_clv))

print(clv_by_segment)

clv_data %>%
  mutate(
    current_revenue = avg_order_value * (frequency + 1)
  ) %>%
  ggplot(aes(x = current_revenue, y = clv)) +
  geom_point(alpha = 0.5, color = "#A23B72") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Current Revenue vs Predicted CLV",
    subtitle = "Points above the line have higher future potential",
    x = "Historical Revenue (£)",
    y = "Predicted CLV (£)"
  ) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

write.csv(clv_data, "customer_lifetime_value.csv", row.names = FALSE)
write.csv(clv_by_segment, "clv_by_segment.csv", row.names = FALSE)



#SECTION 11: FINAL REPORT & EXPORT #############################


dashboard <- list(
  "Total Customers" = nrow(customers),
  "Total Revenue" = sum(customers$total_revenue),
  "Average Revenue per Customer" = mean(customers$total_revenue),
  "Median Revenue per Customer" = median(customers$total_revenue),
  "Top 10 Customers" = customers %>%
    arrange(desc(total_revenue)) %>%
    select(customer_id, total_revenue, n_orders, country) %>%
    head(10),
  "Revenue by Country" = customers %>%
    group_by(country) %>%
    summarise(
      customers = n(),
      total_revenue = sum(total_revenue),
      avg_revenue = mean(total_revenue),
      .groups = "drop"
    ) %>%
    arrange(desc(total_revenue)) %>%
    head(10),
  "RFM Segment Summary" = rfm_summary,
  "Churn Risk Summary" = churn_summary,
  "Top Products" = top_products %>% head(20),
  "CLV Summary" = clv_summary,
  "CLV by Segment" = clv_by_segment
)


all_results <- list(
  "Overview" = data.frame(
    Metric = c("Total Customers", "Total Revenue", "Avg Revenue", "Median Revenue"),
    Value = c(
      dashboard[["Total Customers"]],
      dashboard[["Total Revenue"]],
      round(dashboard[["Average Revenue per Customer"]], 2),
      round(dashboard[["Median Revenue per Customer"]], 2)
    )
  ),
  "Customer_Master" = customers,
  "RFM_Scores" = customers_rfm,
  "Top_Customers" = customers %>% arrange(desc(total_revenue)) %>% head(100),
  "Revenue_by_Country" = customers %>%
    group_by(country) %>%
    summarise(
      customers = n(),
      total_revenue = sum(total_revenue),
      avg_revenue = mean(total_revenue),
      .groups = "drop"
    ) %>%
    arrange(desc(total_revenue)),
  "RFM_Segments" = rfm_summary,
  "Churn_Risk" = churn_summary,
  "Top_Products" = top_products %>% head(50),
  "CLV_Summary" = clv_data,
  "High_Risk_Churn" = high_risk_customers
)

write_xlsx(all_results, "retail_analysis_complete_dashboard.xlsx")

# These two were promised in the checklist but never actually written
# as standalone CSVs in the original code — added here.
write.csv(top_products %>% head(100), "top_products.csv", row.names = FALSE)
write.csv(customers %>% arrange(desc(total_revenue)) %>% head(100), "top_customers.csv", row.names = FALSE)



if (Sys.info()["sysname"] == "Windows") {
  shell.exec(getwd())
} else if (Sys.info()["sysname"] == "Darwin") {
  system(paste("open", getwd()))
} else {
  system(paste("xdg-open", getwd()))
}


# ==============================================================================
# SAVE VISUALIZATIONS


### 1. Data cleaning comparison ###
ggsave("visualizations/01_data_cleaning_comparison.png",
       plot = ggplot(plot_data, aes(x = Dataset, y = Rows, fill = Dataset)) +
         geom_bar(stat = "identity") +
         geom_text(aes(label = scales::comma(Rows)), vjust = -0.5, size = 5, fontface = "bold") +
         labs(title = "Comparison of Retail Datasets",
              subtitle = paste(scales::comma(nrow(retail_with_customers) - nrow(retail_clean)), "rows removed"),
              y = "Number of Rows", x = "Dataset") +
         theme_minimal() +
         theme(legend.position = "none",
               plot.title = element_text(hjust = 0.5, face = "bold"),
               plot.subtitle = element_text(hjust = 0.5)),
       width = 8, height = 6, dpi = 300)

# 2. Monthly revenue trends - top 3 products
ggsave("visualizations/02_monthly_revenue_top3_products.png",
       plot = retail %>%
         mutate(month = floor_date(invoice_date, "month")) %>%
         filter(description %in% top_3_products) %>%
         group_by(month, description) %>%
         summarise(total_revenue = sum(quantity * price), .groups = "drop") %>%
         ggplot(aes(x = month, y = total_revenue, color = description)) +
         geom_line(linewidth = 1.2) +
         labs(title = "Monthly Revenue Trends - Top 3 Products",
              x = "Month", y = "Total Revenue (£)", color = "Product") +
         theme_minimal() +
         theme(legend.position = "bottom"),
       width = 10, height = 6, dpi = 300)

# 3. Customer revenue distribution
ggsave("visualizations/03_customer_revenue_distribution.png",
       plot = ggplot(customers1, aes(x = total_revenue)) +
         geom_histogram(fill = "#008000", bins = 50, alpha = 0.7) +
         scale_x_continuous(labels = comma) +
         labs(title = "Distribution of Customer Revenue",
              subtitle = "Most customers are in the lower revenue range",
              x = "Total Revenue (£)", y = "Number of Customers") +
         theme_minimal(),
       width = 8, height = 6, dpi = 300)

# 4. Customer revenue segments pie chart
ggsave("visualizations/04_revenue_segments_pie.png",
       plot = customers1 %>%
         group_by(revenue_segment) %>%
         summarise(count = n(), .groups = "drop") %>%
         mutate(percentage = count / sum(count) * 100) %>%
         ggplot(aes(x = "", y = count, fill = revenue_segment)) +
         geom_bar(stat = "identity", width = 1) +
         coord_polar("y", start = 0) +
         geom_text(aes(label = paste0(round(percentage, 1), "%")),
                   position = position_stack(vjust = 0.5)) +
         labs(title = "Customer Revenue Segments",
              subtitle = "Top 10% of customers drive the most revenue") +
         theme_void() +
         theme(legend.position = "bottom"),
       width = 8, height = 6, dpi = 300)

# 5. RFM customer segments
ggsave("visualizations/05_rfm_segments.png",
       plot = customers_rfm %>%
         group_by(rfm_segment) %>%
         summarise(count = n(), .groups = "drop") %>%
         mutate(percentage = count / sum(count) * 100) %>%
         ggplot(aes(x = reorder(rfm_segment, count), y = count, fill = rfm_segment)) +
         geom_bar(stat = "identity") +
         geom_text(aes(label = paste0(round(percentage, 1), "%")),
                   hjust = -0.1, size = 3.5) +
         coord_flip() +
         labs(title = "RFM Customer Segments",
              subtitle = "Champions = High recency + High frequency + High spending",
              x = "RFM Segment", y = "Number of Customers") +
         theme_minimal() +
         theme(legend.position = "none"),
       width = 9, height = 6, dpi = 300)

# 6. Customer tenure distribution
ggsave("visualizations/06_tenure_distribution.png",
       plot = ggplot(customers, aes(x = tenure_days)) +
         geom_histogram(fill = "#F18F01", bins = 50, alpha = 0.7) +
         labs(title = "Customer Tenure Distribution",
              subtitle = "How long have customers been with us?",
              x = "Tenure (Days)", y = "Number of Customers") +
         theme_minimal(),
       width = 8, height = 6, dpi = 300)

# 7. Revenue vs tenure
ggsave("visualizations/07_revenue_vs_tenure.png",
       plot = ggplot(customers1, aes(x = tenure_days, y = total_revenue)) +
         geom_point(alpha = 0.4, color = "#008000") +
         geom_smooth(method = "lm", se = TRUE, color = "red") +
         labs(title = "Revenue vs Customer Tenure",
              subtitle = "Longer-tenured customers tend to spend more",
              x = "Tenure (Days)", y = "Total Revenue (£)") +
         scale_y_continuous(labels = comma) +
         theme_minimal(),
       width = 8, height = 6, dpi = 300)

# 8. Churn risk analysis
ggsave("visualizations/08_churn_risk.png",
       plot = customers2 %>%
         group_by(churn_risk) %>%
         summarise(count = n(), .groups = "drop") %>%
         mutate(percentage = count / sum(count) * 100) %>%
         ggplot(aes(x = reorder(churn_risk, -count), y = count, fill = churn_risk)) +
         geom_bar(stat = "identity") +
         geom_text(aes(label = paste0(round(percentage, 1), "%")),
                   vjust = -0.5, size = 4) +
         labs(title = "Customer Churn Risk Analysis",
              subtitle = "Customers at high risk need re-engagement campaigns",
              x = "Churn Risk Category", y = "Number of Customers") +
         theme_minimal() +
         theme(legend.position = "none"),
       width = 8, height = 6, dpi = 300)

# 9. Random Forest feature importance (base R plot -> png())
png("visualizations/09_rf_feature_importance.png", width = 900, height = 700, res = 120)
varImpPlot(rf_model, main = "Feature Importance for Churn Prediction")
dev.off()

# 10. CLV distribution
ggsave("visualizations/10_clv_distribution.png",
       plot = ggplot(clv_data, aes(x = clv)) +
         geom_histogram(bins = 50, fill = "#2E86AB", alpha = 0.7) +
         scale_x_continuous(labels = comma) +
         labs(title = "Distribution of Customer Lifetime Value (CLV)",
              subtitle = paste("Mean CLV: £", round(mean(clv_data$clv), 2)),
              x = "Customer Lifetime Value (£)", y = "Number of Customers") +
         theme_minimal(),
       width = 8, height = 6, dpi = 300)

# 11. Current revenue vs predicted CLV
ggsave("visualizations/11_current_revenue_vs_clv.png",
       plot = clv_data %>%
         mutate(current_revenue = avg_order_value * (frequency + 1)) %>%
         ggplot(aes(x = current_revenue, y = clv)) +
         geom_point(alpha = 0.5, color = "#A23B72") +
         geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
         labs(title = "Current Revenue vs Predicted CLV",
              subtitle = "Points above the line have higher future potential",
              x = "Historical Revenue (£)", y = "Predicted CLV (£)") +
         scale_x_continuous(labels = comma) +
         scale_y_continuous(labels = comma) +
         theme_minimal(),
       width = 8, height = 6, dpi = 300)


