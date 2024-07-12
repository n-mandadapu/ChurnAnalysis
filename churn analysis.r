---
title: "Churn Analysis"
author: "Nanda kishore"
date: "2023-12-03"
output:
  html_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
### AIM:
Business Goals

Churn refers to customers who have exited within the last month, significantly impacting both sales figures and overall business revenue. The root cause of this phenomenon often lies in customer satisfaction levels. Therefore, the business objectives should focus on minimizing customer churn to boost revenue and enhancing overall customer satisfaction.

To effectively address customer churn, it becomes imperative to predict which customers are at a higher risk of churning by identifying early indicators across various sales channels.

*The key challenge is to predict if an individual customer will churn or not.*
*The extra challenge is to identify the key components of churning.*


### questions for exploration include:

- Are there discernible patterns among churned customers?
- What is the percentage distribution between churned and existing customers?
- How are contract types distributed among customers?
- Which variables exhibit correlation with customer churn?
- Conversely, which variables do not display correlation with customer churn?
- What model proves most accurate in predicting churn?
- How are contract types distributed within the customer base?
- What impact does contract length have on customer behavior?
- Is there a relationship between customer usage patterns, additional products, and churn?
- How does the variety of services a customer utilizes influence churn?


### About Dataset
#### Context
"Predict behavior to retain customers. You can analyze all relevant customer data and develop focused customer retention programs." [IBM Sample Data Sets]

#### Content
Each row represents a customer, each column contains customer’s attributes described on the column Metadata.

#### The data set includes information about:

- Customers who left within the last month – the column is called Churn
- Services that each customer has signed up for – phone, multiple lines, internet, online security, online backup, device protection, tech support, and streaming TV and movies
- Customer account information – how long they’ve been a customer, contract, payment method, paperless billing, monthly charges, and total charges
- Demographic info about customers – gender, age range, and if they have partners and dependents

#### Dataset Attributes

- customerID - Customer ID
- gender - Whether the customer is a male or a female
- SeniorCitizen - Whether the customer is a senior citizen (1, 0)
- Partner - Whether the customer has a partner (Yes, No)
- Dependents - Whether the customer has dependents (Yes, No)
- tenure - Number of months the customer has stayed with the company
- PhoneService - Whether the customer has a phone service (Yes, No)
- MultipleLines - Whether the customer has multiple lines (Yes, No, No phone service)
- InternetService - Customer’s internet service provider (DSL, Fiber optic, No)
- OnlineSecurity - Whether the customer has online security (Yes, No, No internet service)
- OnlineBackup - Whether the customer has online backup or not (Yes, No, No internet service)
- DeviceProtection - Whether the customer has device protection (Yes, No, No internet service)
- TechSupport - Whether the customer has tech support (Yes, No, No internet service)
- StreamingTV - Whether the customer has streaming TV service (Yes, No, No internet service)
- StreamingMovies - Whether the customer has streaming movies service (Yes, No, No internet service)
- Contract - Indicates the type of the contract (Month-to-month, One year, Two years)
- PaperlessBilling - Whether the customer has paperless billing (Yes, No)
- PaymentMethod - Indicates the payment method (Electronic check, Mailed check, Bank transfer (automatic), Credit card (automatic))
- MonthlyCharges - Indicates the current monthly subscription cost of the customer
- TotalCharges - Indicates the total charges paid by the customer so far
- Churn - Indicates whether the customer churned

reference [kaggle](https://www.kaggle.com/datasets/blastchar/telco-customer-churn/)

### Loading Dataset 

```{r}

file_path <- "C:/Users/manda/OneDrive - Western Michigan University/Desktop/wmu/fall 2023/applied data minnig/project/WA_Fn-UseC_-Telco-Customer-Churn"

# Read the CSV file into a data frame
setwd("C:/Users/manda/OneDrive - Western Michigan University/Desktop/wmu/fall 2023/applied data minnig/project/")
df <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")

# View the structure of the data frame
str(df)

#Data Exploration

summary(df)
```

### pre processing the data

- before everything else we remove the customer id in order for better analysis 

```{r}
df <- df[, !(names(df) %in% c("customerID"))]
```

#### missing values 
```{r}

# Check for missing values in each column
missing_values <- colSums(is.na(df))

# Display the results
print(missing_values)
# 11 missing values in Total charges 

# Impute missing values in 'TotalCharges' with the mean
df$TotalCharges[is.na(df$TotalCharges)] <- mean(df$TotalCharges, na.rm = TRUE)

#or 

df$TotalCharges[is.na(df$TotalCharges)] <- 0

# Check for missing values in each column
missing_values <- colSums(is.na(df))

# Display the results
print(missing_values)
```
- no missing values 

#### duplicate values 

```{r}
# Check for duplicated rows in the data frame
num_duplicated <- sum(duplicated(df))

# Print the number of duplicated values
print(paste("Number of duplicated values in the dataset: ", num_duplicated))

df <- unique(df)

# Check for duplicated rows in the data frame
num_duplicated <- sum(duplicated(df))

# Print the number of duplicated values
print(paste("Number of duplicated values in the dataset: ", num_duplicated))
```

```{r}
columns <- names(df)

numeric_columns <- character(0)
binary_columns <- character(0)
categorical_columns <- character(0)

for (col in columns) {
  unique_vals <- unique(df[[col]])
  
  if (length(unique_vals) > 10) {
    numeric_columns <- c(numeric_columns, col)
  } else if (length(unique_vals) == 2) {
    binary_columns <- c(binary_columns, col)
  } else {
    categorical_columns <- c(categorical_columns, col)
  }
}

# Print the categorization
cat("Numeric Columns:", numeric_columns, "\n")
cat("Binary Columns:", binary_columns, "\n")
cat("Categorical Columns:", categorical_columns, "\n")

```

```{r}

# Copy the data frame
df1 <- df

# Identify categorical columns
categoric_columns <- c("gender", "Partner", "Dependents", "PhoneService", 
                        "MultipleLines", "InternetService", "OnlineSecurity", 
                        "OnlineBackup", "DeviceProtection", "TechSupport", 
                        "StreamingTV", "StreamingMovies", "Contract", 
                        "PaperlessBilling", "PaymentMethod")

# Convert categorical columns to factors
df1[categoric_columns] <- lapply(df1[categoric_columns], factor)

# Convert 'Churn' column to factor
df1$Churn <- factor(df1$Churn, levels = c("No", "Yes"))

# Print the first few rows of the transformed data frame
head(df1)


```



```{r}

summary(df[, numeric_columns])

```


```{r}
library(ggplot2)

# Data imbalance check
churn_counts <- table(df$Churn)
pie_values <- prop.table(churn_counts) * 100

# Plotting
par(mfrow = c(1, 2), mar = c(5, 5, 2, 2))

# Pie chart
pie(pie_values, labels = c('Not-Churn Customers', 'Churn Customers'), 
    col = c('#1F78B4', '#33A02C'), main = 'Churn and Not-Churn Customers %', 
    cex.main = 1, cex.lab = 1)

# Bar chart
bar_colors <- c('#1F78B4', '#33A02C')
barplot(churn_counts, names.arg = c('Not-Churn Customers', 'Churn Customers'),
        col = bar_colors, main = 'Churn and Not-Churn Customers',
        cex.main = 1, cex.lab = 1, border = 'black')

# Adding percentage labels to the pie chart
text(0, 0, paste0(sprintf("%.2f", pie_values[1]), "%"), col = 'white', cex = 1.5)
text(0, 0, 'Not-Churn Customers', col = 'black', cex = 1, pos = 4)

text(1, 0, paste0(sprintf("%.2f", pie_values[2]), "%"), col = 'white', cex = 1.5)
text(1, 0, 'Churn Customers', col = 'black', cex = 1, pos = 4)

```





```{r}
# Install the e1071 package if not already installed
if (!requireNamespace("e1071", quietly = TRUE)) {
  install.packages("e1071")
}

# Load the e1071 package
library(e1071)

# Assuming df1 is your data frame

dist_custom <- function(dataset, columns_list, rows, cols, suptitle) {
  par(mfrow = c(rows, cols), mar = c(5, 5, 2, 2))
  cat("\n")
  cat(suptitle, "\n", sep = "")
  
  for (i in seq_along(columns_list)) {
    col <- columns_list[i]
    
    # Density plot
    plot(density(dataset[[col]]), main = paste(col, ", skewness is ", round(e1071::skewness(dataset[[col]], na.rm = TRUE), 2)),
         col = '#008080', lwd = 2, ylim = c(0, max(density(dataset[[col]])$y) * 1.1),
         xlab = col, ylab = 'Density')
  }
}

# Usage
dist_custom(dataset = df, columns_list = numeric_columns, rows = 1, cols = 3, suptitle = 'Distribution for each numerical feature')


```




```{r}
# Assuming df1 is your data frame

boxplots_custom <- function(dataset, columns_list, rows, cols, suptitle) {
  par(mfrow = c(rows, cols), mar = c(5, 5, 2, 2))
  cat("\n")
  cat(suptitle, "\n", sep = "")
  
  for (i in seq_along(columns_list)) {
    col <- columns_list[i]
    
    # Boxplot
    boxplot(dataset[[col]], horizontal = TRUE, main = paste(col, ", skewness is: ", round(e1071::skewness(dataset[[col]], na.rm = TRUE), 2)),
            col = '#008080', border = 'black', axes = FALSE, boxwex = 0.5)
    
    # Add labels and grid
    axis(1, at = seq(min(dataset[[col]]), max(dataset[[col]]), length = 5), las = 2)
    grid()
  }
}


boxplots_custom(dataset = df1, columns_list = numeric_columns, rows = 1, cols = 3, suptitle = 'Boxplots for numerical features')

```
- TotalCharges is rightly skewed.


```{r}

detect_outliers_iqr <- function(data) {
  # Calculate the first quartile (Q1) and third quartile (Q3)
  Q1 <- quantile(data, 0.25, na.rm = TRUE)
  Q3 <- quantile(data, 0.75, na.rm = TRUE)
  
  # Calculate the IQR
  IQR <- Q3 - Q1
  
  # Define the lower and upper bounds to identify outliers
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Find the outliers
  outliers <- data[data < lower_bound | data > upper_bound]
  
  return(outliers)
}

# Apply outlier detection to all numerical columns
outliers_by_column <- list()

for (column in numeric_columns) {
  data_column <- df1[[column]]
  outliers <- detect_outliers_iqr(data_column)
  outliers_by_column[[column]] <- outliers
}

# Print the outliers for each column
for (column in names(outliers_by_column)) {
  cat("Outliers in", column, ":", outliers_by_column[[column]], "\n")
}

```

```{r}
#install.packages("ggplot2")

library(ggplot2)

# Monthly Charges vs Churn
ggplot(df1, aes(x = MonthlyCharges, fill = factor(Churn))) +
  geom_density(alpha = 0.7, color = 'black') +
  scale_fill_manual(values = c('#008080', '#FF6347')) +
  labs(title = 'Distribution of Monthly Charges by Churn', x = 'Monthly Charges', y = 'Density') +
  theme_minimal()

# Total Charges vs Churn
ggplot(df1, aes(x = TotalCharges, fill = factor(Churn))) +
  geom_density(alpha = 0.7, color = 'black') +
  scale_fill_manual(values = c('#008080', '#FF6347')) +
  labs(title = 'Distribution of Total Charges by Churn', x = 'Total Charges', y = 'Density') +
  theme_minimal()

# tenure vs Churn
ggplot(df1, aes(x = tenure, fill = factor(Churn))) +
  geom_density(alpha = 0.7, color = 'black') +
  scale_fill_manual(values = c('#008080', '#FF6347')) +
  labs(title = 'Distribution of tenure by Churn', x = 'tenure', y = 'Density') +
  theme_minimal()

```
- The longer the customer has been with the provider the more likely he will not churn.


```{r}
# Assuming df is your data frame

# Convert 'Contract' to a factor with appropriate levels
df$Contract <- factor(df$Contract, levels = c("Month-to-month", "One year", "Two year"))

# Calculate mean tenures for different contract types
two_year_mean <- round(mean(df$tenure[df$Contract == "Two year"], na.rm = TRUE), 2)
month_mean <- round(mean(df$tenure[df$Contract == "Month-to-month"], na.rm = TRUE), 2)
year_mean <- round(mean(df$tenure[df$Contract == "One year"], na.rm = TRUE), 2)

# Create histograms for different contract types
par(mfrow = c(1, 3), mar = c(5, 5, 2, 2), xpd = TRUE)

# Function to handle potential issues in plotting histogram
plot_hist <- function(data, contract_type, mean_value) {
  if (length(data) > 0) {
    hist(data, main = paste(contract_type, "\nChurn\nMean:", mean_value),
         xlab = "Tenure", ylab = "No. of Customers", col = "#008080", breaks = 72,
         xlim = c(0, max(data, na.rm = TRUE)), ylim = c(0, 1500))
  } else {
    cat("No data for", contract_type, "\n")
  }
}

# Plot histograms
plot_hist(df$tenure[df$Contract == "Month-to-month"], "Month to Month Contract", month_mean)
plot_hist(df$tenure[df$Contract == "One year"], "One Year Contract", year_mean)
plot_hist(df$tenure[df$Contract == "Two year"], "Two Year Contract", two_year_mean)


```

- A significant number of customers churned within the first month of their subscription.
- A considerable portion of customers has remained with the provider for a prolonged period of 72 weeks.
- There is a positive correlation between the length of the contract and the customer's tenure, suggesting that customers tend to stay longer with the provider when they have longer-term contracts. This is indicated by a higher mean tenure for customers with longer contracts.


```{r}

df$tenure <- as.numeric(df$tenure)
# Create a categorical variable for tenure groups
df$tenure.group <- cut(df$tenure, breaks = c(0, 12, 24, 48, Inf),
                       labels = c("0-12 months", "12-24 months", "24-48 months", "Over 48 months"))

# Load required libraries
library(ggplot2)

# Create a count plot
ggplot(df, aes(x = tenure.group, fill = Churn)) +
  geom_bar(position = "dodge", color = "black", alpha = 0.7) +
  labs(title = "Count Plot of tenure group by Churn") +
  xlab("Tenure Group") +
  ylab("Count") +
  scale_fill_manual(values = c("#008080", "#FF6347")) +
  theme_minimal()


```

- The analysis gives a bigger picture. HIgher the tenure group, higher chances of retention and lesser chances of churn.

```{r}
# Assuming df is your data frame

library(ggplot2)

# Define the color palette
palette2 <- c('#008080', '#FF6347')

# Scatterplot for TotalCharges vs tenure
ggplot(df, aes(x = TotalCharges, y = tenure, color = Churn)) +
  geom_point(size = 3, alpha = 0.8, shape = 1, stroke = 0.5) +
  scale_color_manual(values = palette2) +
  labs(title = 'TotalCharges vs Tenure')

# Scatterplot for TotalCharges vs MonthlyCharges
ggplot(df, aes(x = TotalCharges, y = MonthlyCharges, color = Churn)) +
  geom_point(size = 3, alpha = 0.8, shape = 1, stroke = 0.5) +
  scale_color_manual(values = palette2) +
  labs(title = 'TotalCharges vs MonthlyCharges')

# Scatterplot for MonthlyCharges vs tenure
ggplot(df, aes(x = tenure, y = MonthlyCharges, color = Churn)) +
  geom_point(size = 3, alpha = 0.8, shape = 1, stroke = 0.5) +
  scale_color_manual(values = palette2) +
  labs(title = 'MonthlyCharges vs Tenure')

```

- Many customers leave after just one month.
- A significant number of customers stay with the provider for 72 weeks.
- Customers tend to stay longer if they have a contract, especially those with higher mean scores.
- Customers with higher Monthly Charges are more likely to leave the service.
- The longer a customer has been with the provider, the less likely they are to leave


- Major customers who moved out were having Electronic Check as Payment Method.
- Customers who opted for Credit-Card automatic transfer or Bank Automatic Transfer and Mailed Check as Payment Method were less likely to move out.

```{r}
# Plotting Senior Citizen Distribution

library(tidyverse)
library(magrittr)

# Convert SeniorCitizen to factor
df$SeniorCitizen <- factor(df$SeniorCitizen)
color_map <- c("1" = '#00CC96', "0" = '#B6E880')
df %>%
  ggplot(aes(x = Churn, fill = SeniorCitizen)) +
  geom_bar(position = "dodge", color = "black", alpha = 0.7) +
  labs(title = "Churn distribution w.r.t. Senior Citizen") +
  xlab("Churn") +
  ylab("Count") +
  scale_fill_manual(values = color_map) +
  theme_minimal()

# Plotting Dependents Distribution
color_map <- c("Yes" = "#FF97FF", "No" = "#00CC96")
df %>%
  ggplot(aes(x = Churn, fill = Dependents)) +
  geom_bar(position = "dodge", color = "black", alpha = 0.7) +
  labs(title = "Dependents distribution") +
  xlab("Churn") +
  ylab("Count") +
  scale_fill_manual(values = color_map) +
  theme_minimal()


# Plotting Partner Distribution
color_map <- c("Yes" = "#FFA15A", "No" = "#00CC96")
df %>%
  ggplot(aes(x = Churn, fill = Partner)) +
  geom_bar(position = "dodge", color = "black", alpha = 0.7) +
  labs(title = "Churn distribution w.r.t. Partners") +
  xlab("Churn") +
  ylab("Count") +
  scale_fill_manual(values = color_map) +
  theme_minimal()



# Plotting Gender Distribution
color_map <- c("Male" = "#FFA15A", "Female" = "#00CC96")
df %>%
  ggplot(aes(x = Churn, fill = gender)) +
  geom_bar(position = "dodge", color = "black", alpha = 0.7) +
  labs(title = "Churn distribution w.r.t. Gender") +
  xlab("Churn") +
  ylab("Count") +
  scale_fill_manual(values = color_map) +
  theme_minimal()


```

Provided services
PhoneService, MultipleLines, InternetService, StreamingTV, StreamingMovies
```{r}
# List of features
list2 <- c('PhoneService', 'MultipleLines', 'InternetService', 'StreamingTV', 'StreamingMovies')

# Create a multi-plot layout
par(mfrow=c(2,3), mar=c(4,4,2,1))

# Plot each feature against Churn
for (i in 1:length(list2)) {
  # Create a count plot
  counts <- table(df[, list2[i]], df$Churn)
  barplot(counts, beside=TRUE, col=palette2, main=paste(list2[i], 'vs Churn'), legend.text=TRUE)
}


```




```{r}
# Support services
list3 <- c('OnlineSecurity', 'OnlineBackup', 'DeviceProtection', 'TechSupport')
palette <- c('#008080', '#FF6347', '#E50000', '#D2691E')

# Create a multi-plot layout
par(mfrow=c(2,2), mar=c(4,4,2,1))

# Plot each support service against Churn using the specified palette
for (i in 1:length(list3)) {
  # Create a count plot
  counts <- table(df[, list3[i]], df$Churn)
  barplot(counts, beside=TRUE, col=palette2, main=paste(list3[i], 'vs Churn'), legend.text=TRUE)
}



```

```{r}
# Specify the columns of interest
list3 <- c('Contract', 'PaperlessBilling', 'PaymentMethod')

# Create a multi-plot layout
par(mfrow=c(2,3), mar=c(4,4,2,1))

# Plot each column against Churn using the specified palette
for (i in 1:length(list3)) {
  # Create a count plot
  counts <- table(df[[list3[i]]], df$Churn)
  
  # Barplot with custom colors
  barplot(counts, beside=TRUE, col=palette2, main=paste(list3[i], 'vs Churn'))
  
  # Customize legend and x-axis labels for PaymentMethod
  if (list3[i] == 'PaymentMethod') {
    legend('topright', legend=c('No Churn', 'Churn'), fill=palette2)
    axis(1, at=1:4, labels=c('e-check', 'm-check', 'Bank transfer', 'Credit card'))
  } else {
    legend('topright', legend=c('No Churn', 'Churn'), fill=palette2)
  }
}

```

In simpler words:

1. Many customers opt for Fiber optic internet, but they tend to leave the service more often. This suggests they might not be happy with this type of internet.

2. Most customers use DSL service, and they tend to stay with the provider without leaving as much as Fiber optic users.

3. Customers without dependents are more likely to leave the service.

4. Customers without partners are more likely to leave the service.

5. There are very few senior citizens among the customers, but most of them leave the service.

6. Most customers leave if they don't have online security.

7. Customers with Paperless Billing are more likely to leave.

8. Customers without TechSupport are more likely to switch to another provider.

9. A very small number of customers don't have phone service, and among them, one-third are likely to leave the service.

10. The relationship between gender and the likelihood of churn is not significant. However, more interesting insights can be gained by exploring the features related to having a partner, dependents, and being a senior citizen.





```{r}
# Select Numeric Variables
numeric_df <- df %>%
  select_if(is.numeric)

# Calculate Correlation Matrix
corr_df <- cor(numeric_df)

# Correlation Plot
corrplot::corrplot(corr_df, method = "circle")

# Extract Correlation with the Target Variable (Assuming it's the first variable)
corr_target <- corr_df[1, -1]

# Sort Correlation Values
corr_sorted <- sort(corr_target, decreasing = TRUE)

# Select Top Correlated Variables (Adjust the threshold as needed)
top_corr_vars <- names(corr_sorted[abs(corr_sorted) > 0.1])

top_corr_vars

```



```{r}

# Select the columns to be used in the regression
columns <- c(
  "gender", "SeniorCitizen", "Partner", "Dependents", 
  "PhoneService", "MultipleLines", "InternetService", "OnlineSecurity",
  "OnlineBackup", "DeviceProtection", "TechSupport", "StreamingTV",
  "StreamingMovies", "Contract", "PaperlessBilling", "PaymentMethod",
   "Churn", "tenure.group"
)

# Create a new dataframe with only the selected columns
df_selected <- df[, columns]

df1 <- df[, columns]

# Convert specified columns to factors
df_selected[columns] <- lapply(df_selected[columns], as.factor)


df_selected$Churn <- as.factor(df_selected$Churn)
df$Churn <- as.factor(df$Churn)
df1$Churn <- as.factor(df_selected$Churn)
# Check levels of all categorical variables
sapply(df_selected, function(x) if (is.factor(x)) length(levels(x)) else NA)

# Perform multiple logistic regression to determine impactful factors
model <- glm(Churn ~ ., data = df_selected, family = "binomial")
# summary(model) # view summary of the model

# Obtain p-values for each variable in the model
p_values <- summary(model)$coefficients[, 4]

# Determine significant variables based on p-values less than 0.05
sig_vars <- names(p_values[p_values < 0.05])

sig_vars

```



```{r}
# Forward selection
full_model <- glm(Churn ~ ., family = binomial(link = 'logit'), data = df)
forward_model <- step(full_model, direction = 'forward')
summary(forward_model)
```
Certainly! Here's a brief explanation of the logistic regression model summary:

1. **Model Overview:**
   - **AIC:** The AIC value is a measure of model fit, and a lower AIC suggests a better-fitted model. The AIC for this model is 5826.1.
   - **Deviance:** The deviance is a measure of how well the model predicts the data. The residual deviance is 5772.1, indicating an improvement over the null deviance (8105.3).

2. **Coefficients:**
   - The coefficients represent the log-odds of the response variable (Churn).
   - Positive coefficients increase the log-odds of the event, while negative coefficients decrease it.

3. **Significant Predictors:**
   - **SeniorCitizen:** Senior citizens are more likely to churn (positive coefficient).
   - **Tenure:** Longer tenure reduces the likelihood of churn (negative coefficient).
   - **InternetService(Fiber optic):** Fiber optic service increases the likelihood of churn (positive coefficient).
   - **Contract (One year, Two years):** Longer contract terms reduce the likelihood of churn (negative coefficients).
   - **PaperlessBilling:** Paperless billing increases the likelihood of churn (positive coefficient).
   - **PaymentMethod(Electronic check):** Electronic check payment method increases the likelihood of churn (positive coefficient).
   - **TotalCharges:** Higher total charges increase the likelihood of churn (positive coefficient).

4. **Variables with No Internet Service:**
   - Several variables related to internet services have a category "No internet service," and their coefficients are marked as `NA`.

5. **Iterations:**
   - The model fitting process involved six iterations.

In summary, the model identifies key predictors influencing customer churn, considering factors like contract length, billing methods, and internet service types.


```{r}
# Backward selection
full_model <- glm(Churn ~ ., family = binomial(link = 'logit'), data = df)
backward_model <- step(full_model, direction = 'backward')
summary(backward_model)
```

In the backward logistic regression:

- **Step 1:**
  - Removed "Partner" variable.
  - The AIC reduced from 5826.13 to 5824.15.
  - The model without "Partner" is preferred.

- **Step 2:**
  - Removed "OnlineBackup" variable.
  - The AIC further reduced to 5822.16.
  - The model without "OnlineBackup" is preferred.

- **Step 3:**
  - Removed "gender" variable.
  - The AIC reduced to 5820.28.
  - The model without "gender" is preferred.

- **Step 4:**
  - Removed "DeviceProtection" variable.
  - The AIC reached 5820.21.
  - The model without "DeviceProtection" is preferred.

- **Final Model:**
  - The selected variables are:
    - SeniorCitizen
    - Dependents
    - Tenure
    - MultipleLines
    - InternetService
    - OnlineSecurity
    - TechSupport
    - StreamingTV
    - StreamingMovies
    - Contract
    - PaperlessBilling
    - PaymentMethod
    - MonthlyCharges
    - TotalCharges
    - Tenure group

- **Model Summary:**
  - The model's AIC is 5820.2, indicating a good fit.
  - Deviance dropped from 8105.3 (null model) to 5774.2.
  - Significant predictors include SeniorCitizen, Tenure, MultipleLines, InternetService, OnlineSecurity, TechSupport, StreamingTV, Contract, PaperlessBilling, PaymentMethod, MonthlyCharges, TotalCharges, and Tenure group.

- **Interpretation:**
  - Senior citizens are more likely to churn.
  - Longer tenure reduces the likelihood of churn.
  - Fiber optic internet service increases churn.
  - Having OnlineSecurity, TechSupport, and certain payment methods decreases churn.
  - PaperlessBilling and certain contract types increase churn.
  - MonthlyCharges and TotalCharges also impact churn.

This final model captures the essential predictors for customer churn in a concise form.

```{r}
# Stepwise selection
full_model <- glm(Churn ~ ., family = binomial(link = 'logit'), data = df)
stepwise_model <- step(full_model)
summary(stepwise_model)

```
The stepwise logistic regression results indicate the model building process. Here's a summary of the steps:

1. **Start:** The initial model includes all the variables listed.
   - AIC = 5826.13

2. **Step 1:**
   - Removed the "Partner" variable.
   - AIC reduced to 5824.15.
   - Variables removed: Partner
   - Remaining variables: gender, SeniorCitizen, Dependents, tenure, MultipleLines, InternetService, OnlineSecurity, OnlineBackup, DeviceProtection, TechSupport, StreamingTV, StreamingMovies, Contract, PaperlessBilling, PaymentMethod, MonthlyCharges, TotalCharges, tenure.group.

3. **Step 2:**
   - Removed the "OnlineBackup" variable.
   - AIC further reduced to 5822.16.
   - Variables removed: OnlineBackup
   - Remaining variables: gender, SeniorCitizen, Dependents, tenure, MultipleLines, InternetService, OnlineSecurity, DeviceProtection, TechSupport, StreamingTV, StreamingMovies, Contract, PaperlessBilling, PaymentMethod, MonthlyCharges, TotalCharges, tenure.group.

4. **Step 3:**
   - Removed the "gender" variable.
   - AIC reduced to 5820.28.
   - Variables removed: gender
   - Remaining variables: SeniorCitizen, Dependents, tenure, MultipleLines, InternetService, OnlineSecurity, DeviceProtection, TechSupport, StreamingTV, StreamingMovies, Contract, PaperlessBilling, PaymentMethod, MonthlyCharges, TotalCharges, tenure.group.

5. **Step 4:**
   - Removed the "DeviceProtection" variable.
   - AIC reached 5820.21.
   - Variables removed: DeviceProtection
   - Remaining variables: SeniorCitizen, Dependents, tenure, MultipleLines, InternetService, OnlineSecurity, TechSupport, StreamingTV, StreamingMovies, Contract, PaperlessBilling, PaymentMethod, MonthlyCharges, TotalCharges, tenure.group.

6. **Final Model:**
   - The final selected model includes the remaining variables after step 4.
   - AIC = 5820.2

**Interpretation of the Final Model:**
   - The final model includes significant predictors such as SeniorCitizen, tenure, MultipleLines, InternetService, OnlineSecurity, TechSupport, StreamingTV, Contract, PaperlessBilling, PaymentMethod, MonthlyCharges, TotalCharges, and tenure.group.

   - Each coefficient's estimate, standard error, z-value, and p-value are provided for interpretation.

   - The model provides insights into the factors influencing customer churn based on the selected variables.

Final Model Summary:


Predictors in the Model:

SeniorCitizen: Being a senior citizen increases the odds of churn.
Dependents: Having dependents decreases the odds of churn.
Tenure: Longer tenure decreases the odds of churn.
MultipleLines: Having multiple lines increases the odds of churn.
InternetService: Having fiber optic internet service increases the odds of churn.
OnlineSecurity: Lack of online security increases the odds of churn.
TechSupport: Lack of tech support increases the odds of churn.
StreamingTV: Having streaming TV increases the odds of churn.
Contract: Shorter contract terms (especially month-to-month) increase the odds of churn.
PaperlessBilling: Having paperless billing increases the odds of churn.
PaymentMethod: Using electronic check for payment increases the odds of churn.
MonthlyCharges: Higher monthly charges increase the odds of churn.
TotalCharges: Higher total charges decrease the odds of churn.
Tenure.Group: Specific groups (12-24 months, Over 48 months) impact churn odds.


Model Evaluation:

The model is built based on the stepwise selection process, optimizing the AIC.
AIC (Akaike Information Criterion) is a measure of the model's goodness of fit, penalizing for complexity.
Lower AIC indicates a better-fitting model.
Practical Insights:
Customer Profiles:

Older customers are more likely to churn.
Customers with dependents tend to be more loyal.
Long-tenured customers are less likely to churn.
Services Impact:

Having additional services like multiple lines, streaming TV, and fiber optic internet may increase churn.
Lack of online security and tech support contributes to higher churn.
Contract and Billing:

Shorter contract terms (month-to-month) and paperless billing are associated with higher churn.
Payment method, especially electronic checks, influences customer retention.
Financial Factors:

Monthly charges impact churn, with higher charges leading to increased churn.
Total charges, however, have a stabilizing effect on customer retention.
Recommendations:
Retention Strategies:

Target promotions or discounts to senior citizens to encourage loyalty.
Enhance services like online security and tech support.
Incentivize longer-term contracts and non-electronic payment methods.
Address concerns related to multiple lines and streaming services.
Communication Strategies:

Tailor communication and promotions based on customer tenure.
Educate customers about the benefits of additional services to improve retention.
Financial Management:

Monitor and potentially revise pricing strategies, especially for high-churn services.
Consider promotions or loyalty programs for long-tenured customers.
Remember, these recommendations are based on statistical associations, and the actual business strategies may need to consider additional factors and nuances specific to your industry and customer base.

