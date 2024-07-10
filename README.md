# Churn Analysis Project

This project performs a logistic regression analysis to determine the factors that impact customer churn. The dataset used contains information about customers, their demographics, and their service usage.

## Table of Contents
- [Introduction](#introduction)
- [Dataset](#dataset)
- [Installation](#installation)
- [Usage](#usage)
- [Analysis](#analysis)
- [Results](#results)
- [Contributing](#contributing)
- [License](#license)

## Introduction

Customer churn is a critical issue for many businesses, especially in subscription-based industries. Understanding the factors that influence churn can help companies take proactive measures to retain customers. This project uses logistic regression to identify significant predictors of churn.

## Dataset

The dataset used in this analysis includes the following features:
- `gender`
- `SeniorCitizen`
- `Partner`
- `Dependents`
- `tenure`
- `PhoneService`
- `MultipleLines`
- `InternetService`
- `OnlineSecurity`
- `OnlineBackup`
- `DeviceProtection`
- `TechSupport`
- `StreamingTV`
- `StreamingMovies`
- `Contract`
- `PaperlessBilling`
- `PaymentMethod`
- `MonthlyCharges`
- `TotalCharges`
- `Churn`
- `tenure.group`

## Installation

1. Clone the repository:
    ```bash
    git clone https://github.com/n-mandadapu/ChurnAnalysis.git
    cd churn-analysis
    ```

2. Install the required packages:
    ```R
    install.packages(c("ggplot2", "dplyr"))
    ```

## Usage

1. Load the dataset:
    ```R
    df <- read.csv("path/to/your/dataset.csv")
    ```

2. Clean and preprocess the data:
    ```R
    # Drop rows with missing or inappropriate values
    df <- df[complete.cases(df$SeniorCitizen, df$Dependents, df$tenure, df$MultipleLines,
                             df$InternetService, df$OnlineSecurity, df$TechSupport,
                             df$StreamingTV, df$StreamingMovies, df$Contract,
                             df$PaperlessBilling, df$MonthlyCharges, df$TotalCharges, df$tenure.group), ]
    
    # Recode 'Churn' variable to factor
    df$Churn <- factor(df$Churn, levels = c("No", "Yes"))
    ```

3. Perform logistic regression:
    ```R
    model <- glm(Churn ~ SeniorCitizen + Dependents + tenure + MultipleLines +
                   InternetService + OnlineSecurity + TechSupport + StreamingTV +
                   StreamingMovies + Contract + PaperlessBilling + MonthlyCharges + TotalCharges + tenure.group,
                 family = binomial(link = 'logit'), data = df)
    
    # View the summary
    summary(model)
    ```

## Analysis

The logistic regression analysis helps identify the significant predictors of churn. The model is trained on the cleaned dataset, and the results are summarized to highlight which factors are most impactful.

## Results

The results of the logistic regression analysis are presented in the summary of the model. The significant variables (p-values < 0.05) are considered to be the most impactful factors affecting customer churn.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License.
