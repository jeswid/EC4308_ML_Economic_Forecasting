# EC4308 Predicting Bull and Bear Markets with Machine Learning  

## Project Overview  

This repository contains the codes and methodologies for predicting stock market states (bull or bear markets) using **machine learning (ML) models**. The study compares ML methods with traditional econometric models to evaluate their predictive accuracy and explore their potential to detect nonlinear interactions in financial data.  

---

## Table of Contents  

- [Background](#background)  
- [Methodology](#methodology)  
- [Files](#files)  
- [Installation](#installation)  
- [Contributors](#contributors)  

---

## Background  

Forecasting bull and bear markets is crucial for investment strategies and risk management. Traditional econometric models, such as logit regressions, assume linear relationships and often struggle to capture the complex dynamics of financial markets. Machine learning models provide an opportunity to enhance prediction accuracy by identifying nonlinear patterns and interactions.  

### Objectives:  

1. Compare the performance of ML models (e.g., LASSO, Random Forest, Gradient Boosting Machines) with traditional logit regression models.  
2. Use **profit-based portfolio strategies** to evaluate the practical utility of market state predictions.  
3. Identify key economic and financial predictors across different forecast horizons (short, medium, and long-term).  

---

## Methodology  

### Data Preprocessing  

- **Source**:  
  - **Robert Shiller's Dataset**: Includes US stock prices, earnings, dividends, and interest rates.  
  - **Amit Goyal's Dataset**: Provides macroeconomic indicators such as unemployment rates and industrial production growth.  

- **Transformations**:  
  - Stationarity ensured via differencing and log transformations.  
  - Lags constructed to capture time dependencies for predictors and the dependent variable.  

- **Market State Classification**:  
  - Applied the Bry and Boschan algorithm with a two-sided moving average to identify bull and bear market cycles.  

### Models  

#### Traditional Model  
- **Logit Regression**: Baseline benchmark for predicting market states.  

#### Machine Learning Models  
- **LASSO Logit**: Regression with L1 regularization for feature selection.  
- **Tree-Based Models**:  
  - Random Forest  
  - Gradient Boosting Machines (GBM)  
  - Extreme Gradient Boosting (XGBoost)  

### Evaluation Metrics  
- **F1 Score**: Evaluates model classification performance for imbalanced market state data.  
- **Profit-Based Portfolio Strategy**: Assesses cumulative returns by applying market predictions to an investment strategy.  

### Profit-Based Portfolio Strategy  
1. **Bear Market Prediction**: Allocate investments to risk-free assets (e.g., Treasury bills).  
2. **Bull Market Prediction**: Allocate investments to stocks.  
3. Compare cumulative returns using thresholds (e.g., 0.5 and sample averages) across all models and horizons.  

---

## Files  

- `data/`: Preprocessed datasets, including predictors and market state classifications.  
- `src/`: Scripts for traditional and machine learning models, as well as performance metrics calculations and portfolio strategy implementation.  
- `visualisations/`: Graphs detailing the cumulative returns from portfolio strategy implementation using each ML model.    
- `README.md`: This file.  

---

## Installation  

1. Clone this repository:  
    ```bash  
    git clone https://github.com/jeswid/EC4308_ML_Economic_Forecasting.git  
    ```  
2. Navigate to the repository directory:  
    ```bash  
    cd EC4308_ML_Economic_Forecasting  
    ```  
3. Install dependencies:  
    ```bash  
    pip install -r requirements.txt  
    ```  

---

## Contributors  

1. Jessica Widyawati - [jessicawidyawati@gmail.com](mailto:jessicawidyawati@gmail.com)  
2. Kelly Wang Tingyu 
3. Jonathan Choo Yi En
4. Toh Kai Lin  
5. Yang Shu Ting  

---

This project showcases how machine learning models can enhance financial forecasting by identifying complex patterns in market behavior. Visit the [GitHub repository](https://github.com/jeswid/EC4308_ML_Economic_Forecasting) for more details!  
