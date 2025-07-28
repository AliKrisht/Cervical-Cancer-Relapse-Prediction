# Predicting Cervical Cancer Relapse in Adults After Treatment

This project analyzes patient data to identify factors contributing to the relapse of cervical cancer using logistic regression and model validation techniques.

## 📁 Project Structure
- `Case Study Code.R`: Full R Markdown code for data cleaning, EDA, and model building.
- `CaseStudy.pdf`: Full case study report.
- `Case_Study_Presentation.pdf`: Presentation slides summarizing the project.

## 🧪 Methods
- Data Cleaning & Transformation
- Exploratory Data Analysis (EDA)
- Variable Significance (via LRT)
- Multicollinearity Check
- Logistic Regression (GLM)
- Model Selection (Stepwise AIC)
- Model Validation (Hosmer-Lemeshow, ROC, Classification Table)

## 🔍 Key Findings
- Final model includes: Age, Disease Status, Max Tumor Depth, Tumor Size, and interaction between MaxDepth:Size
- AUC = 90.8%
- Sensitivity = 58%, Specificity = 99.3%
- Model fits well (p = 0.2768)

## 📊 Tools Used
- R (tidyverse, ggplot2, VGAM, pROC, ResourceSelection)
- Excel for raw data import

## 📄 License
MIT License
