
#  Predicting 30‑Day Hospital Readmission  
### Using the UCI Diabetes 130‑US Hospitals Dataset

##  Overview  
Hospital readmissions within 30 days are a major challenge for healthcare systems, increasing costs and signaling potential gaps in care quality. This project builds machine learning models to predict whether a patient will be readmitted within 30 days using the **UCI Diabetes 130‑US Hospitals dataset**, which contains over **100,000 inpatient encounters** with demographic, diagnostic, medication, and utilization variables.

The project includes:  
- Full preprocessing pipeline in R  
- Feature engineering (ICD‑9 grouping, dummy encoding, transformations)  
- Linear and nonlinear classification models  
- Model comparison using Kappa, specificity, and balanced accuracy  
- Identification of key predictors influencing readmission  

---

##  Dataset Description  
The dataset includes **87,126 observations** and **126 predictors** after preprocessing. It contains:

### **Response Variable**
- **Readmitted**: Binary indicator (“Yes” / “No”) for 30‑day hospital readmission.

### **Predictor Categories**
- **Demographics**: age, gender, race  
- **Hospital Utilization**: time in hospital, number of inpatient/outpatient/emergency visits  
- **Clinical Conditions**: diagnoses (ICD‑9), A1C results, glucose serum levels  
- **Medications**: insulin, diabetesMed, medication changes, number of medications  
- **Admission/Discharge**: admission type, source, discharge disposition  

> The report notes: *“The dataset consists of 87,126 observations and 126 predictor variables after preprocessing.”* 

---

## Preprocessing Pipeline  
A comprehensive preprocessing workflow was implemented to prepare the dataset for modeling.

### **1. Handling Missing Values**
- “?” converted to `NA`  
- Categorical: imputed with **Unknown**  
- Numeric: imputed with **median**  
> *“Missing values were replaced with a new category labeled Unknown… numeric values were imputed using the median.”* 

### **2. ICD‑9 Diagnosis Grouping**
High‑cardinality diagnosis codes were grouped into clinically meaningful categories:
- Circulatory  
- Respiratory  
- Digestive  
- Diabetes  
- Injury  
- Musculoskeletal  
- Genitourinary  
- Other/Unknown  

### **3. Dummy Encoding & Degenerate Predictor Removal**
- One‑hot encoding expanded predictors to 126 variables  
- Removed:
  - single‑level variables  
  - near‑zero variance predictors  
  - high‑cardinality degenerate variables  

### **4. Scaling & Transformation**
- Centering and scaling applied to all numeric predictors  
- Box‑Cox and spatial sign transformations used to reduce skewness and outliers  
> *“Several numeric variables were right‑skewed… Box‑Cox and spatial sign transformations were applied.”* 

---

## Train/Test Split  
Because the dataset is **highly imbalanced** (≈11.4% readmitted), a careful splitting strategy was used:

- **Stratified sampling** to preserve class proportions  
- **Group‑aware split** using patient ID to prevent data leakage  
- **70% training / 30% testing**  
> *“Splitting was performed using a group‑aware strategy based on the patient identifier… to prevent data leakage.”* 

---

##  Models Trained  
Both **linear** and **nonlinear** classification models were evaluated.

### **Linear Models**
- Logistic Regression  
- Elastic Net  
- Partial Least Squares (PLS)  
- Linear Discriminant Analysis (LDA)  
- Naïve Bayes  

### **Nonlinear Models**
- K‑Nearest Neighbors (KNN)  
- Random Forest  
- Decision Tree (CART)  
- Neural Network  

All models were tuned using **3‑fold cross‑validation**.

---

##  Model Performance Summary  

###  **Best Overall Model: Linear Discriminant Analysis (LDA)**  
LDA achieved:
- **Highest Kappa (0.0753)**  
- **Highest balanced accuracy**  
- **Best specificity among all models**  

> *“The best overall model was Linear Discriminant Analysis (LDA)… highest Kappa value (0.0753).”* 

### Key Observations
- Many models achieved high accuracy (~0.886) due to class imbalance.  
- Most nonlinear models predicted all cases as “No readmission,” resulting in **zero specificity**.  
- KNN performed best among nonlinear models but still underperformed compared to LDA.  
- Naïve Bayes performed poorly due to independence assumption violations.  

---

## Important Predictors  
The report identifies several predictors strongly associated with readmission risk, including:

- Number of diagnoses  
- Time in hospital  
- Prior inpatient/outpatient/emergency visits  
- A1C results  
- Diagnosis categories (ICD‑9 groups)  

These variables consistently appeared in top‑ranked importance lists across models.

---

##  Technologies Used  
- **R** (caret, tidyverse, MASS, glmnet, nnet, randomForest)  
- **Statistical Modeling**: Logistic Regression, LDA, PLS  
- **Machine Learning**: KNN, Random Forest, CART, Neural Networks  
- **Visualization**: ggplot2, heatmaps, correlation matrices  

---

##  Repository Structure  
```
.
├── data/                 # Raw and processed datasets
├── src/                  # R scripts for preprocessing and modeling
├── reports/              # Final project report (PDF)
├── plots/                # EDA and model performance visualizations
├── images/               # Figures used in the report
└── README.md             # Project documentation
```

---

##  Authors  
**Beven Mpofu** (bmpofu@mtu.edu)  
**Sibonginkosi Trust Nkashe** (stnkashe@mtu.edu)  
