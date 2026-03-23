


## =========================================================
## 0. Libraries and seed
## =========================================================
library(dplyr)
library(stringr)
library(caret)
library(ggplot2)
library(tidyr)
library(corrplot)
library(Amelia)
library(patchwork)
library(e1071)
library(corrplot)
library(rcompanion)
set.seed(123)


## =========================================================
## 1. Load data and initial inspection
## =========================================================
df_raw <- read.csv("diabetic_data.csv", stringsAsFactors = FALSE)
df_raw
str(df_raw)

# Check duplicates
df_raw %>%
  count(duplicated(.))
df_raw$patient_nbr

pred <- df_raw %>%
  select(-encounter_id, -weight, -payer_code) %>%
  filter(!is.na(readmitted))

# Standardize missing markers
df_missing <- pred
df_missing[df_missing == "?"] <- NA

# Missingness summary plot (on raw with NA)
missing_summary <- colSums(is.na(df_missing)) / nrow(df_missing)

data.frame(
  variable = names(missing_summary),
  missing_rate = missing_summary
) %>%
  ggplot(aes(x = reorder(variable, missing_rate), y = missing_rate)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Missingness Rate per Variable",
    x = "Variable",
    y = "Proportion Missing"
  )
missmap(df_missing, main = "Missing Data Heatmap", col = c("red", "grey"))

# Identify numeric columns for outlier check and transformation
numeric_cols <- c(
  "time_in_hospital", "num_lab_procedures", "num_procedures",
  "num_medications", "number_outpatient", "number_emergency",
  "number_inpatient", "number_diagnoses"
)

# Identify categorical columns
cat_cols <- names(df_missing)[sapply(df_missing, is.character)]

# Replace missing values in categorical variables
df_missing[cat_cols] <- lapply(df_missing[cat_cols], function(x) {
  x[is.na(x)] <- "Unknown"
  x
})

# ---------------------------------------------------------
# Median imputation for numeric columns
# ---------------------------------------------------------
for (col in numeric_cols) {
  if (col %in% names(df_missing)) {
    med_val <- median(df_missing[[col]], na.rm = TRUE)
    df_missing[[col]][is.na(df_missing[[col]])] <- med_val
  }
}

df_missing


# Missingness heatmap after imputation
missmap(df_missing, main = "Missing Data Heatmap (After Imputation)",
        col = c("red", "grey"))

#Converting target to binary 
df <- df_missing %>%
  mutate(
    readmitted_30 = ifelse(readmitted == "<30", 1L, 0L),
    readmitted_30 = factor(readmitted_30, levels = c(0, 1))
  )
df

#Drop non-predictive / leakage variables/degenerate variables and target variable


cols_to_drop <- c( "readmitted")
cols_to_drop <- cols_to_drop[cols_to_drop %in% names(df)]

df <- df %>% select(-all_of(cols_to_drop))
always_no_cols <- c("examide", "citoglipton")
always_no_cols <- always_no_cols[always_no_cols %in% names(df)]
df <- df %>% select(-all_of(always_no_cols))
df

#Group ICD-9 diagnosis codes

group_icd <- function(x) {
  x_num <- suppressWarnings(as.numeric(substr(x, 1, 3)))
  case_when(
    is.na(x_num) ~ "Unknown",
    x_num >= 390 & x_num <= 459 ~ "Circulatory",
    x_num >= 460 & x_num <= 519 ~ "Respiratory",
    x_num >= 520 & x_num <= 579 ~ "Digestive",
    x_num == 250 ~ "Diabetes",
    x_num >= 800 & x_num <= 999 ~ "Injury",
    x_num >= 710 & x_num <= 739 ~ "Musculoskeletal",
    x_num >= 580 & x_num <= 629 ~ "Genitourinary",
    TRUE ~ "Other"
  )
}

diag_vars <- c("diag_1", "diag_2", "diag_3")
diag_vars <- diag_vars[diag_vars %in% names(df)]

for (d in diag_vars) df[[d]] <- group_icd(df[[d]])

df %>%
  pivot_longer(cols = all_of(diag_vars), names_to = "diag", values_to = "category") %>%
  ggplot(aes(category)) +
  geom_bar(fill = "blue") +
  facet_wrap(~ diag) +
  theme_minimal() +
  labs(title = "Diagnosis Category Distribution")

##Convert categorical variables to factors


cat_vars <- c(
  "race", "gender", "age", "weight",
  "payer_code", "medical_specialty",
  "max_glu_serum", "A1Cresult",
  diag_vars,
  "metformin", "repaglinide", "nateglinide", "chlorpropamide",
  "glimepiride", "acetohexamide", "glipizide", "glyburide",
  "tolbutamide", "pioglitazone", "rosiglitazone", "acarbose",
  "miglitol", "troglitazone", "tolazamide",
  "insulin", "glyburide.metformin", "glipizide.metformin",
  "glimepiride.pioglitazone", "metformin.rosiglitazone",
  "metformin.pioglitazone",
  "change", "diabetesMed",
  "admission_type_id", "discharge_disposition_id", "admission_source_id"
)

cat_vars <- intersect(cat_vars, names(df))

df[cat_vars] <- lapply(df[cat_vars], function(x) {
  if (!is.factor(x)) factor(x) else x
})
df$readmitted_30 <- factor(df$readmitted_30, levels = c(0, 1))
df

df_long <- df %>%
  pivot_longer(
    cols = all_of(cat_vars),
    names_to = "Variable",
    values_to = "Category"
  )

# Faceted barplots (histogram equivalent for categorical data)
ggplot(df_long, aes(x = Category)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  facet_wrap(~ Variable, scales = "free_x") +
  labs(
    title = "Frequency Distributions of Categorical Variables",
    x = "Category",
    y = "Count"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 10)
  )
df

# Function to detect degenerate categorical variables
check_cat_degenerate <- function(df, freq_threshold = 0.95, cardinality_threshold = 50) {
  
  results <- lapply(df, function(x) {
    x <- factor(x)
    tab <- table(x)
    prop <- tab / sum(tab)
    
    list(
      single_level = length(tab) == 1,
      near_zero_var = max(prop) > freq_threshold,
      high_cardinality = length(tab) > cardinality_threshold
    )
  })
  
  out <- do.call(rbind, lapply(results, as.data.frame))
  rownames(out) <- names(df)
  out
}

# Subset categorical columns
df_cat <- df[cat_vars]
df_cat

# Detect degeneracy
deg_cat <- check_cat_degenerate(df_cat)

# Identify degenerate categorical variables
degenerate_cols <- rownames(deg_cat)[
  deg_cat$single_level |
    deg_cat$near_zero_var |
    deg_cat$high_cardinality
]

degenerate_cols

# Keep only clean categorical variables
cat_vars_clean <- setdiff(cat_vars, degenerate_cols)

#  combine vectors before selecting 
df_clean <- df %>% select(all_of(c(cat_vars_clean, numeric_cols)))
df_clean
# Encode remaining categorical variables as factors
df_clean[cat_vars_clean] <- lapply(df_clean[cat_vars_clean], factor)

# One-hot encode categorical variables
onehot_mat <- model.matrix(~ . - 1, data = df_clean[cat_vars_clean])
onehot_df <- as.data.frame(onehot_mat)

# Bind numeric variables + encoded categorical variables
df_encoded <- cbind(
  df_clean %>% select(-all_of(cat_vars_clean)),
  onehot_df
)


df_encoded <- cbind(df_encoded,readmitted_30 = df$readmitted_30)
names(df_encoded)

#Numerical predictor distribution


numeric_cols <- c(
  "time_in_hospital", "num_lab_procedures", "num_procedures",
  "num_medications", "number_outpatient", "number_emergency",
  "number_inpatient", "number_diagnoses"
)

df_long <- df_encoded %>% 
  select(all_of(numeric_cols)) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  )

# Histogram panel
p_hist <- ggplot(df_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Distribution of Numeric Predictors",
    x = "Value",
    y = "Frequency"
  )
p_hist


# Boxplot panel
p_box <- ggplot(df_long, aes(x = variable, y = value)) +
  geom_boxplot(fill = "tomato", alpha = 0.7, outlier.alpha = 0.4) +
  coord_flip() +
  theme_minimal(base_size = 13) +
  labs(
    title = "Boxplots of Numeric Predictors",
    x = "",
    y = "Value"
  )

# Combine vertically
p_box


# Extract numeric data
df_num <- df_encoded %>% select(all_of(numeric_cols))


# Skewness table (type=2 is commonly used in practice)
skew_tbl <- df_num %>%
  summarise(across(everything(), ~ e1071::skewness(.x, na.rm = TRUE, type = 2))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "skewness") %>%
  arrange(desc(abs(skewness)))

skew_tbl



ggplot(skew_tbl, aes(x = reorder(variable, skewness), y = skewness)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Skewness of Numeric Predictors (Before Transformation)",
       x = "Variable", y = "Skewness")


# Spatial sign transformation
spatial_trans <- spatialSign(df_num)

df_spatial <- as.data.frame(spatial_trans)
df_spatial$obs_id <- 1:nrow(df_spatial)

# Long format for plotting
df_spatial_long <- df_spatial %>%
  pivot_longer(
    cols = all_of(numeric_cols),
    names_to = "variable",
    values_to = "value"
  )

# Boxplots after spatial sign transformation
ggplot(df_spatial_long, aes(x = variable, y = value)) +
  geom_boxplot(fill = "tomato", alpha = 0.7, outlier.alpha = 0.4) +
  coord_flip() +
  theme_minimal(base_size = 13) +
  labs(
    title = "Boxplots After Spatial Sign Transformation",
    x = "",
    y = "Transformed Value"
  )

# Ensure all numeric variables are positive for Box-Cox
df_num_shifted <- df_num %>% mutate(across(everything(), ~ .x + abs(min(.x)) + 0.01))

# Preprocessing: BoxCox + center + scale
pp <- preProcess(df_num_shifted, method = c("BoxCox", "center", "scale"))

df_boxcox_scaled <- predict(pp, df_num_shifted)

# Check skewness AFTER Box-Cox transformation

skew_after <- sapply(df_boxcox_scaled, function(x) {
  e1071::skewness(x, na.rm = TRUE, type = 2)
})


skew_after

# Skewness AFTER Box-Cox transformation
skew_after_tbl <- df_boxcox_scaled %>%
  summarise(across(everything(), ~ e1071::skewness(.x, na.rm = TRUE, type = 2))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "skewness_after"
  )

# Comparison table: BEFORE vs AFTER
skew_compare <- skew_tbl %>%
  rename(skewness_before = skewness) %>%
  left_join(skew_after_tbl, by = "variable") %>%
  mutate(
    improvement = abs(skewness_before) - abs(skewness_after)
  ) %>%
  arrange(desc(abs(skewness_before)))

# Print result
skew_compare
# Long format for histogram plotting
df_boxcox_long <- df_boxcox_scaled %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  )


# Long format for histogram plotting
df_boxcox_long <- df_boxcox_scaled %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  )

# Histograms after BoxCox + scaling
ggplot(df_boxcox_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Distributions After BoxCox + Centering + Scaling",
    x = "Transformed Value",
    y = "Frequency"
  )
#Correlation between predictors

num_df <- df_encoded[, numeric_cols]

corr_mat <- cor(num_df, method = "pearson")
corr_mat

corrplot(corr_mat,
         method = "color",
         type = "upper",
         tl.col = "black",
         addCoef.col = "black",
         number.cex = 0.6,
         title = "Numeric Variable Correlation Heatmap")




cat_df <- df[, cat_vars_clean]

cramer_matrix <- matrix(NA, ncol = length(cat_vars_clean), nrow = length(cat_vars_clean))
colnames(cramer_matrix) <- cat_vars_clean
rownames(cramer_matrix) <- cat_vars_clean

for (i in seq_along(cat_vars_clean)) {
  for (j in seq_along(cat_vars_clean)) {
    tbl <- table(cat_df[[i]], cat_df[[j]])
    cramer_matrix[i, j] <- cramerV(tbl)
  }
}

corrplot(cramer_matrix,
         method = "color",
         type = "upper",
         tl.col = "black",
         title = "Categorical Variable Association Heatmap (Cramér’s V)")



#Combined heatmap
# Cramér's V for categorical–categorical
cramers_v <- function(x, y) {
  tbl <- table(x, y)
  chi2 <- suppressWarnings(chisq.test(tbl, correct = FALSE)$statistic)
  n <- sum(tbl)
  r <- nrow(tbl)
  k <- ncol(tbl)
  return(sqrt(chi2 / (n * (min(r, k) - 1))))
}

# Correlation Ratio (η) for categorical–numeric
correlation_ratio <- function(categories, values) {
  fcat <- as.factor(categories)
  n <- length(values)
  ss_total <- sum((values - mean(values))^2)
  ss_between <- sum(tapply(values, fcat, function(x) length(x) * (mean(x) - mean(values))^2))
  eta <- sqrt(ss_between / ss_total)
  return(eta)
}

all_vars <- c(numeric_cols, cat_vars_clean)
p <- length(all_vars)

combined_corr <- matrix(NA, nrow = p, ncol = p,
                        dimnames = list(all_vars, all_vars))

for (i in seq_along(all_vars)) {
  for (j in seq_along(all_vars)) {
    
    var_i <- df_clean[[all_vars[i]]]
    var_j <- df_clean[[all_vars[j]]]
    
    # numeric–numeric → Pearson
    if (is.numeric(var_i) && is.numeric(var_j)) {
      combined_corr[i, j] <- cor(var_i, var_j, use = "pairwise.complete.obs")
    }
    
    # categorical–categorical → Cramér’s V
    else if (is.factor(var_i) && is.factor(var_j)) {
      combined_corr[i, j] <- cramers_v(var_i, var_j)
    }
    
    # numeric–categorical → Correlation Ratio (η)
    else if (is.factor(var_i) && is.numeric(var_j)) {
      combined_corr[i, j] <- correlation_ratio(var_i, var_j)
    }
    
    else if (is.numeric(var_i) && is.factor(var_j)) {
      combined_corr[i, j] <- correlation_ratio(var_j, var_i)
    }
  }
}



corrplot(combined_corr,
         method = "color",
         tl.col = "black",
         tl.cex = 0.6,
         number.cex = 0.5,
         title = "Combined Correlation Matrix (Numeric + Categorical)",
         mar = c(0,0,2,0))

#Checking the distribution of the response variable

ggplot(df_encoded, aes(x = readmitted_30)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Readmission Distribution",
    x = "Readmission Category", y = "Count"
  )


##  Define predictors, outcome, and group ID

group_id <- df_raw$patient_nbr

y <- df_encoded$readmitted_30
x <- df_encoded %>% select(-readmitted_30)


#Data Splitting into train and test


# Step 1: Patient-level summary
df_groups <- data.frame(group = group_id, y = y)

group_summary <- df_groups %>%
  group_by(group) %>%
  summarise(y_major = names(which.max(table(y))))

# Candidate K values
candidate_K <- c(3, 4, 5, 6, 7, 8, 10)

# Corrected evaluate_K function
evaluate_K <- function(K) {
  set.seed(123)
  
  folds_group <- createFolds(
    y = group_summary$y_major,
    k = K,
    list = TRUE,
    returnTrain = FALSE
  )
  
  # Compute fold sizes
  fold_sizes <- sapply(folds_group, length)
  
  # Compute class balance deviation per fold
  class_dev <- sapply(folds_group, function(idx) {
    mean(group_summary$y_major[idx] == "1")
  })
  
  # Score: lower is better
  score <- sd(fold_sizes) + sd(class_dev)
  
  return(score)
}

# Evaluate all K
scores <- sapply(candidate_K, evaluate_K)

# Choose optimal K
optimal_K <- candidate_K[which.min(scores)]
optimal_K


set.seed(123)

folds_group <- createFolds(
  y = group_summary$y_major,
  k = optimal_K,
  list = TRUE,
  returnTrain = FALSE
)

folds_row <- lapply(folds_group, function(idx) {
  test_groups <- group_summary$group[idx]
  which(group_id %in% test_groups)
})

for (i in seq_along(folds_row)) {
  test_idx  <- folds_row[[i]]
  train_idx <- setdiff(seq_len(nrow(df_encoded)), test_idx)
  
  x_train <- x[train_idx, ]
  y_train <- y[train_idx]
  
  x_test  <- x[test_idx, ]
  y_test  <- y[test_idx]
  
  # Fit your model here
}


x_train
y_train
x_test
y_train
sizeof(x_train)


#Candidate models training
install.packages("glmnet")
install.packages("randomForest")
install.packages("xgboost")
install.packages("catboost")
install.packages("tabnet")
install.packages("devtools")
devtools::install_url("https://github.com/catboost/catboost/releases/download/v1.2.5/catboost-R-Windows-1.2.5.tgz",
                      INSTALL_opts = c("--no-multiarch"))

library(glmnet)
library(randomForest)
library(xgboost)
library(catboost)
install.packages("torch")
torch::install_torch()
library(tabnet)
library(caret)
library(dplyr)

# Convert to matrix for glmnet
x_train_mat <- as.matrix(x_train)
x_test_mat  <- as.matrix(x_test)

logit_model <- cv.glmnet(
  x = x_train_mat,
  y = y_train,
  family = "binomial",
  alpha = 0.5   # elastic net
)

logit_pred <- predict(logit_model, x_test_mat, type = "response")
logit_class <- ifelse(logit_pred > 0.5, 1, 0)

# Random Forest Model
#rf_model <- randomForest(
#  x = x_train,
#  y = as.factor(y_train),
#  ntree = 500,
#  mtry = floor(sqrt(ncol(x_train)))
#)

#rf_pred <- predict(rf_model, x_test, type = "prob")[,2]
#rf_class <- ifelse(rf_pred > 0.5, 1, 0)

#XGBOOST
dtrain <- xgb.DMatrix(data = x_train_mat, label = y_train)
dtest  <- xgb.DMatrix(data = x_test_mat)

xgb_model <- xgboost(
  data = dtrain,
  objective = "binary:logistic",
  eval_metric = "auc",
  nrounds = 200,
  max_depth = 6,
  eta = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  verbose = 0
)

xgb_pred <- predict(xgb_model, dtest)
xgb_class <- ifelse(xgb_pred > 0.5, 1, 0)

#CATBOOST
train_pool <- catboost.load_pool(data = x_train, label = y_train)
test_pool  <- catboost.load_pool(data = x_test)

cat_model <- catboost.train(
  train_pool,
  params = list(
    loss_function = "Logloss",
    eval_metric = "AUC",
    iterations = 300,
    depth = 6,
    learning_rate = 0.1,
    random_seed = 123
  )
)

cat_pred <- catboost.predict(cat_model, test_pool, prediction_type = "Probability")
cat_class <- ifelse(cat_pred > 0.5, 1, 0)

#Tabnat Model
tabnet_model <- tabnet_fit(
  x_train, y_train,
  epochs = 50,
  batch_size = 1024,
  virtual_batch_size = 128,
  num_steps = 5,
  decision_width = 32,
  attention_width = 32
)

tabnet_pred <- predict(tabnet_model, x_test, type = "prob")[,2]
tabnet_class <- ifelse(tabnet_pred > 0.5, 1, 0)

#MODEL PERFORMANCE VALIDATION

evaluate <- function(pred_prob, pred_class, y_test) {
  data.frame(
    Accuracy = mean(pred_class == y_test),
    AUC = pROC::auc(y_test, pred_prob)
  )
}

results <- list(
  Logistic = evaluate(logit_pred, logit_class, y_test),
  RandomForest = evaluate(rf_pred, rf_class, y_test),
  XGBoost = evaluate(xgb_pred, xgb_class, y_test),
  CatBoost = evaluate(cat_pred, cat_class, y_test),
  TabNet = evaluate(tabnet_pred, tabnet_class, y_test)
)

results_df <- bind_rows(results, .id = "Model")
results_df

