####################################################################################################################
## 04. MODELING
####################################################################################################################

library(dplyr)
library(tidyverse)
library(ggplot2)
library(stringr)
library(glmnet)
library(randomForest)
library(xgboost)
library(caret)
library(car)
library(ranger)
library(tidymodels)
library(scales)

source("03_EDA_Feature_Eng.R")
model_data <- stats %>% select(-player,-all_nba_last3)

# Split the data into train and test sets (80% train, 20% test)
set.seed(123)
model_data$id <- 1:nrow(model_data)

train <- model_data %>% sample_frac(0.80)
test  <- anti_join(model_data, train, by = 'id')

# Drop temporary id column
train <- train %>% select(-id)
test <- test %>% select(-id)

# ----------------------------------------------------------------------------------------------------------------
# Define functions for later use
# ----------------------------------------------------------------------------------------------------------------

# Function to compute evaluation metrics for linear regression models
evaluate_linear_model <- function(model, data, model_name) {
  # Make predictions
  predictions <- predict(model, newdata = data)
  actuals <- data$pct_of_cap
  
  # Compute metrics
  mae <- mean(abs(actuals - predictions), na.rm = TRUE)
  rmse <- sqrt(mean((actuals - predictions)^2, na.rm = TRUE))
  r2 <- summary(model)$r.squared
  
  # Return metrics
  tibble(
    Model = model_name,
    MAE = round(mae, 4),
    RMSE = round(rmse, 4),
    R2 = round(r2, 4),
  )
}

# Function to plot calibration curve
plot_calibration <- function(model, data = train, model_name = "Model", n_bins = 12) {
  # Make predictions
  data <- data %>%
    mutate(pred = predict(model, newdata = data)) %>%
    mutate(pred_bin = ntile(pred, n_bins)) %>%
    group_by(pred_bin) %>%
    summarise(
      avg_pred = mean(pred, na.rm = TRUE),
      avg_actual = mean(pct_of_cap, na.rm = TRUE)
    )
  
  # Plot calibration curve
  ggplot(data, aes(x = avg_pred, y = avg_actual)) +
    geom_point(size = 3, color = suns_purple) +  
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
    labs(
      title = paste0("Calibration Plot: ", model_name),
      x = "Average Predicted % of Cap",
      y = "Average Actual % of Cap"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
}


# ----------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------
# Linear Regression
# ----------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------

# Baseline Model 
# This model serves as a sanity check and initial benchmark. 
# It uses only the top 3 most explanatory/highest correlated features with the target variable, percent of cap.
# If more complex models fail to meaningfully outperform this baseline, we may prefer it for its simplicity and communicability.

baseline_lm <- lm(pct_of_cap ~ mpg + gs_pct + vorp, data = train)
summary(baseline_lm)

baseline_results <- evaluate_linear_model(baseline_lm, train, "Baseline Model")
baseline_results

baseline_calibration <- plot_calibration(baseline_lm, train, model_name = "Baseline Linear Model")
baseline_calibration

# ----------------------------------------------------------------------------------------------------------------
# All Features Model
# Model with ALL available features 
# This model tests whether including the full set of engineered features leads to better performance. 
# While it may show improved fit on training data, it is more prone to multicollinearity and overfitting.
# To address this, we extract the statistically significant subset in the next model.

all_features_lm <- lm(pct_of_cap ~ ., data = train)
summary(all_features_lm)

all_features_results <- evaluate_linear_model(all_features_lm, train, "All Features Model")
all_features_results

all_features_calibration <- plot_calibration(all_features_lm, train, model_name = "All Features Linear Model")
all_features_calibration

# Return all statistically significant features
significant_feats <- as.data.frame(summary(all_features_lm)$coefficients) %>%
  rownames_to_column(var = "feature") %>%
  filter(`Pr(>|t|)` < 0.05 & feature != "(Intercept)") %>%
  select(feature)

# Check for multicollinearity
vifs <- vif(all_features_lm)
high_vif_vars <- vifs[vifs > 10]
high_vif_vars

# ----------------------------------------------------------------------------------------------------------------
# Significant Features Model
# This model leverages all features found to be statistically significant above, providing a data-driven way to balance predictive power with simplicity. 
# It also avoids redundancy by removing high-VIF variables, ensuring more stable coefficient estimates and better generalizability to new data.

# Extract significant features
sig_features <- significant_feats$feature

# Build a formula
formula_sig <- as.formula(paste("pct_of_cap ~", paste(sig_features, collapse = " + ")))

# Train a model
significant_features_lm <- lm(formula_sig, data = train)
summary(significant_features_lm)

significant_features_results <- evaluate_linear_model(significant_features_lm, train, "Significant Features Model")
significant_features_results

significant_features_calibration <- plot_calibration(significant_features_lm, train, model_name = "Significant Features Linear Model")
significant_features_calibration

# Check for multicollinearity
vifs <- vif(significant_features_lm)
high_vif_vars <- vifs[vifs > 10]
high_vif_vars

# ----------------------------------------------------------------------------------------------------------------
# LASSO Regression Model
# LASSO regression provides a more automated feature selection process by shrinking uninformative coefficients to zero
# This helps reduce model complexity and address multicollinearity. 

# Remove target from features
x <- as.matrix(train %>% select(-pct_of_cap))
y <- train$pct_of_cap

# Train LASSO with Cross Validation for optimal Lambda
lasso_cv <- cv.glmnet(x, y, alpha = 1)  

# Extract the best model
best_lambda <- lasso_cv$lambda.min
lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)

# View coefficients 
coef(lasso_model)

# Make predictions and compute evaluation metrics
preds <- predict(lasso_model, s = best_lambda, newx = x)
mae <- mean(abs(preds - y))
rmse <- sqrt(mean((preds - y)^2))
r2 <- 1 - sum((y - preds)^2) / sum((y - mean(y))^2)

lasso_results <- tibble(Model = "LASSO Regression", MAE = mae, RMSE = rmse, R2 = r2)
lasso_results

# Plot calibration
calibration_df <- tibble(
  actual = y,
  pred = as.vector(preds)) %>%
  mutate(bin = ntile(pred, 10)) %>%
  group_by(bin) %>%
  summarise(
    avg_pred = mean(pred, na.rm = TRUE),
    avg_actual = mean(actual, na.rm = TRUE),
    .groups = "drop"
  )

# Plot
lasso_calibration <- ggplot(calibration_df, aes(x = avg_pred, y = avg_actual)) +
  geom_point(size = 3, color = suns_purple) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Calibration Plot: LASSO Regression",
    x = "Average Predicted % of Cap",
    y = "Average Actual % of Cap"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

lasso_calibration

# ----------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------
# Random Forest
# ----------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------
# Random Forests allow us to model complex nonlinear relationships between player statistics and market value, 
# and automatically capture interactions between features without explicit engineering.

# Define 5-fold cross-validation settings
rf_train_control <- trainControl(method = "cv", number = 5)

# Define the grid for mtry and min node size
rf_tune_grid <- expand.grid(
  mtry = c(5, 10, 15),
  splitrule = "variance",
  min.node.size = c(3, 5, 10)
)

# Define the to test for num trees and max tree depth
trees_list <- c(100, 250, 500)
depth_list <- c(5, 10, 15)

# Create a list to store all models
rf_models <- list()
results <- data.frame()

# Loop through each combination
for (n_tree in trees_list) {
  for (depth in depth_list) {
    set.seed(42) 
    
    model <- train(
      pct_of_cap ~ .,
      data = train,
      method = "ranger",
      trControl = rf_train_control,
      tuneGrid = rf_tune_grid,
      num.trees = n_tree,
      max.depth = depth,
      importance = "permutation"
    )
    
    model_id <- paste0("trees_", n_tree, "_depth_", depth)
    rf_models[[model_id]] <- model
    
    # Save performance summary
    temp <- model$results
    temp$num.trees <- n_tree
    temp$max.depth <- depth
    results <- rbind(results, temp)
  }
}

# Find the best model, based on Rsquared
best_result <- results[which.max(results$Rsquared), ]
best_result

rf_results <- tibble(
  Model = "Random Forest",
  MAE = round(best_result$MAE, 4),
  RMSE = round(best_result$RMSE, 4),
  R2 = round(best_result$Rsquared, 4)
)

rf_results

best_rf_model <- rf_models[[paste0("trees_", best_result$num.trees, "_depth_", best_result$max.depth)]]
best_rf_model

# Get feature importance
importance_df <- varImp(best_rf_model, scale = TRUE)$importance %>%
  rownames_to_column("feature") %>%
  arrange(desc(Overall))

# Plot with custom colors
ggplot(importance_df, aes(x = reorder(feature, Overall), y = Overall)) +
  geom_col(fill = suns_purple, alpha = 0.9) +
  coord_flip() +
  labs(
    title = "Feature Importances - Random Forest",
    x = "Feature",
    y = "Importance"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.text.y = element_text(size = 10)
  )

# Create calibration data
calibration_data <- train %>%
  mutate(
    pred = predict(best_rf_model, newdata = train),
    pred_bin = ntile(pred, 10)
  ) %>%
  group_by(pred_bin) %>%
  summarise(
    avg_pred = mean(pred, na.rm = TRUE),
    avg_actual = mean(pct_of_cap, na.rm = TRUE)
  )

# Plot the calibration curve
rf_calibration <- ggplot(calibration_data, aes(x = avg_pred, y = avg_actual)) +
  geom_point(size = 3, color = suns_purple) +  
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Calibration Plot: Best Random Forest Model",
    x = "Average Predicted % of Cap",
    y = "Average Actual % of Cap"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

rf_calibration

# ----------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------
# XGBoost
# ----------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------

# XGBoost is known for its performance on complex regression problems
# Implementation did not outperform the previously tuned Random Forest in this case 

# Define x matrix and y vector
x_train <- model.matrix(pct_of_cap ~ ., data = train)[, -1]  
y_train <- train$pct_of_cap

# Set up training control for 5-fold cross-validation
xg_train_control <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = FALSE
)

# Define parameter grid
xg_tune_grid <- expand.grid(
  nrounds = c(50, 100, 150),
  max_depth = c(3, 5, 7),
  eta = c(0.01, 0.1, 0.3),
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 0.8
)

# Train XGBoost model
xgb <- train(
  x = x_train,
  y = y_train,
  method = "xgbTree",
  trControl = xg_train_control,
  tuneGrid = xg_tune_grid,
  metric = "RMSE"
)

# View the best model & results
best_xgb_model <- xgb$bestTune
best_xgb_model

best_xgb_result <- xgb$results[which.max(xgb$results$Rsquared), ]
best_xgb_result

xg_results <- tibble(
  Model = "XGBoost",
  MAE = round(best_xgb_result$MAE, 4),
  RMSE = round(best_xgb_result$RMSE, 4),
  R2 = round(best_xgb_result$Rsquared, 4)
)

# ----------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------
# Model Selection
# ----------------------------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------------------------

# View all evaluation metrics across all 6 models 
all_models <- rbind(
  baseline_results,
  all_features_results,
  significant_features_results,
  lasso_results,
  rf_results,
  xg_results
) %>% arrange(-R2)

all_models

rf_calibration

# Among all models tested, the Random Forest performed best across all evaluation metrics
# It achieved the lowest MAE (3.77%), lowest RMSE (5.05%), and the highest R² (0.722) on the training data. 
# It significantly outperformed all linear models and also outperformed XGBoost despite its flexibility and tuning. 
# This suggests that the relationship between player performance and free agency value is best captured through a highly nonlinear, ensemble-based approach 
# that can automatically detect interaction effects and variable importance.
# Calibration analysis supports the model's reliability: predictions are well-aligned with actual contract outcomes across most of the distribution. 
# There is slight over-prediction at the low end (minimum/near-minimum deals) and slight under-prediction at the high end (max/supermax players), 
# likely due to the small sample sizes and asymmetric negotiation dynamics at those extremes.

# ----------------------------------------------------------------------------------------------------------------
# Evaluate the Model on the Test Set
# ----------------------------------------------------------------------------------------------------------------

# Make predictions on test data
test$predicted_pct <- predict(best_rf_model, newdata = test)

# Calculate evaluation metrics
actuals <- test$pct_of_cap
predictions <- test$predicted_pct

# Metrics
mae <- mean(abs(actuals - predictions), na.rm = TRUE)
rmse <- sqrt(mean((actuals - predictions)^2, na.rm = TRUE))
r2 <- cor(actuals, predictions)^2 

# Display results
test_results <- tibble(
  MAE = round(mae, 4),
  RMSE = round(rmse, 4),
  R2 = round(r2, 4)
)

test_results

# Calibration plot on test data
calibration_data <- test %>%
  mutate(pred_bin = ntile(predicted_pct, 10)) %>%
  group_by(pred_bin) %>%
  summarise(
    avg_pred = mean(predicted_pct, na.rm = TRUE),
    avg_actual = mean(pct_of_cap, na.rm = TRUE)
  )

# Plot calibration
ggplot(calibration_data, aes(x = avg_pred, y = avg_actual)) +
  geom_point(size = 3, color = suns_purple) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Calibration Plot: Random Forest on Test Data",
    x = "Average Predicted % of Cap",
    y = "Average Actual % of Cap"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Plot of residuals
ggplot(test, aes(x = predicted_pct, y = predicted_pct - pct_of_cap)) +
  geom_point(alpha = 0.5, color = suns_orange) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Random Forest Residuals vs Predicted",
    x = "Predicted % of Cap",
    y = "Residual (Predicted - Actual)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Plot the most important predictors
importance_df %>%
  top_n(10, Overall) %>%
  ggplot(aes(x = reorder(feature, Overall), y = Overall)) +
  geom_col(fill = suns_purple, alpha = 0.9) +
  coord_flip() +
  labs(
    title = "Top 10 Predictors – Random Forest",
    x = "Feature",
    y = "Importance Score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    axis.text.y = element_text(size = 10)
  )

# The Random Forest model generalized exceptionally well to unseen data
# On the test set, MAE, RMSE, and R² values improved compared to those observed during training. (R² = 0.744)
# The model is well-calibrated across the full range of predictions and shows no evidence of overfitting. 
# This confirms that the model can be used reliably to estimate free agent market values moving forward.

# Convert predictions from percentage of cap to dollar amounts, based on season and salary cap value
test <- test %>% 
  left_join(cap, by = "season") %>%
  mutate(actual_salary = pct_of_cap * salary_cap,
         predicted_salary = predicted_pct * salary_cap)

# Compute errors & metrics based on dollars
actual_salary <- test$actual_salary
predicted_salary <- test$predicted_salary

# Metrics
test_mae <- mean(abs(actual_salary - predicted_salary), na.rm = TRUE)
test_rmse <- sqrt(mean((actual_salary - predicted_salary)^2, na.rm = TRUE))
test_r2 <- cor(actual_salary, predicted_salary)^2 

# Display results
dollar_test_results <- tibble(
  MAE = round(test_mae, 4),
  RMSE = round(test_rmse, 4),
  R2 = round(test_r2, 4)
)

dollar_test_results

# By converting predicted salary percentages into actual dollar values using season-specific cap figures, we ensure our results are interpretable in a real-world context. 
# The model achieves a test set MAE of $3.7M and RMSE of $5.2M, with an R² of 0.774, confirming strong generalization. 

# Calibration plot on test data in dollars
calibration_data_dollars <- test %>%
  mutate(pred_bin = ntile(predicted_salary, 10)) %>%
  group_by(pred_bin) %>%
  summarise(
    avg_pred = mean(predicted_salary, na.rm = TRUE),
    avg_actual = mean(actual_salary, na.rm = TRUE)
  )

ggplot(calibration_data_dollars, aes(x = avg_pred, y = avg_actual)) +
  geom_point(size = 4, color = suns_orange) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Calibration Plot: Random Forest on Test Data\n Predicted vs Actual Salary In Dollars",
    x = "Average Predicted Salary",
    y = "Average Actual Salary"
  ) +
  scale_x_continuous(labels = label_dollar(scale = 1e-6, suffix = "M"), breaks = seq(0, 50000000, 5000000)) +
  scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M"), breaks = seq(0, 50000000, 5000000)) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Plotting actuals vs predicted salaries
ggplot(test, aes(x = predicted_salary, y = actual_salary)) +
  geom_point(size = 2, color = suns_orange) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Predicted vs Actual Salary In Dollars",
    x = "Average Predicted Salary",
    y = "Average Actual Salary"
  ) +
  scale_x_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
  scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Actual vs Predicted Distributions
summary_df <- data.frame(
  Values = c("Actual", "Predicted"),
  Mean_Pct_of_Cap = round(c(mean(test$pct_of_cap, na.rm = TRUE), mean(test$predicted_pct, na.rm = TRUE)) * 100, 2),
  Median_Pct_of_Cap = round(c(median(test$pct_of_cap, na.rm = TRUE), median(test$predicted_pct, na.rm = TRUE)) * 100, 2),
  Max_Pct_of_Cap = round(c(max(test$pct_of_cap, na.rm = TRUE), max(test$predicted_pct, na.rm = TRUE)) * 100, 2),
  Mean_Salary = c(mean(test$actual_salary, na.rm = TRUE), mean(test$predicted_salary, na.rm = TRUE)),
  Median_Salary = c(median(test$actual_salary, na.rm = TRUE), median(test$predicted_salary, na.rm = TRUE))
)

summary_df

