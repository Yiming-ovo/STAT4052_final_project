# ============================================================
# STAT 4052 Final Project
# Predicting Food Waste Propensity
# ============================================================

# 1. Load packages
library(tidyverse)
library(janitor)

# 2. Read data
food <- read.csv("data/D2D2016FoodStudy_data.csv")
defs <- read.csv("data/D2D2016FoodStudy_definition.csv")

# 3. Clean variable names
food <- clean_names(food)

# 4. Check basic information
dim(food)
names(food)
summary(food$percent_discard)

# 5. Check missing values
colSums(is.na(food))
# ============================================================
# 6. Data preparation
# ============================================================

# Remove rows with missing response, if any
food_clean <- food %>%
  filter(!is.na(percent_discard))

# Check repeated observations per person
person_counts <- food_clean %>%
  count(person_id) %>%
  arrange(desc(n))

print(dim(food_clean))
print(head(person_counts, 10))

# Identify binary variables
binary_vars <- names(food_clean)[sapply(food_clean, function(x) {
  vals <- unique(na.omit(x))
  all(vals %in% c(0, 1))
})]

print(binary_vars)

# Save person_id separately and remove it from modeling predictors
model_data <- food_clean %>%
  select(-person_id)

# Median imputation for numeric predictors
for (j in seq_along(model_data)) {
  if (is.numeric(model_data[[j]])) {
    model_data[[j]][is.na(model_data[[j]])] <- median(model_data[[j]], na.rm = TRUE)
  }
}

# Check final modeling data
print(dim(model_data))
print(colSums(is.na(model_data)))
# ============================================================
# 7. Train-test split by person_id
# ============================================================

set.seed(123)

unique_ids <- unique(food_clean$person_id)
train_ids <- sample(unique_ids, size = round(0.8 * length(unique_ids)))

train_data <- food_clean %>%
  filter(person_id %in% train_ids)

test_data <- food_clean %>%
  filter(!person_id %in% train_ids)

train_x <- train_data %>% select(-person_id)
test_x  <- test_data %>% select(-person_id)

# Median imputation based on training data only
for (j in seq_along(train_x)) {
  if (is.numeric(train_x[[j]])) {
    med_j <- median(train_x[[j]], na.rm = TRUE)
    train_x[[j]][is.na(train_x[[j]])] <- med_j
    test_x[[j]][is.na(test_x[[j]])] <- med_j
  }
}

print(dim(train_x))
print(dim(test_x))


# ============================================================
# 8. Evaluation functions
# ============================================================

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}


# ============================================================
# 9. Model 1: Multiple Linear Regression
# ============================================================

lm_fit <- lm(percent_discard ~ ., data = train_x)

lm_pred <- predict(lm_fit, newdata = test_x)

rmse_lm <- rmse(test_x$percent_discard, lm_pred)
mae_lm  <- mae(test_x$percent_discard, lm_pred)

print(summary(lm_fit))
print(rmse_lm)
print(mae_lm)
# ============================================================
# 10. Model 2: Lasso Regression
# ============================================================

# Install glmnet first if needed:
# install.packages("glmnet")

library(glmnet)

# Create model matrices for glmnet
x_train <- model.matrix(percent_discard ~ ., data = train_x)[, -1]
y_train <- train_x$percent_discard

x_test <- model.matrix(percent_discard ~ ., data = test_x)[, -1]
y_test <- test_x$percent_discard

# Cross-validated lasso
set.seed(123)
lasso_cv <- cv.glmnet(
  x = x_train,
  y = y_train,
  alpha = 1,
  nfolds = 10,
  standardize = TRUE
)

# Best lambda values
lambda_min <- lasso_cv$lambda.min
lambda_1se <- lasso_cv$lambda.1se

print(lambda_min)
print(lambda_1se)

# Predict using lambda.min
lasso_pred <- predict(lasso_cv, s = "lambda.min", newx = x_test)

rmse_lasso <- rmse(y_test, lasso_pred)
mae_lasso  <- mae(y_test, lasso_pred)

print(rmse_lasso)
print(mae_lasso)

# Extract selected variables at lambda.min
lasso_coef <- coef(lasso_cv, s = "lambda.min")

# Extract selected variables at lambda.min
lasso_coef <- coef(lasso_cv, s = "lambda.min")

lasso_selected <- data.frame(
  variable = rownames(as.matrix(lasso_coef)),
  coefficient = as.numeric(as.matrix(lasso_coef))
) %>%
  filter(coefficient != 0)

print(lasso_selected)
print(nrow(lasso_selected))

# Save lasso selected coefficients
write.csv(
  lasso_selected,
  "tables/lasso_selected_coefficients.csv",
  row.names = FALSE
)

# Save lasso selected coefficients
write.csv(
  lasso_selected,
  "tables/lasso_selected_coefficients.csv",
  row.names = FALSE
)

# Plot cross-validation curve
png("figures/lasso_cv_plot.png", width = 800, height = 600)
plot(lasso_cv)
dev.off()
# ============================================================
# 11. Model 3: Random Forest
# ============================================================

# Install randomForest first if needed:
# install.packages("randomForest")

library(randomForest)

set.seed(123)

rf_fit <- randomForest(
  percent_discard ~ .,
  data = train_x,
  ntree = 500,
  mtry = floor(sqrt(ncol(train_x) - 1)),
  importance = TRUE
)

print(rf_fit)

rf_pred <- predict(rf_fit, newdata = test_x)

rmse_rf <- rmse(test_x$percent_discard, rf_pred)
mae_rf  <- mae(test_x$percent_discard, rf_pred)

print(rmse_rf)
print(mae_rf)

# Variable importance
rf_importance <- importance(rf_fit)

rf_importance_df <- data.frame(
  variable = rownames(rf_importance),
  IncMSE = rf_importance[, "%IncMSE"],
  IncNodePurity = rf_importance[, "IncNodePurity"]
) %>%
  arrange(desc(IncMSE))

print(head(rf_importance_df, 15))

write.csv(
  rf_importance_df,
  "tables/random_forest_variable_importance.csv",
  row.names = FALSE
)

# Plot top 15 variable importance
rf_top15 <- rf_importance_df %>%
  slice_max(order_by = IncMSE, n = 15) %>%
  arrange(IncMSE)

rf_plot <- ggplot(rf_top15, aes(x = reorder(variable, IncMSE), y = IncMSE)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 15 Random Forest Variable Importance",
    x = "Variable",
    y = "% Increase in MSE"
  ) +
  theme_minimal()

print(rf_plot)

ggsave(
  filename = "figures/random_forest_variable_importance.png",
  plot = rf_plot,
  width = 8,
  height = 6
)
# ============================================================
# 12. Final model performance table
# ============================================================

model_performance <- data.frame(
  Model = c("Multiple Linear Regression", "Lasso Regression", "Random Forest"),
  RMSE = c(rmse_lm, rmse_lasso, rmse_rf),
  MAE = c(mae_lm, mae_lasso, mae_rf)
)

print(model_performance)

write.csv(
  model_performance,
  "tables/model_performance.csv",
  row.names = FALSE
)


# ============================================================
# 13. Response distribution plot
# ============================================================

response_plot <- ggplot(model_data, aes(x = percent_discard)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of Food Waste Propensity",
    x = "Percent Discard",
    y = "Count"
  ) +
  theme_minimal()

print(response_plot)

ggsave(
  filename = "figures/response_distribution.png",
  plot = response_plot,
  width = 8,
  height = 6
)


# ============================================================
# 14. Age and food waste tendency plot
# ============================================================

age_plot <- ggplot(model_data, aes(x = age, y = percent_discard)) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Relationship Between Age and Food Waste Propensity",
    x = "Age",
    y = "Percent Discard"
  ) +
  theme_minimal()

print(age_plot)

ggsave(
  filename = "figures/age_food_waste_relationship.png",
  plot = age_plot,
  width = 8,
  height = 6
)


# ============================================================
# 15. Save important linear regression coefficients
# ============================================================

lm_coef_table <- summary(lm_fit)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("Variable") %>%
  rename(
    Estimate = Estimate,
    Std_Error = `Std. Error`,
    t_value = `t value`,
    p_value = `Pr(>|t|)`
  ) %>%
  arrange(p_value)

print(head(lm_coef_table, 15))

write.csv(
  lm_coef_table,
  "tables/linear_regression_coefficients.csv",
  row.names = FALSE
)