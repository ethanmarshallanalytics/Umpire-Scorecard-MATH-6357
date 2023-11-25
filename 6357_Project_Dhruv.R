# Dhruv: Best Subset of Variables

# Best Subset Selection
library(readr)
df = read.csv("C:\\Users\\dhruv\\Downloads\\mlb-umpire-scorecard (1).csv")

# Filtering variables that we won't use
df = subset(df, select = -c(id, date, home, away))
df[,2:15] = sapply(df[,2:15], as.numeric)

# Omitting NAs
df = data.frame(na.omit(df))

# Best Subset Selection (Assumes Linearity)
library(leaps)
df_ump = subset(df, select = -umpire)
scaled_df_ump = data.frame(scale(df_ump))
bss = regsubsets(total_run_impact ~ ., data = scaled_df_ump, really.big = TRUE)
summ_bss = summary(bss)
plot(bss, scale = 'adjr2')
plot(bss, scale = 'Cp')
plot(bss, scale = 'bic')

coef(bss, id = which.min(summ_bss$bic)) # Min BIC value = -20164.46
# Model predictors using BIC:
# home_team_runs, incorrect_calls, consistency, correct_calls_above_expected

coef(bss, id = which.min(summ_bss$cp)) # Min Cp value = 2.593903
coef(bss, id = which.max(summ_bss$adjr2)) # Max Adj R2 = 0.6729685
# Model predictors using Cp and Adjusted R^2:
# home_team_runs, away_team_runs, pitches_called, incorrect_calls, consistency, correct_calls, correct_calls_above_expected


# Backward Selection
full_model = lm(total_run_impact ~ ., data = df)
step(full_model, direction = 'backward')
mean((df$total_run_impact - full_model$fitted.values)^2)
# MSE: 0.1940969
# Predictors:
# home_team_runs, away_team_runs, pitches_called, incorrect_calls, accuracy, accuracy_above_expected, favor_home


# Forward Selection
null_model = lm(total_run_impact ~ 1, data = df)
step(null_model, scope = formula(full_model), direction = 'forward')
mean((df$total_run_impact - null_model$fitted.values)^2)
# MSE: 0.5982644
# Predictors:
# home_team_runs, away_team_runs, incorrect_calls, accuracy, accuracy_above_expected, favor_home, expected_correct_calls


# Cross Validation with best subset selection
library(leaps) # For best subset selection
library(caret) # For cross-validation

lm_formula = as.formula("total_run_impact ~ .")

X = model.matrix(lm_formula, data = scaled_df_ump)
y = scaled_df_ump$total_run_impact

set.seed(123)
folds = createFolds(y, k = 10, list = TRUE)

best_subset_models = list()
best_subset_rmse = numeric()

# Cross-validated best subset selection
for (fold in folds) {
  train_indices = unlist(fold)
  test_indices = setdiff(seq_along(y), train_indices)
  
  # Train data using regsubset
  best_subset = regsubsets(lm_formula, data = scaled_df_ump[train_indices, ,drop = FALSE], nvmax = 13, really.big = TRUE)
  
  # Evaluate models on the test data to figure out the best subset
  rmse_values = numeric()
  for (i in 1:ncol(best_subset$coef)) {
    subset_model = lm(lm_formula, data = scaled_df_ump[test_indices, , drop = FALSE], subset = (coef(best_subset, id = i) != 0))
    rmse_values[i] = sqrt(mean((predict(subset_model, newdata = scaled_df_ump[test_indices, , drop = FALSE]) - y[test_indices])^2))
  }
  
  # Identify the best subset based on RMSE
  best_subset_index = which.min(rmse_values)
  best_subset_models[[fold]] = coef(best_subset, id = best_subset_index)
  best_subset_rmse = c(best_subset_rmse, rmse_values[best_subset_index])
}

# Identify the overall best subset based on cross-validated RMSE
overall_best_subset_index = which.min(best_subset_rmse)
overall_best_subset = best_subset_models[overall_best_subset_index]

print(overall_best_subset)




# Evaluating Best Subset

# Regsubsets(BIC)
model1 = lm(total_run_impact ~ home_team_runs+ incorrect_calls+ consistency+ correct_calls_above_expected, data = df)
summary(model1)
# Adj. R2 = 0.661

# Regsubsets(Cp and Adjusted R^2)
model2 = lm(total_run_impact ~ home_team_runs+ away_team_runs+ pitches_called+ incorrect_calls+ consistency+ correct_calls_above_expected, data = df)
summary(model2)
# Adj. R2 = 0.6713

# Backward Selection
model3 = lm(total_run_impact ~ home_team_runs+ away_team_runs+ pitches_called+ incorrect_calls+ accuracy+ accuracy_above_expected+ favor_home
, data = df)
summary(model3)
# Adj. R2 = 0.673

# Forward Selection
model4 = lm(total_run_impact ~ home_team_runs+ away_team_runs+ incorrect_calls+ accuracy+ accuracy_above_expected+ favor_home+ expected_correct_calls
, data = df)
summary(model4)
# Adj. R2 = 0.673

























