library(tidyverse)
library(ALSM)
library(dplyr)
library(leaps)
library(MASS)
library(ggplot2)
library(lubridate)

df = data.frame(read.csv('umpire_experience.csv'))
df[,7:21] = sapply(df[,7:21], as.numeric) # convert chr to numeric
df$experience = as.factor(df$experience) # create factor with experience
str(df)

df.predictors = df[,7:22]

full.lm = lm((total_run_impact+1) ~., data = df.predictors)
?boxcox
lambda = boxcox(object = full.lm, lambda = seq(-2, 2, 1/10), plotit = FALSE)
which.max(lambda$y)
lambda$x[20]

lm.bc = lm((total_run_impact+1)^-.1~., data = df.predictors) # create model with transform recommended by BC
plot(df.predictors$total_run_impact, lm.bc$residuals) # residuals closer to normal

best_model = regsubsets((total_run_impact+1)^-.1~., data = df.predictors) # find best predictors with reg subsets
best.summary = summary(best_model)
which.max(best.summary$adjr2) # 8 predictors recommended
best.summary$which[8,]

df_best = df.predictors[best.summary$which[8,]] # create df of best performing predictors

# ------------ Creating lm with df_best as dataset -----------
df_best_model = lm((total_run_impact + 1)^(-0.1) ~ ., data = df_best)
summary(df_best_model)
# R^2 of 0.6915 / R^2a = 0.6914

# ------------ Creating Confidence Intervals -----------
coeff_int = cbind(coef(df_best_model), confint(df_best_model))
colnames(coeff_int) = c("Estimate", "Lower CI", "Upper CI")
print(coeff_int)

# ------------ Bonferroni Confidence Intervals ---------
alpha_a = 0.05/9
bonf_conf_int = confint(df_best_model, level = 1 - alpha_a)
print(bonf_conf_int)

# ------------ Predicting 2022 Season ---------------
df$date = as.Date(df$date, format = "%m/%d/%Y")
df$year = as.integer(format(as.Date(df$date, format = "%m/%d/%Y"), "%Y"))

df.2022 = df[df$year == 2022,]
vars = colnames(df_best)
df.2022_best = df.2022[, vars, drop = FALSE]

predict_2022 = predict(df_best_model, newdata = df.2022_best)

actual_values = df.2022_best$total_run_impact
comparison_df = data.frame(Actual = actual_values, Predicted = predict_2022)

ggplot(comparison_df, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Add a diagonal line for reference
  labs(title = "Actual vs Predicted",
       x = "Actual Values",
       y = "Predicted Values")

# ------------ Model Diagnostics ---------------
plot(resid(df_best_model) ~ fitted(df_best_model))
abline(h=0)
qqnorm(resid(df_best_model))
qqline(resid(df_best_model))

#Checking for collinearity
vif(df_best_model)
#home_team_runs: 1.071279    
#away_team_runs: 1.222220         
#pitches_called: 1.831866         
#incorrect_calls: 73.827946 ***
#expected_incorrect_calls: 37.615448 ***  
#accuracy_above_expected: 40.191683 ***
#consistency: 1.367426
#favor_home: 1.001542
#If vif > 10, multicollinearity exists

#Creating a copy of df_best to see correlation better
copy <- df_best
names(copy) <- c('X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8', 'Y')
round(cor(copy))
#Correlation of 1 between X3(pitches called) & X5(expected incorrect calls)
#Correlation of 1 between X4(incorrect calls) & X5(expected incorrect calls) and -1 between X4(incorrect calls) & X6(accuracy above expected)

# ------- check if dropping all interaction terms are appropriate ------
interaction_lm <- lm((total_run_impact + 1)^(-0.1) ~  .^2, data = df_best)
summary(interaction_lm)
#home_team_runs:away_team_runs                     3.275e-05  1.204e-05   2.720  0.00653 **
#home_team_runs:consistency                       -5.161e-05  1.865e-05  -2.767  0.00566 **
#home_team_runs:favor_home                        -1.232e-04  5.177e-05  -2.379  0.01735 * 
#away_team_runs:incorrect_calls                   -1.411e-04  6.252e-05  -2.256  0.02406 *
#away_team_runs:consistency                       -4.327e-05  1.970e-05  -2.196  0.02812 *
#away_team_runs:favor_home                         1.715e-04  5.494e-05   3.121  0.00180 ** 
#pitches_called:consistency                        1.427e-05  3.021e-06   4.723 2.34e-06 ***
#incorrect_calls:expected_incorrect_calls          7.693e-05  1.154e-05   6.666 2.71e-11 ***
#incorrect_calls:accuracy_above_expected          -1.949e-04  1.119e-05 -17.416  < 2e-16 ***
#expected_incorrect_calls:accuracy_above_expected -5.081e-05  2.482e-05  -2.047  0.04065 * 

intercation_anov <- anova(interaction_lm)
intercation_anov
#home_team_runs:away_team_runs                        1 0.0022  0.0022    10.6787 0.0010858 **
#home_team_runs:incorrect_calls                       1 0.0011  0.0011     5.0818 0.0241905 * 
#home_team_runs:favor_home                            1 0.0017  0.0017     8.3140 0.0039387 ** 
#away_team_runs:expected_incorrect_calls              1 0.0022  0.0022    10.5012 0.0011951 **
#away_team_runs:favor_home                            1 0.0026  0.0026    12.4165 0.0004266 ***
#pitches_called:incorrect_calls                       1 0.0529  0.0529   253.9279 < 2.2e-16 ***
#pitches_called:consistency                           1 0.0018  0.0018     8.4142 0.0037276 ** 
#incorrect_calls:expected_incorrect_calls             1 0.0811  0.0811   389.3315 < 2.2e-16 ***
#incorrect_calls:accuracy_above_expected              1 0.0978  0.0978   469.3212 < 2.2e-16 ***
#expected_incorrect_calls:consistency                 1 0.0025  0.0025    12.2146 0.0004753 ***

#H0: All interaction terms are not significant, Ha: At least 1 interaction terms may be significant, alpha : 0.05
(sum(intercation_anov$'Sum Sq'[9:36])/28)/(intercation_anov$'Mean Sq'[37])
1-pf(43.13098,28,18056)
# 0 < 0.05 Reject the null hypothesis, at least 1 interaction terms should be present

#Using the vif info, only going to choose the interaction terms for incorrect calls, expected incorrect calls, accuracy above expected
#incorrect_calls:expected_incorrect_calls             1 0.0811  0.0811   389.3315 < 2.2e-16 ***
#incorrect_calls:accuracy_above_expected              1 0.0978  0.0978   469.3212 < 2.2e-16 ***

df_interaction <- lm((total_run_impact + 1)^(-0.1) ~ df_best$home_team_runs + df_best$away_team_runs + df_best$pitches_called + 
                       df_best$incorrect_calls + df_best$expected_incorrect_calls + df_best$accuracy_above_expected + df_best$consistency + 
                       df_best$favor_home + df_best$incorrect_calls*df_best$expected_incorrect_calls + df_best$incorrect_calls*df_best$accuracy_above_expected, data = df_best)
summary(df_interaction)
# R^2 of 0.7094 / R^2a = 0.7092
anova(df_interaction)

plot(resid(df_interaction) ~ fitted(df_interaction))
abline(h=0)
qqnorm(resid(df_interaction))
qqline(resid(df_interaction))

#Cooks distance
cooks.distance(df_best_model)
plot(cooks.distance(df_best_model),type = "o")
text(cooks.distance(df_best_model), labels=rownames(df_best), cex=0.9, font=2)
title("Cook's Distance")
#14892 appears to be an outlier

#DFFITS
dffits(df_best_model)
plot(dffits(df_best_model),type = "o")
text(dffits(df_best_model), labels=rownames(df_best), cex=0.9, font=2)
title("DFFITS")
#14892 appears to be an outlier again

#DFBETAS
dfbetas(df_best_model)
plot(dfbetas(df_best_model),type = "o")
text(dfbetas(df_best_model), labels=rownames(df_best), cex=0.9, font=2)
title("DFBETAS")
#14892 appears to be an outlier again

#Brown-Forscythe Test
g<-rep(1,18093) #18093 obs
mean(df_best$home_team_runs) #mean = 4
g[df_best$home_team_runs<=4]=0
bftest(df_best_model,g,alpha=.05)
#H0:Variance of the groups are equal
#Ha: At least one group has a different variance
#alpha = 0.05
#p-value = 0.0045
#Reject the null hypothesis. No evidence to suggest that variance of the groups are equal

#Breusch-Pagan Test
library(lmtest)
bptest(df_best_model, student = FALSE)
#H0: Variance of the errors is constant across all observations (homoskedasticity)
#Ha:Variance of the errors is not constant across all observations (heteroskedasticity)
#alpha = 0.05
#p-value = < 2.2e-16
#Conclusion: Reject the null hypothesis.No evidence to suggest that Variance of the errors is constant across all observations (homoskedasticity)
