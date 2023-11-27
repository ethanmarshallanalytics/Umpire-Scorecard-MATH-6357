library(tidyverse)
library(ALSM)
library(dplyr)
library(leaps)
library(MASS)
library(lubridate)

df <- data.frame(read.csv('umpire_experience.csv'))
df[,7:22] <- sapply(df[,7:22], as.numeric) # convert chr to numeric
df$experience <- as.factor(df$experience) # create factor with experience
str(df)

df.predictors <- df[,7:23]

# Create model with selected features
df_selected <- df %>% dplyr::select(expected_consistency, expected_accuracy, expected_incorrect_calls, total_run_impact,
                                  experience)

# A simple model is easier to explain, but here we don't care about interpret ability

# centering values
center_scale <- function(x){
  scale(x,scale = FALSE)
}
df_centered <- data.frame(center_scale(df_selected[1:4]))
df_centered['experience'] <- df_selected$experience 

# split into training and test set to get an idea of out of test performance
set.seed(37)
train_id <- sample(1:nrow(df_centered), .8*nrow(df_centered)) # split into training/test
train_df <- df_centered[train_id,]
test_df <- df_centered[-train_id,]

#Base model
lm.base <- lm((total_run_impact+1)^-.1~., data = train_df)
summary(lm.base)
#adj R^2 = .2444

# interactions only
lm.int <- lm((total_run_impact+1)^-.1~.^2, data = train_df)
summary(lm.int)
# adj R^2 = .2539

# Polynomial transformations
lm.poly <- lm((total_run_impact+1)^-.1~.+ poly(expected_consistency, 3) + poly(expected_accuracy, 3) +
               poly(expected_incorrect_calls, 3), data = train_df)
summary(lm.poly)
# adj R^2 = .2608

# Adding interactions
lm.int.poly <- update(lm.poly, .~. + .^2, data = train_df )
summary(lm.int.poly)
# adj R^2 = .265

# predict test set with simple model for comparison to complex
simple_pred <- predict(lm.base, test_df)
error <- mean((test_df$total_run_impact - ((simple_pred^-10) -1))^2)
sqrt(error)
# avg error = .76225

# Predict using complex set
complex_pred <- predict(lm.int.poly, test_df) # create prediction with simplest
error <- mean((test_df$total_run_impact - ((base_pred^-10) -1))^2)
sqrt(error)
# avg error = .64896

# Prediction on 2023 season
df_2023 <- read.csv('2023_Season_Games.csv')
df_2023[,7:22] <- sapply(df_2023[,7:22], as.numeric)
df_2023 <- na.omit(df_2023)

lm.poly <- lm((total_run_impact+1)^-.1~.+ poly(expected_consistency, 3) + poly(expected_accuracy, 3) +
                poly(expected_incorrect_calls, 3), data = df_centered)

lm.int.poly <- update(lm.poly, .~.^2)
pred.2023 <- predict(lm.int.poly, df_2023)
pred.2023 <- (pred.2023^-10)-1
error <- mean((df_2023$total_run_impact - pred.2023)^2)
sqrt(error)
# 2.32 average error