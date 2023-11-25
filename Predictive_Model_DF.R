library(tidyverse)
library(ALSM)
library(dplyr)
library(leaps)
library(MASS)
library(lubridate)

df <- data.frame(read.csv('umpire_experience.csv'))
df[,7:21] <- sapply(df[,7:21], as.numeric) # convert chr to numeric
df$experience <- as.factor(df$experience) # create factor with experience
str(df)

df.predictors <- df[,7:22]

full.lm <- lm((total_run_impact+1) ~., data = df.predictors)
lambda <- boxcox(object = full.lm, lambda = seq(-2, 2, 1/10), plotit = FALSE)
which.max(lambda$y)
lambda$x[20]

lm.bc <- lm((total_run_impact+1)^-.1~., data = df.predictors) # create model with transform recommended by BC
plot(df.predictors$total_run_impact, lm.bc$residuals) # residuals closer to normal

best_model <- regsubsets((total_run_impact+1)^-.1~., data = df.predictors) # find best predictors with reg subsets
best.summary <- summary(best_model)
which.max(best.summary$adjr2) # 8 predictors recommended
best.summary$which[8,]

df_best <- df.predictors[best.summary$which[8,]] # create df of best performing predictors

# Create model with selected features
df_selected <- df %>% dplyr::select(consistency, expected_accuracy, expected_incorrect_calls, total_run_impact,
                                  experience)

# split into training and test set to get an idea of out of test performance
set.seed(37)
train_id <- sample(1:nrow(df_selected), .8*nrow(df_selected)) # split into training/test
train_df <- df_selected[train_id,]
test_df <- df_selected[-train_id,]

lm.simple <- lm((total_run_impact+1)~., data = train_df) # base model
lambda <- boxcox(object = lm.simple, lambda = seq(-2, 2, 1/10), plotit = FALSE) # box cox to verify transformation
which.max(lambda$y)
lambda$x[20]

# create model with transformations
lm.simple <- lm((total_run_impact+1)^-.1~., data = train_df)
base_pred <- predict(lm.future, test_df) # create prediction with simplest
error <- mean((test_df$total_run_impact - ((base_pred^-10) -1))^2)
sqrt(error)
summary(lm.simple)

# A simple model is easier to explain, but if we can transform the x variables 
# and improve accuracy in a significant way it may be worthwhile.

# Adding interactions
lm.int <- lm((total_run_impact+1)^-.1~.^2, data = train_df)
summary(lm.int)
# experience still doesn't matter so drop it
lm.int2 <- lm((total_run_impact+1)^-.1~.+consistency*expected_accuracy+expected_accuracy*expected_incorrect_calls-experience, data = train_df)
summary(lm.int2)
base_pred <- predict(lm.int2, test_df) # create prediction with simplest
(base_pred^-10)-1
error <- mean((test_df$total_run_impact - ((base_pred^-10) -1))^2)
sqrt(error)

# interactions and polynomials
lm.poly <- lm((total_run_impact+1)^-.1~.+consistency*expected_accuracy+expected_accuracy*expected_incorrect_calls-experience,
              data = train_df)
summary(lm.poly)
anova(lm.poly, update(lm.poly,.~.+poly(consistency,2)))
anova(lm.poly, update(lm.poly,.~.+poly(expected_accuracy,2)))
anova(lm.poly, update(lm.poly,.~.+poly(expected_incorrect_calls,2)))

# For all but consistency adding 2nd degree polynomial had a low p-value. Lets make
# more predictions and see how much it improves our accuracy.

lm.polys <- update(lm.poly, .~. + poly(expected_accuracy,2) + poly(expected_incorrect_calls,2), data = train_df)
base_pred <- predict(lm.polys, test_df) # create prediction with simplest
error <- mean((test_df$total_run_impact - ((base_pred^-10) -1))^2)
sqrt(error)

# Barely reduces error, we'll stick with the simplest model.


# Prediction on 2023 season
lm.simple <- update(lm.simple,.~.-experience)
df_2023 <- read.csv('2023_Season_Games.csv')
df_2023[,5:18] <- sapply(df_2023[,5:18], as.numeric)
df_2023 <- df_2023[complete.cases(df_2023),]
pred.2023 <- predict(lm.simple, df_2023)
pred.2023 <- (pred.2023^-10)-1
error <- mean((df_2023$total_run_impact - pred.2023)^2)
sqrt(error)
str(df_2023)
