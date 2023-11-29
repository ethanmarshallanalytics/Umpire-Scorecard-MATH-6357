library(tidyverse)
library(ALSM)
library(dplyr)
library(leaps)
library(MASS)
library(lubridate)


# Load data into dataframe
df <- data.frame(read.csv('umpire_experience.csv'))
# Fix data types of numeric columns
df <- subset(df, select = -c(id, date, home, away, umpire,ump_games))
df[,1:15] <- sapply(df[,1:15], as.numeric)
# Convert experience into factor with 3 levels.
df$experience <- as.factor(df$experience) # create factor with experience
# Examine structure of data frame to make sure it's kosher
str(df)

# Create a basic linear regression model with all factors to assess LINE assumptions
lm.basic <- lm(total_run_impact~., data = df)
# View output of basic model
summary(lm.basic)
# We are missing 3 coefficients due to singularities. This means that 3 of our
# predictors are perfectly correlated with another and are dropped by the model automatically.
# These 3 are correct_calls, expected_correct_calls, and correct_calls_above_expected.

# ======= Linearity of Model ======= #
# Per Ch3 sec 3 p 106 of the book (in case we get asked) "Whether a linear regression function
# is appropriate for the data being analyzed can be studied from a residual plot against the
# predictor variable or equivalently from a residual plot against fitted values
# we'd have to make 16+ plots if we were going route 1, so we're sticking with fitted values

plot(y = lm.basic$residuals, x = lm.basic$fitted.values,
     xlab = 'Fitted Values', ylab = 'Residuals',main = 'Residual Plot to Determine Linearity')
abline(h = 0, col = 'red')

# From this plot we can see that the residuals are centered around 0, however they fan
# out along the line indicating departure from normality. With so many points
# it's difficult to see any intricate patterns. Let's select only a few points so we
# can see if there are any other patterns.
# As X increases, the variance in Y is larger

plot(y = lm.basic$residuals[0:150], x = lm.basic$fitted.values[0:150],
     xlab = 'Fitted Values', ylab = 'Residuals',main = 'Residual Plot to Determine Linearity')
abline(h = 0, col = 'red')
# Same pattern emerges, but now clearer.

# From the summary of the basic model we saw the P-value for the general linear F-test
# was <2.2x10^(-16), which indicates at least one of the variables has a linear
# relationship with total_run impact.

# ======= Independence of Error Terms ======= #
# We generally assume that every baseball game is independent of every other baseball game
# We expect that we won't see any strange patterns emerge from plotting the residuals against time.

plot(y = lm.basic$residuals, x = df$id,
     xlab = 'Time', ylab = 'Residuals',
     main = 'Residuals Over Time')
abline(h = 0, col = 'red')
# There doesn't appear to be any systemic variation in residuals over time.
# Let's zoom and enhance again to be sure.

plot(y = lm.basic$residuals[0:150], x = df$id[0:150],
     xlab = 'Time', ylab = 'Residuals',
     main = 'Residuals Over Time')
abline(h = 0, col = 'red')
# Still looks good.

# ======= Normality of Error Terms ======= #
# We'll start by visualizing the distribution with a QQ plot which graphs
# the residual values against their theoretical standardized values
# this gives us an idea of how close to normal our residuals are distributed.
qqnorm(lm.basic$residuals)
qqline(lm.basic$residuals)
# Oh shit that ain't normal. That's weird. Let's do a Shapiro-Wilks test to confirm
shapiro.test(lm.basic$residuals[0:5000])
# Our p-val is 2.2x10^(-16) and our null hypothesis is that we have a normal distribution
# This test proves we have a non-normal distribution of residuals.

# We can see a clear curve at the right end of the graph indicating a right 
# skew near one of the distribution's tails.
# This suggests we need to perform some kind of transformation. Let's start by
# min-max normalizing things and see if that helps.

# Normalizing the data sets each value between 0 and 1 to apply this to the whole
# data set we will use the preProcess function from caret. Which idk is magic I guess.
library(caret)
process <- preProcess(as.data.frame(df[0:15]), method=c("range"))
# Okay really it takes the range of values in each column and then uses the predict
# function to estimate the normal value
df.norm <- predict(process, as.data.frame(df[0:15]))
# as before let's add experience back in
df.norm['experience'] <- df$experience
  
lm.norm <- lm(total_run_impact~.,data = df.norm)
qqnorm(lm.norm$residuals)
qqline(lm.norm$residuals)
# Looks the same to me. Let's see what Shapiro-Wilk thinks
shapiro.test(lm.norm$residuals[0:5000])
# Well it's the exact same. Oh well. Normalizing is really to deal with outliers
# Guess we'll have to try something else

# The Boxcox procedure is a method of determining which y transformation will
# yield residuals most approximately normal. Some of our Y-values are 0, which can't
# has a log value of infinity. To get around this we add a constant to each value of Y
# (There are more elegant ways of doing this) and re-run the basic regression.
lm.bc <- lm(total_run_impact+1~., data = df)
lambda <- boxcox(object = lm.bc, lambda = seq(-2, 2, .05), plotit = TRUE)
# The graph shows us the power transformation which has the highest likelihood of being normal
# To find out which it refers to we need the coordinates
which.max(lambda$y)
lambda$x[48]
# so our best transformation is (Y+1)^-.1. Let's try this and see how normal we are.

# Creating the new model with the transformed Y
lm.bc.transformed <- lm((total_run_impact+1)^-.1~.-ump_games, data = df)
qqnorm(lm.bc.transformed$residuals)
qqline(lm.bc.transformed$residuals)
# We no longer have the heavy skew, but we're still not normal. Let's see what Shapiro thinks
shapiro.test(lm.bc.transformed$residuals[0:5000])
# Still not close to normal

# for fun let's try transformations on the normalized set?
# First we gotta make sure we don't have 0's. Since we're normalized we'll add .01 to
# each y instead of 1.
lm.norm <- update(lm.norm, total_run_impact+.01~.)
lambda <- boxcox(object = lm.norm, lambda = seq(-2, 2, .05), plotit = TRUE)
which.max(lambda$y)
lambda$x[59]
# Recommends a transformation of .3434. Let's try it.
lm.bc.norm <- lm((total_run_impact+.01)^.3434~.-ump_games, data = df.norm)
qqnorm(lm.bc.norm$residuals)
qqline(lm.bc.norm$residuals)
# Now the curve looks flipped.
shapiro.test(lm.bc.norm$residuals[0:5000])

# Well it's not normal so our model is unreliable. Such is life. Next.

# ======= Equal Variance of Error Terms ======= #

# The Brown-Forsythe Test splits the data set into two groups, then compares the 
# absolute deviation of the residuals from the median of their group. This
# gives an idea of how much residuals vary at different X levels.
g<-rep(1,18093) #18093 obs
median(df$home_team_runs) #median = 4
g[df$home_team_runs<=4]=0
bftest(lm.bc,g,alpha=.05)
#H0:Variance of the groups are equal
#Ha: At least one group has a different variance
#alpha = 0.05
#p-value = 9.048x10^-10
#Reject the null hypothesis. No evidence to suggest that variance of the groups are equal

# The Breusch-Pagan Test is another test for equal variance. However, it assumes
# independent normally distributed residuals. We have one of those, but let's do it
library(lmtest)
bptest(lm.bc, student = FALSE)
#H0: Variance of the errors is constant across all observations (homoskedasticity)
#Ha:Variance of the errors is not constant across all observations (heteroskedasticity)
#alpha = 0.05
#p-value = < 2.2e-16
#Conclusion: Reject the null hypothesis.No evidence to suggest that Variance of the errors is constant across all observations (homoskedasticity)

# ======= Variable Selection ======= #

bss = regsubsets((total_run_impact+1)^-.1 ~ ., data = df, really.big = TRUE)
summ_bss = summary(bss)
plot(bss, scale = 'adjr2')
plot(bss, scale = 'Cp')
plot(bss, scale = 'bic')

coef(bss, id = which.min(summ_bss$bic)) # Min BIC value = -1.198x10^-2
# Model predictors using BIC:
# home_team_runs,pitches_called,incorrect_calls,favor_home, expected_consistency,
# experienceVeteran

coef(bss, id = which.min(summ_bss$cp)) # Min Cp value = 2.593903
coef(bss, id = which.max(summ_bss$adjr2)) # Max Adj R2 = 0.6729685
# Model predictors using Cp and Adjusted R^2:
# home_team_runs, away_team_runs, pitches_called, accuracy, favor_home,
# expected_consistency, experienceSeasoned, experienceVeteran, correct_calls

# Backward Selection
full_model = lm((total_run_impact+1)^-.1 ~ ., data = df)
step(full_model, direction = 'backward')
sqrt(mean((df$total_run_impact - ((full_model$fitted.values)^-10)-1)^2))
# MSE: 2.02
# Predictors:
# home_team_runs, away_team_runs, pitches_called, incorrect_calls, accuracy, accuracy_above_expected, favor_home


# Forward Selection
null_model = lm((total_run_impact+1)^-.1 ~ 1, data = df)
step(null_model, scope = formula(full_model), direction = 'forward')
mean((df$total_run_impact - null_model$fitted.values)^2)
# MSE: 0.5982644
# Predictors:
# home_team_runs, away_team_runs, incorrect_calls, accuracy, accuracy_above_expected, favor_home, expected_correct_calls

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

# ======= WLS to fix Multicollinearity ======= #

#Weighted least squares
abs.res = abs(residuals(df_best_model))
abs.res.fit <- lm(abs.res~df_best$home_team_runs + df_best$away_team_runs +  df_best$incorrect_calls + df_best$expected_correct_calls + 
                    df_best$accuracy_above_expected + df_best$consistency_x + df_best$favor_home + df_best$expected_consistency)
abs.res.fit 
wts <- 1/fitted(abs.res.fit)^2
final.fit <- lm((total_run_impact + 1)^(-0.1) ~ ., data = df_best, weights = wts)
summary(final.fit)
# R^2 of 0.6903 / R^2a = 0.6901
# R^2 got worse

# ======= Predictions with Chosen Variables ======= #
# A sensible use our model would be for gambling on outcomes of baseball games.
# However; many of our predictors are unknown until the game is over.
# Let's see how good of a model we can build with the predictors we could know before the game.

library(dplyr)
# Create the data frame with only knowable predictors
df.known <- df %>% dplyr::select(expected_consistency, 
                  expected_accuracy, expected_incorrect_calls,
                  total_run_impact, experience)

# Start with a basic regression
lm.known.basic <- lm((total_run_impact+1)^-.1~., data = df.known)
summary(lm.known.basic)
# R^2: 0.3727
# Residual Standard Error: 0.02124

plot(x = (df.known$total_run_impact+1)^-.1, y = df.known$expected_incorrect_calls)

# Play around with interaction terms and polynomials
# Polynomial model
lm.known.poly <- lm((total_run_impact+1)^-.1~.+ poly(expected_consistency, 3) + poly(expected_accuracy, 3) +
                         poly(expected_incorrect_calls, 3), data = df.known)
summary(lm.known.poly)
# R^2: 0.3816
# Residual Standard Error: 0.0211

## Running Stepwise Backwards Regression on lm.known.poly
m1 = lm((total_run_impact+1)^-.1~1, data = df.known)
step(lm.known.poly, scope=list(lower= m0, upper=lm.known.poly, direction = "backwards"))

best_model2 = lm((total_run_impact+1)^-.1~.+ poly(expected_consistency, 3) + poly(expected_accuracy, 3) +
                   poly(expected_incorrect_calls, 3), data = df.known)
# Interaction model
lm.known.int <- lm(total_run_impact~.^2, data = df.known)
summary(lm.known.int)
# R^2: 0.3731
# Residual Standard Error: 0.6127

# Polynomial and Interaction Model
lm.known.combo <- update(lm.known.poly, .~.^2)
summary(lm.known.combo)
# R^2: 0.3846
# Residual Standard Error: 0.02107


# Load 2023 set to test model
df.2023 <- read.csv('2023_Season_Games.csv')
# Dropping NAs
df.2023 = na.omit(df.2023)
# Make dtype changes as before
df.2023[,7:23] <- sapply(df.2023[,7:23], as.numeric)
df.2023$experience <- as.factor(df.2023$experience)
df.2023 <- na.omit(df.2023) # drop missing values
str(df.2023)

# Predict Impact and Check Error
pred.2023 <- ((predict(lm.known.basic, df.2023))^-10)-1
error <- mean((df.2023$total_run_impact - pred.2023)^2)
sqrt(error)
pred.2023
# Okay for some reason it's better now.

# Comparison of error if you plug in mean.
error <- mean((df.2023$total_run_impact - mean(df.2023$total_run_impact))^2)
sqrt(error)

## Calculate Prediction Intervals
# Going to use lm.known.poly model
ws_data = read.csv("ws_games.csv")
ws_data = ws_data[,c("xIC", "xAcc", "totRI")]
cols = c("expected_incorrect_calls", "expected_accuracy", "total_run_impact")
colnames(ws_data) = cols
ws_data$expected_consistency = c(92.876,93.213,93.448,93.384,93.378)
ws_data$experience = "Seasoned"
pred_interval = predict(lm.known.poly, newdata = ws_data, 
                        interval = "prediction", level = 0.95)
pred_interval
