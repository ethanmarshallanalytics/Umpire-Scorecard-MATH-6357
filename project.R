umpire <- read.csv("C:/Users/jesse/OneDrive/Desktop/Grad School/MATH 6357 15443 - Linear Models and Design of Experiments/Project/mlb-umpire-scorecard.csv")
#Convert char to num
umpire$incorrect_calls <- as.numeric(umpire$incorrect_calls)
umpire$pitches_called <- as.numeric(umpire$pitches_called)
umpire$expected_incorrect_calls <- as.numeric(umpire$expected_incorrect_calls)
umpire$correct_calls <- as.numeric(umpire$correct_calls)
umpire$expected_correct_calls <- as.numeric(umpire$expected_correct_calls)
umpire$correct_calls_above_expected <- as.numeric(umpire$correct_calls_above_expected)
umpire$accuracy <- as.numeric(umpire$accuracy)
umpire$expected_accuracy <- as.numeric(umpire$expected_accuracy)
umpire$accuracy_above_expected <- as.numeric(umpire$accuracy_above_expected)
umpire$consistency <- as.numeric(umpire$consistency)
umpire$favor_home <- as.numeric(umpire$favor_home)
umpire$total_run_impact <- as.numeric(umpire$total_run_impact)
#This created NA's in our data, so we need to get rid of these rows

#Create a new data frame with out NA's
ump <- data.frame(na.omit(umpire))
#Data still contains char columns: id, date, umpire, home, away

#Data with id, date, home, away
ump <- subset(ump, select = -c(id, date, home, away))

## Plotting the all data data 
names(ump) <- c('umpire', 'X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8', 'X9', 'X10', 'X11', 'X12', 'X13', 'Y')
any(is.na(ump))

avg.lm <- lm(Y~.-umpire, data = ump)
summary(avg.lm)
anova(avg.lm)
resid.avg.lm <- residuals(avg.lm)
plot(ump$Y, avg.lm$residuals)
hist(resid.avg.lm, breaks = 30)
log.lm <- lm(Y ~.-umpire, data = ump)


lm.1 <- lm(log(Y+1)~X1+X2+X4+X13, data = ump)
resid.1 <- residuals(lm.1)
hist(resid.1)

df <- data.frame(ump$X1, ump$X2, ump$X4, ump$X13, ump$Y)

cor(df)

pred <- predict(lm.1)
mean((ump$Y - lm.1$fitted.values)^2)


plot(ump$Y, pred)
abline(lm.1)

full.model <- lm(Y~.-umpire, data = ump)
step(full.model, direction = 'backward')
mean((ump$Y - full.model$fitted.values)^2)





# Home Team Runs VS Total Run Impact
plot(ump$X1, ump$Y, main = 'Home Team Runs Vs Total Run Impact', xlab = 'Home Team Runs', ylab = 'Total Run Impact')
# Away Team Runs VS Total Run Impact
plot(ump$X2, ump$Y, main = 'Away Team Runs VS Total Run Impact', xlab = 'Away Team Runs', ylab = 'Total Run Impact')
# Pitches Called VS Total Run Impact
plot(ump$X3, ump$Y, main = 'Pitches Called VS Total Run Impact', xlab = 'Pitches Called', ylab = 'Total Run Impact')
# Incorrect Calls VS Total Run Impact
plot(ump$X4, ump$Y, main = 'Incorrect Calls VS Total Run Impact', xlab = 'Incorrect Calls', ylab = 'Total Run Impact')
# Expected Incorrect Calls VS Total Run Impact
plot(ump$X5, ump$Y, main = 'Expected Incorrect Calls VS Total Run Impact', xlab = 'Expected Incorrect Calls', ylab = 'Total Run Impact')
# Correct Calls VS Total Run Impact
plot(ump$X6, ump$Y, main = 'Correct Calls VS Total Run Impact', xlab = 'Correct Calls', ylab = 'Total Run Impact')
# Expected Correct Calls VS Total Run Impact
plot(ump$X7, ump$Y, main = 'Expected Correct Calls VS Total Run Impact', xlab = 'Expected Correct Calls', ylab = 'Total Run Impact')
# Correct Calls Above Expected VS Total Run Impact
plot(ump$X8, ump$Y, main = 'Correct Calls Above Expected VS Total Run Impact', xlab = 'Correct Calls Above Expected', ylab = 'Total Run Impact')
# Accuracy VS Total Run Impact
plot(ump$X9, ump$Y, main = 'Accuracy VS Total Run Impact', xlab = 'Accuracy', ylab = 'Total Run Impact')
# Expected Accuracy VS Total Run Impact
plot(ump$X10, ump$Y, main = 'Expected Accuracy VS Total Run Impact', xlab = 'Expected Accuracy', ylab = 'Total Run Impact')
# Accuracy Above Expected VS Total Run Impact
plot(ump$X11, ump$Y, main = 'Accuracy Above Expected VS Total Run Impact', xlab = 'Accuracy Above Expected', ylab = 'Total Run Impact')
# Consistency VS Total Run Impact
plot(ump$X12, ump$Y, main = 'Consistency VS Total Run Impact', xlab = 'Consistency', ylab = 'Total Run Impact')
# Favor Home VS Total Run Impact
plot(ump$X13, ump$Y, main = 'Favor Home VS Total Run Impact', xlab = 'Favor Home', ylab = 'Total Run Impact')

## Plotting the all averaged data data 
ump2 <- average_umpire_data
ump2 <- subset(ump2, select = -umpire_name)
names(ump2) <- c('umpire', 'X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8', 'X9', 'X10', 'X11', 'X12', 'X13', 'Y')


# Home Team Runs VS Total Run Impact
plot(ump$X1, ump$Y, main = 'Home Team Runs Vs Total Run Impact', xlab = 'Home Team Runs', ylab = 'Total Run Impact')
# Away Team Runs VS Total Run Impact
plot(ump$X2, ump$Y, main = 'Away Team Runs VS Total Run Impact', xlab = 'Away Team Runs', ylab = 'Total Run Impact')
# Pitches Called VS Total Run Impact
plot(ump$X3, ump$Y, main = 'Pitches Called VS Total Run Impact', xlab = 'Pitches Called', ylab = 'Total Run Impact')
# Incorrect Calls VS Total Run Impact
plot(ump$X4, ump$Y, main = 'Incorrect Calls VS Total Run Impact', xlab = 'Incorrect Calls', ylab = 'Total Run Impact')
# Expected Incorrect Calls VS Total Run Impact
plot(ump$X5, ump$Y, main = 'Expected Incorrect Calls VS Total Run Impact', xlab = 'Expected Incorrect Calls', ylab = 'Total Run Impact')
# Correct Calls VS Total Run Impact
plot(ump$X6, ump$Y, main = 'Correct Calls VS Total Run Impact', xlab = 'Correct Calls', ylab = 'Total Run Impact')
# Expected Correct Calls VS Total Run Impact
plot(ump$X7, ump$Y, main = 'Expected Correct Calls VS Total Run Impact', xlab = 'Expected Correct Calls', ylab = 'Total Run Impact')
# Correct Calls Above Expected VS Total Run Impact
plot(ump$X8, ump$Y, main = 'Correct Calls Above Expected VS Total Run Impact', xlab = 'Correct Calls Above Expected', ylab = 'Total Run Impact')
# Accuracy VS Total Run Impact
plot(ump$X9, ump$Y, main = 'Accuracy VS Total Run Impact', xlab = 'Accuracy', ylab = 'Total Run Impact')
# Expected Accuracy VS Total Run Impact
plot(ump$X10, ump$Y, main = 'Expected Accuracy VS Total Run Impact', xlab = 'Expected Accuracy', ylab = 'Total Run Impact')
# Accuracy Above Expected VS Total Run Impact
plot(ump$X11, ump$Y, main = 'Accuracy Above Expected VS Total Run Impact', xlab = 'Accuracy Above Expected', ylab = 'Total Run Impact')
# Consistency VS Total Run Impact
plot(ump$X12, ump$Y, main = 'Consistency VS Total Run Impact', xlab = 'Consistency', ylab = 'Total Run Impact')
# Favor Home VS Total Run Impact
plot(ump$X13, ump$Y, main = 'Favor Home VS Total Run Impact', xlab = 'Favor Home', ylab = 'Total Run Impact')

#Normalizing data with scale
ump3 <- average_umpire_data
ump3 <- subset(ump3, select = -c(umpire, umpire_name))
names(ump3) <- c('X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8', 'X9', 'X10', 'X11', 'X12', 'X13', 'Y')
ump_scaled <- scale(ump3)
ump4 <- ump_scaled
ump5 <- data.frame(ump4)
ump.lm <- lm(Y~., data = ump5)
summary(ump.lm)
qqnorm(ump5$X1)
shapiro.test(ump5$X1)
shapiro.test(ump5$X2)
shapiro.test(ump5$X3)
shapiro.test(ump5$X4)
shapiro.test(ump5$X5)
shapiro.test(ump5$X6)
shapiro.test(ump5$X7)
shapiro.test(ump5$X8)
shapiro.test(ump5$X9)
shapiro.test(ump5$X10)
shapiro.test(ump5$X11)
shapiro.test(ump5$X12)
shapiro.test(ump5$X13)
#Only X8 and X11 are normal again according to shapiro.wilk
pred <- predict(ump.lm)
plot(ump5$Y, pred)
abline(0, 1)


#Checking normality
#All data
qqnorm(ump$X1)
qqline(ump$X1)
shapiro.test(ump$X1)

qqnorm(ump$X2)
qqline(ump$X2)
shapiro.test(ump$X2)

qqnorm(ump$X3)
qqline(ump$X3)
shapiro.test(ump$X3)

qqnorm(ump$X4)
qqline(ump$X4)
shapiro.test(ump$X4)

qqnorm(ump$X5)
qqline(ump$X5)
shapiro.test(ump$X5)

qqnorm(ump$X6)
qqline(ump$X6)
shapiro.test(ump$X6)

qqnorm(ump$X7)
qqline(ump$X7)
shapiro.test(ump$X7)

qqnorm(ump$X8)
qqline(ump$X8)
shapiro.test(ump$X8)

qqnorm(ump$X9)
qqline(ump$X9)
shapiro.test(ump$X9)

qqnorm(ump$X10)
qqline(ump$X10)
shapiro.test(ump$X10)

qqnorm(ump$X11)
qqline(ump$X11)
shapiro.test(ump$X11)

qqnorm(ump$X12)
qqline(ump$X12)
shapiro.test(ump$X12)

qqnorm(ump$X13)
qqline(ump$X13)
shapiro.test(ump$X13)


#Averaged Data
qqnorm(ump2$X1)
qqline(ump2$X1)
shapiro.test(ump2$X1)

qqnorm(ump2$X2)
qqline(ump2$X2)
shapiro.test(ump2$X2)

qqnorm(ump2$X3)
qqline(ump2$X3)
shapiro.test(ump2$X3)

qqnorm(ump2$X4)
qqline(ump2$X4)
shapiro.test(ump2$X4)

qqnorm(ump2$X5)
qqline(ump2$X5)
shapiro.test(ump2$X5)

qqnorm(ump2$X6)
qqline(ump2$X6)
shapiro.test(ump2$X6)

qqnorm(ump2$X7)
qqline(ump2$X7)
shapiro.test(ump2$X7)

qqnorm(ump2$X8)
qqline(ump2$X8)
shapiro.test(ump2$X8)

qqnorm(ump2$X9)
qqline(ump2$X9)
shapiro.test(ump2$X9)

qqnorm(ump2$X10)
qqline(ump2$X10)
shapiro.test(ump2$X10)

qqnorm(ump2$X11)
qqline(ump2$X11)
shapiro.test(ump2$X11)

qqnorm(ump2$X12)
qqline(ump2$X12)
shapiro.test(ump2$X12)

qqnorm(ump2$X13)
qqline(ump2$X13)
shapiro.test(ump2$X13)


#Unique data frames for each umpire with all of their data
umpire_names <- unique(ump$umpire)
for (umpire_name in umpire_names) {
  df_name <- paste(umpire_name, ".df", sep = "")
  assign(df_name, ump[ump$umpire == umpire_name, ])
}

#Individual umpire data averaged
averages_list <- list()
for (umpire_name in umpire_names) {
  df_name <- paste(umpire_name, ".df", sep = "")
  current_df <- ump[ump$umpire == umpire_name, ]
  
  averages_list[[df_name]] <- aggregate(cbind(home_team_runs, away_team_runs, pitches_called, incorrect_calls, expected_incorrect_calls, correct_calls, expected_correct_calls, correct_calls_above_expected, accuracy, expected_accuracy, accuracy_above_expected, consistency, favor_home, total_run_impact) ~ umpire, data = current_df, FUN = mean)
}

#Data frame with each all umpires and averaged data
average_umpire_data <- data.frame()
for (umpire_name in umpire_names) {
  current_df <- ump[ump$umpire == umpire_name, ]
  avg_data <- aggregate(cbind(home_team_runs, away_team_runs, pitches_called, incorrect_calls, expected_incorrect_calls, correct_calls, expected_correct_calls, correct_calls_above_expected, accuracy, expected_accuracy, accuracy_above_expected, consistency, favor_home, total_run_impact) ~ umpire, data = current_df, FUN = mean)
  avg_data$umpire_name <- umpire_name
  average_umpire_data <- rbind(average_umpire_data, avg_data)
}


#Testing to see which subsets work best
## Trying stuff with all data
ump2 <- subset(ump, select = -umpire)
names(ump2) <- c('X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8', 'X9', 'X10', 'X11', 'X12', 'X13', 'Y')
ump_scaled <- scale(ump2)


library(leaps)
#adjr2
regsub <- regsubsets(Y ~ ., data = ump_scaled)
summary(regsub)
plot(regsub, scale = 'adjr2')
best_subset_adjr2 <- which.max(summary(regsub)$adjr2)
best_subset_adjr2
# 7 subsets
coefficients_adjr2 <- coef(regsub, id = best_subset_adjr2)
coefficients_adjr2
# Y = -0.691106893 + 0.008514047X1 + 0.002181240X2 + 0.129090433X4 + 0.001090528X12 + 0.003941241X6 -0.003477565X8
#Says X3 should be included but its tiny 0.0000...

#cp
plot(regsub, scale = 'Cp')
best_subset_cp <- which.min(summary(regsub)$cp)
best_subset_cp
# 7 subsets
coefficients_cp <- coef(regsub, id = best_subset_cp)
coefficients_cp
# Y = -0.691106893 + 0.008514047X1 + 0.002181240X2 + 0.129090433X4 + 0.001090528X12 + 0.003941241X6 -0.003477565X8
#Says X3 should be included but its tiny 0.0000...

#backward
full.model <- lm(Y~., data = ump2)
step(full.model, direction = 'backward')
# Y = -6.8132643 + 0.0084389X1 + 0.0023858X2 + 0.0006799X3 + 0.1677282X4 + 0.0673885X9 -0.0062533X11 + 0.0259794X13

#forward
start.model <- lm(Y~1, data = ump2)
step(start.model, direction = 'forward', scope = formula(full.model))
# Y = -6.7683982 + 0.0084417X1 + 0.0023905X2 + 0.1681118X4 + 0.0007035X7 + 0.0669037X9 - 0.0051894X11 + 0.0259807X13 

#both
step(start.model, direction = 'both', scope = formula(full.model))
# Y = -6.7683982 + 0.0084417X1 + 0.0023905X2 + 0.1681118X4 + 0.0007035X7 + 0.0669037X9 - 0.0051894X11 + 0.0259807X13






## Trying stuff with averaged data
ump2 <- subset(average_umpire_data, select = -umpire)
names(ump2) <- c('X1', 'X2', 'X3', 'X4', 'X5', 'X6', 'X7', 'X8', 'X9', 'X10', 'X11', 'X12', 'X13', 'Y')

library(leaps)
#adjr2
regsub <- regsubsets(Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13, data = ump2)
summary(regsub)
plot(regsub, scale = 'adjr2')
best_subset_adjr2 <- which.max(summary(regsub)$adjr2)
best_subset_adjr2
# 7 subsets
coefficients_adjr2 <- coef(regsub, id = best_subset_adjr2)
coefficients_adjr2
# Y = -2.44452959 + 0.02623155X1 + 0.11700647X4 - 0.22050790X6 + 0.22564328X7 + 0.32370884X11 + 0.01889726 - 0.06619664

#cp
plot(regsub, scale = 'Cp')
best_subset_cp <- which.min(summary(regsub)$cp)
best_subset_cp
# 6 subsets
coefficients_cp <- coef(regsub, id = best_subset_cp)
coefficients_cp
# Y = -2.854766057 + 0.034500787X1 + 0.118435353X4 + 0.004886546X6 - 0.018580958X11 + 0.023065908X12 - 0.048767941X13
#Says X3 should be included but its tiny 0.0000...

#backward
full.model <- lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13, data = ump2)
step(full.model, direction = 'backward')
#6 subsets
# Y = -13.08058 + 0.02281X1 + 0.37729X4 - 0.18292X5 + 1.11696X9 - 0.98456X10 - 0.72389X11

#forward
start.model <- lm(Y~1, data = ump2)
step(start.model, direction = 'forward', scope = formula(full.model))
#6 subsets
# Y = -13.08058 + 0.02281X1 + 0.37729X4 - 0.18292X5 + 1.11696X9 - 0.98456X10 - 0.72389X11

#both
step(start.model, direction = 'both', scope = formula(full.model))
# 6 subsets
# Y = -13.08058 + 0.02281X1 + 0.37729X4 - 0.18292X5 + 1.11696X9 - 0.98456X10 - 0.72389X11

