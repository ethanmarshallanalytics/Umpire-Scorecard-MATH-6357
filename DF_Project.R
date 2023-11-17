library(tidyverse)
library(ALSM)
library(dplyr)
library(leaps)
library(caret)

df <- data.frame(read.csv('mlb-umpire-scorecard.csv'))
df[,6:19] <- sapply(df[,6:19], as.numeric) # convert chr to numeric
str(df)

df.drop <- drop_na(df) # drop missing values
df.numeric <- df.drop[6:19]
full.lm <- lm(log(total_run_impact)~.,data = df.numeric) # build model on log of y

# run best subsets to determine predictors
lm.best <- regsubsets(total_run_impact~., data = df.numeric, method = 'exhaustive')
plot(lm.best$rss)

# K-folds CV to estimate out of sample error
for(j in 1:k){
  # Fit the model with each subset of predictors on the training part of the fold
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=j,], nvmax=19) 
  # For each subset
  for(i in 1:19){
    # Predict on the hold out part of the fold for that subset
    pred=predict(best.fit, Hitters[folds==j,],id=i)
    # Get the mean squared error for the model trained on the fold with the subset
    cv.errors[j,i]=mean((Hitters$Salary[folds==j]-pred)^2)
  }
}
