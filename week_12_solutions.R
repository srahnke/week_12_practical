setwd("~/Desktop")
library(tidyverse)
library(readxl)
library(janitor)
library(plotrix)
library(lubridate)
library(emmeans)
library(cowplot)

survey_data<-read_tsv("https://raw.githubusercontent.com/mahdi-b/MBIO612-F2021/main/week_8/data/RIKZ.txt")
view(survey_data)


## Add a richness column to the RIKZ data frame
species_cols = 2:76
counts = apply(survey_data[, species_cols] > 0, 1, sum)
survey_data["richness"] = counts
survey_data_richness = survey_data[,-species_cols]
head(survey_data_richness, n=2)


#install.packages("leaps")
library(leaps)

regfit.full <- regsubsets(richness ~., data = survey_data_richness)
summary(regfit.full)  #indicates that NAP and chalk are the best variables 


## Use nvmax to return all variables 
regfit.full <- regsubsets(richness ~., data = survey_data_richness, nvmax = 15)
reg.summary <- summary(regfit.full)

names(reg.summary)
reg.summary$rsq


par(mfrow = c(2,2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l") 


which.max(reg.summary$adjr2)
points (6, reg.summary$adjr2[6], col = " red ", cex = 2, pch = 20)


## Indicate model with the smallest statistic, in this instance = 3 
plot (reg.summary$cp, xlab = " Number of Variables ",
ylab = "Cp", type = "l")
which.min(reg.summary$cp)

points (3, reg.summary$cp[3], col = " red ", cex = 2,
pch = 20)
which.min (reg.summary$bic)

plot(reg.summary$bic , xlab = " Number of Variables ",
ylab = " BIC ", type = "l")

points (3, reg.summary$bic[3], col = " red ", cex = 2,
pch = 20)

## Select variables for the best model
plot (regfit.full, scale = "r2")
plot (regfit.full, scale = "adjr2")
plot (regfit.full, scale = "Cp")
plot (regfit.full, scale = "bic")


coef (regfit.full, 3)
#Exposure, salinity, and NAP appear to be the best variables


## Forward and Backwards Stepwise Selection

regfit.fwd <- regsubsets (richness ~., survey_data_richness, nvmax = 15, method = "forward")
summary(regfit.fwd)

regfit.bwd <- regsubsets(richness ~., data = survey_data_richness, nvmax = 19, method = "backward")
summary (regfit.bwd)


## Looking at the difference between the top variables in each method
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

## Choosing among models using validation-set approach and cross-validation 
set.seed(1)
train <- sample (c(TRUE , FALSE), nrow (survey_data_richness), replace = TRUE)
test <- (!train)


regfit.best <- regsubsets (richness ~ ., data = survey_data_richness[train , ], nvmax = 15)
test.mat <- model.matrix (richness ~ ., data = survey_data_richness[test , ])


val.errors <- rep(NA, 14) 
for (i in 1:14) { 
  coefi<- coef(regfit.best, id=i)
  pred<- test.mat[, names(coefi)] %*% coefi
  val.errors[i] <- mean(survey_data_richness$richness[test] - pred)^2
  }

##Find how many varibales the best model has
val.errors
which.min(val.errors)
coef(regfit.best, 7)


##Create Prediction Function
predict.regsubsets <- function (object , newdata , id, ...) {
  form <- as.formula (object$call[[2]])
  mat <- model.matrix (form, newdata)
  coefi <- coef (object, id = id)
  xvars <- names (coefi)
  mat[, xvars] %*% coefi
}

regfit.best <- regsubsets (richness ~ ., data = survey_data_richness, nvmax = 15)
coef (regfit.best, 7)



k <- 10
n <- nrow (survey_data_richness)
set.seed (1)
folds <- sample ( rep (1:k, length = n))
cv.errors <- matrix (NA, k, 15,
dimnames = list (NULL , paste (1:15)))



for (j in 1:k) {
best.fit <- regsubsets (richness~.,
data = survey_data_richness[folds != j, ],
nvmax = 15)
for (i in 1:14) {
pred <- predict (best.fit, survey_data_richness[folds == j, ], id = i)
cv.errors[j, i] <-
mean ((survey_data_richness$richness[folds == j] - pred)^2)
}
}


mean.cv.errors <- apply(cv.errors , 2, mean)
mean.cv.errors
par (mfrow = c(1, 1))
plot (mean.cv.errors, type = "b")


reg.best <- regsubsets (richness~ ., data = survey_data_richness, nvmax = 15)
coef(reg.best, 10)

