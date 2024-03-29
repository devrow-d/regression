
install.packages("readxl")
install.packages("dplyr")
library(readxl)
library(dplyr)


install.packages("ggplot2")
install.packages("psych")
install.packages("Hmisc")
install.packages("ggthemes")
install.packages("extrafont")
install.packages("caret")
install.packages("car")

library(ggplot2)
library(tidyverse)
library(psych)
library(Hmisc)
library(ggthemes)
library(extrafont)
library(scales)
library(caret)
library(car)


setwd("F:/!MSc Business Analytics/MGT7177 Statistics for Business/!_Assignment 2")
getwd()

train <- read_excel("bank_train.xlsx")
test <- read_excel("bank_test.xlsx")
data <- rbind(test, train, deparse.level = 1)
summary(data)

classTable <- t(data.frame(lapply(data, class)))
classTable1 <- t(data.frame(lapply(data, class)))

### convert multiple variables to factor ###
data <- data %>% mutate_if(is.character, as.factor)
summary(data$contact)

## another way to change all characters to factors is
# data[3:11] <- lapply(data[3:11], factor)
# summary(data[3:11])


### AGE ###
summary(data$age)
sum(data$age > 100)
data$age[data$age > 100] <- mean(data$age)
data$age[data$age < 16] <- mean(data$age)

summary(data)

### contact ###
summary(data$contact)
data$contact[data$contact == "mobile"] <- "cellular"
data$contact <- droplevels(data$contact)

### month ###
### arrange months in order
data$month = factor(data$month, levels(data$month)[c(6,1,7,5,4,2,10,9,8,3)])
summary(data$month)

### days of week ###
### change and add 'Friday' data to 'fri'
### remove 'Friday' from the data
### arrange days of the week in order
summary(data$day_of_week)
data$day_of_week[data$day_of_week == 'Friday'] <- 'fri'
data$day_of_week <- droplevels(data$day_of_week)
data$day_of_week = factor(data$day_of_week, levels(data$day_of_week)[c(2,4,5,3,1)])
summary(data$day_of_week)


### duration ###
summary(data$duration)

outliersDuration <- boxplot(data$duration)$out
summary(outliersDuration)
sum(outliersDuration > 1800)

sum(data$duration > 1800)
data$duration[data$duration > 1800] <- mean(data$duration)

boxplot(data$duration)$out

### campaign ###
summary(data$campaign)


### Summary pdays ###
### remove 999 from data, replace with NA - 999 not previously contacted
summary(data$pdays)
sum(data$pdays == 999)
sum(data$pdays == 0)
sum(data$pdays == 'NA') #0 NA in original data
summary(data$pdays)

### Employment Variation Rate ###
summary(data$emp.var.rate)
plot(data$emp.var.rate)
barplot(data$emp.var.rate)

### Consumer Price Index ###
summary(data$cons.price.idx)
plot(data$cons.price.idx)

sum(data$cons.price.idx < 92)
sum(data$cons.price.idx > 95)
sum(data$cons.price.idx == 149)

### Remove outlier values from the CPI and apply mean value
outlierCPI <- boxplot(data$cons.price.idx)$out
data$cons.price.idx[data$cons.price.idx > 100] <- mean(data$cons.price.idx)
hist(data$cons.price.idx, breaks = 50)

#################################
### Consumer Confidence Index ###

summary(data$cons.conf.idx)
hist(data$cons.conf.idx, data$emp.var.rate, breaks = 20)

### Euribor 3 Month rate ###
summary(data$euribor3m)
plot(data$euribor3m)
plot(data$euribor3m ~ data$cons.conf.idx)

### Number of Employees ###
summary(data$nr.employed)
plot(data$nr.employed ~ data$campaign)
############################
### subscribed ###
### change the name of variable y to 'subscribed'
summary(data$y)
names(data)[names(data) == 'y'] <- "subscribed"
summary(data$subscribed)

summary(data)

##########################################################################################
########## Measures of Association ##########

#### Correlation #####

### CHI Sq Test ###

chisq.test(data$subscribed, data$marital, correct = F)
chisq.test(data$subscribed, data$job, correct = F)
chisq.test(data$subscribed, data$education, correct = F)
summary(data$education)
chisq.test(data$subscribed, data$default, correct = F)
summary(data$default)
chisq.test(data$subscribed, data$housing, correct = F)
chisq.test(data$subscribed, data$loan, correct = F)
chisq.test(data$subscribed, data$contact, correct = F)
chisq.test(data$subscribed, data$month, correct = F)
chisq.test(data$subscribed, data$day_of_week, correct = F)
chisq.test(data$subscribed, data$poutcome, correct = F)

t.test(data$age ~ data$subscribed, data=data)
t.test(data$duration ~ data$subscribed, data=data)
t.test(data$pdays ~ data$subscribed, data=data)
t.test(data$previous ~ data$subscribed, data=data)
t.test(data$emp.var.rate ~ data$subscribed, data=data)
t.test(data$cons.price.idx ~ data$subscribed, data=data)
t.test(data$cons.conf.idx ~ data$subscribed, data=data)
t.test(data$euribor3m ~ data$subscribed, data=data)
t.test(data$nr.employed ~ data$subscribed, data=data)

## why use spearmans below???
cor(data$age, data$emp.var.rate, use = "complete.obs", method = "spearman")
cor(data$age, data$emp.var.rate, use = "complete.obs", method = "pearson")
cor.test(data$emp.var.rate, data$cons.conf.idx)

###########################################
#### Visualisations ###

plot()
plot(data$euribor3m ~ data$nr.employed)
plot(data$euribor3m ~ data$emp.var.rate)
plot(data$age ~ data$subscribed)
scatter.smooth(data$age, data$subscribed)
plot(data$duration ~ data$campaign)
plot(data$campaign, data$nr.employed)
barplot(data$day_of_week, data$campaign)
plot(data$campaign, data$day_of_week)
hist(data$day_of_week)
plot(lm(data$day_of_week))
summary(data$day_of_week)

plot(data$euribor3m)
hist(data$subscribed ~ data$job)
summary(data$job)

### Visualisation 1 ###

### Age ~ Emp Var Rate ###
ggplot(data=data) +
  geom_point(mapping = aes(x = age, y = emp.var.rate, col=subscribed)) +
  labs(title="Customer Age ~ Employee Variable Rate",
       x="Age (years)",
       y="Emp Var Rate") +
  theme(legend.title=element_text(color="blue"),
        legend.background = element_rect(fill = "darkgray"),
        legend.position = "right")+
        facet_wrap(~ subscribed)

### Visualisation 2 ###

### cons.conf.idx ~ emp.var.rate ###

ggplot(data=data) +
  geom_point(mapping = aes(x = cons.conf.idx, y = emp.var.rate, color= subscribed, alpha =0.5)) +
  geom_smooth(mapping = aes(x = cons.conf.idx, y = emp.var.rate), method = lm, se=F, col="Red") +
  labs(title="Consumer Confidence Index v Emlpoyee Variable Rate",
       x="CCI",
       y="Emp Var Rate") +
  theme_par() +
  theme(legend.title=element_text(color="blue"),
        legend.background = element_rect(fill = "darkgray"),
        legend.position = "right") +
  scale_alpha(guide = "none")

##############################################
### Skewness & Kurtosis of the variable ###

psych::describe(data$age, na.rm=T, interp=F, skew=T)
psych::describe(log(data$age))


psych::describe(data$emp.var.rate, na.rm=T, interp=F, skew=T)

qqplot(data$age, data$cons.conf.idx)

psych::describe(data$emp.var.rate)


#######################################################
################  Logistic Regression  ################

################# Model 1 ####################

formula1 <- subscribed ~ age + marital + job + cons.conf.idx + euribor3m + nr.employed
formula1 <- subscribed ~ .

model1 <- glm(formula1, data=data, family ="binomial")
summary(model1)

## evaluate model assumptions
train$predictProbabilities <- fitted(model1)
summary(train$predictProbabilities)

predictions <- predict(model1, test, type ="response")
head(predictions)
summary(predictions)
class_pred <- as.factor(ifelse(predictions > 0.5, "Yes", "No"))
summary(class_pred)

exp(model1$coefficients)
summary(data$marital)
summary(data$job)

### Pseudo R Sqaured ###

logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <- length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
}

logisticPseudoR2s(model1)
#Source: Field, 2012


# check how many residuals
resid <- model1$residuals
plot(resid)

std_resid <- rstandard(model1)
sum(std_resid > 1.96) # must be less than 5% of the data
summary(std_resid) #check if there are any std residual above 3

large_resid <- std_resid > 2 | std_resid < -2
sum(large_resid)

library(car)
vif(model1)
mean(vif(model1))
1/(vif(model1))

train$cook <- cooks.distance(model1)
cooks <- cooks.distance(model1) #check for observatiosn having any undue influence on the model
sum(cooks > 1)

# add standardised residuals to the data
data$standardisedResid <- rstandard(model1)
sum(train$standardisedResiduals > 1.96) #should be less than 5% of the data

################  Split Data  ################

set.seed(123)

### create a 80/20 partition 
trainIndex <- createDataPartition(data$subscribed, p=0.8,
                                  list = FALSE,
                                  times = 1)

train <- data[trainIndex,]
test <- data[-trainIndex,]

summary(test$job)
summary(train$marital)

##############################################
################# Model 2 ####################

formula2 <- subscribed ~ age + marital + job + cons.conf.idx + euribor3m + nr.employed
formula2 <- subscribed ~ age + marital + job + cons.conf.idx + euribor3m + nr.employed

model2 <- train(formula2, data=train, method="glm")
model2a <- glm(formula2, data=train, family ="binomial")

predictions2 <- predict(model2, test)
summary(predictions2)
summary(model2)

## evaluate model assumptions
train$predictProbabilities <- fitted(model2)
summary(train$predictProbabilities)
exp(model2$coefficients)

postResample(predictions2, test$subscribed)

confusionMatrix(data = predictions2, test$subscribed)

### Pseudo R Sqaured ###

logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <- length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
}

logisticPseudoR2s(model2)
#Source: Field, 2012

# check how many residuals
resid2 <- model2$residuals
plot(resid2)

resid2a <- model2a$residuals
plot(resid2a)

std_resid2 <- rstandard(model2)
sum(std_resid2 > 1.96) # must be less than 5% of the data
summary(std_resid2) #check if there are any std residual above 3

large_resid2 <- std_resid2 > 2 | std_resid2 < -2
sum(large_resid2)

vif(model1)

train$cook <- cooks.distance(model2)
cooks <- cooks.distance(model2) #check for observatiosn having any undue influence on the model
sum(cooks > 1)


# add standardised residuals to the data
data$standardisedResid <- rstandard(model2)
sum(train$standardisedResiduals > 1.96) #should be less than 5% of the data


##############################################
################# Model 3 ####################

formula3 <- subscribed ~ age + job + month + day_of_week + contact + campaign + pdays + cons.conf.idx + nr.employed
formula3 <- subscribed ~ age + job + month + day_of_week + contact + campaign + pdays + cons.conf.idx + nr.employed + train$ageLogInt + train$pdaysLogInt + train$emplLogInt

model3 <- train(formula3, data=train, method="glm", na.action=na.omit)
model3a <- glm(formula3, data=train, family ="binomial")
summary(model3a)

predictions3 <- predict(model3, test)
summary(predictions3)

predictions3a <- predict(model3a, test)
summary(predictions3a)
class_pred3a <- as.factor(ifelse(predictions3a > 0.5, "Yes", "No"))
summary(class_pred3a)
summary(model3)

## evaluate model assumptions

postResample(predictions3, test$subscribed)
confusionMatrix(data = predictions3, test$subscribed)

## evaluate model assumptions
train$predictProbabilities <- fitted(model3)
summary(train$predictProbabilities)
exp(model3a$coefficients)

vif(model3a)
mean(vif(model3a))

### Pseudo R Sqaured ###

logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <- length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
}

logisticPseudoR2s(model3a)
#Source: Field, 2012

# check how many residuals
resid3 <- model3a$residuals
plot(resid3)

std_resid3 <- rstandard(model3a)
sum(std_resid3 > 1.96) # must be less than 5% of the data
summary(std_resid3) #check if there are any std residual above 3

large_resid2 <- std_resid2 > 2 | std_resid2 < -2
sum(large_resid2)

vif(model1)

train$cook <- cooks.distance(model3a)
cooks <- cooks.distance(model3a)

#check for observatiosn having any undue influence on the model
sum(cooks > 1)
summary(cooks)

# add standardised residuals to the data
data$standardisedResid <- rstandard(model2)
sum(train$standardisedResiduals > 1.96) #should be less than 5% of the data

durbinWatsonTest(model3a)

### Linearity of the Logit ###

train$ageLogInt <- log(train$age) * train$age
train$pdaysLogInt <- log(train$pdays) * train$pdays
train$cLogInt <- log(train$cons.conf.idx) * train$cons.conf.idx
train$emplLogInt <- log(train$nr.employed) * train$nr.employed

### confidence interval ###
exp(confint(model3a))

### odds ratio ###
exp(model3a$coefficients)

####################### END ################################


