# Load the data
library("readr")
cal_data<- read.csv(file.choose(), header = T)
View(cal_data)

# Exploratory data analysis
summary(cal_data)

install.packages("Hmisc")
library(Hmisc)
describe(cal_data)


#install.packages("lattice")
library("lattice") # dotplot is part of lattice package

# Graphical exploration
dotplot(cal_data$Weight.gained..grams., main = "Dot Plot of weight")
dotplot(cal_data$Calories.Consumed, main = "Dot Plot of calories consumed")


boxplot(cal_data$Weight.gained..grams., col = "dodgerblue4")
boxplot(cal_data$Calories.Consumed, col = "red", horizontal = T)

hist(cal_data$Weight.gained..grams.)
hist(cal_data$Calories.Consumed)

# Normal QQ plot
qqnorm(cal_data$Weight.gained..grams.)
qqline(cal_data$Weight.gained..grams.)

qqnorm(cal_data$Calories.Consumed)
qqline(cal_data$Calories.Consumed)

hist(wc.at$Waist, prob = TRUE)          # prob=TRUE for probabilities not counts
lines(density(cal_data$Weight.gained..grams.))             # add a density estimate with defaults
lines(density(cal_data$Weight.gained..grams., adjust = 2), lty = "dotted")   # add another "smoother" density

hist(cal_data$Calories.Consumed, prob = TRUE)          # prob=TRUE for probabilities not counts
lines(density(cal_data$Calories.Consumed))             # add a density estimate with defaults
lines(density(cal_data$Calories.Consumed, adjust = 2), lty = "dotted")   # add another "smoother" density

# Bivariate analysis
# Scatter plot
plot(cal_data$Weight.gained..grams., cal_data$Calories.Consumed, main = "Scatter Plot", col = "Dodgerblue4", 
     col.main = "Dodgerblue4", col.lab = "Dodgerblue4", xlab = "weight", 
     ylab = "calories consumed", pch = 20)  # plot(x,y)

?plot

## alternate simple command
plot(cal_data$Weight.gained..grams.,cal_data$Calories.Consumed)

attach(cal_data)

# Correlation Coefficient
cor(Weight.gained..grams.,Calories.Consumed)

# Covariance
cov(Weight.gained..grams.,Calories.Consumed)

# Linear Regression model
reg <- lm(Weight.gained..grams.~  Calories.Consumed, data = cal_data) # Y ~ X
summary(reg)

confint(reg, level = 0.95)


pred <- predict(reg, interval = "predict")
pred <- as.data.frame(pred)

View(pred)


# ggplot for adding Regression line for data
library(ggplot2)

ggplot(data = cal_data, aes(Weight.gained..grams.,Calories.Consumed) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ x)

# Alternate way
ggplot(data = wc.at, aes(x = Waist, y = AT)) + 
  geom_point(color = 'blue') +
  geom_line(color = 'red', data = wc.at, aes(x = Waist, y = pred$fit))

# Evaluation the model for fitness 
cor(pred$fit, Calories.Consumed)

reg$residuals
rmse <- sqrt(mean(reg$residuals^2))
rmse


# Transformation Techniques

# input = log(x); output = y

plot(log(Weight.gained..grams.), Calories.Consumed)
cor(log(Weight.gained..grams.), Calories.Consumed)

reg_log <- lm(Calories.Consumed ~ log(Weight.gained..grams.), data = cal_data)
summary(reg_log)

confint(reg_log,level = 0.95)
pred <- predict(reg_log, interval = "predict")

pred <- as.data.frame(pred)
cor(pred$fit, Calories.Consumed)

rmse <- sqrt(mean(reg_log$residuals^2))
rmse

# Regression line for data
ggplot(data = cal_data, aes(log(Weight.gained..grams.), Calories.Consumed) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ log(x))





# Log transformation applied on 'y'
# input = x; output = log(y)
input=Weight.gained..grams.
output=Calories.Consumed
plot(Weight.gained..grams., log(Calories.Consumed))
cor(input, log(output))

reg_log1 <- lm(log(output) ~ input, data = cal_data)
summary(reg_log1)

predlog <- predict(reg_log1, interval = "predict")
predlog <- as.data.frame(predlog)
reg_log1$residuals
sqrt(mean(reg_log1$residuals^2))

pred <- exp(predlog)  # Antilog = Exponential function
pred <- as.data.frame(pred)
cor(pred$fit, output)

res_log1 = output- pred$fit
rmse <- sqrt(mean(res_log1^2))

rmse

# Regression line for data
ggplot(data = cal_data, aes(input, log(output)) ) +
  geom_point() + stat_smooth(method = lm, formula = log(y) ~ x)




# Non-linear models = Polynomial models
# input = x & x^2 (2-degree) and output = log(y)

reg2 <- lm(log(output) ~ input + I(input*input), data = cal_data)
summary(reg2)

predlog <- predict(reg2, interval = "predict")
pred <- exp(predlog)

pred <- as.data.frame(pred)
cor(pred$fit, output)

res2 = output - pred$fit
rmse <- sqrt(mean(res2^2))
rmse

# Regression line for data
ggplot(data = cal_data, aes(input, log(output)) ) +
  geom_point() + stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))



# Data Partition

# Random Sampling
n <- nrow(cal_data)
n1 <- n * 0.8
n2 <- n - n1

train_ind <- sample(1:n, n1)
train <- cal_data[train_ind, ]
test <-  cal_data[-train_ind, ]

# Non-random sampling
train <- cal_data[1:10, ]
test <- cal_data[11:14, ]

plot(train$Weight.gained..grams., log(train$Calories.Consumed))
plot(test$Weight.gained..grams., log(test$Calories.Consumed))

model <- lm(log(Calories.Consumed) ~ Weight.gained..grams. + I(Weight.gained..grams. * Weight.gained..grams.), data = train)
summary(model)

confint(model,level=0.95)

log_res <- predict(model,interval = "confidence", newdata = test)

predict_original <- exp(log_res) # converting log values to original values
predict_original <- as.data.frame(predict_original)
test_error <- test$AT - predict_original$fit # calculate error/residual
test_error

test_rmse <- sqrt(mean(test_error^2))
test_rmse

log_res_train <- predict(model, interval = "confidence", newdata = train)

predict_original_train <- exp(log_res_train) # converting log values to original values
predict_original_train <- as.data.frame(predict_original_train)
train_error <- train$AT - predict_original_train$fit # calculate error/residual
train_error

train_rmse <- sqrt(mean(train_error^2))

train_rmse
