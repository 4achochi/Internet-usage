# Internet Usage - Multiple Linear Regression Analysis

# Load the Dataset
df <- read.csv("C:/Users/jutha/OneDrive - United States International University (USIU)/Desktop/USIU/3.1/STA3010XA/internetlinear.csv")

# Make gender a factor since it is coded 0/1
df$gender <- as.factor(df$gender)

str(df)
summary(df)

install.packages(c("car","lmtest","glmnet"))

# 1. Fit a Multiple Linear Regression Model -----------------------------

model1 <- lm(usage ~ age + gender + webpages + videohours + income, data = df)
summary(model1)


# 2. Linearity ------------------------------------------------------------
# Basic Scatter plots of Y against each numeric X

plot(df$age, df$usage, main = "Usage vs Age", xlab = "Age", ylab = "Usage", pch = 19, col = "blue")
plot(df$webpages, df$usage, main = "Usage vs Web Pages", xlab = "Web Pages", ylab = "Usage", pch = 19, col = "blue")
plot(df$videohours, df$usage, main = "Usage vs Video Hours", xlab = "Video Hours", ylab = "Usage", pch = 19, col = "blue")
plot(df$income, df$usage, main = "Usage vs Income", xlab = "Income", ylab = "Usage", pch = 19, col = "blue")

library(tidyverse)
pairs(~usage + age + webpages + videohours + income, data = df, main = "Scatter plot matrix")

# Residuals vs Fitted plot (checks linearity of the overall model)
plot(model1, which = 1)


# 3. Multicollinearity ----------------------------------------------------

library(car)
vif(model1)


# 4. Homoscedasticity -------------------------------------------------------

library(lmtest)
bptest(model1)


# 5. Multivariate Normality --------------------------------------------------
# Create Q-Q plot
qqnorm(residuals(model1))

# Add straight diagonal line to plot
qqline(residuals(model1), col = "red", lwd = 2, lty = 2)

# Shapiro-Wilk test
shapiro.test(residuals(model1))


# 6. Evaluate the Fitted Regression Model ----------------------------------

summary(model1)
confint(model1)
AIC(model1)
BIC(model1)

# Independence check (bonus, from your earlier script)
durbinWatsonTest(model1)

par(mfrow = c(2, 2))
plot(model1)
par(mfrow = c(1, 1))


# 7. Ridge and LASSO Regression --------------------------------------------

library(glmnet)

x <- model.matrix(usage ~ age + gender + webpages + videohours + income, data = df)[, -1]
y <- df$usage

# Ridge Regression (alpha = 0)
set.seed(123)
ridge_cv <- cv.glmnet(x, y, alpha = 0)
ridge_model <- glmnet(x, y, alpha = 0, lambda = ridge_cv$lambda.min)
coef(ridge_model)
plot(ridge_cv)

# LASSO Regression (alpha = 1)
set.seed(123)
lasso_cv <- cv.glmnet(x, y, alpha = 1)
lasso_model <- glmnet(x, y, alpha = 1, lambda = lasso_cv$lambda.min)
coef(lasso_model)
plot(lasso_cv)
