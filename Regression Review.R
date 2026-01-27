library(ggplot2)
library(lmtest)
library(MASS)
library(car)

#1 Correlation means that two variables are related or move together, but it does not mean that one causes the other.
   # Causation means that one variable directly causes a change in another.

#Example:
   #Ice cream sales and drowning incidents both increase in the summer.
   #They are correlated.
   #Ice cream sales do not cause drowning.
   #No causation

# Load data
data <- read.csv("C:\\Users\\fayez\\OneDrive\\Documents\\مدرسة\\Hamline\\Year 4\\Spring\\ADA\\Regression Review\\coffee_shop_revenue.csv") # add your file path here

View(data)

summary(data)

sum(is.na(data))

#2 Scatter plot
ggplot(data, aes(x = Number_of_Customers_Per_Day, y = Daily_Revenue)) +
  geom_point(alpha = 0.5) +
  ggtitle("Scatterplot: Number of Customers vs Daily Revenue") +
  xlab("Number of Customers Per Day") +
  ylab("Daily Revenue ($)") +
  theme_minimal()

# Yes, there is a linear relationship

#Calculate correlation
correlation_matrix <- cor(data[, 1:6], method = "pearson", use = "pairwise.complete.obs")
print(correlation_matrix)

#3 simple linear regression model
model <- lm(Daily_Revenue ~ Number_of_Customers_Per_Day, data = data)
summary(model)

cat("\nInterpretation:\n")
cat("Intercept (β₀): $", round(coef(model)[1], 2), "\n")
cat("Slope (β₁): $", round(coef(model)[2], 3), "\n")
cat("For each additional customer, revenue increases by approximately $", 
    round(coef(model)[2], 3), "\n")

# The model predicts that if there were 0 customers that showed up, daily revenue would be $393.85
# And for every customer that shows up revenue will go up $5.55

# 4 The R^2 of the model is 0.5424 
# Meaning that about 54% of the variation in daily revenue is explained by the number of customers per day.
# The remaining 45.76% is explained by other factors not in the model.

# 5 
# Residual analysis
par(mfrow = c(1, 2))
plot(model$fitted.values, model$residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

hist(model$residuals, main = "Histogram of Residuals",
     xlab = "Residuals", breaks = 30)

# Durbin-Watson test
dw_test <- dwtest(model)
dw_test$statistic
cat("\nDurbin-Watson test p-value:", dw_test$p.value, "\n")

#The residuals appear randomly scattered around zero with no obvious pattern, suggesting the linear model is appropriate.
#The Durbin-Watson statistic (~2.0) indicates no significant autocorrelation.

# 6 Multiple regression
model_multi <- lm(Daily_Revenue ~ ., data = data)
summary(model_multi)

# Significant predictors
sig_vars <- names(which(summary(model_multi)$coefficients[,4] < 0.05))
cat("\nSignificant predictors (p < 0.05):\n")
print(sig_vars)


# Number of Customers per Day, Average Order Value, Operating Hours per Day, Location Foot traffic.
# These predictors are significant as all thier p-values are <.05


# 7 Feature scaling
data_scaled <- as.data.frame(scale(data[, -which(names(data) == "Daily_Revenue")]))
data_scaled$Daily_Revenue <- data$Daily_Revenue

# Feature scaling is necessary to put variables on the same scale ensuring that algorithms treat all variables equally.

#8 Multicollinearity is where two or more independent variables are highly correlated
   # It can be detected with a VIF > 5 or 10
   # Or when the coefficients are above 0.7 or 0.8
# You can remove, combine the variables or increase the sample size.

# Scaled model
model_scaled <- lm(Daily_Revenue ~ ., data = data_scaled)
summary(model_scaled)

# 9 Stepwise selection
model_full <- lm(Daily_Revenue ~ ., data = data)
model_step <- step(model_full, direction = "both", trace = FALSE)
summary(model_step)
cat("\nStepwise selected variables:\n")
print(names(coef(model_step))[-1])

# 10 Adjusted R^2 = .8913. Provides a more realistic measure of fit. Prevents over fitting by decreasing when useless variables are added

# Heteroscedasticity tests
install.packages("lmtest")
library(lmtest)
bptest(model_multi)

# 11 It causes inefficient, unreliable, and incorrect standard errors. Which can mess with things like the F-test to see whether the regression model as a whole was significant.

#12 VIF calculation
install.packages("car")
library(car)

vif_values <- vif(model_multi)
print("Variance Inflation Factors:")
print(vif_values)

# ll VIF values are extremely close to 1
# So that means none of the predictors are meaningfully correlated with the others
# The regression coefficients should be stable and reliable

# 13 The multiple linear regression model is the better model compared to the simple linear regression
# The multiple regression has a higher R^2 and adjusted R^2
# No multicollinearity issues.VIF values were close to 1 for all predictors
# Feature Scaling was applied 


