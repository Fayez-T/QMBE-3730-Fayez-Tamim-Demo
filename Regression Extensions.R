library(ggplot2)
library(lmtest)
library(MASS)
library(car)
library(readxl)

#1
wages <- read_excel("wages.xlsx")
View(wages)

View(wages)

summary(wages)

sum(is.na(wages))

ggplot(wages, aes(x = Age, y = Wage)) +
  geom_point(alpha = 0.5) +
  ggtitle("Scatterplot: Age vs Wage") +
  xlab("Age") +
  ylab("Wage") +
  theme_minimal()

#B 

#LR
model_lin <- lm(Wage ~ Age + Educ, data = wages)
summary(model_lin)


#C

#QR
model_quad <- lm(Wage ~ Age + I(Age^2) + Educ, data = wages)
summary(model_quad)


#2
Ann <- read_excel("AnnArbor.xlsx")
View(Ann)
summary(Ann)

# Log
hist(Ann$Rent)

#Log
ggplot(Ann, aes(x = Beds, y = Rent)) +
  geom_point(alpha = 0.5) +
  ggtitle("Scatterplot: Beds vs Rent") +
  xlab("Beds") +
  ylab("Rent") +
  theme_minimal()

#Linear
ggplot(Ann, aes(x = Baths, y = Rent)) +
  geom_point(alpha = 0.5) +
  ggtitle("Scatterplot: Baths vs Rent") +
  xlab("Baths") +
  ylab("Rent") +
  theme_minimal()


#Linear
ggplot(Ann, aes(x = Sqft, y = Rent)) +
  geom_point(alpha = 0.5) +
  ggtitle("Scatterplot: Sqft vs Rent") +
  xlab("Sqft") +
  ylab("Rent") +
  theme_minimal()


model <- lm(
  log(Rent) ~ log(Sqft) + Beds + Baths,
  data = Ann
)

summary(model)


