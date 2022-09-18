library(dplyr)
library(mlbench)
library(corrplot)


data(BostonHousing)
hdata <- BostonHousing
str(hdata)

# find and handle NULL values
sum(is.na(hdata))

# pair plot on perhaps correlated values
pairs(hdata)


#exploratory data analysis
pairs(~ age + lstat + medv + crim + tax, hdata, 
      main = "Exploratory analysis on Boston Data")

corrplot(cor(select(hdata, -chas)))

# training data
# we split the data into 70% training and 30% testing
library(caTools)
splits <- sample.split(hdata[, 1], SplitRatio = 0.7)
data.train <- subset(hdata, splits == TRUE)
data.test <- subset(hdata, splits == FALSE)

head(train)
head(test)

# train using lm
pricePredicts <- lm(medv ~ rm + rad + lstat + tax, data.train)
summary(pricePredicts)

# predicted vals
data.test$predict = predict(pricePredicts, data.test)

cat("Difference in predicted and actual value")
round(data.test$predict - data.test$medv, 2)

library(ggplot2)
ggplot(data.test, aes(x=predict, y=medv)) + 
  geom_point() + 
  geom_smooth(method='lm') +
  labs(title="Predicted vs Actual price",
       x="Predicted price", y = "Actual price") +
  theme_classic()


