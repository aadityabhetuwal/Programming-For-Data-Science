options(device = "windows")
dev.new()

#load iris data set
library(datasets)
iris = datasets::iris
head(iris)

# i. base R
# here species is a perfect label Encoder
library(dplyr)
tmp <- iris %>% 
    mutate(speciesEncoded = as.numeric(factor(Species)) )
slice(tmp, 48:52, 98:102)

# ii. using CatEncoders
library(CatEncoders)

tmp = iris
labels = LabelEncoder.fit(iris$Species)
tmp$speciesEncoded = transform(labels, tmp$Species)
slice(tmp, 48:52, 98:102)


# 2 perform one-hot encoding in iris dataset
# use dummyVars from caret package
library(caret)

labels <- dummyVars(" ~ .", data = iris)
tmp <- data.frame(predict(labels, newdata = iris))
slice(tmp, 48:52, 98:102)


# 3. feature scaling

# normalization of data
chkWeight = datasets::ChickWeight
head(chkWeight)

#using a log scale normalize DAX and CAC
tmp = log(chkWeight['weight'])
print(tmp[1:10])

# using min-max scaling
tmp <- preProcess(as.data.frame(chkWeight$weight), method=c("range"))

minMaxScaled <- predict(tmp, as.data.frame(chkWeight$weight))
print(minMaxScaled$`chkWeight$weight`[1:20])


# standard R scaling
tmp <- scale(chkWeight$weight)
print(tmp[1:20])


# b. Z scaling

# create generate random elements
ve <- sample(10:1000, size = 20, replace = TRUE)
print(ve)

avg <- mean(ve)
avg

sDev <- sd(ve)
sDev


# z scaling = (x - mean(x)) / sd(x)
zscaled = (ve - avg) / sDev
zscaled


# PCA of iris data set
iris <- datasets::iris
str(iris)

pairs(iris, col = c('red', 'orange', 'green')[iris$Species])

# log transform data 
iris.log <- log(select(iris, 1:4))
iris.log$Species <- iris$Species
str(iris.log)

# create historgram on iris dataset
par(mfrow=c(2, 2))
cols = names(iris.log)

for(i in 1:(ncol(iris.log) - 1)){
  hist(iris.log[[i]], main = paste("Histogram of", cols[i]), breaks = 10, xlab=cols[i])
}




# use prcomp
iris.pca <- prcomp(select(iris.log, -5), center = TRUE)
iris.pca

summary(iris.pca)

library(factoextra)
fviz_eig(iris.pca)

# create a correlation matrix
iris.corr <- cor(select(iris.log, -5))
iris.corr

library(reshape2)
iris.meltCorr <- melt(iris.corr)
head(iris.meltCorr)

library(ggplot2)
ggplot(data = iris.meltCorr, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() + geom_text(aes(Var2, Var1, label = value),
                         color = "black", size = 4)



### House price prediction

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






### SVM classification
x=read.csv(url('https://raw.githubusercontent.com/AlkhaMohan/Programming-for-Data-Science/main/Cancer_Data.csv'))

names(x)
x=x[-c(1,33)]
#Check NA in dataset
sum(is.na(x))
colSums(is.na(x))


#Label Encoder
library(superml)
label=LabelEncoder$new()
x$diagnosis=label$fit_transform(x$diagnosis)
head(x)

#Train-test split
library(caTools)
split=sample.split(x$diagnosis,SplitRatio =0.7)
train=subset(x,split==TRUE)
test=subset(x,split==FALSE)

dim(train)
dim(test)

#SVM
library(e1071)
train[-1]=scale(train[-1])
test[-1]=scale(test[-1])
names(train)

classifier = svm(formula = diagnosis ~ .,
                 data = train,
                 type = 'C-classification',
                 kernel = 'linear')

summary(classifier)

#Prediction
Diag_pred = predict(classifier, newdata = test[-1])
Diag_pred

# Making the Confusion Matrix
cm = table(test[,1], Diag_pred)
print(cm)
