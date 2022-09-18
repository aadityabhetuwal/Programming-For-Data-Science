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
