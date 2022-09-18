# question 1
# Create user defined functions to perform various arithmetic 
# operations and call each functions using menu driven format 
# (try functions with and without parameters, functions with default argument)

calc <- function(a, b = 10, operator = '+'){
  
  add <- function(x, y) { return(x + y) }
  sub <- function(x, y) { return(x - y) }
  mul <- function(x, y) { return(x * y) }
  div <- function(x, y) { return(x / y) }
  pow <- function(x, y) { return(x ^ y) }
  mod <- function(x, y) { return(x %% y) }
  
  switch(operator,
    '+'= cat(a, operator , b, "=", add(a, b)),
    '-'= cat(a, operator , b, "=", sub(a, b)),
    '*'= cat(a, operator , b, "=", mul(a, b)),
    '/'= cat(a, operator , b, "=", div(a, b)),
    '^'= cat(a, "^", b, "=", pow(a, b)),
    'mod'= cat(a, "mod", b, "=", mod(a, b)),
   )
}

val1 <- as.integer(readline("Enter first number :: "))
val2 <- as.integer(readline("Enter second number :: "))

opn <- readline("Enter operatorion(+, -, *, /, mod, ^) :: ")

#normal function call
calc(val1, val2, opn)

#using default parameters
calc(val1, operator = opn)

# generate vector of 100 random elements using runif
ve1 <- sample(-10:10, size = 100, replace = TRUE)
ve1

# generate vector of 100 random elements using runif
ve <- runif(100, min = -10, max = 10)
ve

mean(ve)

median(ve)

range(ve)

IQR(ve)

sd(ve)

summary(ve)

hist(ve)

hist(ve, col ='blue', border = 'yellow')

table(ve1)

rbind(one_to10 = ve1[1: 10],
  ten_to20 = ve1[11:20],
  twenty_to30 = ve1[21: 30],
  thirty_to40 = ve1[31:40],
  fourty_to50 = ve1[41:50])
  

# custom function to get mode
getMode <- function(v){
  uniq <- unique(v)
  return(uniq[which.max(tabulate(match(v, uniq)))])
}

getMode(ve1)

# create a data frame
# a.	Create a data frame
# b.	Access a component ([, [[, $)
# c.	Structure of data frame
# d.	Add new column
# e.	Add new row 
# f.	Delete column
# g.	Delete specific row
# h.	Order data frame (with, order, arrange)

dates <- c(as.Date('09-JUN-68', format='%d-%b-%y'),
           as.Date('07-FEB-73', format='%d-%b-%y'),
           as.Date('08-DEC-72', format='%d-%b-%y'),
           as.Date('20-JUN-83', format='%d-%b-%y'),
           as.Date('09-JAN-87', format='%d-%b-%y'),
           as.Date('15-SEP-85', format='%d-%b-%y')
  )

df <- data.frame(
  sn=seq(1:6),
  name=c('Doug','Joyce', 'Frankin', 'Jennifer', 'John', 'Ramesh'),
  ssn_number=c(123, 124, 125, 564, 678, NA),
  birthday=dates,
  address=c('Chennai', 'Vellore', 'Delhi', 'Chennai', NA, 'Bangalore'),
  sex=c('M', 'F', 'M', NA, 'M', 'M'),
  salary=c(80000, NA, 40000, 43000, 30000, 38000)
)
df

#summary of df
str(df)

# access components
df$'salary'

df[['salary']]

df['salary']

#add new col
df$'Dept No' <- c(1, 1, 2, 2, 1, 3)
df

# add new row
df[nrow(df) + 1,] = c(7, "Aaditya", 234, NA, 
                        "Kathmandu", "M", 80000, 3)
df

# delete column
library(dplyr)
newDf <- select(df, 1:7)
newDf

# or
newDf <- subset(df, select = -8)
newDf


#delete specific row i.e. employees living in chennai

#using filter
newDf <- df %>% filter(address != 'Chennai')
newDf

#using indexing
newDf <- df[df$address != 'Chennai',]
newDf

#use subset
newDf <- subset(df, address != 'Chennai')
newDf


#order data using arrange(by row)

#arrange by descending salary and ascending birthday
arrange(df, desc(salary), birthday)

# order by department number and decreasing salary
dept_order <- df[order(df$'Dept No', desc(df$salary)),]
dept_order


#read air quality dataset
library(datasets)

airdf <- datasets::airquality
head(airdf)


# drop rows with na
rowCleanedDf <- na.omit(airdf)
head(rowCleanedDf)


# drop a column having NA
colCleanedDf <- airdf[, colSums(is.na(airdf)) == 0]
head(colCleanedDf)

colCleanedDf <- airdf %>% select_if(~ !any(is.na(.)))
head(colCleanedDf)


# replace NA
# replace NA with default value
x <- airdf
x[is.na(x)] = 0
head(x)


library(tidyr)

# replace with mean
meanReplaced <- airdf

for(i in 1:ncol(meanReplaced) ){
  if( is.numeric(meanReplaced[,i]) ){
    meanReplaced[is.na(meanReplaced[,i]), i] = mean(meanReplaced[,i], na.rm = TRUE)
  }
}

head(meanReplaced)

# or
meanReplaced <- airdf

meanReplaced <- meanReplaced %>% mutate_all(
  ~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x) 
)

head(meanReplaced)

# group data by month
library(zoo)
library(data.table)
setDT(airdf)

cols <- names(airdf)

meanReplacedDf <- airdf[, (cols) := lapply(.SD, na.aggregate)
    , by = Month
    , .SDcols = cols][]
head(meanReplacedDf)


#interpolation of missing values
airdf <- airdf %>% mutate(Ozone = na.approx(Ozone))
airdf
