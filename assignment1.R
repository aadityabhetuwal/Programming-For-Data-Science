# print hello world
print("Hello World")
cat("Hello world")

# read number from prompt
num <- as.integer(readline("Enter any number :: "))
print(paste("Input number is ::", num))

#using scan
num <- scan()
print(paste("Input number is ::", num))


# perform operator on vectors
# 3. Input two vectors and perform following operations
# a. Arithmetic Operators (+, -, *, /, %%, %/%, ^)
# b. Relational Operators (<, >, <=, >=, ==, !=)
# c. Logical Operators (&, |, !, &&, ||, )
# d. Assignment Operators (left assignment, right assignment)
# e. Miscellaneous Operators (:, %in%, , %*%)
prompt <- "Enter vector elements :: "

ve1 <- scan()
ve1
ve2 <- scan()


ve1 <- as.integer(strsplit(readline(prompt), ' ')[[1]])
ve2 <- as.integer(strsplit(readline(prompt), ' ')[[1]])

print("Vector 1 :")
print(ve1)
print("Vector 2 :")
print(ve2)

print("Addition operation : ")
print(ve1 + ve2)

print("Subtraction operation : ")
print(ve1 - ve2)

print("Multlipication operation : ")
print(ve1 * ve2)

print("Division operation : ")
print(ve1 / ve2)

print("Modulo operation : ")
print(ve1 %% ve2)

print("Floor Division operation : ")
print(ve1 %/% ve2)

v1 <- c(1, 2, 3, 4)
v2 <- c(2, 2, 3, 3)
print("Power operation : ")
print(v1 ^ v2)


# relational operations on vectors
ve1 <- as.integer(strsplit(readline(prompt), ' ')[[1]])
ve2 <- as.integer(strsplit(readline(prompt), ' ')[[1]])

print(ve1)
print(ve2)

print("Less than (<) operator : ")
print(ve1 < ve2)

print("Greater than (>) operator : ")
print(ve1 > ve2)

print("Less than or equal to(<=) operator : ")
print(ve1 <= ve2)

print("Greater than or equal to (>=) operator : ")
print(ve1 >= ve2)

print("Equals (==) operator : ")
print(ve1 == ve2)

print("Not Equals (!=) operator : ")
print(ve1 != ve2)

# Logical operators

print("AND (&) operation : ")
print(ve1 & ve2)

print("OR (|) operation : ")
print(ve1 | ve2)

print("NOT (!) operation : ")
print(ve1 | ve2)

print("Logical AND (&&) operation : ")
print(ve1 && ve2)

print("Logical OR (||) operation : ")
print(ve1 || ve2)


# assignment operators in R
print("Assignment using equals (=)")
ve = as.integer(strsplit(readline(prompt), ' ')[[1]])
print(ve)

print("Assignment using left arrow (<-)")
ve <- as.integer(strsplit(readline(prompt), ' ')[[1]])
print(ve)

print("Assignment using right arrow (->)")
as.integer(strsplit(readline(prompt), ' ')[[1]]) -> ve
print(ve)

# Miscellaneous Operators (:, %in%, , %*%)
ve = c(1, 20, 35, -10)

print("%in% operator : ")
print(20 %in% ve)


# scalar operation on a vector
ve <- c(1, 2, 3, 4)

print("Scalar multipication (%*%) operation :: ")
print(2 %*% ve)

print("Range operator (:) ")
print(1:15)


# read a number and check odd or even
num <- as.integer(readline("Enter any number :: "))

if ( (num %% 2) == 1 ){
  print(paste(num, "is odd"))
}else{
  print(paste(num, "is even"))
}

# Read the mark of a student and print his /her grade (if…else…if)
marks <- as.integer(readline("Enter student marks : "))

print(paste("Entered marks :", marks))

if(marks >= 90){
  print("Student has obtained Grade S")
}else if(marks >= 80){
  print("Student has obtained Grade A")
}else if(marks >= 70){
  print("Student has obtained Grade B")
}else if(marks >= 60){
  print("Student has obtained Grade C")
}else if(marks >= 50){
  print("Student has obtained Grade D")
}else{
  print("Student has failed.")
}

# design arithmetic calculator
val1 <- as.integer(readline("Enter first number :: "))
val2 <- as.integer(readline("Enter second number :: "))

opn <- readline("Enter operatorion(+, -, *, /, mod, ^) :: ");

switch (opn,
        "+"= cat("Addition =", val1 + val2),  
        "-"= cat("Subtraction =", val1 - val2),  
        "/"= cat("Division = ", val1 / val2),  
        "*"= cat("Multiplication =", val1 * val2),
        "mod"= cat("Modulus =", val1 %% val2),
        "^"= cat("Power =", val1 ^ val2)
)


# find factorial of a number
num <- as.integer(readline("Enter any number :: "))
print(paste("Entered num : ", num))
ans = 1


for(i in 2:num){
  ans = ans * i;
}

print(paste("Factorial of", num, "is = ", ans))


# check if number is armstrong sum of cubes is equal to number itself
num <- as.integer(readline("Enter any number :: "))
print(paste("Entered num : ", num))
x = num
sumCubes = 0

while(num > 0){
  last = num %% 10;
  sumCubes = sumCubes + last ^ 3;
  num = num %/% 10
}

if(sumCubes == x){
  print(paste(x,  "is an armstrong number") )
}else{
  print(paste(x,  "is not an armstrong number") )
}

# repeat adding numbers till sum is less than hundred
n <- 1
sum = 0

repeat{
  sum = sum + n

  if(sum > 100){
    print(paste("Numbers to reach sum > 100 : ", n))
    break
  }
  
  n = n + 1
}

#break when given element is found
ve <- c(10, -30, 40, 50, 40)
target <- 40

for(i in ve){
  print(i)
  if(i == target){
    print(paste("Element found at ", i))
    break
  }
}

# count frequency of a given element in list
ve <- c(1, 2, 3, 3, 2, 3, 3, 4, 4)
t <- 2
count = 0

for(i in ve){
  if(i != t){
    next
  }
  print(i)
  count = count + 1
}
print(paste("Frequency of", t, "is =", count))