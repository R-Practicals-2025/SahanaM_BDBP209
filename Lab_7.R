# Lab 7 Feb 14 2025
a = 5.0
b = 10.0

# if(a<b)
#   print("a is less than b.")

c = 15.0
d = 20.0

if(a>b){
  print("a is greater than b")
}else if(b<c){
  print("b is less than c")}

for (i in 1:5)print(i^2)


j=0
k=1
for (i in 1:5){
  j <- j+1
  k <- k+2
  print(paste(j,k))
}


fn1 <- function(x)x^2

func1 <- function(x){
  f <- 1
  if(x<2)return(1)
  for(i in 2:x){
    f <- f*i
  }
  return(f)
  }

print(func1(4))

print(sapply(0:5,func1))

fac2 <- function(x){
  f <- 1
  t <- x
  while(t>1){
    f <- f * t
    t <- t-1}
  return(f)
  }
print(fac2(4))

fac3 <- function(x){
  f <- 1
  t <- x
  repeat{
    if (t<2)break
      f <- f*t
      t <- t-1
  }
      return(f)
  }

print(fac3(4))
print(cumprod(1:4))
print(max(cumprod(1:4)))

fac5 <- function(x) gamma(x+1)

print(fac5(4))

pc <- proc.time()

result <- func1(100000)
print(result)
time <- proc.time()-pc

print(time)

x <- runif(1000000000)
pc <- proc.time()
cmax <- x[1]
for(i in 1000000000){
  if (x[i]>cmax)cmax<-x[i]
}
time <- proc.time()-pc
print(time)


#  ex1:

amat <- matrix(data = seq(10, 120, by = 10), nrow = 3, ncol = 4)
print(amat)

amat1 <- matrix(data = seq(10, 120, by = 10), nrow = 3, ncol = 4, byrow = TRUE)
print(amat1)

#  amat is transpose matrix of amat1

rownames(amat) <- c('R1', 'R2', 'R3')
colnames(amat) <- c('C1', 'C2', 'C3', 'C4')
print(rownames(amat))
print(colnames(amat))
print(amat)

A <- matrix(c(2, 5, 7, 3, 1, 8, 9, 10, 1, 12, 5, 10, 4, 17, 15, 11), nrow = 4, ncol = 4, byrow = TRUE)
B <- matrix(c(12, 5, 3, 17, 1, 18, 9, 10, 1, 12, 5, 10, 4, 15, 15, 4), nrow = 4, ncol = 4, byrow = TRUE)
print(A)
print(B)

# Element-wise multiplication
elementwise_product <- A * B
print(elementwise_product)

# Matrix-matrix multiplication
matrix_product <- A %*% B
print(matrix_product)

X <- c(5, 6, 8, 9)
Y <- c(8, 10, 12, 5)

# Compute the outer product
outer_product <- outer(X, Y)
print(outer_product)
# Compute the inner product (dot product)
inner_product <- sum(X * Y)
print(inner_product)

# X_matrix <- matrix(X, ncol = 1)  # Convert X into a column matrix (4x1)
# Y_matrix <- matrix(Y, nrow = 1)  # Convert Y into a row matrix (1x4)
# 
# outer_product <- X_matrix %*% Y_matrix
# print(outer_product)

diag_matrix <- diag(X)
print(diag_matrix)

diagonal_elements <- diag(diag_matrix)
print(diagonal_elements)

identity_matrix <- diag(6)
print(identity_matrix)

A <- matrix(c(3, 4, -2, 4, -5, 1, 10, -6, 5), nrow = 3, ncol = 3)
print(A)

B <- matrix(c(5, -3, 13), nrow = 3, ncol = 1)
print(B)

X <- solve(A, B)
print("Solution Vector X:")
print(X)

print("Type of X:")
print(typeof(X))
print(class(X))

Ainv <- solve(A)
print(Ainv)

identity_check <- Ainv%*%A
print(identity_check)

results <- eigen(A)
print(results$values)
print(results$vectors)
print(class(results))

second_eigenvector <- results$vectors[,2]  
eigen_multiplication <- A %*% second_eigenvector
print(eigen_multiplication)

lambda2 <- results$values[2]  
expected_result <- lambda2 * second_eigenvector
print(expected_result)

# typof is actual datatype...which it is stored


# Ex:2

df=read.csv("~/Desktop/biostat/BrainCancer.csv", header=TRUE)

print(dim(df))
# data$time <- NULL
print(dim(df))
df <- read.csv("BrainCancer.csv")

df$new_col <- df$GTV^2 + df$time

print(rownames(df))
print(colnames(df))

rownames(brain_cancer_data) <- paste("Row-", 1:nrow(brain_cancer_data), sep = "")
# rownames(df) <- paste0("Row-", 1:nrow(df))

df$ki <- NULL

print(df)

# EX: 3

library(readxl)

data2 <- read_excel("/home/ibab/pone.0148733.s001.xlsx",1)
print(names(data2))
print(dim(data2))


#  Ex4:

setA <- c("a", "b", "c", "d", "e")
setB <- c("d" ,"e" ,"f" ,"g")
union(setA, setB)
intersect(setA, setB)
setdiff(setA, setB)
setdiff(setB, setA)

setA[setA%in%setB]   # intersection ... inside bracket it gives indices
setA%in%setB
setA[TRUE]

print(c(setdiff(setA, setB), intersect(setA, setB), setdiff(setB, setA)))  # for union

setequal(c(setdiff(setA, setB), intersect(setA, setB), setdiff(setB, setA)), union(setA, setB))

# Ex5:

#  creating a vector
vec <- c(8,10,12,7,14,16,2,4,9,19,20,3,6)
vec[vec > 12]         # vec[values >12]
vec[vec > 10 & vec < 20]   

A <- c(2,7,29,32,41,11,15,NA,NA,55,32,NA,42,109)

A_filtered <- A[!is.na(A) & A < 100]
print(A_filtered)

A[is.na(A)] <- 0
print(A)         # NA values are now filled with zeros

# creating columns separately: (genes, results, gender) 

genes <- c("gene-1", "gene-2", "gene-3", "gene-4", "gene-5", "gene-6", "gene-7")
gender <- c("M", "M", "F", "M", "F", "F", "M")

result1 <- c(12.3, 11.5, 13.6, 15.4, 9.4, 8.1, 10.0)
result2 <- c(22.1, 25.7, 32.5, 42.5, 12.6, 15.5, 17.6)
result3 <- c(15.5, 13.4, 11.5, 21.7, 14.5, 16.5, 12.1)
result4 <- c(14.4, 16.6, 45.0, 11.0, 9.7, 10.0, 12.5)
result5 <- c(12.2, 15.5, 17.4, 19.4, 10.2, 9.8, 9.0)
result6 <- c(13.3, 14.5, 21.6, 17.9, 15.6, 14.4, 12.0)
result7 <- c(11.0, 10.0, 12.2, 14.3, 23.3, 19.8, 13.4)

# creating dataframe using those columns :
dataframe1 <- data.frame(genes, gender, result1, result2, result3, result4, result5, result6, result7)

# adding colnames:
colnames(dataframe1) <- c("GeneName", "Gender", "expt1", "expt2", "expt3", "expt4", "expt5", "expt6", "expt7")

subset_expt2 <- dataframe1[dataframe1$expt2 > 20, ]
print(subset_expt2)

subset_female <- dataframe1[dataframe1$Gender == "F", ]
print(subset_female)

subset_male_expt2 <- dataframe1[dataframe1$Gender == "M" & dataframe1$expt2 < 30, ]
print(subset_male_expt2)

#6

find_quadrant <- function(angle) {
  angle <- angle %% 360  
  
  if (angle > 0 & angle <= 90) {
    print("First quadrant")
  } else if (angle > 90 & angle <= 180) {
    print("Second quadrant")
  } else if (angle > 180 & angle <= 270) {
    print("Third quadrant")
  } else if (angle > 270 & angle < 360) {
    print("Fourth quadrant")
  } else {
    print("Angle lies on an axis or is 0/360 degrees")
  }
}

#calling the function
find_quadrant(45)  
find_quadrant(120)  
find_quadrant(360)  

sort_num<- function(a, b, c) {
  if (a >= b & a >= c) {
    if (b >= c) {
      print(c(a, b, c))
    } else {
      print(c(a, c, b))
    }
  } else if (b >= a & b >= c) {
    if (a >= c) {
      print(c(b, a, c))
    } else {
      print(c(b, c, a))
    }
  } else {
    if (a >= b) {
      print(c(c, a, b))
    } else {
      print(c(c, b, a))
    }
  }
}

#sorting all the numbers in descending order
sort_num(15, 9, 20)

ticket_cost <- function(d, age) {
  if (d <= 100) {
    cost <- 100
  } else if (d <= 1000) {
    cost <- 100 + (d - 100) * 1.50
  } else {
    cost <- 100 + (900 * 1.50) + (d - 1000) * 2
  }
  
  
  if (age > 60) {
    cost <- cost * 0.75  
  } else if (age < 6) {
    cost <- cost * 0.50  
  }
  
  print(paste("Ticket cost: Rs.", round(cost, 2)))
}

ticket_cost(500, 65)  # Senior citizen discount
ticket_cost(1200, 5)  # Child discount
ticket_cost(2000, 40) # Normal fare for 2000 km


#7

#Write a function to replace all the negative values in a vector by zeros.

replace <- function(vec) {
  vec[vec < 0] <- 0
  return(vec)
}

v <- c(-5, 10, -3, 8, -1, 7)
replace(v)  



#Write a function to calculate the factorial of a number using the Stirling’s approximation: #couldn't copy the whole question due to format

stirling <- function(n)
{
  pi <- sqrt(2 * pi * n)
  power<- (n / exp(1))^n
  corr<- 1 + (1 / (12 * n)) + (1 / (288 * n^2)) - (139 / (51840 * n^3)) - (571 / (2488320 * n^4))
  return(pi * power* corr)
}

stirling(5)


#Write a function to sum the digits of a number.


sum_digits <- function(num) {
  total <- 0
  while (num > 0) {
    total <- total + (num %% 10)
    num <- num %/% 10  
  }
  return(total)
}

sum_digits(9876)  




