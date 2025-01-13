#Lab 3 jan 3 2025

#-------Exercise: Vector functions:----------------------
#ex1:

vec<-c(23,4,5,32,78,98,56)

print(class(vec))
print(length(vec))
print(min(vec))
print(max(vec))

#ex 2:
vec1 <- scan()

#ex 3:
print(vec1[4])

#ex 4:
ind <- c(2,3,6) 
print(vec[ind])
print(vec[c(2,3,6)])

#ex 5 and 6:

vec[-1]   # for popping first element:
vec[-length(vec)]    # for popping last element:

#ex 7:
func <- function(x) {
  x1<-sort(x) 
  x2 <-x1[-(1:2)]
  x3 <- x2[-((length(x2)-1): length(x2))]
  print(x3)
}

func(vec)

#ex 8:
func2 <- function(x) sort(x) [c(-1,-2,-(length(x)-1), -length(x))]

func2(vec)

vec<-c(23,4,5,32,78,98,56)
print(vec[1:length(vec) %%2==0])

#ex 9:
x <- 0:10
print(x[x < 5])

#ex 10:
lar_3 <- function(x) sort(x) [c(x[length(x)], x[length(x)-1], x[length(x)-2])]
lar_3(x)                      


lar_3.2 <- function(x) {
  sorted_x <- sort(x, decreasing = TRUE) 
  return(sorted_x[1:3])                  
}

x <- 0:10
lar_3.2(x)

#ex 11:
which.max(x)
which.min(x)

#ex 12:
cbind(1:10,10:1)
rbind(1:10,10:1)

#ex 13:
X <- c(1:10)
X
Y <- c(1:10*5)
Y
X*Y
X+Y
X/Y
X^Y
log(X)
exp(Y)


#-------Exercise: Matrices/Data frames/Arrays------------:

# 3D Matrix:
x <- c(1,2,3,4,5,"stat", "sahana", 8)
dim(x)<-c(2,2,2)
print(x)

# 4D Matrix: 
x <- c(1,2,3,4,5,"stat", "sahana", 8,2,3,4,5,6,7,8,4)
dim(x)<-c(2,2,2,2)
print(x)

# 5D Matrix:
x <- c(1:432)
dim(x)<-c(2,3,4,3,6)
print(x)

#ex 1:
X <- matrix (c(1,0,0,0,1,0,0,0,1),nrow=3)
print(X)

#ex 2:
vector <- c(1,2,3,4,4,3,2,1)
V <- matrix(vector,byrow=T,nrow=2) 
dim(vector) <- c(4,2) 
is.matrix(vector)

x<- c(1:24)
dim(x)<-c(3,8)

y<- c(1:8)
dim(y)<-c(8,1)

#ex 3:
print(crossprod(x,y))

x<- c(1,0,0,0,1,0,0,0,1)
dim(x)<-c(3,3)
x[2,3]<-NA
y<- c(1:6)
dim(y)<-c(3,2)

print(crossprod(x,y))
print(x %*% y)
print(x %o% y)   # 2+2 --> 4D
print(sum(x*y))


Z <- X[1:4] %o% Y[1:3]
Z
YoX <- Y[1:3] %o% X[1:4]
YoX
t(Z) #transpose
t(YoX)
X %*% Y #dot product
sum(X*Y) #another way to carry out dot product
crossprod(X[1:4],Z)
diag(4)   #identity matrix
class(X)