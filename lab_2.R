#Lab 1 jan 3 2025

#ex1:

print(round(12.1343,digits=3))
print(round(123.12344,digits=3))
print(round(1234.12344,digits=3))
print(round(12345.12344,digits=3))
options(digits=15)
print(round(12345.12344,digits=3))
print(formatC(round(12345.12344,digits=3),format="f",digits=3))   # its giving a string
print(1234.12344)
print(1234.723,digits=3)
print(1234.723,digits=5)
print(round(123456788.123,digits=3))
print(round(123456788.123,digits=2),digits=20)   # its actually rounding off to 2 but adding 0s..
print(round(123456789.1234,digits=4),digits=20)
print(round(1232346554.344, digit=3),digit=4)   # gives exponential
print(paste("Hello World"))
print(paste("Hello","World"))
print(paste(1:10))    # strings are generated 
print(paste(1:10)[4]) # prints"4"
print(paste(10:20)[4]) # prints"13"
print(as.numeric(paste(1:10)))  # now its integers
print(paste(1:10,collapose="."))
print(paste(c("Hello","World"),1:10,sep="-"))   #its taking hello--1 to 5, world--6 to 10
print(paste("Hello",1:10,sep="-"))              # here hello is joined with 1 to 10

# ex 2

print(0:10)
print(c(0:10))    # gives same
print(15:5)
print(seq(15:5))  # gives output 1,2,.....11 takes as along
print(seq(0,1.5,0.1))  
print(seq(6,4,-0.2))
print(seq(-10))   # for neg numbers it generate seq 1 to...that number ...-10
# print(seq(1,0,1))  # Error in seq.default(1, 0, 1) : wrong sign in 'by' argument
# print(c(6,4,-0.2)) .....doesn't work
x=seq(0,1.5,0.1)
print(class(x))    # numeric

print(seq(from=0, to=1.5, by =0.1))     # by--> steps
print(seq(from=0, to=1.5, length =30))  # length--> no of numbers generated
N<- c(1,2,3,4,5,6)
print(seq(from=0, to=1.5, along =N))    # along---> length of object N


print(rep(9,5))
print(rep(1:4, 3))
N <- c(55,76,92,103,84,88,121,91,65,77,99)
print(seq(from=0.04,by=0.01,length=11))
print(seq(0.04,by=0.01,along=N))
print(seq(from=0.04,to=0.14,along=N))  # above steps by 0.01 but this keeps last number as 0.14

#print(sequence(4,3,4,4))
print(sequence(c(4,3,4,4)))

print(rep(9,5))
print(rep(1:4,2))
print(rep(1:4,each=2))
print(rep(1:4,each=2,times=3))
print(rep(1:4,1:4))

print(rep(1:4,c(4,1,4,2)))  #  1 1 1 1 2 3 3 3 3 4 4

print(rep(c("cat","dog","goldfish","rat"), c(2,3,2,1,3)))  # gives error as no of times is more
print(rep(c("cat","dog","goldfish","rat"), c(2,3,2,1)))   # 2 times cat, 3 times dog.....
print(seq(-1,1,by=0.1))
print(seq(-1,1,0.1))
print(seq(-1,1,length=7))


# ex 3 :

print(exp(-Inf))
print(class(exp(-Inf)))
print(Inf - Inf)
print(Inf / Inf)

print(3/0)   # gives Inf
print(0/0)   # gives NAN
print((0:3)^Inf)
y<- c(4, NA, 7)   # NA is missing value
print(y)
print(is.na(y))
print(y=='NA')

y[!is.na(y)]  #..removes NA from y
print(y)     #.....NA is not removed permanently

c1<- c(1,2,3,NA)
c2<- c(5,6,NA,8)
c3<- c(9,NA,11,12)
c4<- c(NA,14,15,16)
full.frame <- data.frame(c1,c2,c3,c4)   # Unlike arrays it keeps headers also...
print(full.frame)

reduced.frame <- full.frame[! is.na(full.frame$c1),]     # removes NA from c1 and prints whole row
print(reduced.frame)

reduced.frame <- full.frame[! is.na(full.frame$c1),1]    # removes NA from c1 and prints c1
print(reduced.frame)

reduced.frame <- full.frame[! is.na(full.frame$c1),2]    # removes NA from c1 and prints c2
print(reduced.frame)

full.frame[! is.na(full.frame)]
full.frame[! is.na(full.frame),]    #,.....

x <- (1:10)
print(x)
print(mean(x))

y <- c(1, 2, 3, 4, 5, 6, NA, NA, 9, 10, 11, 12)
print(mean(y))           # gives NA
print(mean(y,na.rm=T))   # gives correct mean leaving NA s

v <- c(1:6,NA,NA,9:12)
print(v)
print(seq(along=v)[is.na(v)])
which(is.na(v))

n <- (-1):(1/0.1) * 0.1 + -1
print(n)
