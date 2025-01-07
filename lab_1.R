#Lab 1 jan 3 2025

#ex1:
print(2.7/2)
print(2.7%/%2)  #gives qoutient
print(2.7%%2)   #gives remainder
print(10+5i/2)
print(round(2.5))
print(round(-2.5))
print(2%/%4-1)
print(3*2**2)
print(7%/%4)
print(7%%4)
print(-7%%4)
trunc(5.7)
trunc(-5.7)
print(-9%%4)


#ex 2:

x <- 5.7
print(ceiling(x))
print(floor(x+0.5))
f1<-function(x)floor(x+1)
fi(5.7)


#ex 3:
a <- 1
b <- 2
c <- 4
print(a & b)
print(!(a<b) | (c>b))


#ex 4:

x <- c(5,3,7,8)
print(is.integer(x))   #gives False
print(is.numeric(x))   #gives True
#x <- integer(x)       #Error in integer(x) : invalid 'length' argument
#print(x)

x <- as.integer(x)    #gives 5 3 7 8
print(x)

print(is.numeric(x))  #gives True

#ex 5:
x <- sqrt(2)
print(x*x ==2)  #gives False
print(x*x - 2)  
print(all.equal(x*x,2))







