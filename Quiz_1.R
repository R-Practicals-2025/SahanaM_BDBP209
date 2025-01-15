# Quiz 1 jan 15 :

#question 1:

n <- 4
k <- 8      
#n <- scan()   # scan()--- working when each line is ran separately....   
#k <- scan()

func <- function(n,k){
  add <- n+k
  sub <- n-k
  mult <- n*k
  div <- k/n
  
  return (c(add, sub, mult, div))   # I am not able to format the print statement 
}

print(func(n, k))


#question 2:

a <- 4
b <- 9
c <- 3


func2 <- function(a,b,c){
  r1=((-b)+(sqrt((b*b)-(4*a*c))))/(2*a)
  r2=((-b)-(sqrt((b*b)-(4*a*c))))/(2*a)
  return (c(r1, r2))
}


print(func2(a,b,c))
#.....for non real roots its giving error and returning NAN
