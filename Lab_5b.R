#Lab 5 jan 31 2025


data=read.csv("/home/ibab/R_Practicals_2025/Heart.csv")
print(data)

# ex 2
print(dim(data))   # gives dimensions
print(length(data))   # gives no of columns --- no of categories
print(colnames(data))    # name of categories  
print(rownames(data))   
print(head(data, 30))
# print(table(data$ChestPain))    # gives frequency table of data



unique(data$ChestPain)    # has NA also....neglected while considering levels
data$ChestPain <- factor(data$ChestPain, levels = c( "typical","asymptomatic", "nonanginal", "nontypical"  ))
print(levels(data$ChestPain))
print(nlevels(data$ChestPain))   # gives number of levels..


# ....to extract categorical variables :
X <- sapply(data, is.numeric)   #...gives numeric
print(X)  

y <- data[!X]    #....gives rows containing categorical variables as it removes numeric
print(colnames(y))   # to print column names...


# Ex 3
print(data$ChestPain[1:5])
print(mean(data$RestBP))
print(mean(data$Age))
print(median(data$RestBP))   # median is smaller than mean...as data is skewed towards right
# print(mode(data$gtv))
V <- (data$RestBP)
mode = function(V){
  return (which.max(table(V)))
}


print(mode(V))

print(sd(data$RestBP))
print(summary(data$RestBP))
print(hist(data$RestBP))
library(moments)
print(skewness(data$RestBP))
print(kurtosis(data$RestBP))
boxplot(data$RestBP)
boxplot(data$RestBP, xlab="spread of BP", ylab="BP", horizontal=FALSE, border ='blue', col='red')
boxplot(data$Age)
boxplot(data$Chol)

# Ex 4

RBP_20=subset(data, data$RestBP>20)
print(dim(RBP_20))
specific_rows=data[c(1,3,8,9,13,14, 18, 21),]
print(specific_rows)
ind_fem=which(data$Sex==1)
ind=data[ind_fem,]
print(ind)

new_data <- data.frame(RestBP = data$RestBP, Chol = data$Chol, New_Col = data$RestBP * data$Chol / 234)
print(new_data)

female_data <- subset(data, Sex == 1)
write.csv(female_data, "/home/ibab/R_Practicals_2025/Heart.csv", row.names = FALSE)

print(head(female_data))


