#Lab 5 jan 31 2025

data=read.csv("/home/ibab/R_Practicals_2025/BrainCancer.csv")
print(data)

# ex 2
print(dim(data))   # gives dimensions
print(length(data))   # gives no of columns --- no of categories
print(colnames(data))    # name of categories  
print(rownames(data))   
print(head(data, 30))
# print(table(data$diagnosis))    # gives frequency table of data



unique(data$diagnosis)    # has NA also....neglected while considering levels
data$diagnosis <- factor(data$diagnosis, levels = c( "Meningioma","HG glioma","LG glioma","Other" ))
print(levels(data$diagnosis))
print(nlevels(data$diagnosis))   # gives number of levels..


# ....to extract categorical variables :
X <- sapply(data, is.numeric)   #...gives numeric
print(X)  

y <- data[!X]    #....gives rows containing categorical variables as it removes numeric
print(colnames(y))   # to print column names...


# Ex 3
print(data$diagnosis[1:5])
print(mean(data$gtv))
print(mean(data$time))
print(median(data$gtv))   # median is smaller than mean...as data is skewed towards right
# print(mode(data$gtv))
V <- (data$gtv)
mode = function(V){
  return (which.max(table(V)))
}


print(mode(V))

print(sd(data$gtv))
print(summary(data$gtv))
print(hist(data$gtv))
library(moments)
print(skewness(data$gtv))
print(kurtosis(data$gtv))
boxplot(data$gtv)
boxplot(data$gtv, xlab="spread of GTV", ylab="GTV", horizontal=FALSE, border ='blue', col='red')
boxplot(data$time)
boxplot(data$ki)

# Ex 4

gtv_20=subset(data, data$gtv>20)
print(dim(gtv_20))
specific_rows=data[c(1,3,8,9,13,14, 18, 21),]
print(specific_rows)
ind_fem=which(data$sex=='Female')
ind=data[ind_fem,]
print(ind)

new_data <- data.frame(gtv = data$gtv, ki = data$ki, New_Col = data$gtv * data$ki / 234)
print(new_data)

female_data <- subset(data, sex == "Female")
write.csv(female_data, "/home/ibab/R_Practicals_2025/BrainCancer.csv", row.names = FALSE)

print(head(female_data))


