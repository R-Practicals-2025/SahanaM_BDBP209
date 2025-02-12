# lab5 continued

data=read.csv("/home/ibab/R_Practicals_2025/BrainCancer.csv")
print(data)

print(gl(4,3))
print(gl(4,3,12))            # gl(number of categories, repeats, Total no)
print(gl(4,3,43))

Temp <- gl(2,2,12, labels = c('low', 'high'))   # first index = = no of labels
print(Temp)

Temp1 <- gl(3,8,24, labels = c('Hard', 'medium', 'soft'))

# Ex 6:


D=dim(data)
row_no=D[1]
print(row_no)
Temperature <- gl(3, 4, row_no , labels = c('Hot', 'Cold','lukewarm'))
print(Temperature)

newcoladded = data.frame(data, Temperature)
print(colnames(newcoladded))

# Ex 7:

print(tapply(data$gtv, data$ki, mean))  
print(tapply(data$gtv, data$ki, mean, trim=0.1))
sub=subset(data$gtv,data$ki==90)
print(sub)
print((mean(sub)))

trimmed_mean = mean(sort(sub), trim=0.1)
print(trimmed_mean)

# Ex 8:

print(pmin(data$gtv, data$ki, data$time))
print(pmax(data$gtv, data$ki, data$time))

# Ex 9:

ranks <- rank(data$gtv)
sorted <- sort(data$gtv)
ordered <- order(data$gtv)

view <- data.frame(data$gtv, ranks, sorted, ordered)
print(view)

print(data$gtv[68])
print(data$time[ordered])

newData = data.frame(sorted , data$diagnosis[ordered])
print(newData)
write.csv(newData, "lab4 ordered data.csv", row.names = FALSE)


# Ex 10:

# dataframe into Matrix:

filter1= data[1:6, 3:8]
filter1_mat= as.matrix(filter1)

print(filter1_mat)
print(class(filter1_mat))
print(mode(filter1_mat))
print(attributes(filter1_mat))


# How to add column to a matrix:

newcol = data$gtv + data$time + data$ki

newcoladded = data.frame(data, newcol)
print(colnames(newcoladded))


newcoladded2 = cbind(data, newcol)
print(colnames(newcoladded2))

filter2= data[c(1,3,8,8),]
newrowadded = rbind(data, filter2)
print(newrowadded)

filter3= data[c(26,35),]
newrowadded2 = rbind(data, filter3)
print(newrowadded2)
print(dim(newrowadded2))

# EX 11:
X <- matrix(c(1,0,2,5,3,1,1,3,1,3,3,1,0,2,2,1,0,2,1,0), nrow = 4)
print(X)
print(rownames((X)))
print(colnames(X))

rownames(X) <- rownames(X, do.NULL= FALSE, prefix = 'Trail.')
print(rownames(X))


drugs <- c('as-pirin','paracetamol','nurofen','hedex','placebo')
colnames(X) <- drugs
print(colnames(X))
print(X)

dimnames(X) <- list(NULL, paste("drug.",1:5,sep=""))
print(X)

# Ex12:
# calculation on rows/col of mat   rix:
print(mean(X[,5]))
print(var(X[4,]))
print(rowSums(X))  #3 method 1
print(colSums(X))

# method 2:

print(apply(X, 1, sum))     # ----->1-->rows
print(apply(X, 2, sum))     # second argu-->2--col
print(apply(X, 2, sqrt))     

print(apply(X, 1, function(X)X^2+X))    # 1 --->transpose

print(rowMeans(X))
print(colMeans(X))
print(apply(X, 1, mean))

group = c("A", "B", "B", "A")
print(rowsum(X, group))   # different from rowSums

print(row(X))  
print(col(X)) 

print(group[row(X)])
print(col(X))
print(tapply(X, list(group[row(X)], col(X)), sum))


#13
#sweep() function
#to perform sweep action
data=read.csv("~/Desktop/biostat/BrainCancer.csv", header=TRUE)
eg_sweep = data.frame(data$ki,data$gtv,data$time)
cols <- apply(eg_sweep,2,mean)
print(cols)

cols.means <- matrix(rep(cols,rep(dim(eg_sweep)[1],dim(eg_sweep)[2])),
                     nrow=dim(eg_sweep)[1])
print(cols.means)
eg_sweep_alt <- eg_sweep - cols.means
print("Method 1")
print(eg_sweep_alt)

eg_sweep_alt2 <- sweep(eg_sweep,2,cols)
print("Method 2")
print(eg_sweep_alt2)

#sapply() used for vectors
eg_sapply <- sapply(3:7,seq)
print(attributes(eg_sapply))


#14:
data <- read.table("pgfull.txt", header = TRUE)
species <- data[, 1:54]
max_indices <- max.col(species)
print(max_indices)
species_names <- names(species)[max_indices]
print(species_names)
species_freq <- table(species_names)
print(species_freq)
min_indices <- max.col(-species)  #Negating the values in 'species'
print(min_indices)
#alt method:
min_indices <- apply(species, 1, which.min)
print(min_indices)


#trying something out
vec <- c(10, 20, 30)
names(vec) <- c("X", "Y", "Z")
print(names(vec))
print(vec)

#15
apples <- c(4,4.5,4.2, 5.1,3.9)
oranges <- c(TRUE,TRUE,FALSE)
chalk <- c("limestone","marl","oolite","CaCO3")
cheese <- c(3.2-4.5i,12.8+2.2i)
items <- list(apples, oranges, chalk, cheese)
print(items)
#subscripts on lists have double square brackets
print(items[[3]])
print(items[[3]][3])

items[3] #this works
items[3][3] #this doesnt
print(names(items))
items <- list(first=apples,second=oranges,third=chalk,fourth=cheese)
print(items$fourth)
print(class(items))
print(lapply(items,length))
print(lapply(items,class))
print(lapply(items,mean))
  
  
  
  
  
  
  
  
  
  
  
  
  