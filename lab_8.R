phrase <- "The quick brown fox jumps over the lazy dog"

substr(phrase,1,1)



for (x in 1:nchar(phrase)){
  print(substr(phrase,1,x))
}

strsplit(phrase, split=" ")
strsplit(phrase, split="")

table(lapply(strsplit(phrase, split = ""), nchar))
lapply(strsplit(phrase, split = ""), nchar)

table(lapply(strsplit(phrase, split = " "), nchar))
lapply(strsplit(phrase, split = " "), nchar)

strsplit(phrase, split = NULL)

# to split the strings;

q <- function(x) sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")
print(q(phrase))
lapply(strsplit(phrase, NULL), rev)

length(strsplit(phrase, split= " "))
length(strsplit(phrase,split=" ")[[1]])

table(strsplit(phrase, NULL))
str(table(strsplit(phrase, NULL)))   # here 1st-- no of spaces-entry  is taken for next command
words <- 1+table(strsplit(phrase, NULL))[1]
print(words)

tolower(phrase)
toupper(phrase)

first <- c(5,8,5,3,3,6,4,4,2,8,8,8,4,6) # it gives index where it matches
second<- c(8,6,4,2)
match(first, second)

subject <- c("A", "B", "G", "M", "N", "S", "T", "V", "Z")
patients <- c("E", "G", "S", "U", "Z")

match(subject, patients)

# ifelse

y <- c(1,2,-1,2,4,8,6,7,23,-9,10,-23)
z <- ifelse(y<0, -1, 1) 
print(z)

y <- c(1, 2.2, 9.7, 3.4, 5.6, 7.8, 10, 2.3, 4.7, 8, 8.4, 6, 10.2)
z <- ifelse(y>median(y), "big", "small")
print(median(y))
print(z)

drug <- c("conventional", "new")
matched_sub <- match(subject, patients)
print(matched_sub)
drug_sugg <- drug[ifelse(is.na(matched_sub),2, 1)]
print(drug_sugg)

text <- c("arm", "foot", "leg", "head", "hindleg", "elbow")
gsub("h", "H", text) 

sub("o", "O", text)

# ^--> first char of string, ^. --> any char in first place:

gsub("^.", "O", text)

s <- "filename.doc"
strsplit(s, split = ".")

# \\ --> as "." is a metacharacter:
s <- "filename.doc"
strsplit(s, split = "\\.")

print(text)
grep("o{1}", text, value=T)
grep("o{2}", text, value=T)
grep("o{3}", text, value=T)

grep("he{1}", text, value=T)


grep( "[[:alnum:]]{4,}",text,value=T)
s1<-"aaacccysgdfg"
grep("a{3}c{3}y", s1, value=T)


# ex1: Check if an integer is a palindrome
n<-123454321

is_palindrome <- function(n) {
  str_n <- as.character(n)
  return(str_n == paste(rev(strsplit(str_n, NULL)[[1]]), collapse=""))
}
print(is_palindrome(n))

# ex2:
text <- "seemerightnow"

part_a <- substr(text, 1, 3)   # "see"
part_b <- substr(text, 4, 5)   # "me"
part_c <- substr(text, 6, 10)  # "right"

print(part_a)
print(part_b)
print(part_c)

# ex3:

gc_fraction <- function(seq) {
  nucleotide_counts <- table(strsplit(seq, NULL)[[1]])
  gc_count <- sum(nucleotide_counts[c("G", "C")], na.rm = TRUE)
  return(gc_count / sum(nucleotide_counts))
}


dna_seq <- "ATTGCGCATAGTCCGGG"
gc_fraction(dna_seq)


# ex4:

complement_dna <- function(seq) {
  complement <- c(A="T", T="A", G="C", C="G")
  rev_complement <- rev(sapply(strsplit(seq, NULL)[[1]], function(x) complement[x]))
  return(paste(rev_complement, collapse=""))
}

is_palindromic_dna <- function(seq) {
  return(seq == complement_dna(seq))
}

is_palindromic_dna("TGGATCCA")  

# ex5:
find_largest_words <- function(sentence) {
  words <- unlist(strsplit(sentence, " "))
  # words <- gsub("[^a-zA-Z]", "", words)  
  
  word_lengths <- nchar(words)

  max_length <- max(word_lengths)
  second_max_length <- max(word_lengths[word_lengths < max_length])
  largest_words <- words[word_lengths == max_length]
  second_largest_words <- words[word_lengths == second_max_length]

  return(list(
    largest = largest_words, 
    second_largest = second_largest_words
  ))
}

sentence <- "She sells hundreds of sea oysters on the sea shore."

find_largest_words(sentence)

install.packages("stringr")
library(stringr)
str_count("AATTGCCCGTACGGTGCA", pattern='GC')

# ex6:

library(dplyr)
library(e1071)

worldfloras <- read.table("/home/ibab/R_Practicals_2025/worldfloras.txt", header = TRUE, sep = "\t")

continent_groups <- split(worldfloras, worldfloras$Continent)

boxplot(worldfloras$FloralCount ~ worldfloras$Continent, main="Floral Count by Continent")

summary(worldfloras$FloralCount)
sd(worldfloras$FloralCount)
skewness(worldfloras$FloralCount)
kurtosis(worldfloras$FloralCount)

boxplot(worldfloras$Population ~ worldfloras$Continent, main="Population by Continent")
hist(worldfloras$Population, main="Population Distribution", breaks=20)

summary(worldfloras$Population)
sd(worldfloras$Population)
skewness(worldfloras$Population)
kurtosis(worldfloras$Population)

#(b) Make a boxplot of the distribution of floral count within each continent and print the statistical summary. What are the mean and standard deviation values? Also calculate and comment on the skewness and kurtosis parameters (interpret them)
# Boxplot for floral count distribution within each continent
boxplot(Flora ~ Continent, data = world_data, main = "Floral Count Distribution by Continent", xlab = "Continent", ylab = "Floral Count", col = "lightgreen", border = "black")
summary_stats <- aggregate(Flora ~ Continent, data = world_data, summary)
print(summary_stats)
mean_floral <- mean(world_data$Flora)
sd_floral <- sd(world_data$Flora)
print(mean_floral)
print(sd_floral)
library(e1071)
floral_skewness <- skewness(world_data$Flora)
floral_kurtosis <- kurtosis(world_data$Flora)
print(floral_skewness) #skewness is 3.835473 which indicates that the data has a long tail to the right
print(floral_kurtosis) #kurtosis is 19.1006 which indicates a more peaked distribution than a normal distribution (leptokurtic)


#(c) Make a boxplot and histogram plot of the population distribution within each con-tinent and print the statistical summary. Calculate and comment on the skewnessand kurtosis parameters (interpret them). Does this have any relation with the floral count data?
boxplot(Population ~ Continent, data = world_data, main = "Population Distribution by Continent", xlab = "Continent", ylab = "Population", col = "lightgreen", border = "black")
hist(world_data$Population, main = "Population Distribution", xlab = "Population", col = "purple", border = "black")
population_stats <- aggregate(Population ~ Continent, data = world_data, summary)
print(population_stats)
population_skewness <- skewness(world_data$Population)
population_kurtosis <- kurtosis(world_data$Population)
print(population_skewness)
print(population_kurtosis)
#Population distributions tend to be skewed right because of the small number of countries with extremely high populations.
# Population distributions can have heavy tails (high kurtosis), meaning a few countries with very large populations may significantly affect the distribution.

#(7) Read in the data from ‘HumanBones.txt’ and group the data into categories “Chest”,“Spine”,“Skull”, “Ear Bones”, “Arms” and “Legs”. The number in the brackets indicates the number of bones in that type. Create a dataframe with 3 columns- category, name of the bone and number of bones.
bones_data <- read.table("/home/ibab/Downloads/HumanBones.txt", header = FALSE,sep = "\t", stringsAsFactors = FALSE)
head(bones_data)

categories <- c()
bone_names <- c()
bone_numbers <- c()
current_category <- NULL
for (i in 1:nrow(bones_data)) {
  line <- bones_data$V1[i]
  if (!grepl("\\(", line)) {
    current_category <- line
  } else {
    bone_info <- strsplit(line, "\\(")[[1]]
    bone_name <- trimws(bone_info[1])  # Get the bone name
    bone_number <- gsub("[^0-9]", "", bone_info[2])  # Extract the number of bones
    categories <- c(categories, current_category)
    bone_names <- c(bone_names, bone_name)
    bone_numbers <- c(bone_numbers, as.numeric(bone_number))
  }
}
bones_info <- data.frame(category = categories, name_of_bone = bone_names, number_of_bones = bone_numbers, stringsAsFactors = FALSE)
head(bones_info)

#Ex8
category_bones_summary <- aggregate(number_of_bones ~ category, data = bones_info, sum)
max_category <- category_bones_summary[which.max(category_bones_summary$number_of_bones), ]
print(max_category$category) 
category_frequency <- table(bones_info$category)
print(category_frequency)
barplot(category_bones_summary$number_of_bones, names.arg = category_bones_summary$category, main = "Number of Bones by Category", xlab = "Category", ylab = "Number of Bones", col = "pink")

#Ex9
legs_data <- subset(bones_info, category == "Legs")
long_bones_legs <- subset(legs_data, nchar(name_of_bone) > 5)
print(long_bones_legs$name_of_bone)

#Ex10
bones_starting_M <- subset(bones_info, grepl("^M", name_of_bone))
bones_starting_M$name_of_bone <- gsub("a", "A", bones_starting_M$name_of_bone)
print(bones_starting_M$name_of_bone)

#Ex11
bones_ending_with_e <- subset(bones_info, grepl("e$", name_of_bone))
bones_ending_with_e$name_of_bone <- tolower(bones_ending_with_e$name_of_bone)
print(bones_ending_with_e$name_of_bone)

#Ex12
bones_with_two_o <- subset(bones_info, grepl("o.*o", name_of_bone))
print(bones_with_two_o$name_of_bone)

