# Lab15 26 April


# Question 1:
# Data
x <- c(0.5, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0)
y <- c(0.87, 7.12, 14.01, 24.37, 39.058, 58.62, 83.92)

model <- lm(y ~ poly(x, 2))
model2 <-lm(y ~ poly(x, 3))
print(coef(model)[2])  
print(coef(model)[3])  
print(coef(model)[4])  

plot(x, y, pch=19, col="blue")
curve(predict(model, data.frame(x=x)), add=TRUE, col="red", lwd=2)
curve(predict(model2, data.frame(x=x)), add=TRUE, col="green", lwd=2)



# Question 2:

library(datasets)
df <- trees


df$Girth <- df$Girth * 0.0254  
df$Height <- df$Height * 0.3048  
df$Volume <- df$Volume * 0.028317  

head(df)

library(lattice)
splom(df, xlab = "Scatter Plot Matrix")

y <- df$Volume

beta0 <- rep(1, nrow(df)) 

# (v) Build the X matrix
X <- cbind(beta0, df$Girth, df$Height)  # Combine into a matrix

# (vi) Solve for (beta) using matrix method
model1 <- solve(t(X) %*% X) %*% t(X) %*% y

# Print the coefficients
cat("beta0 (intercept) =", model1[1], "\n")
cat("beta1 (Girth) =", model1[2], "\n")
cat("beta2 (Height) =", model1[3], "\n")

new_X <- cbind(1, c(0.3, 0.4, 0.5), c(20, 21, 22))  # 1 for intercept
predicted_volume <- new_X %*% model1

print(predicted_volume)


model2 <- lm(Volume ~ Girth + Height, data = df)

coef(model2)

# (ix) Predict using lm() model

newdata <- data.frame(Girth = c(0.3, 0.4, 0.5), 
                      Height = c(20, 21, 22))

predicted_volume_lm <- predict(model2, newdata = newdata)

# Print predictions
print(predicted_volume_lm)

# VII. Non-linear regression
# 1. Enter data
xdat <- c(1.0, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 2.1, 2.2,
          2.3, 2.4, 2.5, 2.6, 2.7, 2.8, 2.9, 3.0)
ydat <- c(-0.09, 0.59, 0.42, 2.03, 3.43, 1.84, 5.30, 4.40, 6.67, 7.40,
          7.34, 8.76, 10.25, 10.93, 13.78, 14.21, 17.82, 21.83, 23.04,
          24.25, 27.48)

# 2. Create data frame
data_nlr <- data.frame(xdat, ydat)

# 3. Fit non-linear model y = a*x^n
model_nlr <- nls(ydat ~ a * xdat^n, data = data_nlr,
                 start = list(a = 1, n = 2))

# 4. Summary of fit
summary(model_nlr)

# 5. Degrees of freedom
df <- df.residual(model_nlr)
cat("Degrees of Freedom:", df, "\n")

# 6. Plot fitted curve
xseq <- seq(1, 3, by = 0.1)
yfit <- predict(model_nlr, newdata = data.frame(xdat = xseq))

plot(xdat, ydat, pch = 19, col = "blue", main = "Non-linear Regression Fit")
lines(xseq, yfit, col = "red", lwd = 2)

# VIII. Clustering methods

# 1. Hierarchical Clustering
# (i) Load packages
library(tidyverse)
library(dplyr)
library(RColorBrewer)

# (ii) Read spellman-wide.csv
spellman <- read.csv("spellman-wide.csv")
cat("Dimensions of spellman data:", dim(spellman), "\n")

# (iii) First 5 rows and 8 columns
print(spellman[1:5, 1:8])

# (iv) Correlation matrix and distance
spellman_cor <- select(spellman, -time, -expt) %>% cor(use = "pairwise.complete.obs")
spellman_dist <- as.dist(1 - spellman_cor)

# (v) Hierarchical clustering
spellman_tree <- hclust(spellman_dist, method = "complete")
plot(spellman_tree, cex = 0.1)

# (vi) Dendrogram without labels
library(dendextend)
spellman_dend <- as.dendrogram(spellman_tree)
plot(spellman_dend, leaflab = "none")

# (vii) Cut tree into 4 clusters
clusters <- cutree(spellman_dend, k = 4)
table(clusters)
clusters[1:6]

# (viii) Colour branches
plotc <- color_branches(spellman_tree, k = 4)
plot(plotc, leaflab = "none")

plotc8 <- color_branches(spellman_tree, k = 8)
plot(plotc8, leaflab = "none")
table(cutree(spellman_dend, k = 8))

# (ix) Create cluster dataframe
clusters_df <- data.frame(gene = names(clusters), cluster = clusters)
clusters_df %>% filter(gene == "YALO22C")

# (x) Genes in 3rd cluster
cluster3_genes <- clusters_df %>% filter(cluster == 3) %>% pull(gene)
print(cluster3_genes)

# (xi) Heatmap for genes in cluster 3
spellman_long <- gather(spellman, gene, expression, -expt, -time)
head(spellman_long)

color_scheme <- rev(brewer.pal(8, "RdBu"))

spellman_long %>%
  filter(gene %in% cluster3_genes & expt == "alpha") %>%
  ggplot(aes(x = time, y = gene)) +
  geom_tile(aes(fill = expression)) +
  scale_fill_gradientn(colors = color_scheme, limits = c(-2, 2)) +
  theme(axis.text.y = element_text(size = 6))

# (xii) Combining heatmap and dendrogram
sub_trees <- cut(spellman_dend, h = 1.48)
sub_trees$lower

cluster3_tree <- sub_trees$lower[[3]]
cluster3_tree %>%
  set("labels_cex", 0.45) %>%
  set("labels_col", "red") %>%
  plot(horiz = TRUE)

library(gplots)

alpha_factor <- filter(spellman, expt == "alpha")
alpha_mtx <- as.matrix(select(alpha_factor, -time, -expt))
rownames(alpha_mtx) <- alpha_factor$time
transposed_alpha_mtx <- t(alpha_mtx)

heatmap.2(transposed_alpha_mtx,
          Rowv = cluster3_tree,
          Colv = NULL,
          dendrogram = "row",
          breaks = seq(-2, 2, length.out = 9),
          col = color_scheme,
          trace = "none",
          density.info = "none",
          xlab = "Time (mins)")

# 2. K-means Clustering on Airbnb data

# (i) Load packages
library(ggplot2)
library(tidyverse)

# (ii) Read AirBNB data
listings <- read.csv("listings_airbnb.csv")
cat("Number of rows:", nrow(listings), "\n")
cat("Column names:", names(listings), "\n")

# (iii) Scatter plot
ggplot(listings, aes(number_of_reviews, price, color = room_type, shape = room_type)) +
  geom_point(alpha = 0.25) +
  xlab("Number of reviews") +
  ylab("Price")

# (iv) Scale price and reviews
listings[, c("price", "number_of_reviews")] <- scale(listings[, c("price", "number_of_reviews")])

# (v) Subset selection
airbnb_2cols <- listings %>% select(price, number_of_reviews)
print(head(airbnb_2cols))

# (vi) K-means clustering
set.seed(23)
km_out <- kmeans(na.omit(airbnb_2cols), centers = 3, nstart = 20)
print(km_out)

# (vii) Scree plot
nclusters <- 10
wss <- numeric(nclusters)

for (i in 1:nclusters) {
  km_tmp <- kmeans(na.omit(airbnb_2cols), centers = i, nstart = 20)
  wss[i] <- km_tmp$tot.withinss
}

# (viii) Scree plot
wss_df <- tibble(clusters = 1:nclusters, wss = wss)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4) +
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters')

scree_plot +
  geom_hline(yintercept = wss,
             linetype = 'dashed',
             col = c(rep('#000000',4),'#FF0000', rep('#000000',5)))

# (ix) K=5 clustering and plotting
set.seed(23)
km_out_final <- kmeans(na.omit(airbnb_2cols), centers = 5, nstart = 20)

airbnb_2cols$cluster_id <- factor(km_out_final$cluster)

ggplot(airbnb_2cols, aes(number_of_reviews, price, color = cluster_id)) +
  geom_point(alpha = 0.25) +
  xlab("Number of reviews") +
  ylab("Price")






