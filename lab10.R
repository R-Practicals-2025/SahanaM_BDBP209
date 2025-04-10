# Lab 10 Mar 28:

# Ex1:
# 1)
x <- seq(1,100)
s <- sample(x,10)
s
#  the above sampling is happening without replacement,

x <- seq(1,100)
s <- sample(x,10,replace=TRUE)
s

# now it is with Replacement..

# 2)

# install.packages("gtools")

library(gtools)
x <- c("A","B","C","D")

# where r is the size of the target vector, v is the source vector, 
# is the size of the source vector.
per <- permutations (n=length(x), r=3,v=x,repeats.allowed=TRUE)
print(per)
comb <- combinations(n=length(x), r=3, v=x)
print(comb)

# Ex2:

# 1)

# Set parameters
n <- 10
p1 <- 0.4
p2 <- 0.7
m <- 3

# (a) Probability value for the given parameters
prob_value <- dbinom(m, n, p1)
print(paste("P(X =", m, ") =", prob_value))

# (b) Cumulative probability value
cum_prob_value <- pbinom(m, n, p1)
print(paste("P(X <=", m, ") =", cum_prob_value))

# (c) Finding m corresponding to cumulative probability of 0.8
m_for_0.8 <- qbinom(0.8, n, p1)
print(paste("m corresponding to cumulative probability of 0.8: ", m_for_0.8))

# (d) Generate 5 random samples
random_samples <- rbinom(5, n, p1)
print("5 Random Samples from Binomial Distribution:")
print(random_samples)

# (e) Plot PDFs for p = 0.4 and p = 0.7
x_vals <- 0:n
pdf_p1 <- dbinom(x_vals, n, p1)
pdf_p2 <- dbinom(x_vals, n, p2)

plot(x_vals, pdf_p1, type="b", col="gold", pch=18, xlab="X", ylab="Probability", main="Binomial PDF")
lines(x_vals, pdf_p2, type="b", col="magenta", pch=18)
legend("topright", legend=c("p=0.4", "p=0.7"), col=c("gold", "magenta"), pch=18)

# (f) Generate samples and plot frequency distributions
samples_100 <- rbinom(100, n, p1)
samples_10000 <- rbinom(10000, n, p1)

par(mfrow=c(2,1)) # 2x1 grid
barplot(table(samples_100), col="lightgreen", main="Frequency of 100 samples")
barplot(table(samples_10000), col="violet", main="Frequency of 10,000 samples")

par(mfrow=c(1,1)) # 1X1 grid


# 2)

# Setting up the parameters for the hypergeometric distribution
N <- 100   # total number of items in the population
K <- 70    # number of successes
n <- 12    # sample size
x_vals <- 0:n  # Listing all possible values that X can take

# (a) Plotting a histogram for the probability mass function (PMF)
pdf_vals <- dhyper(x_vals, K, N-K, n)  # Calculating probabilities for each possible X value
barplot(pdf_vals, names.arg=x_vals, col="lightblue", 
        main="Hypergeometric Distribution", xlab="X", ylab="Probability")

# Adding text inside the plot to display parameter values
text(4, 0.2, labels=paste("N =", N, "\nK =", K, "\nn =", n), 
     cex=1.2, col="black")

# (b) Computing the cumulative probability P(X ≤ 10) and rounding it to 3 decimal places
cum_prob <- round(phyper(10, K, N-K, n), 3)
print(paste("Cumulative probability P(X ≤ 10):", cum_prob))

# (c) Finding the smallest X where cumulative probability P(X ≤ X) ≥ 0.9
x_for_0.9 <- qhyper(0.9, K, N-K, n)
print(paste("Smallest x where P(X ≤ x) >= 0.9:", x_for_0.9))

# (d) Drawing 5 random samples from the hypergeometric distribution
random_samples <- signif(rhyper(5, K, N-K, n), 2)  # Rounding to 2 significant digits
print("Random Samples from the Hypergeometric Distribution:")
print(random_samples)

# 3)

# (a) Plotting probability mass functions (PMFs) for p = 0.3 and p = 0.8

p1 <- 0.3  # Probability of success in the first case
p2 <- 0.8  # Probability of success in the second case
x_vals <- 1:15  # Possible trial numbers (Geometric distribution starts at 1)

# Setting up a 1x2 grid for plotting
par(mfrow=c(1,2)) 

# Plot for p = 0.3
barplot(dgeom(x_vals - 1, p1), names.arg=x_vals, col="maroon",
        main="Geometric PMF (p=0.3)", xlab="Trial Number", ylab="Probability")

# Plot for p = 0.8
barplot(dgeom(x_vals - 1, p2), names.arg=x_vals, col="lightyellow",
        main="Geometric PMF (p=0.8)", xlab="Trial Number", ylab="Probability")

# Resetting plot layout
par(mfrow=c(1,1)) 

# (b) Finding cumulative probability P(X ≤ 4)
cum_prob <- pgeom(4 - 1, p1)  # Adjusting for R's zero-based indexing
print(paste("Cumulative probability P(X ≤ 4):", round(cum_prob, 3)))

# (c) Finding the smallest trial number where cumulative probability is 0.2
m_for_0.2 <- qgeom(0.2, p1) + 1  # Converting to one-based indexing
print(paste("Smallest m where P(X ≤ m) >= 0.2:", m_for_0.2))

# (d) Generating 6 random samples from the Geometric distribution with p = 0.4
random_samples <- rgeom(6, 0.4) + 1  # Converting to one-based indexing
print("6 Random Samples:")
print(random_samples)

# 4)
# (a) Computing the probability P(Y = 5) for r = 3, p = 0.3
p <- 0.3   # Probability of success
r <- 3     # Number of successes desired
y <- 5     # Number of failures before getting r successes

prob <- dnbinom(y, size=r, prob=p)  # Computing probability
print(paste("Negative binomial probability P(Y=5):", round(prob, 4)))

# (b) Computing the cumulative probability P(Y ≤ 5)
cum_prob <- pnbinom(y, size=r, prob=p)
print(paste("Cumulative probability P(Y≤5):", round(cum_prob, 4)))

# (c) Finding the y-value where cumulative probability is 0.5 (median)
y_median <- qnbinom(0.5, size=r, prob=p)
print(paste("Median (y where P(Y ≤ y) = 0.5):", y_median))

# (d) Generating 4 random samples from the Negative Binomial distribution (r=3, p=0.3)
random_samples <- rnbinom(4, size=r, prob=p)
print("4 Random Samples:")
print(random_samples)

# (e) Plotting the negative binomial distribution for r=10, p=0.3
r_plot <- 10
y_vals <- 0:30  # Possible values for failures
prob_vals <- dnbinom(y_vals, size=r_plot, prob=p)

barplot(prob_vals, names.arg=y_vals, col="grey", 
        main="Negative Binomial Distribution (r=10, p=0.3)", 
        xlab="Number of Failures (y)", ylab="Probability")

# (f) Generating a frequency histogram for 10,000 random values (r=10, p=0.3)
samples <- rnbinom(10000, size=r_plot, prob=p)
hist(samples, breaks=30, col="hotpink", border="black", 
     main="Histogram of 10,000 Samples (r=10, p=0.3)", 
     xlab="Number of Failures (y)", ylab="Frequency")

# 5)
lambda <- 10
m <- 7

# poisson probabilities:
poisson_prob <- dpois(m, lambda)
print(poisson_prob)

# cummulative frequencies:

cumulative_prob <- ppois(m, lambda)
print(cumulative_prob)

# 
# (c) Binomial and Poisson distributions comparison
n <- 3000  # Number of trials
p <- 0.3   # Probability of success
lambda_np <- n * p  

# Binomial probability distribution
binomial_x <- 0:1000
binomial_pmf <- dbinom(binomial_x, n, p)

# Poisson probability distribution
poisson_x <- 0:1000
poisson_pmf <- dpois(poisson_x, lambda_np)

# Plotting Binomial Distribution
barplot(binomial_pmf, names.arg = binomial_x, col = "blue", 
        main = "Binomial Probability Distribution", 
        xlab = "k", ylab = "Probability", border = "black")

# Plotting Poisson Distribution
barplot(poisson_pmf, names.arg = poisson_x, col = "red", 
        main = "Poisson Probability Distribution", 
        xlab = "k", ylab = "Probability", border = "black")

# Parameters

n <- 3000   
p <- 0.3    
lambda_np <- n * p  

# Generate random samples
binomial_samples <- rbinom(30000, n, p)  # binomial Distribution
poisson_samples <- rpois(30000, lambda_np)  # Poisson Distribution

# Create a histogram for both distributions
hist(binomial_samples, probability = TRUE, col = "blue", 
     main = "Overlapping Histogram: Binomial vs Poisson", 
     xlab = "k", ylab = "probability density", breaks = 50)

hist(poisson_samples, probability = TRUE, col = "red" , 
     add = TRUE, breaks = 50)

legend("topright", legend = c("Binomial", "Poisson"), 
       fill = c("blue", "red"))


# (d) Find Poisson quantile value for cumulative probability 0.22 and λ = 10
# quantile_value <- qpois(0.22, lambda_val)
# print(quantile_value)

# (e) Generate 10000 random Poisson samples with λ = 9 and plot histogram
samples <- rpois(10000, 9)
samples_bin <- rbinom(10000, 30 ,0.3)

# install.packages("ggplot2") 
library(ggplot2)
# histogram
ggplot(data.frame(samples), aes(x = samples)) +
  geom_histogram(binwidth = 1, fill = "purple", color = "black", alpha = 0.7) +
  ggtitle("Histogram of Poisson Samples (λ=9)") +
  xlab("Value") + ylab("Frequency")

ggplot(data.frame(samples_bin), aes(x = samples_bin)) +
  geom_histogram(binwidth = 1, fill = "red", color = "black") +
  ggtitle("Histogram of binomial distribution") + 
  xlab("Value") + ylab("Frequency")

# Combining both the graphs
ggplot() +
  geom_histogram(data = data.frame(samples_bin), aes(x = samples_bin), binwidth = 1, fill = "blue", col = "black") +
  geom_histogram(data = data.frame(samples), aes(x = samples), binwidth = 1, fill = "red", col="black") +
  ggtitle("Poisson vs Binomial Histogram") +
  xlab("Value") + ylab("Frequency")

# 6)
# Gaussian(Normal) Distribution

# (a) Finding and printing normal PDF at µ=12, σ=2
mean_value <- 12
std_dev <- 2
pdf_value <- dnorm(mean_value, mean_value, std_dev)
print(pdf_value)


# (b) Finding and printing cumulative probability at Z=2, checking symmetry
cum_prob_2 <- pnorm(2)
symmetry_check <- 1 - pnorm(-2)
print(cum_prob_2)
print(symmetry_check)


# (c) Plotting normal curve with ±4σ range and adding text
x_vals <- seq(mean_value - 4 * std_dev, mean_value + 4 * std_dev, length = 100)
y_vals <- dnorm(x_vals, mean_value, std_dev)
plot(x_vals, y_vals, type = "l", col = "blue", main = "Normal Distribution (mean=12, std=2)")
text(mean_value, max(y_vals) * 0.8, labels = "mean=12, std=2", col = "red")
   
# (d) Finding and printing 75th percentile
quantile_75 <- qnorm(0.75, mean_value, std_dev)
print(quantile_75)

# (e) Sampling 10,000 values, plotting histogram with normal curve
set.seed(123)
samples <- rnorm(10000, mean_value, std_dev)
hist(samples, probability = TRUE, col = "grey", main = "Histogram of Normal Samples")
lines(x_vals, y_vals, col = "blue", lwd = 2)


# (f) Normalizing binomial distribution and comparing with normal curve
n_trials <- 3390
p_success <- 0.5
mean_binom <- n_trials * p_success
std_binom <- sqrt(n_trials * p_success * (1 - p_success))
binom_samples <- rbinom(10000, n_trials, p_success)
normalized_binom <- (binom_samples - mean_binom) / std_binom    # x - mean/ std 

hist(normalized_binom, probability = TRUE, col = "skyblue", main = "Normalized Binomial Distribution")
curve(dnorm(x), add = TRUE, col = "red", lwd = 2)

# ............/////.............
n_trials <- 100           
p_success <- 0.5         
n_samples <- 100000    
mean_binom <- n_trials * p_success
std_binom <- sqrt(n_trials * p_success * (1 - p_success))

binom_samples <- rbinom(n_samples, n_trials, p_success)
normalized_binom <- (binom_samples - mean_binom) / std_binom

hist(normalized_binom, probability = TRUE, col = "skyblue", main = "Normalized Binomial Distribution (n=100, p=0.5)", xlab = "Normalized Value")

curve(dnorm(x), add = TRUE, col = "red", lwd = 2)



# (g) Plotting Poisson PDFs for λ = 1, 10, 100, 1000 with normal approximation
par(mfrow = c(2,2))
lambda_vals <- c(1, 10, 100, 1000)
 
for (lambda in lambda_vals) {
  x_vals <- seq(0, lambda + 3 * sqrt(lambda), 1)
  poisson_pmf <- dpois(x_vals, lambda)
  barplot(poisson_pmf, names.arg = x_vals, col = "lightblue", main = paste("Poisson (λ=", lambda, ")"))
   
  normal_approx <- dnorm(x_vals, lambda, sqrt(lambda))
  lines(x_vals, normal_approx, col = "red", lwd = 2)
  
}

# (h) Generating correlated normal distributions
library(MASS)
set.seed(123)
data <- mvrnorm(1000, mu = c(50, 60), Sigma = matrix(c(4, 3.7, 3.7, 9), 2))
# data <- mvrnorm(1000, mu = c(50, 60), Sigma = matrix(c(4, 0.5, 0.5, 9), 2))
# (i) Checking variance and covariance
print(var(data))

# Extracting x and y, plotting them, printing variances
x_vals <- data[,1]
y_vals <- data[,2]
plot(x_vals, y_vals, col="blue", main = "Correlation Plot")
print(var(x_vals))
print(var(y_vals))

# Checking independence by comparing variances
print(var(x_vals) + var(y_vals))
print(var(x_vals + y_vals))
 
# Computing covariance from correlation and variances
corr_coeff <- cor(x_vals, y_vals)
computed_cov <- corr_coeff * sqrt(var(x_vals) * var(y_vals))
reported_cov <- var(data)[1,2]
print(computed_cov)
print(reported_cov)



# 7)
# Uniform Distribution
 
# (a) Generating and printing 5 uniform random numbers between 0 and 1
random_uniform_1 <- runif(5, 0, 1)
print(random_uniform_1)                                                              

# (b) Generating and printing 5 uniform random numbers between 50 and 100
random_uniform_50_100 <- runif(5, 50, 100)
print(random_uniform_50_100)
 
# (c) Generating 10,000 uniform random numbers and plotting histogram
uniform_samples <- runif(10000, 1, 2)
hist(uniform_samples, breaks = 30, probability = TRUE, col = "lightblue", 
     main = "Uniform Distribution (1 to 2)", xlim = c(1, 2))
 
# 8)
# Exponential Distribution
 
# (a) Computing probability density at x=3 for λ=2
lambda_exp <- 2
pdf_exp <- dexp(3, rate = lambda_exp)
print(pdf_exp)
 
# (b) Finding quantile value for cumulative probability 0.995
quantile_exp <- qexp(0.995, rate = lambda_exp)
print(quantile_exp)
 
# (c) Plotting exponential cumulative probability functions for λ = 2, 10, 100
lambda_values <- c(2, 10, 100)
x_vals <- seq(0, 1, length = 100)
 
plot(x_vals, pexp(x_vals, rate = lambda_values[1]), type = "l", col = "black",    ylim = c(0, 1), main = "Exponential CDFs", xlab = "x", ylab = "Cumulative Probability")
lines(x_vals, pexp(x_vals, rate = lambda_values[2]), col = "blue")
lines(x_vals, pexp(x_vals, rate = lambda_values[3]), col = "red")
legend("bottomright", legend = c("λ = 2", "λ = 10", "λ = 100"), col = c("black", "blue", "red"), lty = 1)

# (d) Generating and printing 4 random values from exponential distribution (λ=3)
random_exp_values <- rexp(4, rate = 3)
print(random_exp_values)


# 9)
# Gamma Distribution
# (a) Plotting Gamma PDFs for different α and θ values on a 1x2 grid
par(mfrow = c(1, 2))
# First graph: α varies, θ = 4
x_vals <- seq(0, 20, length = 100)
alpha_values <- c(1, 2, 3, 4)

theta_fixed <- 4
colors <- c("black", "blue", "red", "magenta")


plot(x_vals, dgamma(x_vals, shape = alpha_values[1], scale = theta), type = "l", col = colors[1],
     ylim = c(0, 0.15), main = "Gamma PDFs (theta = 4)", xlab = "x", ylab = "Density")
for (i in 2:4) {
  lines(x_vals, dgamma(x_vals, shape = alpha_values[i], scale = theta_fixed), col = colors[i])
}
legend("topright", legend = paste("α =", alpha_values), col = colors, lty = 1)

# Second graph: θ varies, α = 4
theta_values <- c(1, 2, 3, 4)
alpha_fixed <- 4

plot(x_vals, dgamma(x_vals, shape = alpha_fixed, scale = theta_values[1]), type = "l", col = colors[1],
     ylim = c(0, 0.15), main = "Gamma PDFs (alpha = 4)", xlab = "x", ylab = "Density")
for (i in 2:4) {
  lines(x_vals, dgamma(x_vals, shape = alpha_fixed, scale = theta_values[i]), col = colors[i])
}
legend("topright", legend = paste("theta =", theta_values), col = colors, lty = 1)

# (b) Computing probability density at x=6 for α=4, θ=1
pdf_gamma <- dgamma(6, shape = 4, scale = 1)
print(pdf_gamma)

# (c) Computing cumulative probability up to x=6
cum_prob_gamma <- pgamma(6, shape = 4, scale = 1)
print(cum_prob_gamma)

# (d) Finding x value corresponding to cumulative probability 0.95
quantile_gamma <- qgamma(0.95, shape = 4, scale = 1)
print(quantile_gamma)

# (e) Generating 10,000 random gamma deviates and plotting histogram
gamma_samples <- rgamma(10000, shape = 4, scale = 1)
hist(gamma_samples, breaks = 30, probability = TRUE, col = "lightblue", 
     main = "Gamma Distribution Samples", xlab = "x")
lines(x_vals, dgamma(x_vals, shape = 4, scale = 1), col = "red", lwd = 2)

# 10)
# Chi-square distribution

# Load necessary library
library(ggplot2)

# (a) Plot the χ2 distribution with degrees of freedom 2,3,5 and 10
df_values <- c(2, 3, 5, 10)
x_vals <- seq(0, 20, length.out = 500)
colors <- c("red", "blue", "green", "purple")

plot(x_vals, dchisq(x_vals, df_values[1]), type = "l", col = colors[1], lwd = 2,
     main = "Chi-square (χ²) Distribution", xlab = "x", ylab = "Density", ylim = c(0, 0.3))
for (i in 2:length(df_values)) {
  lines(x_vals, dchisq(x_vals, df_values[i]), col = colors[i], lwd = 2)
}
legend("topright", legend = paste("df =", df_values), col = colors, lwd = 2)

# (b) Compute and print the probability density for x=6 and 5 degrees of freedom
x <- 6
df <- 5
prob_density <- dchisq(x, df)
print(paste("Probability density at x=6 for df=5:", prob_density))

# (c) Compute and print the cumulative probability up to x=6 and 10 degrees of freedom
cum_prob_6_df5 <- pchisq(6, df = 5)
cum_prob_6_df10 <- pchisq(6, df = 10)
print(paste("Cumulative probability up to x=6 for df=5:", cum_prob_6_df5))
print(paste("Cumulative probability up to x=6 for df=10:", cum_prob_6_df10))

# (d) Obtain the 85th quantile for this distribution for 6 degrees of freedom
quantile_85 <- qchisq(0.85, df = 6)
print(paste("85th quantile for df=6:", quantile_85))

# (e) Plot histogram of 10,000 random deviates from this distribution with df=6
set.seed(123)  # For reproducibility
chi_sq_samples <- rchisq(10000, df = 6)
hist(chi_sq_samples, breaks = 30, col = "red", main = "Histogram of χ² Distribution (df=6)",
     xlab = "x", ylab = "Frequency", border = "black")
text(15, 500, "r=6", col = "blue", cex = 1.5)

# (f) Compute and plot the χ² PDF with 1 degree of freedom
mu <- 2
sigma <- 1
x_vals_z2 <- seq(-4, 8, length.out = 500)
z2_vals <- ((x_vals_z2 - mu)^2) / sigma^2

plot(x_vals_z2, dchisq(z2_vals, df = 1), type = "l", col = "blue", lwd = 2,
     main = "Chi-square PDF with 1 degree of freedom", xlab = "x", ylab = "Density")







