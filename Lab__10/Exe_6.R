# 6) Gaussian Distribution


mu = 12
sigma = 2
x <- mu

pdf_value <- dnorm(x,mean = mu,sd=sigma)
print(paste("PDF value at X=",mu,":",round(pdf_value,4)))


z <- 2
cpdf_z <- pnorm(z)
cpdf_neg_z <- pnorm(-z)

print(paste("CPDF at z=",cpdf_z,":",round(cpdf_z,4)))
print(paste("CPDF at negative z=",cpdf_neg_z,":",round(cpdf_neg_z,4)))
print(paste("Checking if CPDF and neg CPDF is same or not :",cpdf_z == (1 - cpdf_neg_z)))


x_vals <- seq(mu - 4*sigma, mu + 4*sigma, length.out=100)
y_vals <- dnorm(x_vals, mean=mu, sd=sigma)

plot(x_vals, y_vals, type="l", col="blue", lwd=2, main="Normal Distribution",
     xlab="X", ylab="Density")

text(mu, max(y_vals) * 0.9, labels=paste("μ =", mu, "\nσ =", sigma), col="red")

q_75 <- qnorm(0.75, mean=mu, sd=sigma)
print(paste("75th Quantile:", round(q_75, 4)))


#6e)
set.seed(42)
samples <- rnorm(10000, mean=mu, sd=sigma)

hist(samples, breaks=50, probability=TRUE, col="green", main="Random Deviates")
curve(dnorm(x, mean=mu, sd=sigma), col="red", lwd=2, add=TRUE)

# Set parameters
n <- 100  # Increase number of trials for better normal approximation
p <- 0.5
num_samples <- 10000  # More samples give a smoother histogram

# Generate binomial samples
m <- rbinom(num_samples, n, p)

# Standardize to get W
W <- (m - n*p) / sqrt(n*p*(1-p))

# Plot the histogram (density scaling)
hist(W, breaks=30, probability=TRUE, col="lightblue", border="black",
     main="Normalised Binomial Histogram", xlab="W", ylab="Density")

##overlay the standard normal curve
curve(dnorm(x, mean=0, sd=1), col="red", lwd=2, add=TRUE)


#6f) 
n <- 100 #no of trials
p <- 0.5 #probability
mu <- n * p #mean
sigma <- sqrt(n * p * (1 - p)) #std deviation

# binomial data
num_samples <- 10000
m <- rbinom(num_samples, size = n, prob = p) #no of success in n trials

# Normalize
W <- (m - mu) / sigma

# Plot histogram
hist(W, probability = TRUE, breaks = 30, col = "skyblue",
     main = "Normalized Binomial vs Standard Normal",
     xlab = "W", xlim = c(-4, 4))

# Overlay standard normal density
curve(dnorm(x), add = TRUE, col = "red", lwd = 2)



### 6g) Poisson Distributions vs Normal Approximation

lambda_vals <- c(1, 10, 100, 1000)
par(mfrow=c(2,2))

for (lambda in lambda_vals) {
  x_vals <- seq(0, 2 * lambda, by=1)
  pois_vals <- dpois(x_vals, lambda=lambda)
  
  # Poisson PMF plot
  plot(x_vals, pois_vals, type="h", col="blue", lwd=2,
       main=paste("Poisson (λ =", lambda, ") vs Normal"),
       xlab="X", ylab="Probability")
  
  # Normal approximation overlay
  norm_vals <- dnorm(x_vals, mean=lambda, sd=sqrt(lambda))
  lines(x_vals, norm_vals, col="darkgreen", lwd=2)
}

### 6h-i) Generate Correlated Normal Variables and Check Covariance

install.packages("MASS")  # Run only once
library(MASS)

# Generate correlated samples
xy <- mvrnorm(1000, mu=c(50, 60), Sigma=matrix(c(4, 3.7, 3.7, 9), 2, 2))

# Covariance matrix
print("h-i) Sample Covariance Matrix:")
print(var(xy))

### 6h-ii) Extract Vectors and Plot Correlation

x <- xy[,1]
y <- xy[,2]

# Scatter plot
plot(x, y, main="Scatter Plot of x vs y", col="darkgreen", pch=19)

# Variances
cat("Variance of x:", var(x), "\n")
cat("Variance of y:", var(y), "\n")

### 6h-iii) Check Independence by Variance of Sum

var_sum <- var(x + y)
sum_var <- var(x) + var(y)

cat("Variance of (x + y):", var_sum, "\n")
cat("Sum of variances:         ", sum_var, "\n")

### 6h-iv) Compute Covariance from Correlation

cov_xy_calc <- cor(x, y) * sqrt(var(x) * var(y))
cat("Computed Covariance from cor():", cov_xy_calc, "\n")
cat("Reported Covariance from var(xy):    ", var(xy)[1,2], "\n")


